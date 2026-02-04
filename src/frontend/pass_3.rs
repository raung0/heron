#![allow(clippy::result_large_err)]

use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

use crate::frontend::{
	AST, ASTValue, FrontendError, FrontendWarning, Lexer, Parser, SourceLocation,
	pass_1::pass_1, pass_2::pass_2,
};

pub type ModuleId = String;

#[derive(Clone)]
pub struct ResolvedProgram {
	pub entry: ModuleId,
	pub modules: HashMap<ModuleId, ModuleInfo>,
}

#[derive(Clone)]
pub struct ModuleInfo {
	pub id: ModuleId,
	pub file_path: String,
	pub package_path: Vec<String>,
	pub ast: Box<AST>,
	pub imports: ModuleImports,
	pub exports: ModuleExports,
}

#[derive(Clone)]
pub struct ModuleImports {
	pub alias_to_module: HashMap<String, ModuleId>,
}

#[derive(Clone)]
pub struct ModuleExports {
	pub types: HashMap<String, ExportStub>,
	pub constexprs: HashMap<String, ExportStub>,
}

#[derive(Clone)]
pub struct ExportStub {
	pub name: String,
	pub node: Box<AST>,
	pub location: SourceLocation,
}

pub struct Pass3Result {
	pub program: ResolvedProgram,
	pub errors: Vec<FrontendError>,
	pub warnings: Vec<FrontendWarning>,
}

pub fn dot_module_graph(program: &ResolvedProgram) -> String {
	let mut modules: Vec<&ModuleId> = program.modules.keys().collect();
	modules.sort();

	let mut edges: Vec<(ModuleId, ModuleId)> = Vec::new();
	for (module_id, module) in &program.modules {
		let mut imports: Vec<ModuleId> =
			module.imports.alias_to_module.values().cloned().collect();
		imports.sort();
		imports.dedup();
		for imported in imports {
			edges.push((module_id.clone(), imported));
		}
	}
	edges.sort();

	let mut out = String::new();
	out.push_str("digraph modules {\n");
	out.push_str("  rankdir=LR;\n");
	out.push_str("  node [shape=box, fontname=\"monospace\"];\n");
	for module_id in modules {
		out.push_str(&format!("  \"{}\";\n", module_id));
	}
	for (from, to) in edges {
		out.push_str(&format!("  \"{}\" -> \"{}\";\n", from, to));
	}
	out.push_str("}\n");
	out
}

pub(crate) fn pass_3(entry: Box<AST>, entry_file: &str, module_paths: &[String]) -> Pass3Result {
	let mut errors = Vec::new();
	let mut warnings = Vec::new();

	let roots = build_module_roots(entry_file, module_paths);
	let (entry_package, entry_package_loc) = match extract_package(&entry, entry_file) {
		Ok(found) => found,
		Err(err) => {
			errors.push(err);
			return Pass3Result {
				program: ResolvedProgram {
					entry: String::new(),
					modules: HashMap::new(),
				},
				errors,
				warnings,
			};
		}
	};

	let entry_id = entry_package.join(".");
	let mut modules = HashMap::new();
	let mut queue = VecDeque::new();
	queue.push_back(ModuleQueueItem {
		id: entry_id.clone(),
		file_path: entry_file.to_string(),
		ast: Some(entry),
	});

	while let Some(item) = queue.pop_front() {
		if modules.contains_key(&item.id) {
			continue;
		}

		let (ast, file_path, package_path, package_loc) = if let Some(ast) = item.ast {
			(
				ast,
				item.file_path,
				entry_package.clone(),
				entry_package_loc.clone(),
			)
		} else {
			match parse_module(&item.file_path) {
				ModuleParseResult::Ok { ast, .. } => {
					let ast = pass_1(ast);
					let mut ast = ast;
					let mut pass_errors = pass_2(&mut ast);
					errors.append(&mut pass_errors);
					match extract_package(&ast, &item.file_path) {
						Ok((package_path, package_loc)) => (
							ast,
							item.file_path,
							package_path,
							package_loc,
						),
						Err(err) => {
							errors.push(err);
							continue;
						}
					}
				}
				ModuleParseResult::Err(mut parse_errors) => {
					errors.append(&mut parse_errors);
					continue;
				}
			}
		};

		let expected_package = item.id.clone();
		let found_package = package_path.join(".");
		if expected_package != found_package {
			errors.push(FrontendError::PackageMismatch {
				location: package_loc,
				expected: expected_package.clone(),
				found: found_package,
			});
		}

		let uses = collect_uses(&ast);
		let import_result = resolve_imports(&uses, &roots, &mut warnings, &mut errors);
		let imports = import_result.imports;
		for resolved in import_result.modules {
			queue.push_back(ModuleQueueItem {
				id: resolved.id,
				file_path: resolved.file_path,
				ast: None,
			});
		}

		let exports = collect_exports(&ast, &mut errors);

		modules.insert(
			item.id.clone(),
			ModuleInfo {
				id: item.id,
				file_path,
				package_path,
				ast,
				imports,
				exports,
			},
		);
	}

	Pass3Result {
		program: ResolvedProgram {
			entry: entry_id,
			modules,
		},
		errors,
		warnings,
	}
}

struct ModuleQueueItem {
	id: ModuleId,
	file_path: String,
	ast: Option<Box<AST>>,
}

struct ResolvedImport {
	id: ModuleId,
	file_path: String,
}

struct ImportResolution {
	imports: ModuleImports,
	modules: Vec<ResolvedImport>,
}

struct UseDirective {
	path: Vec<String>,
	alias: Option<String>,
	location: SourceLocation,
}

enum ModuleParseResult {
	Ok { ast: Box<AST> },
	Err(Vec<FrontendError>),
}

fn build_module_roots(entry_file: &str, module_paths: &[String]) -> Vec<PathBuf> {
	let mut roots = Vec::new();
	let mut seen = HashSet::new();
	let entry_path = Path::new(entry_file);
	if let Some(parent) = entry_path.parent() {
		let normalized = normalize_root(parent);
		let key = normalized.to_string_lossy().to_string();
		if seen.insert(key) {
			roots.push(normalized);
		}
	}
	for path in module_paths {
		let normalized = normalize_root(Path::new(path));
		let key = normalized.to_string_lossy().to_string();
		if seen.insert(key) {
			roots.push(normalized);
		}
	}
	roots
}

fn normalize_root(path: &Path) -> PathBuf {
	std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

fn extract_package(
	ast: &AST,
	file_path: &str,
) -> Result<(Vec<String>, SourceLocation), FrontendError> {
	let mut package = None;
	let mut package_loc = None;
	if let ASTValue::ExprList(items) = &ast.v {
		for item in items {
			if let ASTValue::Package { path } = &item.v {
				package = Some(path.clone());
				package_loc = Some(item.location.clone());
				break;
			}
		}
	}

	match (package, package_loc) {
		(Some(path), Some(loc)) => Ok((path, loc)),
		_ => Err(FrontendError::MissingPackage(
			SourceLocation::new_from_file(file_path.to_string()),
		)),
	}
}

fn collect_uses(ast: &AST) -> Vec<UseDirective> {
	let mut uses = Vec::new();
	let ASTValue::ExprList(items) = &ast.v else {
		return uses;
	};

	for item in items {
		if let ASTValue::Use { path, alias } = &item.v {
			uses.push(UseDirective {
				path: path.clone(),
				alias: alias.clone(),
				location: item.location.clone(),
			});
		}
	}

	uses
}

fn resolve_imports(
	uses: &[UseDirective],
	roots: &[PathBuf],
	warnings: &mut Vec<FrontendWarning>,
	errors: &mut Vec<FrontendError>,
) -> ImportResolution {
	let mut alias_to_module = HashMap::new();
	let mut seen_modules = HashSet::new();
	let mut resolved = Vec::new();

	for use_item in uses {
		let module = use_item.path.join(".");
		if !seen_modules.insert(module.clone()) {
			errors.push(FrontendError::DuplicateModuleImport {
				location: use_item.location.clone(),
				module: module.clone(),
			});
			continue;
		}

		let alias = use_item
			.alias
			.clone()
			.unwrap_or_else(|| use_item.path.last().cloned().unwrap_or_default());
		if alias_to_module.contains_key(&alias) {
			errors.push(FrontendError::DuplicateModuleAlias {
				location: use_item.location.clone(),
				alias,
			});
			continue;
		}

		match resolve_module_path(
			&module,
			&use_item.path,
			roots,
			use_item.location.clone(),
			warnings,
		) {
			Ok(path) => {
				alias_to_module.insert(alias, module.clone());
				resolved.push(ResolvedImport {
					id: module,
					file_path: path.to_string_lossy().to_string(),
				});
			}
			Err(err) => {
				errors.push(err);
			}
		}
	}

	ImportResolution {
		imports: ModuleImports { alias_to_module },
		modules: resolved,
	}
}

fn resolve_module_path(
	module: &str,
	path: &[String],
	roots: &[PathBuf],
	location: SourceLocation,
	warnings: &mut Vec<FrontendWarning>,
) -> Result<PathBuf, FrontendError> {
	let mut matches = Vec::new();
	for root in roots {
		let candidate = root.join(PathBuf::from_iter(path)).join("mod.he");
		if candidate.is_file() {
			matches.push(candidate);
		}
	}

	if matches.is_empty() {
		return Err(FrontendError::ModuleNotFound {
			location,
			module: module.to_string(),
		});
	}

	if matches.len() > 1 {
		let chosen = matches.last().unwrap();
		let ignored_paths: Vec<String> = matches
			.iter()
			.take(matches.len() - 1)
			.map(|path| path.to_string_lossy().to_string())
			.collect();
		warnings.push(FrontendWarning::ModuleConflict {
			location,
			module: module.to_string(),
			chosen_path: chosen.to_string_lossy().to_string(),
			ignored_paths,
		});
	}

	Ok(matches.pop().unwrap())
}

fn parse_module(file_path: &str) -> ModuleParseResult {
	let input = match read_to_string(file_path) {
		Ok(input) => input,
		Err(_) => {
			return ModuleParseResult::Err(vec![FrontendError::ModuleNotFound {
				location: SourceLocation::new_from_file(file_path.to_string()),
				module: file_path.to_string(),
			}]);
		}
	};

	let mut lexer = Lexer::new(input, file_path.to_string());
	let mut parser = match Parser::new(&mut lexer) {
		Ok(parser) => parser,
		Err(err) => return ModuleParseResult::Err(vec![FrontendError::ParseError(err)]),
	};
	let ast = parser.parse();
	let parse_errors = parser.take_errors();
	if !parse_errors.is_empty() {
		return ModuleParseResult::Err(
			parse_errors
				.into_iter()
				.map(FrontendError::ParseError)
				.collect(),
		);
	}
	match ast {
		Ok(ast) => ModuleParseResult::Ok { ast },
		Err(err) => ModuleParseResult::Err(vec![FrontendError::ParseError(err)]),
	}
}

fn collect_exports(ast: &AST, errors: &mut Vec<FrontendError>) -> ModuleExports {
	let mut types = HashMap::new();
	let mut constexprs = HashMap::new();
	let ASTValue::ExprList(items) = &ast.v else {
		return ModuleExports { types, constexprs };
	};

	for item in items {
		let ASTValue::Pub(inner) = &item.v else {
			continue;
		};
		match &inner.v {
			ASTValue::DeclarationConstexpr(name, value) => {
				if is_type_value(value.as_ref()) {
					insert_export_value(
						&mut types,
						name.clone(),
						value.clone(),
						inner.location.clone(),
						errors,
					);
				} else {
					insert_export_value(
						&mut constexprs,
						name.clone(),
						value.clone(),
						inner.location.clone(),
						errors,
					);
				}
			}
			ASTValue::DeclarationMulti {
				names,
				values: Some(values),
				constexpr: true,
				..
			} => {
				for (idx, name) in names.iter().enumerate() {
					if let Some(value) = values.get(idx) {
						let target = if is_type_value(value.as_ref()) {
							&mut types
						} else {
							&mut constexprs
						};
						insert_export_value(
							target,
							name.clone(),
							value.clone(),
							inner.location.clone(),
							errors,
						);
					}
				}
			}
			_ => {}
		}
	}

	ModuleExports { types, constexprs }
}

fn is_type_value(node: &AST) -> bool {
	matches!(
		node.v,
		ASTValue::Struct { .. }
			| ASTValue::Enum { .. }
			| ASTValue::Union { .. }
			| ASTValue::RawUnion { .. }
			| ASTValue::Newtype { .. }
			| ASTValue::Alias { .. }
	)
}

fn insert_export_value(
	map: &mut HashMap<String, ExportStub>,
	name: String,
	value: Box<AST>,
	location: SourceLocation,
	errors: &mut Vec<FrontendError>,
) {
	if map.contains_key(&name) {
		errors.push(FrontendError::DuplicateExport { location, name });
		return;
	}
	map.insert(
		name.clone(),
		ExportStub {
			name,
			node: value,
			location,
		},
	);
}

#[cfg(test)]
mod tests {
	use super::*;
	use std::path::PathBuf;

	#[test]
	fn resolves_modules_from_custom_root() {
		let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("testdata/pass3");
		let entry_file = root.join("app/main.he");
		let module_root = root.join("modules");

		let ast = match parse_module(entry_file.to_str().expect("entry path")) {
			ModuleParseResult::Ok { ast } => ast,
			ModuleParseResult::Err(errs) => {
				panic!("parse error: {errs:?}");
			}
		};

		let module_paths = vec![module_root.to_string_lossy().to_string()];
		let result = pass_3(ast, entry_file.to_str().expect("entry path"), &module_paths);

		assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
		assert!(
			result.warnings.is_empty(),
			"warnings: {:?}",
			result.warnings
		);
		assert!(result.program.modules.contains_key("app"));
		assert!(result.program.modules.contains_key("core.io"));
		let app_module = result.program.modules.get("app").expect("app module");
		assert_eq!(
			app_module.imports.alias_to_module.get("io"),
			Some(&"core.io".to_string())
		);
	}
}
