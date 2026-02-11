use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::read_to_string;

use crate::frontend::{
	AST, ASTValue, BuiltinType, ConstExprKey, ConstValue, CtfeEngine, CtfeLimits,
	EnumVariantInfo, ExprKey, FieldInfo, FrontendError, GenericArg, GenericExprArgKey,
	GenericParam, InitializerItem, MatchCase, MatchCasePattern, MethodInfo, ModuleExports,
	ModuleId, ModuleImports, ResolvedGenericArg, ResolvedProgram, ResolvedType, SourceLocation,
	Type, TypeArena, TypeId, TypeLevelExprKey, TypedAst, TypedEnsuresClause, TypedEnumVariant,
	TypedFnBody, TypedFnParam, TypedGenericArg, TypedGenericParam, TypedInitializerItem,
	TypedMatchBinder, TypedMatchCase, TypedMatchCasePattern, TypedModule, TypedPostClause,
	TypedProgram, TypedValue,
};

#[path = "pass_4_type_level.rs"]
mod pass_4_type_level;

pub struct Pass4Result {
	pub program: TypedProgram,
	pub semantics: Semantics,
	pub errors: Vec<FrontendError>,
}

pub struct Semantics {
	pub entry: ModuleId,
	pub arena: TypeArena,
	pub builtins: BuiltinIds,
	pub modules: HashMap<ModuleId, ModuleState>,
	pub integer_types: HashMap<(u8, bool), TypeId>,
	pub type_locations: HashMap<TypeId, SourceLocation>,
	pub type_field_locations: HashMap<TypeId, HashMap<String, SourceLocation>>,
}

#[derive(Clone, Copy)]
pub struct BuiltinIds {
	pub bool_id: TypeId,
	pub rune_id: TypeId,
	pub void_id: TypeId,
	pub type_id: TypeId,
	pub int_id: TypeId,
	pub uint_id: TypeId,
	pub usize_id: TypeId,
	pub isize_id: TypeId,
	pub f32_id: TypeId,
	pub f64_id: TypeId,
	pub untyped_int_id: TypeId,
	pub untyped_float_id: TypeId,
	pub unknown_id: TypeId,
}

#[derive(Clone)]
pub struct ValueInfo {
	pub ty: TypeId,
	#[allow(dead_code)]
	pub constexpr: bool,
	pub mutable: bool,
	pub moved: bool,
	pub moved_at: Option<SourceLocation>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BorrowKind {
	Shared,
	Mutable,
}

#[derive(Clone)]
struct BorrowState {
	shared_locations: Vec<SourceLocation>,
	mutable_location: Option<SourceLocation>,
}

#[derive(Clone)]
struct RefBinding {
	base: String,
	kind: BorrowKind,
	is_static: bool,
}

#[derive(Clone)]
pub struct ModuleState {
	pub id: ModuleId,
	pub file_path: String,
	pub package_path: Vec<String>,
	pub ast: Box<AST>,
	pub imports: ModuleImports,
	pub exports: ModuleExports,
	pub types: HashMap<String, TypeId>,
	pub values: HashMap<String, ValueInfo>,
}

struct TypeContext<'a> {
	module_id: &'a ModuleId,
	module: &'a ModuleState,
	value_scopes: Vec<HashMap<String, ValueInfo>>,
	borrow_state: HashMap<String, BorrowState>,
	ref_scopes: Vec<HashMap<String, RefBinding>>,
	temporary_borrows: Vec<RefBinding>,
	generic_types: HashMap<String, TypeId>,
	self_type: Option<TypeId>,
	return_type: Option<TypeId>,
	in_constexpr: bool,
	unsafe_depth: usize,
	suppress_move: bool,
	suppress_borrow_check: bool,
}

struct Pass4State {
	program: ResolvedProgram,
	arena: TypeArena,
	builtins: BuiltinIds,
	modules: HashMap<ModuleId, ModuleState>,
	integer_types: HashMap<(u8, bool), TypeId>,
	unknown_type_errors: HashSet<(String, i32, i32, String)>,
	type_param_ops: HashMap<TypeId, Vec<(String, SourceLocation)>>,
	type_param_ops_by_name: HashMap<String, Vec<(String, SourceLocation)>>,
	type_param_members: HashMap<TypeId, Vec<(String, SourceLocation)>>,
	type_param_members_by_name: HashMap<String, Vec<(String, SourceLocation)>>,
	type_locations: HashMap<TypeId, SourceLocation>,
	type_field_locations: HashMap<TypeId, HashMap<String, SourceLocation>>,
	errors: Vec<FrontendError>,
}

pub enum MemberAccess<T> {
	Found(T),
	Inaccessible,
	Missing,
}

struct ParamEntry {
	name: String,
	has_default: bool,
}

pub(crate) fn pass_4(program: ResolvedProgram) -> Pass4Result {
	let mut state = Pass4State::new(program);
	state.collect_type_decls();
	state.define_type_decls();
	state.check_cyclic_type_decls();
	state.collect_value_decls();
	let typed_program = state.type_program();
	state.validate_constexpr_decls();
	let errors = std::mem::take(&mut state.errors);
	let semantics = state.into_semantics();
	Pass4Result {
		program: typed_program,
		semantics,
		errors,
	}
}

#[allow(clippy::borrowed_box, clippy::collapsible_if)]
impl Pass4State {
	fn new(program: ResolvedProgram) -> Self {
		let mut arena = TypeArena::new();
		let bool_id = arena.add(ResolvedType::Builtin(BuiltinType::Bool));
		let rune_id = arena.add(ResolvedType::Builtin(BuiltinType::Rune));
		let void_id = arena.add(ResolvedType::Builtin(BuiltinType::Void));
		let type_id = arena.add(ResolvedType::Builtin(BuiltinType::Type));
		let int_id = arena.add(ResolvedType::Builtin(BuiltinType::Integer {
			name: "int".to_string(),
			bit_size: None,
			signed: true,
		}));
		let uint_id = arena.add(ResolvedType::Builtin(BuiltinType::Integer {
			name: "uint".to_string(),
			bit_size: None,
			signed: false,
		}));
		let usize_id = arena.add(ResolvedType::Builtin(BuiltinType::Integer {
			name: "usize".to_string(),
			bit_size: None,
			signed: false,
		}));
		let isize_id = arena.add(ResolvedType::Builtin(BuiltinType::Integer {
			name: "isize".to_string(),
			bit_size: None,
			signed: true,
		}));
		let f32_id = arena.add(ResolvedType::Builtin(BuiltinType::Float {
			name: "f32".to_string(),
			bit_size: Some(32),
		}));
		let f64_id = arena.add(ResolvedType::Builtin(BuiltinType::Float {
			name: "f64".to_string(),
			bit_size: Some(64),
		}));
		let untyped_int_id = arena.add(ResolvedType::UntypedInt);
		let untyped_float_id = arena.add(ResolvedType::UntypedFloat);
		let unknown_id = arena.add(ResolvedType::Unknown);

		let builtins = BuiltinIds {
			bool_id,
			rune_id,
			void_id,
			type_id,
			int_id,
			uint_id,
			usize_id,
			isize_id,
			f32_id,
			f64_id,
			untyped_int_id,
			untyped_float_id,
			unknown_id,
		};

		let integer_types = HashMap::new();

		let mut modules = HashMap::new();
		for (id, module) in &program.modules {
			modules.insert(
				id.clone(),
				ModuleState {
					id: id.clone(),
					file_path: module.file_path.clone(),
					package_path: module.package_path.clone(),
					ast: module.ast.clone(),
					imports: ModuleImports {
						alias_to_module: module
							.imports
							.alias_to_module
							.clone(),
					},
					exports: ModuleExports {
						types: module.exports.types.clone(),
						constexprs: module.exports.constexprs.clone(),
					},
					types: HashMap::new(),
					values: HashMap::new(),
				},
			);
		}

		Self {
			program,
			arena,
			builtins,
			modules,
			integer_types,
			unknown_type_errors: HashSet::new(),
			type_param_ops: HashMap::new(),
			type_param_ops_by_name: HashMap::new(),
			type_param_members: HashMap::new(),
			type_param_members_by_name: HashMap::new(),
			type_locations: HashMap::new(),
			type_field_locations: HashMap::new(),
			errors: Vec::new(),
		}
	}

	fn into_semantics(self) -> Semantics {
		let Pass4State {
			program,
			arena,
			builtins,
			modules,
			integer_types,
			type_locations,
			type_field_locations,
			..
		} = self;
		Semantics {
			entry: program.entry,
			arena,
			builtins,
			modules,
			integer_types,
			type_locations,
			type_field_locations,
		}
	}

	fn collect_type_decls(&mut self) {
		let module_ids: Vec<ModuleId> = self.modules.keys().cloned().collect();
		for module_id in module_ids {
			let items = self.module_items(&module_id);
			for item in items {
				let node = Self::unwrap_pub(item.as_ref());
				match &node.v {
					ASTValue::DeclarationConstexpr(name, value) => {
						self.register_type_decl(
							&module_id,
							name,
							value.as_ref(),
						);
					}
					ASTValue::DeclarationMulti {
						names,
						values: Some(values),
						constexpr: true,
						..
					} => {
						for (idx, name) in names.iter().enumerate() {
							if let Some(value) = values.get(idx) {
								self.register_type_decl(
									&module_id,
									name,
									value.as_ref(),
								);
							}
						}
					}
					_ => {}
				}
			}
			self.add_builtin_type_names(&module_id);
		}
	}

	fn add_builtin_type_names(&mut self, module_id: &ModuleId) {
		let Some(module) = self.modules.get_mut(module_id) else {
			return;
		};
		let entries = [
			("bool", self.builtins.bool_id),
			("rune", self.builtins.rune_id),
			("void", self.builtins.void_id),
			("type", self.builtins.type_id),
			("int", self.builtins.int_id),
			("uint", self.builtins.uint_id),
			("usize", self.builtins.usize_id),
			("isize", self.builtins.isize_id),
			("f32", self.builtins.f32_id),
			("f64", self.builtins.f64_id),
		];
		for (name, id) in entries {
			module.types.insert(name.to_string(), id);
		}
	}

	fn register_type_decl(&mut self, module_id: &ModuleId, name: &str, value: &AST) {
		let Some(module) = self.modules.get_mut(module_id) else {
			return;
		};
		let ty_id = match &value.v {
			ASTValue::Struct { .. } => self.arena.add(ResolvedType::Struct {
				name: name.to_string(),
				module: module_id.clone(),
				extends: Vec::new(),
				fields: Vec::new(),
				methods: HashMap::new(),
			}),
			ASTValue::Enum { .. } => self.arena.add(ResolvedType::Enum {
				name: name.to_string(),
				module: module_id.clone(),
				variants: Vec::new(),
			}),
			ASTValue::Union { .. } => self.arena.add(ResolvedType::Union {
				name: name.to_string(),
				module: module_id.clone(),
				variants: Vec::new(),
				methods: HashMap::new(),
			}),
			ASTValue::RawUnion { .. } => self.arena.add(ResolvedType::RawUnion {
				name: name.to_string(),
				module: module_id.clone(),
				fields: Vec::new(),
			}),
			ASTValue::Newtype { .. } => self.arena.add(ResolvedType::Newtype {
				name: name.to_string(),
				module: module_id.clone(),
				underlying: self.builtins.unknown_id,
			}),
			ASTValue::Alias { .. } => self.arena.add(ResolvedType::Alias {
				name: name.to_string(),
				module: module_id.clone(),
				underlying: self.builtins.unknown_id,
			}),
			_ => return,
		};
		module.types.insert(name.to_string(), ty_id);
		self.type_locations
			.entry(ty_id)
			.or_insert_with(|| value.location.clone());
	}

	fn define_type_decls(&mut self) {
		let module_ids: Vec<ModuleId> = self.modules.keys().cloned().collect();
		for module_id in module_ids {
			let items = self.module_items(&module_id);
			for item in items {
				let node = Self::unwrap_pub(item.as_ref());
				match &node.v {
					ASTValue::DeclarationConstexpr(name, value) => {
						self.define_type_decl(&module_id, name, value);
					}
					ASTValue::DeclarationMulti {
						names,
						values: Some(values),
						constexpr: true,
						..
					} => {
						for (idx, name) in names.iter().enumerate() {
							if let Some(value) = values.get(idx) {
								self.define_type_decl(
									&module_id, name, value,
								);
							}
						}
					}
					_ => {}
				}
			}
		}
	}

	fn define_type_decl(&mut self, module_id: &ModuleId, name: &str, value: &AST) {
		let module = match self.modules.get(module_id) {
			Some(module) => module,
			None => return,
		};
		let ty_id = match module.types.get(name) {
			Some(&ty_id) => ty_id,
			None => return,
		};

		match &value.v {
			ASTValue::Struct {
				attributes,
				generics,
				extends,
				implements: _,
				body,
			} => {
				let struct_is_unsafe =
					attributes.iter().any(|attr| attr == "unsafe");
				let (typed_generics, mut generic_map) = self
					.resolve_generic_params(module_id, generics, None, None);
				let self_type = Some(ty_id);
				generic_map.insert("Self".to_string(), ty_id);
				let mut extends_ty = Vec::new();
				for ty in extends {
					extends_ty.push(self.resolve_type(
						module_id,
						ty,
						&generic_map,
						self_type,
						&value.location,
					));
				}
				if let ResolvedType::Struct { extends, .. } =
					self.arena.get_mut(ty_id)
				{
					*extends = extends_ty.clone();
				}
				let mut fields = Vec::new();
				let mut methods: HashMap<String, MethodInfo> = HashMap::new();
				let ASTValue::ExprList { items, .. } = &body.v else {
					return;
				};
				for item in items {
					self.collect_struct_member(
						module_id,
						item.as_ref(),
						&generic_map,
						self_type,
						&mut fields,
						&mut methods,
					);
				}

				if !struct_is_unsafe {
					for field in &fields {
						if self.type_contains_pointer(field.ty) {
							let location = self
								.type_field_locations
								.get(&ty_id)
								.and_then(|fields| {
									fields.get(&field.name)
								})
								.cloned()
								.unwrap_or_else(|| {
									value.location.clone()
								});
							self.errors.push(
								FrontendError::PointerRequiresUnsafe {
									location,
								},
							);
						}
					}
				}
				if let ResolvedType::Struct {
					extends,
					fields: dst_fields,
					methods: dst_methods,
					..
				} = self.arena.get_mut(ty_id)
				{
					*extends = extends_ty;
					*dst_fields = fields;
					*dst_methods = methods;
				}
				let _ = typed_generics;
			}
			ASTValue::Enum {
				variants,
				implements: _,
			} => {
				let mut info = Vec::new();
				for variant in variants {
					info.push(EnumVariantInfo {
						name: variant.name.clone(),
						ty: None,
					});
				}
				if let ResolvedType::Enum { variants: dst, .. } =
					self.arena.get_mut(ty_id)
				{
					*dst = info;
				}
			}
			ASTValue::Union {
				generics,
				variants,
				methods,
				implements: _,
			} => {
				let (_, generic_map) = self
					.resolve_generic_params(module_id, generics, None, None);
				let mut resolved_variants = Vec::new();
				for variant in variants {
					resolved_variants.push(self.resolve_type(
						module_id,
						variant,
						&generic_map,
						None,
						&value.location,
					));
				}
				let mut fields = Vec::new();
				let mut methods_map: HashMap<String, MethodInfo> = HashMap::new();
				for method in methods {
					self.collect_struct_member(
						module_id,
						method.as_ref(),
						&generic_map,
						Some(ty_id),
						&mut fields,
						&mut methods_map,
					);
				}
				for idx in 0..resolved_variants.len() {
					for prev in 0..idx {
						let lhs = resolved_variants[idx];
						let rhs = resolved_variants[prev];
						let is_duplicate = match (
							self.arena.get(lhs),
							self.arena.get(rhs),
						) {
							(
								ResolvedType::TypeParam(left),
								ResolvedType::TypeParam(right),
							) => left == right,
							(ResolvedType::TypeParam(_), _)
							| (_, ResolvedType::TypeParam(_)) => false,
							_ => self.type_matches(lhs, rhs),
						};
						if is_duplicate {
							self.errors.push(
								FrontendError::DuplicateUnionVariantType {
									location: value.location.clone(),
									ty: self.type_name(lhs),
								},
							);
							break;
						}
					}
				}
				for variant_ty in &resolved_variants {
					if self.type_contains_pointer(*variant_ty) {
						self.errors.push(
							FrontendError::PointerRequiresUnsafe {
								location: value.location.clone(),
							},
						);
						break;
					}
				}
				if let ResolvedType::Union {
					variants: dst,
					methods: dst_methods,
					..
				} = self.arena.get_mut(ty_id)
				{
					*dst = resolved_variants;
					*dst_methods = methods_map;
				}
			}
			ASTValue::RawUnion {
				generics,
				body,
				implements: _,
			} => {
				let (_, generic_map) = self
					.resolve_generic_params(module_id, generics, None, None);
				let mut fields = Vec::new();
				if let ASTValue::ExprList { items, .. } = &body.v {
					for item in items {
						let (node, is_public) = match &item.v {
							ASTValue::Pub(inner) => {
								(inner.as_ref(), true)
							}
							_ => (item.as_ref(), false),
						};
						if let ASTValue::DeclarationMulti {
							names,
							types,
							..
						} = &node.v
						{
							for (index, field_name) in
								names.iter().enumerate()
							{
								let ty = types
									.get(index)
									.or_else(|| types.first());
								if let Some(ty) = ty {
									let resolved = self
										.resolve_type(
										module_id,
										ty,
										&generic_map,
										None,
										&node.location,
									);
									fields.push(FieldInfo {
										name: field_name
											.clone(),
										ty: resolved,
										public: is_public,
									});
									self.record_field_location(
										Some(ty_id),
										field_name,
										node.location
											.clone(),
									);
									if self.type_contains_pointer(resolved) {
									self.errors.push(
										FrontendError::PointerRequiresUnsafe {
											location: node.location.clone(),
										},
									);
								}
								}
							}
						}
					}
				}
				if let ResolvedType::RawUnion { fields: dst, .. } =
					self.arena.get_mut(ty_id)
				{
					*dst = fields;
				}
			}
			ASTValue::Newtype { underlying, .. } => {
				let resolved = self.resolve_type(
					module_id,
					underlying,
					&HashMap::new(),
					None,
					&value.location,
				);
				if self.type_contains_pointer(resolved) {
					self.errors.push(FrontendError::PointerRequiresUnsafe {
						location: value.location.clone(),
					});
				}
				if let ResolvedType::Newtype {
					underlying: dst, ..
				} = self.arena.get_mut(ty_id)
				{
					*dst = resolved;
				}
			}
			ASTValue::Alias { underlying } => {
				let resolved = self.resolve_type(
					module_id,
					underlying,
					&HashMap::new(),
					None,
					&value.location,
				);
				if self.type_contains_pointer(resolved) {
					self.errors.push(FrontendError::PointerRequiresUnsafe {
						location: value.location.clone(),
					});
				}
				if let ResolvedType::Alias {
					underlying: dst, ..
				} = self.arena.get_mut(ty_id)
				{
					*dst = resolved;
				}
			}
			_ => {}
		}
	}

	fn check_cyclic_type_decls(&mut self) {
		let mut seen = HashSet::new();
		let mut visiting = HashSet::new();
		let mut reported = HashSet::new();
		let mut stack: Vec<(TypeId, Option<String>, Option<SourceLocation>)> = Vec::new();
		let mut type_ids = Vec::new();
		for module in self.modules.values() {
			for &ty in module.types.values() {
				type_ids.push(ty);
			}
		}
		type_ids.sort();
		type_ids.dedup();
		for ty_id in type_ids {
			self.visit_type_cycle(
				ty_id,
				&mut seen,
				&mut visiting,
				&mut reported,
				&mut stack,
			);
		}
	}

	fn visit_type_cycle(
		&mut self,
		ty_id: TypeId,
		seen: &mut HashSet<TypeId>,
		visiting: &mut HashSet<TypeId>,
		reported: &mut HashSet<TypeId>,
		stack: &mut Vec<(TypeId, Option<String>, Option<SourceLocation>)>,
	) {
		if seen.contains(&ty_id) {
			return;
		}
		if visiting.contains(&ty_id) {
			if reported.insert(ty_id) {
				let location = self.type_definition_location(ty_id);
				let type_name = self.type_name(ty_id);
				let cycle = self.format_cycle_path(stack, ty_id, None);
				let cycle_locations =
					self.cycle_locations_from_stack(stack, ty_id, None, None);
				self.errors.push(FrontendError::CyclicTypeDefinition {
					location,
					type_name,
					cycle,
					cycle_locations,
				});
			}
			return;
		}
		visiting.insert(ty_id);
		stack.push((ty_id, None, None));
		let deps = self.type_dependency_edges(ty_id);
		for (dep, label, edge_location) in deps {
			if visiting.contains(&dep) {
				if reported.insert(dep) {
					let location = self.type_definition_location(dep);
					let type_name = self.type_name(dep);
					let cycle =
						self.format_cycle_path(stack, dep, Some(&label));
					let cycle_locations = self.cycle_locations_from_stack(
						stack,
						dep,
						Some(&label),
						edge_location.clone(),
					);
					self.errors.push(FrontendError::CyclicTypeDefinition {
						location,
						type_name,
						cycle,
						cycle_locations,
					});
				}
				continue;
			}
			stack.push((dep, Some(label), edge_location));
			self.visit_type_cycle(dep, seen, visiting, reported, stack);
			stack.pop();
		}
		stack.pop();
		visiting.remove(&ty_id);
		seen.insert(ty_id);
	}

	fn format_cycle_path(
		&self,
		stack: &[(TypeId, Option<String>, Option<SourceLocation>)],
		start: TypeId,
		closing_label: Option<&str>,
	) -> String {
		let start_idx = stack
			.iter()
			.position(|(ty_id, _, _)| *ty_id == start)
			.unwrap_or(0);
		let mut out = self.type_name(start);
		for entry in &stack[(start_idx + 1)..] {
			let Some(label) = entry.1.as_deref() else {
				continue;
			};
			let type_name = self.type_name(entry.0);
			out.push_str(&format!(".{label} -> {type_name}"));
		}
		if let Some(label) = closing_label {
			let type_name = self.type_name(start);
			out.push_str(&format!(".{label} -> {type_name}"));
		}
		out
	}

	fn cycle_locations_from_stack(
		&self,
		stack: &[(TypeId, Option<String>, Option<SourceLocation>)],
		start: TypeId,
		closing_label: Option<&str>,
		closing_location: Option<SourceLocation>,
	) -> Vec<(SourceLocation, String)> {
		let start_idx = stack
			.iter()
			.position(|(ty_id, _, _)| *ty_id == start)
			.unwrap_or(0);
		let mut out = Vec::new();
		for entry in &stack[(start_idx + 1)..] {
			let Some(label) = entry.1.as_deref() else {
				continue;
			};
			let Some(location) = entry.2.clone() else {
				continue;
			};
			out.push((location, format!("field `{label}`")));
		}
		if let (Some(label), Some(location)) = (closing_label, closing_location) {
			out.push((location, format!("field `{label}`")));
		}
		out
	}

	fn type_definition_location(&self, ty_id: TypeId) -> SourceLocation {
		if let Some(location) = self.type_locations.get(&ty_id) {
			return location.clone();
		}
		let module_id = match self.arena.get(ty_id) {
			ResolvedType::Struct { module, .. }
			| ResolvedType::Enum { module, .. }
			| ResolvedType::Union { module, .. }
			| ResolvedType::RawUnion { module, .. }
			| ResolvedType::Newtype { module, .. }
			| ResolvedType::Alias { module, .. } => Some(module.clone()),
			ResolvedType::GenericInstance { base, .. } => match self.arena.get(*base) {
				ResolvedType::Struct { module, .. }
				| ResolvedType::Enum { module, .. }
				| ResolvedType::Union { module, .. }
				| ResolvedType::RawUnion { module, .. }
				| ResolvedType::Newtype { module, .. }
				| ResolvedType::Alias { module, .. } => Some(module.clone()),
				_ => None,
			},
			_ => None,
		};
		module_id
			.map(|module_id| self.module_location(&module_id))
			.unwrap_or_else(|| self.module_location(&self.program.entry))
	}

	fn type_dependency_edges(
		&self,
		ty_id: TypeId,
	) -> Vec<(TypeId, String, Option<SourceLocation>)> {
		match self.arena.get(ty_id) {
			ResolvedType::Struct {
				fields, extends, ..
			} => {
				let mut deps = Vec::new();
				for extend in extends {
					if let Some(dep) = self.by_value_dep(*extend) {
						deps.push((dep, "extends".to_string(), None));
					}
				}
				for field in fields {
					if let Some(dep) = self.by_value_dep(field.ty) {
						let location = self
							.type_field_locations
							.get(&ty_id)
							.and_then(|fields| fields.get(&field.name))
							.cloned();
						deps.push((dep, field.name.clone(), location));
					}
				}
				deps
			}
			ResolvedType::Union { variants, .. } => variants
				.iter()
				.filter_map(|ty| {
					self.by_value_dep(*ty)
						.map(|dep| (dep, "variant".to_string(), None))
				})
				.collect(),
			ResolvedType::RawUnion { fields, .. } => fields
				.iter()
				.filter_map(|field| {
					self.by_value_dep(field.ty).map(|dep| {
						let location = self
							.type_field_locations
							.get(&ty_id)
							.and_then(|fields| fields.get(&field.name))
							.cloned();
						(dep, field.name.clone(), location)
					})
				})
				.collect(),
			ResolvedType::Newtype { underlying, .. }
			| ResolvedType::Alias { underlying, .. } => self
				.by_value_dep(*underlying)
				.map(|dep| vec![(dep, "underlying".to_string(), None)])
				.unwrap_or_default(),
			ResolvedType::Array { underlying, .. }
			| ResolvedType::CArray { underlying } => self
				.by_value_dep(*underlying)
				.map(|dep| vec![(dep, "element".to_string(), None)])
				.unwrap_or_default(),
			ResolvedType::GenericInstance { base, .. } => {
				vec![(*base, "base".to_string(), None)]
			}
			_ => Vec::new(),
		}
	}

	fn by_value_dep(&self, ty_id: TypeId) -> Option<TypeId> {
		match self.arena.get(ty_id) {
			ResolvedType::Reference { .. }
			| ResolvedType::Pointer { .. }
			| ResolvedType::Slice { .. }
			| ResolvedType::Fn { .. }
			| ResolvedType::Builtin(..)
			| ResolvedType::TypeParam(_)
			| ResolvedType::UntypedInt
			| ResolvedType::UntypedFloat
			| ResolvedType::Unknown => None,
			ResolvedType::Array { underlying, .. }
			| ResolvedType::CArray { underlying } => Some(*underlying),
			ResolvedType::GenericInstance { base, .. } => Some(*base),
			ResolvedType::Alias { underlying, .. }
			| ResolvedType::Newtype { underlying, .. } => Some(*underlying),
			ResolvedType::Struct { .. }
			| ResolvedType::Enum { .. }
			| ResolvedType::Union { .. }
			| ResolvedType::RawUnion { .. } => Some(ty_id),
		}
	}

	fn collect_struct_member(
		&mut self,
		module_id: &ModuleId,
		node: &AST,
		generic_map: &HashMap<String, TypeId>,
		self_type: Option<TypeId>,
		fields: &mut Vec<FieldInfo>,
		methods: &mut HashMap<String, MethodInfo>,
	) {
		let (node, is_public) = match &node.v {
			ASTValue::Pub(inner) => (inner.as_ref(), true),
			_ => (node, false),
		};
		match &node.v {
			ASTValue::DeclarationMulti {
				names,
				types,
				values,
				..
			} => {
				for (index, field_name) in names.iter().enumerate() {
					if let Some(ty) = types.get(index).or_else(|| types.first())
					{
						let resolved = self.resolve_type(
							module_id,
							ty,
							generic_map,
							self_type,
							&node.location,
						);
						fields.push(FieldInfo {
							name: field_name.clone(),
							ty: resolved,
							public: is_public,
						});
						self.record_field_location(
							self_type,
							field_name,
							node.location.clone(),
						);
						continue;
					}
					let Some(values) = values else {
						continue;
					};
					let Some(value) = values.get(index) else {
						continue;
					};
					if let Some(fn_ty) = self.method_type_from_value(
						module_id,
						field_name,
						value,
						generic_map,
						self_type,
						&node.location,
					) {
						methods.insert(
							field_name.clone(),
							MethodInfo {
								ty: fn_ty,
								public: is_public,
							},
						);
					}
				}
			}
			ASTValue::DeclarationConstexpr(field_name, value) => {
				if let Some(fn_ty) = self.method_type_from_value(
					module_id,
					field_name,
					value,
					generic_map,
					self_type,
					&node.location,
				) {
					methods.insert(
						field_name.clone(),
						MethodInfo {
							ty: fn_ty,
							public: is_public,
						},
					);
				}
			}
			_ => {}
		}
	}

	fn fn_type_from_value(
		&mut self,
		module_id: &ModuleId,
		value: &AST,
		generic_map: &HashMap<String, TypeId>,
		self_type: Option<TypeId>,
		location: &SourceLocation,
	) -> Option<TypeId> {
		let ASTValue::Fn {
			generics,
			params,
			return_type,
			..
		} = &value.v
		else {
			return None;
		};
		let (_, method_generic_map) = self.resolve_generic_params(
			module_id,
			generics,
			Some(generic_map),
			self_type,
		);
		Some(self.fn_type_from_signature(
			module_id,
			params,
			return_type.as_ref(),
			&method_generic_map,
			self_type,
			location,
		))
	}

	fn operator_missing_self(&self, params: &[crate::frontend::FnParam]) -> bool {
		params.first()
			.is_none_or(|param| !param.names.iter().any(|name| name == "self"))
	}

	fn operator_self_hint(&self, self_type: Option<TypeId>) -> String {
		let name = self_type
			.map(|ty| self.type_name(ty))
			.unwrap_or_else(|| "Self".to_string());
		format!("add `self: {name}` as the first parameter")
	}

	fn is_numeric_operator(&self, op: &crate::frontend::Operator) -> bool {
		matches!(
			op,
			crate::frontend::Operator::Add
				| crate::frontend::Operator::Sub
				| crate::frontend::Operator::Mul
				| crate::frontend::Operator::Divide
		)
	}

	fn is_copy_type(&self, ty: TypeId) -> bool {
		match self.arena.get(ty) {
			ResolvedType::Builtin(_) => true,
			ResolvedType::Pointer { .. } => true,
			ResolvedType::Reference { mutable, .. } => !*mutable,
			ResolvedType::UntypedInt | ResolvedType::UntypedFloat => true,
			ResolvedType::Alias { underlying, .. } => self.is_copy_type(*underlying),
			ResolvedType::GenericInstance { base, .. } => self.is_copy_type(*base),
			_ => false,
		}
	}

	fn is_static_mut_ref(&self, ty: TypeId) -> bool {
		match self.arena.get(ty) {
			ResolvedType::Reference {
				mutable: true,
				lifetime: Some('s'),
				..
			} => true,
			ResolvedType::Alias { underlying, .. } => {
				self.is_static_mut_ref(*underlying)
			}
			ResolvedType::GenericInstance { base, .. } => self.is_static_mut_ref(*base),
			_ => false,
		}
	}

	fn type_contains_pointer(&self, ty: TypeId) -> bool {
		match self.arena.get(ty) {
			ResolvedType::Pointer { .. } => true,
			ResolvedType::Array { underlying, .. }
			| ResolvedType::Slice { underlying }
			| ResolvedType::CArray { underlying }
			| ResolvedType::Reference { underlying, .. }
			| ResolvedType::Alias { underlying, .. } => self.type_contains_pointer(*underlying),
			ResolvedType::GenericInstance { base, args } => {
				self.type_contains_pointer(*base)
					|| args.iter().any(|arg| match arg {
						ResolvedGenericArg::Type(arg_ty) => {
							self.type_contains_pointer(*arg_ty)
						}
						_ => false,
					})
			}
			ResolvedType::Fn {
				params,
				return_type,
			} => {
				params.iter()
					.any(|param| self.type_contains_pointer(*param))
					|| self.type_contains_pointer(*return_type)
			}
			_ => false,
		}
	}

	fn is_module_scope(&self, ctx: &TypeContext) -> bool {
		ctx.return_type.is_none() && ctx.value_scopes.len() <= 2
	}

	fn is_unsafe_context(&self, ctx: &TypeContext) -> bool {
		ctx.unsafe_depth > 0
	}

	#[allow(clippy::only_used_in_recursion)]
	fn lhs_base_id<'a>(&self, node: &'a AST) -> Option<&'a str> {
		match &node.v {
			ASTValue::Id(name) => Some(name.as_str()),
			ASTValue::Deref(inner) => self.lhs_base_id(inner),
			ASTValue::BinExpr {
				op: crate::frontend::Operator::Dot,
				lhs,
				..
			} => self.lhs_base_id(lhs),
			ASTValue::Index { target, .. } => self.lhs_base_id(target),
			ASTValue::ExprList { items, .. }
			| ASTValue::ExprListNoScope { items, .. } => {
				items.last().and_then(|item| self.lhs_base_id(item))
			}
			_ => None,
		}
	}

	#[allow(clippy::only_used_in_recursion)]
	fn top_level_ref<'a>(&self, node: &'a AST) -> Option<(bool, &'a AST)> {
		match &node.v {
			ASTValue::Ref { mutable, v } => Some((*mutable, v.as_ref())),
			ASTValue::ExprListNoScope { items, .. } if items.len() == 1 => {
				items.first().and_then(|item| self.top_level_ref(item))
			}
			_ => None,
		}
	}

	fn scope_index_for_value(&self, ctx: &TypeContext, name: &str) -> Option<usize> {
		for (idx, scope) in ctx.value_scopes.iter().enumerate().rev() {
			if scope.contains_key(name) {
				return Some(idx);
			}
		}
		if ctx.module.values.contains_key(name) {
			return Some(0);
		}
		None
	}

	fn lookup_value_info(&self, ctx: &TypeContext, name: &str) -> Option<ValueInfo> {
		for scope in ctx.value_scopes.iter().rev() {
			if let Some(info) = scope.get(name) {
				return Some(info.clone());
			}
		}
		ctx.module.values.get(name).cloned()
	}

	fn is_static_ref_type(&self, ty: TypeId) -> bool {
		match self.arena.get(ty) {
			ResolvedType::Reference {
				lifetime: Some('s'),
				..
			} => true,
			ResolvedType::Alias { underlying, .. } => {
				self.is_static_ref_type(*underlying)
			}
			ResolvedType::GenericInstance { base, .. } => {
				self.is_static_ref_type(*base)
			}
			_ => false,
		}
	}

	fn borrow_conflicts(&self, ctx: &TypeContext, name: &str, kind: BorrowKind) -> bool {
		let Some(state) = ctx.borrow_state.get(name) else {
			return false;
		};
		match kind {
			BorrowKind::Shared => state.mutable_location.is_some(),
			BorrowKind::Mutable => {
				state.mutable_location.is_some()
					|| !state.shared_locations.is_empty()
			}
		}
	}

	fn push_scope(&self, ctx: &mut TypeContext) {
		ctx.value_scopes.push(HashMap::new());
		ctx.ref_scopes.push(HashMap::new());
	}

	fn pop_scope(&mut self, ctx: &mut TypeContext) {
		if let Some(bindings) = ctx.ref_scopes.pop() {
			for (_, binding) in bindings {
				self.release_borrow(ctx, &binding);
			}
		}
		ctx.value_scopes.pop();
	}

	fn register_borrow(
		&mut self,
		ctx: &mut TypeContext,
		name: &str,
		kind: BorrowKind,
		location: &SourceLocation,
	) -> bool {
		let state = ctx
			.borrow_state
			.entry(name.to_string())
			.or_insert(BorrowState {
				shared_locations: Vec::new(),
				mutable_location: None,
			});
		match kind {
			BorrowKind::Shared => {
				if state.mutable_location.is_some() {
					self.errors.push(FrontendError::BorrowSharedWhileMut {
						location: location.clone(),
						name: name.to_string(),
						borrowed_at: state.mutable_location.clone(),
					});
					return false;
				}
				state.shared_locations.push(location.clone());
			}
			BorrowKind::Mutable => {
				if state.mutable_location.is_some()
					|| !state.shared_locations.is_empty()
				{
					let borrowed_at =
						state.mutable_location.clone().or_else(|| {
							state.shared_locations.first().cloned()
						});
					self.errors.push(FrontendError::BorrowMutWhileShared {
						location: location.clone(),
						name: name.to_string(),
						borrowed_at,
					});
					return false;
				}
				state.mutable_location = Some(location.clone());
			}
		}
		true
	}

	fn release_borrow(&mut self, ctx: &mut TypeContext, binding: &RefBinding) {
		if binding.is_static {
			return;
		}
		let Some(state) = ctx.borrow_state.get_mut(&binding.base) else {
			return;
		};
		match binding.kind {
			BorrowKind::Shared => {
				state.shared_locations.pop();
			}
			BorrowKind::Mutable => {
				state.mutable_location = None;
			}
		}
		if state.shared_locations.is_empty() && state.mutable_location.is_none() {
			ctx.borrow_state.remove(&binding.base);
		}
	}

	fn release_binding_for_name(&mut self, ctx: &mut TypeContext, name: &str) {
		for scope in ctx.ref_scopes.iter_mut().rev() {
			if let Some(binding) = scope.remove(name) {
				self.release_borrow(ctx, &binding);
				break;
			}
		}
	}

	fn record_temporary_borrow(&mut self, ctx: &mut TypeContext, binding: RefBinding) {
		ctx.temporary_borrows.push(binding);
	}

	fn promote_temporary_borrow(
		&mut self,
		ctx: &mut TypeContext,
		binding_name: &str,
		binding: RefBinding,
		location: &SourceLocation,
	) {
		let mut removed = None;
		if let Some(index) = ctx
			.temporary_borrows
			.iter()
			.position(|entry| entry.base == binding.base && entry.kind == binding.kind)
		{
			removed = Some(ctx.temporary_borrows.remove(index));
		}
		if removed.is_none() {
			if binding.kind == BorrowKind::Mutable {
				if let Some(info) = self.lookup_value_info(ctx, &binding.base) {
					if !info.mutable {
						return;
					}
				}
			}
			if self.borrow_conflicts(ctx, &binding.base, binding.kind) {
				return;
			}
			if !self.register_borrow(ctx, &binding.base, binding.kind, location) {
				return;
			}
		}
		let Some(scope_idx) = self.scope_index_for_value(ctx, binding_name) else {
			return;
		};
		if let Some(scope) = ctx.ref_scopes.get_mut(scope_idx) {
			scope.insert(binding_name.to_string(), binding);
		}
	}

	fn clear_temporary_borrows(&mut self, ctx: &mut TypeContext) {
		let borrows = std::mem::take(&mut ctx.temporary_borrows);
		for binding in borrows {
			self.release_borrow(ctx, &binding);
		}
	}

	fn mark_moved(&mut self, ctx: &mut TypeContext, name: &str, location: &SourceLocation) {
		if !ctx.suppress_borrow_check {
			if let Some(state) = ctx.borrow_state.get(name) {
				if !state.shared_locations.is_empty()
					|| state.mutable_location.is_some()
				{
					let borrowed_at =
						state.mutable_location.clone().or_else(|| {
							state.shared_locations.first().cloned()
						});
					self.errors.push(FrontendError::MoveWhileBorrowed {
						location: location.clone(),
						name: name.to_string(),
						borrowed_at,
					});
					return;
				}
			}
		}
		for scope in ctx.value_scopes.iter_mut().rev() {
			if let Some(info) = scope.get_mut(name) {
				if info.moved {
					self.errors.push(FrontendError::UseAfterMove {
						location: location.clone(),
						name: name.to_string(),
						moved_at: info.moved_at.clone(),
					});
					return;
				}
				if !self.is_copy_type(info.ty) {
					info.moved = true;
					info.moved_at = Some(location.clone());
				}
				return;
			}
		}
	}

	fn record_field_location(
		&mut self,
		self_type: Option<TypeId>,
		field_name: &str,
		location: SourceLocation,
	) {
		let Some(type_id) = self_type else {
			return;
		};
		self.type_field_locations
			.entry(type_id)
			.or_default()
			.entry(field_name.to_string())
			.or_insert(location);
	}

	fn record_type_param_op(&mut self, ty: TypeId, op_name: &str, location: &SourceLocation) {
		self.type_param_ops
			.entry(ty)
			.or_default()
			.push((op_name.to_string(), location.clone()));
		if let ResolvedType::TypeParam(name) = self.arena.get(ty) {
			self.type_param_ops_by_name
				.entry(name.clone())
				.or_default()
				.push((op_name.to_string(), location.clone()));
		}
	}

	fn record_numeric_type_param_ops(
		&mut self,
		lhs_ty: TypeId,
		rhs_ty: TypeId,
		op_name: &str,
		location: &SourceLocation,
	) {
		for ty in [lhs_ty, rhs_ty] {
			if matches!(self.arena.get(ty), ResolvedType::TypeParam(_)) {
				self.record_type_param_op(ty, op_name, location);
			}
		}
	}

	fn record_type_param_member(
		&mut self,
		ty: TypeId,
		member: &str,
		location: &SourceLocation,
	) {
		self.type_param_members
			.entry(ty)
			.or_default()
			.push((member.to_string(), location.clone()));
		if let ResolvedType::TypeParam(name) = self.arena.get(ty) {
			self.type_param_members_by_name
				.entry(name.clone())
				.or_default()
				.push((member.to_string(), location.clone()));
		}
	}

	fn method_type_from_value(
		&mut self,
		module_id: &ModuleId,
		method_name: &str,
		value: &AST,
		generic_map: &HashMap<String, TypeId>,
		self_type: Option<TypeId>,
		location: &SourceLocation,
	) -> Option<TypeId> {
		let ASTValue::Fn {
			generics,
			params,
			return_type,
			..
		} = &value.v
		else {
			return None;
		};
		let (_, method_generic_map) = self.resolve_generic_params(
			module_id,
			generics,
			Some(generic_map),
			self_type,
		);
		let missing_self =
			method_name.starts_with("operator") && self.operator_missing_self(params);
		if missing_self {
			let hint = self.operator_self_hint(self_type);
			self.errors.push(FrontendError::MissingOperatorSelf {
				location: value.location.clone(),
				operator: method_name.to_string(),
				hint,
			});
		}
		let fn_ty = if missing_self {
			self.fn_type_from_signature_inner(
				module_id,
				params,
				return_type.as_ref(),
				&method_generic_map,
				self_type,
				location,
				true,
			)
		} else {
			self.fn_type_from_signature(
				module_id,
				params,
				return_type.as_ref(),
				&method_generic_map,
				self_type,
				location,
			)
		};
		Some(fn_ty)
	}

	fn insert_value_decl(
		&mut self,
		module_id: &ModuleId,
		name: &str,
		ty: TypeId,
		constexpr: bool,
		mutable: bool,
	) {
		if let Some(module) = self.modules.get_mut(module_id) {
			module.values.insert(
				name.to_string(),
				ValueInfo {
					ty,
					constexpr,
					mutable,
					moved: false,
					moved_at: None,
				},
			);
		}
	}

	fn collect_value_decls(&mut self) {
		let module_ids: Vec<ModuleId> = self.modules.keys().cloned().collect();
		for module_id in module_ids {
			let items = self.module_items(&module_id);
			for item in items {
				let node = Self::unwrap_pub(item.as_ref());
				let empty_generic_map = HashMap::new();
				match &node.v {
					ASTValue::DeclarationConstexpr(name, value) => {
						if let Some(fn_ty) = self.fn_type_from_value(
							&module_id,
							value,
							&empty_generic_map,
							None,
							&node.location,
						) {
							self.insert_value_decl(
								&module_id, name, fn_ty, true,
								false,
							);
						}
					}
					ASTValue::Declaration {
						name,
						value,
						mutable,
					} => {
						if let Some(fn_ty) = self.fn_type_from_value(
							&module_id,
							value,
							&empty_generic_map,
							None,
							&node.location,
						) {
							self.insert_value_decl(
								&module_id, name, fn_ty, false,
								*mutable,
							);
						}
					}
					ASTValue::DeclarationMulti {
						names,
						types,
						values: Some(values),
						constexpr,
						mutable,
					} => {
						for (idx, name) in names.iter().enumerate() {
							if let Some(value) = values.get(idx)
								&& let Some(fn_ty) = self
									.fn_type_from_value(
										&module_id,
										value,
										&empty_generic_map,
										None,
										&node.location,
									) {
								self.insert_value_decl(
									&module_id, name, fn_ty,
									*constexpr, *mutable,
								);
								continue;
							}
							let ty = types
								.get(idx)
								.or_else(|| types.first());
							if let Some(ty) = ty {
								let resolved = self.resolve_type(
									&module_id,
									ty,
									&empty_generic_map,
									None,
									&node.location,
								);
								self.insert_value_decl(
									&module_id, name, resolved,
									*constexpr, *mutable,
								);
							}
						}
					}
					_ => {}
				}
			}
		}
	}

	fn type_program(&mut self) -> TypedProgram {
		let mut typed_modules = HashMap::new();
		let module_ids: Vec<ModuleId> = self.modules.keys().cloned().collect();
		for module_id in module_ids {
			let module_snapshot = match self.modules.get(&module_id) {
				Some(module) => module.clone(),
				None => continue,
			};
			let mut ctx = TypeContext {
				module_id: &module_id,
				module: &module_snapshot,
				value_scopes: vec![HashMap::new()],
				borrow_state: HashMap::new(),
				ref_scopes: vec![HashMap::new()],
				temporary_borrows: Vec::new(),
				generic_types: HashMap::new(),
				self_type: None,
				return_type: None,
				in_constexpr: false,
				unsafe_depth: 0,
				suppress_move: false,
				suppress_borrow_check: false,
			};
			let typed_ast = self.type_node(&module_snapshot.ast, &mut ctx, None);
			let values = module_snapshot
				.values
				.iter()
				.map(|(name, info)| (name.clone(), info.ty))
				.collect();
			let typed_module = TypedModule {
				id: module_snapshot.id.clone(),
				file_path: module_snapshot.file_path.clone(),
				package_path: module_snapshot.package_path.clone(),
				ast: typed_ast,
				imports: module_snapshot.imports.clone(),
				exports: module_snapshot.exports.clone(),
				types: module_snapshot.types.clone(),
				values,
			};
			typed_modules.insert(module_id, typed_module);
		}
		TypedProgram {
			entry: self.program.entry.clone(),
			modules: typed_modules,
		}
	}

	fn validate_constexpr_decls(&mut self) {
		let module_ids: Vec<ModuleId> = self.modules.keys().cloned().collect();
		for module_id in module_ids {
			let Some(module) = self.modules.get(&module_id) else {
				continue;
			};
			let mut engine = CtfeEngine::new(CtfeLimits::default());
			let (ASTValue::ExprList { items, .. }
			| ASTValue::ExprListNoScope { items, .. }) = &module.ast.v
			else {
				continue;
			};
			let required_constexprs = Self::collect_required_constexpr_names(items);
			for item in items {
				let node = Self::unwrap_pub(item.as_ref());
				engine.register_function_decls(node);
				match &node.v {
					ASTValue::DeclarationConstexpr(name, value) => {
						if !required_constexprs.contains(name) {
							continue;
						}
						if self.has_non_ctfe_error_in(&node.location) {
							continue;
						}
						if Self::should_eval_constexpr_value(value.as_ref())
						{
							match engine.eval_expr(value.as_ref()) {
								Ok(evaluated) => engine.bind_const(
									name, evaluated,
								),
								Err(err) => {
									self.errors.push(FrontendError::CtfeError(err));
								}
							}
						}
					}
					ASTValue::DeclarationMulti {
						names,
						values: Some(values),
						constexpr: true,
						..
					} => {
						for (idx, name) in names.iter().enumerate() {
							if !required_constexprs.contains(name) {
								continue;
							}
							let Some(value) = values
								.get(idx)
								.or_else(|| values.first())
							else {
								continue;
							};
							if self.has_non_ctfe_error_in(
								&node.location,
							) {
								continue;
							}
							if !Self::should_eval_constexpr_value(
								value.as_ref(),
							) {
								continue;
							}
							match engine.eval_expr(value.as_ref()) {
								Ok(evaluated) => engine.bind_const(
									name, evaluated,
								),
								Err(err) => {
									self.errors.push(FrontendError::CtfeError(err));
								}
							}
						}
					}
					_ => {}
				}
			}
		}
	}

	#[allow(clippy::vec_box)]
	fn collect_required_constexpr_names(items: &[Box<AST>]) -> HashSet<String> {
		let mut constexpr_values: HashMap<String, Box<AST>> = HashMap::new();
		let mut queue: VecDeque<String> = VecDeque::new();

		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(name, value) => {
					constexpr_values.insert(name.clone(), value.clone());
				}
				ASTValue::DeclarationMulti {
					names,
					values: Some(values),
					constexpr: true,
					..
				} => {
					for (idx, name) in names.iter().enumerate() {
						if let Some(value) =
							values.get(idx).or_else(|| values.first())
						{
							constexpr_values.insert(
								name.clone(),
								value.clone(),
							);
						}
					}
				}
				ASTValue::Declaration { value, .. } => {
					Self::collect_ids_in_ast(value.as_ref(), &mut queue);
				}
				ASTValue::DeclarationMulti {
					values: Some(values),
					constexpr: false,
					..
				} => {
					for value in values {
						Self::collect_ids_in_ast(
							value.as_ref(),
							&mut queue,
						);
					}
				}
				_ => {}
			}
		}

		let mut required = HashSet::new();
		while let Some(name) = queue.pop_front() {
			if !required.insert(name.clone()) {
				continue;
			}
			if let Some(value) = constexpr_values.get(&name) {
				Self::collect_ids_in_ast(value.as_ref(), &mut queue);
			}
		}

		required
	}

	fn collect_ids_in_ast(node: &AST, out: &mut VecDeque<String>) {
		match &node.v {
			ASTValue::Id(name) => out.push_back(name.clone()),
			ASTValue::Pub(inner)
			| ASTValue::UnaryPlus(inner)
			| ASTValue::UnaryMinus(inner)
			| ASTValue::Not(inner)
			| ASTValue::Deref(inner)
			| ASTValue::Mut(inner)
			| ASTValue::PtrOf(inner)
			| ASTValue::Defer(inner) => Self::collect_ids_in_ast(inner.as_ref(), out),
			ASTValue::Ref { v, .. }
			| ASTValue::DeclarationConstexpr(_, v)
			| ASTValue::Set(_, v) => Self::collect_ids_in_ast(v.as_ref(), out),
			ASTValue::Cast { value, .. } | ASTValue::Transmute { value, .. } => {
				Self::collect_ids_in_ast(value.as_ref(), out)
			}
			ASTValue::BinExpr { lhs, rhs, .. } => {
				Self::collect_ids_in_ast(lhs.as_ref(), out);
				Self::collect_ids_in_ast(rhs.as_ref(), out);
			}
			ASTValue::Declaration { value, .. } => {
				Self::collect_ids_in_ast(value.as_ref(), out)
			}
			ASTValue::DeclarationMulti {
				values: Some(values),
				..
			}
			| ASTValue::ExprList { items: values, .. }
			| ASTValue::ExprListNoScope { items: values, .. } => {
				for value in values {
					Self::collect_ids_in_ast(value.as_ref(), out);
				}
			}
			ASTValue::SetMulti { values, .. } => {
				for value in values {
					Self::collect_ids_in_ast(value.as_ref(), out);
				}
			}
			ASTValue::Call { callee, args } => {
				Self::collect_ids_in_ast(callee.as_ref(), out);
				for arg in args {
					Self::collect_ids_in_ast(arg.as_ref(), out);
				}
			}
			ASTValue::NamedArg { value, .. } | ASTValue::Return(Some(value)) => {
				Self::collect_ids_in_ast(value.as_ref(), out)
			}
			ASTValue::GenericApply { target, args } => {
				Self::collect_ids_in_ast(target.as_ref(), out);
				let _ = args;
			}
			ASTValue::If {
				cond,
				decl,
				body,
				else_,
			} => {
				Self::collect_ids_in_ast(cond.as_ref(), out);
				if let Some(decl) = decl {
					Self::collect_ids_in_ast(decl.as_ref(), out);
				}
				Self::collect_ids_in_ast(body.as_ref(), out);
				if let Some(else_) = else_ {
					Self::collect_ids_in_ast(else_.as_ref(), out);
				}
			}
			ASTValue::While { cond, decl, body } => {
				Self::collect_ids_in_ast(cond.as_ref(), out);
				if let Some(decl) = decl {
					Self::collect_ids_in_ast(decl.as_ref(), out);
				}
				Self::collect_ids_in_ast(body.as_ref(), out);
			}
			ASTValue::ForLoop {
				init,
				cond,
				step,
				body,
			} => {
				if let Some(init) = init {
					Self::collect_ids_in_ast(init.as_ref(), out);
				}
				if let Some(cond) = cond {
					Self::collect_ids_in_ast(cond.as_ref(), out);
				}
				if let Some(step) = step {
					Self::collect_ids_in_ast(step.as_ref(), out);
				}
				Self::collect_ids_in_ast(body.as_ref(), out);
			}
			ASTValue::For { iter, body, .. } => {
				Self::collect_ids_in_ast(iter.as_ref(), out);
				Self::collect_ids_in_ast(body.as_ref(), out);
			}
			ASTValue::InitializerList(items)
			| ASTValue::TypedInitializerList { items, .. } => {
				for item in items {
					match item {
						InitializerItem::Positional(value)
						| InitializerItem::Named { value, .. } => Self::collect_ids_in_ast(value.as_ref(), out),
					}
				}
			}
			ASTValue::Index { target, indices } => {
				Self::collect_ids_in_ast(target.as_ref(), out);
				for idx in indices {
					Self::collect_ids_in_ast(idx.as_ref(), out);
				}
			}
			ASTValue::Match {
				binder,
				scrutinee,
				cases,
			} => {
				let _ = binder;
				Self::collect_ids_in_ast(scrutinee.as_ref(), out);
				for case in cases {
					if let MatchCasePattern::Exprs(exprs) = &case.pattern {
						for expr in exprs {
							Self::collect_ids_in_ast(
								expr.as_ref(),
								out,
							);
						}
					}
					if let Some(guard) = &case.guard {
						Self::collect_ids_in_ast(guard.as_ref(), out);
					}
					Self::collect_ids_in_ast(case.body.as_ref(), out);
				}
			}
			ASTValue::Fn {
				pre,
				where_clause,
				ensures,
				body,
				..
			} => {
				for pre in pre {
					Self::collect_ids_in_ast(pre.as_ref(), out);
				}
				if let Some(where_clause) = where_clause {
					Self::collect_ids_in_ast(where_clause.as_ref(), out);
				}
				for ensure in ensures {
					Self::collect_ids_in_ast(ensure.condition.as_ref(), out);
				}
				match body {
					crate::frontend::FnBody::Expr(expr)
					| crate::frontend::FnBody::Block(expr) => Self::collect_ids_in_ast(expr.as_ref(), out),
					crate::frontend::FnBody::Uninitialized => {}
				}
			}
			_ => {}
		}
	}

	fn has_non_ctfe_error_in(&self, location: &SourceLocation) -> bool {
		self.errors.iter().any(|err| match err {
			FrontendError::CtfeError(_) => false,
			_ => err.get_location().is_some_and(|err_loc| {
				err_loc.file == location.file
					&& err_loc.range.begin >= location.range.begin
					&& err_loc.range.end <= location.range.end
			}),
		})
	}

	fn should_eval_constexpr_value(value: &AST) -> bool {
		!matches!(
			value.v,
			ASTValue::Fn { .. }
				| ASTValue::Struct { .. } | ASTValue::Enum { .. }
				| ASTValue::Union { .. } | ASTValue::RawUnion { .. }
				| ASTValue::Newtype { .. } | ASTValue::Alias { .. }
		)
	}

	fn is_ctfe_bindable_value(value: &AST) -> bool {
		matches!(value.v, ASTValue::Fn { .. }) || Self::should_eval_constexpr_value(value)
	}

	#[allow(clippy::vec_box)]
	fn module_items(&self, module_id: &ModuleId) -> Vec<Box<AST>> {
		self.modules
			.get(module_id)
			.and_then(|module| match &module.ast.v {
				ASTValue::ExprList { items, .. }
				| ASTValue::ExprListNoScope { items, .. } => Some(items.clone()),
				_ => None,
			})
			.unwrap_or_default()
	}

	fn unwrap_pub(node: &AST) -> &AST {
		match &node.v {
			ASTValue::Pub(inner) => inner.as_ref(),
			_ => node,
		}
	}

	fn integer_type_id(&mut self, bit_size: u8, signed: bool) -> TypeId {
		if bit_size == 0 || bit_size > 64 {
			return self.builtins.unknown_id;
		}
		if let Some(id) = self.integer_types.get(&(bit_size, signed)) {
			return *id;
		}
		let prefix = if signed { 'i' } else { 'u' };
		let name = format!("{prefix}{bit_size}");
		let id = self.arena.add(ResolvedType::Builtin(BuiltinType::Integer {
			name,
			bit_size: Some(bit_size),
			signed,
		}));
		self.integer_types.insert((bit_size, signed), id);
		id
	}

	fn type_node(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		expected: Option<TypeId>,
	) -> Box<TypedAst> {
		let mut typed = match &node.v {
			ASTValue::Package { path } => TypedAst::from(
				node.location.clone(),
				TypedValue::Package { path: path.clone() },
			),
			ASTValue::Use { path, alias } => TypedAst::from(
				node.location.clone(),
				TypedValue::Use {
					path: path.clone(),
					alias: alias.clone(),
				},
			),
			ASTValue::Id(name) => {
				let mut found = None;
				let bool_literal = name == "true" || name == "false";
				for scope in ctx.value_scopes.iter_mut().rev() {
					if let Some(info) = scope.get_mut(name) {
						if !ctx.suppress_borrow_check {
							if let Some(state) =
								ctx.borrow_state.get(name)
							{
								if state.mutable_location.is_some()
								{
									self.errors.push(
									FrontendError::AccessWhileMutBorrowed {
										location: node.location.clone(),
										name: name.clone(),
										borrowed_at: state.mutable_location.clone(),
									},
								);
								}
							}
						}
						if info.moved {
							self.errors.push(
								FrontendError::UseAfterMove {
									location: node
										.location
										.clone(),
									name: name.clone(),
									moved_at: info
										.moved_at
										.clone(),
								},
							);
						}
						if !ctx.suppress_move && !self.is_copy_type(info.ty)
						{
							let mut can_move = true;
							if !ctx.suppress_borrow_check {
								if let Some(state) =
									ctx.borrow_state.get(name)
								{
									if !state
										.shared_locations
										.is_empty() || state
										.mutable_location
										.is_some()
									{
										let borrowed_at = state
											.mutable_location
											.clone()
											.or_else(|| {
												state.shared_locations.first().cloned()
											});
										self.errors.push(
											FrontendError::MoveWhileBorrowed {
												location: node.location.clone(),
												name: name.clone(),
												borrowed_at,
											},
										);
										can_move = false;
									}
								}
							}
							if can_move {
								info.moved = true;
								info.moved_at =
									Some(node.location.clone());
							}
						}
						found = Some(info.clone());
						break;
					}
				}
				if found.is_none() {
					if let Some(info) = ctx.module.values.get(name) {
						if !ctx.suppress_borrow_check {
							if let Some(state) =
								ctx.borrow_state.get(name)
							{
								if state.mutable_location.is_some()
								{
									self.errors.push(
										FrontendError::AccessWhileMutBorrowed {
											location: node.location.clone(),
											name: name.clone(),
											borrowed_at: state.mutable_location.clone(),
										},
									);
								}
							}
						}
						found = Some(info.clone());
					}
				}
				let ty = if let Some(info) = found {
					info.ty
				} else if bool_literal {
					self.builtins.bool_id
				} else {
					self.errors.push(FrontendError::UnknownValue {
						location: node.location.clone(),
						name: name.clone(),
					});
					self.builtins.unknown_id
				};
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Id(name.clone()),
				);
				out.ty = Some(ty);
				out
			}
			ASTValue::String(value) => {
				let slice_underlying = self.integer_type_id(8, false);
				let slice_ty = self.arena.add(ResolvedType::Slice {
					underlying: slice_underlying,
				});
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::String(value.clone()),
					slice_ty,
				)
			}
			ASTValue::Char(value) => self.typed_with_ty(
				node.location.clone(),
				TypedValue::Char(*value),
				self.builtins.rune_id,
			),
			ASTValue::Integer(value) => self.typed_with_ty(
				node.location.clone(),
				TypedValue::Integer(*value),
				self.builtins.untyped_int_id,
			),
			ASTValue::Float(value) => self.typed_with_ty(
				node.location.clone(),
				TypedValue::Float(*value),
				self.builtins.untyped_float_id,
			),
			ASTValue::DotId(name) => {
				let mut ty = self.builtins.unknown_id;
				if let Some(expected) = expected {
					if matches!(
						self.arena.get(expected),
						ResolvedType::Enum { .. }
					) {
						ty = expected;
					}
				}
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::DotId(name.clone()),
					ty,
				)
			}
			ASTValue::Not(inner) => {
				let typed_inner =
					self.type_node(inner, ctx, Some(self.builtins.bool_id));
				let ty = typed_inner.ty.unwrap_or(self.builtins.unknown_id);
				if !self.type_matches(ty, self.builtins.bool_id) {
					self.errors.push(FrontendError::TypeMismatch {
						location: node.location.clone(),
						expected: "bool".to_string(),
						found: self.type_name(ty),
					});
				}
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Not(typed_inner),
				);
				out.ty = Some(self.builtins.bool_id);
				out
			}
			ASTValue::UnaryPlus(inner) | ASTValue::UnaryMinus(inner) => {
				let typed_inner = self.type_node(inner, ctx, None);
				let ty = typed_inner.ty.unwrap_or(self.builtins.unknown_id);
				if !self.is_numeric_type(ty) && !self.is_untyped_numeric(ty) {
					self.errors.push(FrontendError::InvalidOperator {
						location: node.location.clone(),
						operator: if matches!(
							node.v,
							ASTValue::UnaryPlus(_)
						) {
							"+".to_string()
						} else {
							"-".to_string()
						},
						lhs: self.type_name(ty),
						rhs: None,
					});
				}
				let mut out = TypedAst::from(
					node.location.clone(),
					if matches!(node.v, ASTValue::UnaryPlus(_)) {
						TypedValue::UnaryPlus(typed_inner)
					} else {
						TypedValue::UnaryMinus(typed_inner)
					},
				);
				out.ty = Some(ty);
				out
			}
			ASTValue::Ref { mutable, v } => {
				let prev_borrow_check = ctx.suppress_borrow_check;
				ctx.suppress_borrow_check = true;
				let typed_inner = self.type_node_without_move(v, ctx, None);
				ctx.suppress_borrow_check = prev_borrow_check;
				if let Some(base_name) = self
					.lhs_base_id(v.as_ref())
					.filter(|name| *name != "_")
					.map(|name| name.to_string())
				{
					let kind = if *mutable {
						BorrowKind::Mutable
					} else {
						BorrowKind::Shared
					};
					let mut can_borrow = true;
					if *mutable {
						if let Some(info) =
							self.lookup_value_info(ctx, &base_name)
						{
							if !info.mutable {
								self.errors.push(
									FrontendError::MutBorrowOfImmutable {
										location: node.location.clone(),
										name: base_name.clone(),
									},
								);
								can_borrow = false;
							}
						}
					}
					if can_borrow
						&& self.register_borrow(
							ctx,
							&base_name,
							kind,
							&node.location,
						) {
						self.record_temporary_borrow(
							ctx,
							RefBinding {
								base: base_name,
								kind,
								is_static: false,
							},
						);
					}
				}
				let underlying = typed_inner.ty.unwrap_or(self.builtins.unknown_id);
				let ref_ty = self.arena.add(ResolvedType::Reference {
					mutable: *mutable,
					lifetime: None,
					underlying,
				});
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::Ref {
						mutable: *mutable,
						v: typed_inner,
					},
					ref_ty,
				)
			}
			ASTValue::Deref(inner) => {
				let typed_inner = self.type_node(inner, ctx, None);
				let mut ty = typed_inner.ty.unwrap_or(self.builtins.unknown_id);
				match self.arena.get(ty) {
					ResolvedType::Pointer { underlying }
					| ResolvedType::Reference { underlying, .. } => {
						ty = *underlying;
					}
					_ => {
						self.errors.push(FrontendError::InvalidOperator {
							location: node.location.clone(),
							operator: "^".to_string(),
							lhs: self.type_name(ty),
							rhs: None,
						});
					}
				}
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::Deref(typed_inner),
					ty,
				)
			}
			ASTValue::PtrOf(inner) => {
				if ctx.in_constexpr {
					self.errors.push(FrontendError::PointerInConstexpr {
						location: node.location.clone(),
					});
				}
				if !self.is_unsafe_context(ctx) {
					self.errors.push(FrontendError::PointerRequiresUnsafe {
						location: node.location.clone(),
					});
				}
				let typed_inner = self.type_node_without_move(inner, ctx, None);
				let underlying = typed_inner.ty.unwrap_or(self.builtins.unknown_id);
				let ptr_ty = self.arena.add(ResolvedType::Pointer { underlying });
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::PtrOf(typed_inner),
					ptr_ty,
				)
			}
			ASTValue::Mut(inner) => {
				let typed_inner = self.type_node(inner, ctx, expected);
				let ty = typed_inner.ty.unwrap_or(self.builtins.unknown_id);
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::Mut(typed_inner),
					ty,
				)
			}
			ASTValue::BinExpr {
				op,
				lhs,
				rhs,
				has_eq,
			} => self.type_binary(node, ctx, expected, op, lhs, rhs, *has_eq),
			ASTValue::Call { callee, args } => {
				self.type_call(node, ctx, callee, args, expected)
			}
			ASTValue::NamedArg { name, value } => {
				let typed_value = self.type_node(value, ctx, None);
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::NamedArg {
						name: name.clone(),
						value: typed_value,
					},
					self.builtins.unknown_id,
				)
			}
			ASTValue::GenericApply { target, args } => {
				let typed_target = self.type_node(target, ctx, None);
				let mut typed_args = Vec::new();
				for arg in args {
					let typed_arg = match arg {
						GenericArg::Type(ty) => {
							TypedGenericArg::Type(self.resolve_type(
								ctx.module_id,
								ty,
								&ctx.generic_types,
								ctx.self_type,
								&node.location,
							))
						}
						GenericArg::Expr(expr) => {
							TypedGenericArg::Expr(expr.to_string())
						}
						GenericArg::Name(name) => {
							TypedGenericArg::Name(name.clone())
						}
					};
					typed_args.push(typed_arg);
				}
				let ty = typed_target.ty.unwrap_or(self.builtins.unknown_id);
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::GenericApply {
						target: typed_target,
						args: typed_args,
					},
				);
				out.ty = Some(ty);
				out
			}
			ASTValue::InitializerList(items) => {
				let typed_items = self.type_initializer_items(ctx, items, None);
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::InitializerList(typed_items),
				);
				out.ty = Some(self.builtins.unknown_id);
				out
			}
			ASTValue::TypedInitializerList { ty, items } => {
				let list_ty = self.resolve_type(
					ctx.module_id,
					ty,
					&ctx.generic_types,
					ctx.self_type,
					&node.location,
				);
				let mut element_ty = None;
				if let ResolvedType::Array { underlying, .. }
				| ResolvedType::Slice { underlying } = self.arena.get(list_ty)
				{
					element_ty = Some(*underlying);
				}
				let typed_items =
					self.type_initializer_items(ctx, items, element_ty);
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::TypedInitializerList {
						ty: list_ty,
						items: typed_items,
					},
				);
				out.ty = Some(list_ty);
				out
			}
			ASTValue::Cast { ty, value } | ASTValue::Transmute { ty, value } => {
				let typed_value = self.type_node(value, ctx, None);
				let value_ty = typed_value.ty.unwrap_or(self.builtins.unknown_id);
				let resolved_ty = ty.as_ref().map(|t| {
					self.resolve_type(
						ctx.module_id,
						t,
						&ctx.generic_types,
						ctx.self_type,
						&node.location,
					)
				});
				let mut out = TypedAst::from(
					node.location.clone(),
					if matches!(node.v, ASTValue::Cast { .. }) {
						TypedValue::Cast {
							ty: resolved_ty,
							value: typed_value,
						}
					} else {
						TypedValue::Transmute {
							ty: resolved_ty,
							value: typed_value,
						}
					},
				);
				out.ty = Some(resolved_ty.unwrap_or(value_ty));
				out
			}
			ASTValue::Index { target, indices } => {
				let typed_target = self.type_node(target, ctx, None);
				let target_ty = typed_target.ty.unwrap_or(self.builtins.unknown_id);
				let mut typed_indices = Vec::new();
				match self.lookup_method_access(
					target_ty,
					"operator[]",
					ctx.self_type,
				) {
					MemberAccess::Found((method_ty, needs_ref)) => {
						let (params, return_type) =
							match self.arena.get(method_ty) {
								ResolvedType::Fn {
									params,
									return_type,
								} => (params.clone(), *return_type),
								_ => {
									let mut out = TypedAst::from(
								node.location.clone(),
								TypedValue::Index {
									target: typed_target,
									indices: Vec::new(),
								},
							);
									out.ty = Some(self
										.builtins
										.unknown_id);
									return out;
								}
							};
						let expected_args = params.len().saturating_sub(1);
						if expected_args != indices.len() {
							self.errors.push(
								FrontendError::InvalidCall {
									location: node
										.location
										.clone(),
									callee: "operator[]"
										.to_string(),
								},
							);
						}
						let this_param = params
							.first()
							.copied()
							.unwrap_or(self.builtins.unknown_id);
						let mut this_ty = target_ty;
						if needs_ref {
							if !matches!(
								self.arena.get(this_ty),
								ResolvedType::Reference { .. }
							) {
								this_ty = self.arena.add(
									ResolvedType::Reference {
										mutable: false,
										lifetime: None,
										underlying:
											target_ty,
									},
								);
							}
						}
						if !self.type_matches(this_ty, this_param)
							&& !self.reference_allows_inheritance(
								this_ty, this_param,
							) {
							self.errors.push(
								FrontendError::TypeMismatch {
									location: node
										.location
										.clone(),
									expected: self.type_name(
										this_param,
									),
									found: self
										.type_name(this_ty),
								},
							);
						}
						for (idx, index) in indices.iter().enumerate() {
							let expected = params.get(idx + 1).copied();
							let mut typed_index = self
								.type_node(index, ctx, expected);
							let mut index_ty =
								typed_index.ty.unwrap_or(
									self.builtins.unknown_id,
								);
							if let Some(expected) = expected {
								let mut coerced = index_ty;
								if !self.type_matches(
									index_ty, expected,
								) && !self.coerce_untyped(
									&mut coerced,
									expected,
								) {
									self.errors.push(FrontendError::TypeMismatch {
									location: index.location.clone(),
									expected: self.type_name(expected),
									found: self.type_name(index_ty),
								});
								} else {
									index_ty = coerced;
									typed_index.ty =
										Some(index_ty);
								}
							}
							typed_indices.push(typed_index);
						}
						let mut out = TypedAst::from(
							node.location.clone(),
							TypedValue::Index {
								target: typed_target,
								indices: typed_indices,
							},
						);
						out.ty = Some(return_type);
						out
					}
					MemberAccess::Inaccessible => {
						self.errors.push(
							FrontendError::InaccessibleMember {
								location: node.location.clone(),
								type_name: self
									.type_name(target_ty),
								member: "operator[]".to_string(),
							},
						);
						let mut out = TypedAst::from(
							node.location.clone(),
							TypedValue::Index {
								target: typed_target,
								indices: typed_indices,
							},
						);
						out.ty = Some(self.builtins.unknown_id);
						out
					}
					MemberAccess::Missing => {
						if matches!(
							self.arena.get(target_ty),
							ResolvedType::Struct { .. }
								| ResolvedType::Enum { .. }
								| ResolvedType::Union { .. }
								| ResolvedType::RawUnion { .. }
								| ResolvedType::Newtype { .. }
								| ResolvedType::Alias { .. }
						) {
							let mut arg_types = Vec::new();
							for index in indices {
								let typed_index = self.type_node(
									index, ctx, None,
								);
								let index_ty =
									typed_index.ty.unwrap_or(
										self.builtins
											.unknown_id,
									);
								arg_types.push(
									self.type_name(index_ty)
								);
								typed_indices.push(typed_index);
							}
							self.errors.push(
								FrontendError::InvalidOperator {
									location: node
										.location
										.clone(),
									operator: "[]".to_string(),
									lhs: self.type_name(
										target_ty,
									),
									rhs: Some(arg_types
										.join(", ")),
								},
							);
							let mut out = TypedAst::from(
								node.location.clone(),
								TypedValue::Index {
									target: typed_target,
									indices: typed_indices,
								},
							);
							out.ty = Some(self.builtins.unknown_id);
							return out;
						}
						let mut current_ty = target_ty;
						for index in indices {
							let typed_index =
								self.type_node(index, ctx, None);
							let index_ty = typed_index.ty.unwrap_or(
								self.builtins.unknown_id,
							);
							if !self.is_integer_type(index_ty)
								&& !self.is_untyped_numeric(
									index_ty,
								) {
								self.errors.push(
									FrontendError::InvalidIndex {
										location: index
											.location
											.clone(),
										target: self.type_name(
											current_ty,
										),
										index: self.type_name(
											index_ty,
										),
									},
								);
							}
							match self.arena.get(current_ty) {
								ResolvedType::Array {
									underlying,
									..
								}
								| ResolvedType::Slice {
									underlying,
								} => {
									current_ty = *underlying;
								}
								_ => {
									self.errors.push(FrontendError::InvalidIndex {
										location: index.location.clone(),
										target: self.type_name(current_ty),
										index: self.type_name(index_ty),
									});
								}
							}
							typed_indices.push(typed_index);
						}
						let mut out = TypedAst::from(
							node.location.clone(),
							TypedValue::Index {
								target: typed_target,
								indices: typed_indices,
							},
						);
						out.ty = Some(current_ty);
						out
					}
				}
			}
			ASTValue::ExprList {
				items: exprs,
				attributes,
			}
			| ASTValue::ExprListNoScope {
				items: exprs,
				attributes,
			} => {
				let is_scoped = matches!(node.v, ASTValue::ExprList { .. });
				let has_unsafe = attributes.iter().any(|attr| attr == "unsafe");
				if has_unsafe {
					ctx.unsafe_depth += 1;
				}
				if is_scoped {
					self.push_scope(ctx);
				}
				let mut typed_exprs = Vec::new();
				let mut last_ty = self.builtins.void_id;
				for expr in exprs {
					let typed_expr = self.type_node(expr, ctx, None);
					last_ty = typed_expr.ty.unwrap_or(self.builtins.unknown_id);
					typed_exprs.push(typed_expr);
					self.clear_temporary_borrows(ctx);
				}
				if typed_exprs.len() > 1 {
					for typed_expr in
						typed_exprs.iter().take(typed_exprs.len() - 1)
					{
						if let Some(ty) = typed_expr.ty
							&& ty != self.builtins.void_id && ty != self
							.builtins
							.unknown_id
						{
							self.errors.push(FrontendError::UnusedValue {
								location: typed_expr.location.clone(),
								hint:
									"use `_ = <expr>` to ignore the return value"
										.to_string(),
							});
						}
					}
				}
				if is_scoped {
					self.pop_scope(ctx);
				}
				if has_unsafe {
					ctx.unsafe_depth = ctx.unsafe_depth.saturating_sub(1);
				}
				let mut out = TypedAst::from(
					node.location.clone(),
					if is_scoped {
						TypedValue::ExprList {
							items: typed_exprs,
							attributes: attributes.clone(),
						}
					} else {
						TypedValue::ExprListNoScope {
							items: typed_exprs,
							attributes: attributes.clone(),
						}
					},
				);
				out.ty = Some(last_ty);
				out
			}
			ASTValue::Return(value) => {
				let typed_value = value
					.as_ref()
					.map(|v| self.type_node(v, ctx, ctx.return_type));
				if let Some(typed_value) = &typed_value {
					if let Some(expected) = ctx.return_type {
						let found = typed_value
							.ty
							.unwrap_or(self.builtins.unknown_id);
						if found != self.builtins.unknown_id
							&& !self.type_matches(found, expected)
						{
							self.errors.push(
								FrontendError::TypeMismatch {
									location: node
										.location
										.clone(),
									expected: self.type_name(
										expected,
									),
									found: self
										.type_name(found),
								},
							);
						}
					}
				}
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::Return(typed_value),
					self.builtins.void_id,
				)
			}
			ASTValue::Defer(value) => {
				let typed_value = self.type_node(value, ctx, None);
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::Defer(typed_value),
					self.builtins.void_id,
				)
			}
			ASTValue::Hide(name) => {
				let mut found = None;
				for scope in ctx.value_scopes.iter_mut().rev() {
					if scope.remove(name).is_some() {
						self.release_binding_for_name(ctx, name);
						found = Some(());
						break;
					}
				}
				if found.is_none() {
					self.errors.push(FrontendError::UnknownValue {
						location: node.location.clone(),
						name: name.clone(),
					});
				}
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::Hide(name.clone()),
					self.builtins.void_id,
				)
			}
			ASTValue::Match {
				binder,
				scrutinee,
				cases,
			} => self.type_match(node, ctx, binder, scrutinee, cases),
			ASTValue::If {
				cond,
				decl,
				body,
				else_,
			} => self.type_if(node, ctx, cond, decl, body, else_),
			ASTValue::While { cond, decl, body } => {
				self.type_while(node, ctx, cond, decl, body)
			}
			ASTValue::ForLoop {
				init,
				cond,
				step,
				body,
			} => self.type_for_loop(node, ctx, init, cond, step, body),
			ASTValue::For {
				bindings,
				iter,
				body,
			} => self.type_for(node, ctx, bindings, iter, body),
			ASTValue::Pub(inner) => {
				let typed_inner = self.type_node(inner, ctx, expected);
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Pub(typed_inner),
				);
				out.ty = Some(self.builtins.void_id);
				out
			}
			ASTValue::Set(name, value) => self.type_assignment(node, ctx, name, value),
			ASTValue::Declaration {
				name,
				value,
				mutable,
			} => self.type_declaration(node, ctx, name, value, *mutable, false),
			ASTValue::DeclarationConstexpr(name, value) => {
				self.type_declaration(node, ctx, name, value, false, true)
			}
			ASTValue::SetMulti { names, values } => {
				let mut typed_values = Vec::new();
				for value in values {
					typed_values.push(self.type_node(value, ctx, None));
				}
				for (idx, name) in names.iter().enumerate() {
					if name != "_" {
						if let Some(state) = ctx.borrow_state.get(name) {
							if !state.shared_locations.is_empty()
								|| state.mutable_location.is_some()
							{
								let borrowed_at = state
									.mutable_location
									.clone()
									.or_else(|| {
										state.shared_locations.first().cloned()
									});
								self.errors.push(
									FrontendError::AssignWhileBorrowed {
										location: node.location.clone(),
										name: name.clone(),
										borrowed_at,
									},
								);
							}
						}
					}
					let mut target_mutable = None;
					let mut target_ty = None;
					let mut target_found = false;
					let mut found_in_scope = false;
					for scope in ctx.value_scopes.iter_mut().rev() {
						if let Some(info) = scope.get_mut(name) {
							target_mutable = Some(info.mutable);
							target_ty = Some(info.ty);
							target_found = true;
							found_in_scope = true;
							break;
						}
					}
					if target_mutable.is_none() {
						if let Some(info) = ctx.module.values.get(name) {
							target_mutable = Some(info.mutable);
							target_ty = Some(info.ty);
							target_found = true;
						}
					}
					if target_found && name != "_" {
						self.release_binding_for_name(ctx, name);
					}
					if let Some(target_mutable) = target_mutable {
						if !target_mutable && name != "_" {
							self.errors.push(
								FrontendError::AssignToImmutable {
									location: node
										.location
										.clone(),
									name: name.clone(),
								},
							);
						}
						if target_mutable && found_in_scope {
							for scope in
								ctx.value_scopes.iter_mut().rev()
							{
								if let Some(info) =
									scope.get_mut(name)
								{
									info.moved = false;
									info.moved_at = None;
									break;
								}
							}
						}
					}
					if name != "_" {
						if let Some(value_ast) =
							values.get(idx).or_else(|| values.first())
						{
							if let Some((ref_mutable, inner)) = self
								.top_level_ref(value_ast.as_ref())
							{
								if let Some(base) =
									self.lhs_base_id(inner)
								{
									let kind = if ref_mutable {
										BorrowKind::Mutable
									} else {
										BorrowKind::Shared
									};
									let is_static = target_ty
										.map(|ty| {
											self.is_static_ref_type(ty)
										})
										.unwrap_or(false);
									self.promote_temporary_borrow(
										ctx,
										name,
										RefBinding {
											base: base.to_string(),
											kind,
											is_static,
										},
										&node.location,
									);
								}
							}
						}
					}
				}
				self.typed_with_ty(
					node.location.clone(),
					TypedValue::SetMulti {
						names: names.clone(),
						values: typed_values,
					},
					self.builtins.void_id,
				)
			}
			ASTValue::DeclarationMulti {
				names,
				types,
				values,
				constexpr,
				mutable,
			} => self.type_multi_declaration(
				node, ctx, names, types, values, *constexpr, *mutable,
			),
			ASTValue::Type(ty) => {
				let resolved = self.resolve_type(
					ctx.module_id,
					ty,
					&ctx.generic_types,
					ctx.self_type,
					&node.location,
				);
				if self.type_contains_pointer(resolved)
					&& !self.is_unsafe_context(ctx)
				{
					self.errors.push(FrontendError::PointerRequiresUnsafe {
						location: node.location.clone(),
					});
				}
				if ctx.in_constexpr && self.type_contains_pointer(resolved) {
					self.errors.push(FrontendError::PointerInConstexpr {
						location: node.location.clone(),
					});
				}
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Type(resolved),
				);
				out.ty = Some(self.builtins.type_id);
				out
			}
			ASTValue::Fn {
				attributes,
				generics,
				params,
				return_type,
				pre,
				post,
				where_clause,
				ensures,
				body,
			} => self.type_fn(
				node,
				ctx,
				attributes,
				generics,
				params,
				return_type,
				pre,
				post,
				where_clause,
				ensures,
				body,
			),
			ASTValue::Struct {
				attributes,
				generics,
				extends,
				implements,
				body,
			} => {
				let (typed_generics, generic_map) = self.resolve_generic_params(
					ctx.module_id,
					generics,
					None,
					None,
				);
				let mut typed_extends = Vec::new();
				for ty in extends {
					typed_extends.push(self.resolve_type(
						ctx.module_id,
						ty,
						&generic_map,
						ctx.self_type,
						&node.location,
					));
				}
				let mut typed_implements = Vec::new();
				for ty in implements {
					typed_implements.push(self.resolve_type(
						ctx.module_id,
						ty,
						&generic_map,
						ctx.self_type,
						&node.location,
					));
				}
				let prev_generic_types = std::mem::replace(
					&mut ctx.generic_types,
					generic_map.clone(),
				);
				let typed_body = self.type_node(body, ctx, None);
				ctx.generic_types = prev_generic_types;
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Struct {
						attributes: attributes.clone(),
						generics: typed_generics,
						extends: typed_extends,
						implements: typed_implements,
						body: typed_body,
					},
				);
				out.ty = Some(self.builtins.type_id);
				out
			}
			ASTValue::Interface {
				attributes,
				generics,
				body,
			} => {
				let (typed_generics, generic_map) = self.resolve_generic_params(
					ctx.module_id,
					generics,
					None,
					None,
				);
				let prev_generic_types = std::mem::replace(
					&mut ctx.generic_types,
					generic_map.clone(),
				);
				let typed_body = self.type_node(body, ctx, None);
				ctx.generic_types = prev_generic_types;
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Interface {
						attributes: attributes.clone(),
						generics: typed_generics,
						body: typed_body,
					},
				);
				out.ty = Some(self.builtins.type_id);
				out
			}
			ASTValue::Enum {
				variants,
				implements,
			} => {
				let mut typed_implements = Vec::new();
				for ty in implements {
					typed_implements.push(self.resolve_type(
						ctx.module_id,
						ty,
						&HashMap::new(),
						ctx.self_type,
						&node.location,
					));
				}
				let mut typed_variants = Vec::new();
				for variant in variants {
					let typed_value = variant
						.value
						.as_ref()
						.map(|v| self.type_node(v, ctx, None));
					typed_variants.push(TypedEnumVariant {
						name: variant.name.clone(),
						value: typed_value,
					});
				}
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Enum {
						variants: typed_variants,
						implements: typed_implements,
					},
				);
				out.ty = Some(self.builtins.type_id);
				out
			}
			ASTValue::Union {
				generics,
				variants,
				methods,
				implements,
			} => {
				let (typed_generics, generic_map) = self.resolve_generic_params(
					ctx.module_id,
					generics,
					None,
					None,
				);
				let mut typed_variants = Vec::new();
				for variant in variants {
					let resolved = self.resolve_type(
						ctx.module_id,
						variant,
						&generic_map,
						ctx.self_type,
						&node.location,
					);
					typed_variants.push(resolved);
				}
				let mut typed_implements = Vec::new();
				for ty in implements {
					typed_implements.push(self.resolve_type(
						ctx.module_id,
						ty,
						&generic_map,
						ctx.self_type,
						&node.location,
					));
				}
				let prev_generic_types = std::mem::replace(
					&mut ctx.generic_types,
					generic_map.clone(),
				);
				for method in methods {
					let _ = self.type_node(method, ctx, None);
				}
				ctx.generic_types = prev_generic_types;
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Union {
						generics: typed_generics,
						variants: typed_variants,
						implements: typed_implements,
					},
				);
				out.ty = Some(self.builtins.type_id);
				out
			}
			ASTValue::RawUnion {
				generics,
				body,
				implements,
			} => {
				let (typed_generics, generic_map) = self.resolve_generic_params(
					ctx.module_id,
					generics,
					None,
					None,
				);
				let mut typed_implements = Vec::new();
				for ty in implements {
					typed_implements.push(self.resolve_type(
						ctx.module_id,
						ty,
						&generic_map,
						ctx.self_type,
						&node.location,
					));
				}
				let typed_body = self.type_node(body, ctx, None);
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::RawUnion {
						generics: typed_generics,
						body: typed_body,
						implements: typed_implements,
					},
				);
				out.ty = Some(self.builtins.type_id);
				out
			}
			ASTValue::Newtype {
				underlying,
				constraint,
			} => {
				let resolved = self.resolve_type(
					ctx.module_id,
					underlying,
					&ctx.generic_types,
					ctx.self_type,
					&node.location,
				);
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Newtype {
						underlying: resolved,
						constraint: constraint.clone(),
					},
				);
				out.ty = Some(self.builtins.type_id);
				out
			}
			ASTValue::Alias { underlying } => {
				let resolved = self.resolve_type(
					ctx.module_id,
					underlying,
					&ctx.generic_types,
					ctx.self_type,
					&node.location,
				);
				let mut out = TypedAst::from(
					node.location.clone(),
					TypedValue::Alias {
						underlying: resolved,
					},
				);
				out.ty = Some(self.builtins.type_id);
				out
			}
		};

		typed.trivia = node.trivia.clone();
		if let Some(expected) = expected {
			if let Some(actual) = typed.ty {
				let mut coerced = actual;
				if self.coerce_untyped(&mut coerced, expected) {
					typed.ty = Some(coerced);
				}
			}
		}
		let ty = typed.ty.unwrap_or(self.builtins.unknown_id);
		if expected.is_none() && !ctx.in_constexpr {
			if ty == self.builtins.untyped_int_id {
				typed.ty = Some(self.builtins.int_id);
			} else if ty == self.builtins.untyped_float_id {
				typed.ty = Some(self.builtins.f64_id);
			}
		}
		typed
	}

	#[allow(clippy::too_many_arguments)]
	fn type_binary(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		expected: Option<TypeId>,
		op: &crate::frontend::Operator,
		lhs: &Box<AST>,
		rhs: &Box<AST>,
		has_eq: bool,
	) -> Box<TypedAst> {
		if matches!(op, crate::frontend::Operator::Dot) {
			return self.type_dot(node, ctx, lhs, rhs);
		}
		let is_assignment = matches!(op, crate::frontend::Operator::Set) && !has_eq;
		if is_assignment {
			if let ASTValue::Id(name) = &lhs.v
				&& name == "_"
			{
				let typed_rhs = self.type_node(rhs, ctx, None);
				let typed_lhs = self.build_typed_id(
					lhs.location.clone(),
					name.clone(),
					self.builtins.unknown_id,
				);
				return self.build_bin_expr(
					node.location.clone(),
					op.clone(),
					typed_lhs,
					typed_rhs,
					has_eq,
					self.builtins.void_id,
				);
			}
			if let ASTValue::Id(name) = &lhs.v {
				let mut target_ty = None;
				let mut target_mutable = None;
				let mut found_in_scope = false;
				let mut target_found = false;
				if name != "_" {
					if let Some(state) = ctx.borrow_state.get(name) {
						if !state.shared_locations.is_empty()
							|| state.mutable_location.is_some()
						{
							let borrowed_at =
								state.mutable_location
									.clone()
									.or_else(|| {
										state.shared_locations.first().cloned()
									});
							self.errors
								.push(FrontendError::AssignWhileBorrowed {
								location: node.location.clone(),
								name: name.clone(),
								borrowed_at,
							});
						}
					}
				}
				for scope in ctx.value_scopes.iter().rev() {
					if let Some(info) = scope.get(name) {
						target_ty = Some(info.ty);
						target_mutable = Some(info.mutable);
						found_in_scope = true;
						target_found = true;
						break;
					}
				}
				if target_ty.is_none() {
					if let Some(info) = ctx.module.values.get(name) {
						target_ty = Some(info.ty);
						target_mutable = Some(info.mutable);
						target_found = true;
					}
				}
				if target_found && name != "_" {
					self.release_binding_for_name(ctx, name);
				}
				let mut typed_rhs = self.type_node(rhs, ctx, target_ty);
				let mut rhs_ty = typed_rhs.ty.unwrap_or(self.builtins.unknown_id);
				if let Some(expected) = target_ty {
					let mut coerced = rhs_ty;
					if !self.type_matches(rhs_ty, expected)
						&& !self.coerce_untyped(&mut coerced, expected)
					{
						self.errors.push(FrontendError::TypeMismatch {
							location: node.location.clone(),
							expected: self.type_name(expected),
							found: self.type_name(rhs_ty),
						});
					} else {
						rhs_ty = coerced;
						typed_rhs.ty = Some(rhs_ty);
					}
				} else if name != "_" {
					self.errors.push(FrontendError::UnknownValue {
						location: node.location.clone(),
						name: name.clone(),
					});
				}
				if let Some(target_mutable) = target_mutable {
					if !target_mutable {
						self.errors.push(
							FrontendError::AssignToImmutable {
								location: node.location.clone(),
								name: name.clone(),
							},
						);
					} else if found_in_scope {
						for scope in ctx.value_scopes.iter_mut().rev() {
							if let Some(info) = scope.get_mut(name) {
								info.moved = false;
								info.moved_at = None;
								break;
							}
						}
					}
				}
				if name != "_" {
					if let Some((ref_mutable, inner)) =
						self.top_level_ref(rhs.as_ref())
					{
						if let Some(base) = self.lhs_base_id(inner) {
							let kind = if ref_mutable {
								BorrowKind::Mutable
							} else {
								BorrowKind::Shared
							};
							let is_static = target_ty
								.map(|ty| {
									self.is_static_ref_type(ty)
								})
								.unwrap_or(false);
							self.promote_temporary_borrow(
								ctx,
								name,
								RefBinding {
									base: base.to_string(),
									kind,
									is_static,
								},
								&node.location,
							);
						}
					}
				}
				let typed_lhs = self.build_typed_id(
					lhs.location.clone(),
					name.clone(),
					target_ty.unwrap_or(self.builtins.unknown_id),
				);
				return self.build_bin_expr(
					node.location.clone(),
					op.clone(),
					typed_lhs,
					typed_rhs,
					has_eq,
					self.builtins.void_id,
				);
			}
			if let Some(name) = self.lhs_base_id(lhs.as_ref())
				&& name != "_"
			{
				let mut target_mutable = None;
				if let Some(state) = ctx.borrow_state.get(name) {
					if !state.shared_locations.is_empty()
						|| state.mutable_location.is_some()
					{
						let borrowed_at = state
							.mutable_location
							.clone()
							.or_else(|| {
								state.shared_locations
									.first()
									.cloned()
							});
						self.errors.push(
							FrontendError::AssignWhileBorrowed {
								location: node.location.clone(),
								name: name.to_string(),
								borrowed_at,
							},
						);
					}
				}
				for scope in ctx.value_scopes.iter().rev() {
					if let Some(info) = scope.get(name) {
						target_mutable = Some(info.mutable);
						break;
					}
				}
				if target_mutable.is_none() {
					if let Some(info) = ctx.module.values.get(name) {
						target_mutable = Some(info.mutable);
					}
				}
				if let Some(false) = target_mutable {
					self.errors.push(FrontendError::AssignToImmutable {
						location: node.location.clone(),
						name: name.to_string(),
					});
				}
			}
		}
		let mut typed_lhs = self.type_node(lhs, ctx, None);
		let mut typed_rhs = self.type_node(rhs, ctx, None);
		let mut lhs_ty = typed_lhs.ty.unwrap_or(self.builtins.unknown_id);
		let mut rhs_ty = typed_rhs.ty.unwrap_or(self.builtins.unknown_id);

		let op_name = match op {
			crate::frontend::Operator::Add => "+",
			crate::frontend::Operator::Sub => "-",
			crate::frontend::Operator::Mul => "*",
			crate::frontend::Operator::Divide => "/",
			crate::frontend::Operator::LessThan => "<",
			crate::frontend::Operator::GreaterThan => ">",
			crate::frontend::Operator::And => "&&",
			crate::frontend::Operator::Or => "||",
			crate::frontend::Operator::BinAnd => "&",
			crate::frontend::Operator::BinOr => "|",
			crate::frontend::Operator::BinXOR => "^",
			crate::frontend::Operator::Set => "=",
			crate::frontend::Operator::Not => "!",
			crate::frontend::Operator::Dot => ".",
		};

		if lhs_ty == self.builtins.unknown_id || rhs_ty == self.builtins.unknown_id {
			if self.is_numeric_operator(op) {
				self.record_numeric_type_param_ops(
					lhs_ty,
					rhs_ty,
					op_name,
					&node.location,
				);
			}
			return self.build_bin_expr(
				node.location.clone(),
				op.clone(),
				typed_lhs,
				typed_rhs,
				has_eq,
				self.builtins.unknown_id,
			);
		}

		let is_comparison = matches!(
			op,
			crate::frontend::Operator::LessThan
				| crate::frontend::Operator::GreaterThan
				| crate::frontend::Operator::Set
				| crate::frontend::Operator::Not
		) && has_eq;
		let is_logic = matches!(
			op,
			crate::frontend::Operator::And | crate::frontend::Operator::Or
		);
		let is_bitwise = matches!(
			op,
			crate::frontend::Operator::BinAnd
				| crate::frontend::Operator::BinOr
				| crate::frontend::Operator::BinXOR
		);

		if is_assignment {
			if let ASTValue::Id(name) = &lhs.v {
				if let Some(info) = ctx.module.values.get(name) {
					lhs_ty = info.ty;
				}
			}
		}

		if is_logic {
			if !self.type_matches(lhs_ty, self.builtins.bool_id) {
				self.errors.push(FrontendError::TypeMismatch {
					location: lhs.location.clone(),
					expected: "bool".to_string(),
					found: self.type_name(lhs_ty),
				});
			}
			if !self.type_matches(rhs_ty, self.builtins.bool_id) {
				self.errors.push(FrontendError::TypeMismatch {
					location: rhs.location.clone(),
					expected: "bool".to_string(),
					found: self.type_name(rhs_ty),
				});
			}
			return self.build_bin_expr(
				node.location.clone(),
				op.clone(),
				typed_lhs,
				typed_rhs,
				has_eq,
				self.builtins.bool_id,
			);
		}

		if is_comparison {
			if !self.coerce_untyped(&mut lhs_ty, rhs_ty)
				&& !self.coerce_untyped(&mut rhs_ty, lhs_ty)
				&& !self.type_matches(lhs_ty, rhs_ty)
			{
				self.errors.push(FrontendError::TypeMismatch {
					location: node.location.clone(),
					expected: self.type_name(lhs_ty),
					found: self.type_name(rhs_ty),
				});
			}
			typed_lhs.ty = Some(lhs_ty);
			typed_rhs.ty = Some(rhs_ty);
			return self.build_bin_expr(
				node.location.clone(),
				op.clone(),
				typed_lhs,
				typed_rhs,
				has_eq,
				self.builtins.bool_id,
			);
		}

		if is_bitwise {
			if !self.is_integer_type(lhs_ty) && !self.is_untyped_numeric(lhs_ty) {
				self.errors.push(FrontendError::InvalidOperator {
					location: node.location.clone(),
					operator: op_name.to_string(),
					lhs: self.type_name(lhs_ty),
					rhs: Some(self.type_name(rhs_ty)),
				});
			}
			if !self.is_integer_type(rhs_ty) && !self.is_untyped_numeric(rhs_ty) {
				self.errors.push(FrontendError::InvalidOperator {
					location: node.location.clone(),
					operator: op_name.to_string(),
					lhs: self.type_name(lhs_ty),
					rhs: Some(self.type_name(rhs_ty)),
				});
			}
			self.coerce_untyped(&mut lhs_ty, rhs_ty);
			self.coerce_untyped(&mut rhs_ty, lhs_ty);
			typed_lhs.ty = Some(lhs_ty);
			typed_rhs.ty = Some(rhs_ty);
			let result_ty = if self.type_matches(lhs_ty, rhs_ty) {
				lhs_ty
			} else {
				self.errors.push(FrontendError::TypeMismatch {
					location: rhs.location.clone(),
					expected: self.type_name(lhs_ty),
					found: self.type_name(rhs_ty),
				});
				self.builtins.unknown_id
			};
			return self.build_bin_expr(
				node.location.clone(),
				op.clone(),
				typed_lhs,
				typed_rhs,
				has_eq,
				result_ty,
			);
		}

		let numeric_op = self.is_numeric_operator(op);
		if numeric_op {
			self.record_numeric_type_param_ops(lhs_ty, rhs_ty, op_name, &node.location);
			let invalid_lhs =
				!self.is_numeric_type(lhs_ty) && !self.is_untyped_numeric(lhs_ty);
			let invalid_rhs =
				!self.is_numeric_type(rhs_ty) && !self.is_untyped_numeric(rhs_ty);
			let generic_lhs =
				matches!(self.arena.get(lhs_ty), ResolvedType::TypeParam(_));
			let generic_rhs =
				matches!(self.arena.get(rhs_ty), ResolvedType::TypeParam(_));
			if (invalid_lhs || invalid_rhs)
				&& !generic_lhs && !generic_rhs
				&& !self.try_operator_overload(
					op_name,
					lhs_ty,
					rhs_ty,
					ctx.self_type,
				) {
				self.errors.push(FrontendError::InvalidOperator {
					location: node.location.clone(),
					operator: op_name.to_string(),
					lhs: self.type_name(lhs_ty),
					rhs: Some(self.type_name(rhs_ty)),
				});
			}
			if self.coerce_untyped(&mut lhs_ty, rhs_ty)
				|| self.coerce_untyped(&mut rhs_ty, lhs_ty)
			{
				typed_lhs.ty = Some(lhs_ty);
				typed_rhs.ty = Some(rhs_ty);
				let result_ty = if self.type_matches(lhs_ty, rhs_ty) {
					lhs_ty
				} else {
					self.errors.push(FrontendError::TypeMismatch {
						location: rhs.location.clone(),
						expected: self.type_name(lhs_ty),
						found: self.type_name(rhs_ty),
					});
					self.builtins.unknown_id
				};
				return self.build_bin_expr(
					node.location.clone(),
					op.clone(),
					typed_lhs,
					typed_rhs,
					has_eq,
					result_ty,
				);
			}
			if !self.type_matches(lhs_ty, rhs_ty)
				&& !self.try_operator_overload(
					op_name,
					lhs_ty,
					rhs_ty,
					ctx.self_type,
				) {
				self.errors.push(FrontendError::TypeMismatch {
					location: rhs.location.clone(),
					expected: self.type_name(lhs_ty),
					found: self.type_name(rhs_ty),
				});
			}
		}
		if numeric_op
			&& (self.is_numeric_type(lhs_ty) || self.is_untyped_numeric(lhs_ty))
			&& self.type_matches(lhs_ty, rhs_ty)
		{
			typed_lhs.ty = Some(lhs_ty);
			typed_rhs.ty = Some(rhs_ty);
			return self.build_bin_expr(
				node.location.clone(),
				op.clone(),
				typed_lhs,
				typed_rhs,
				has_eq,
				lhs_ty,
			);
		}

		if is_assignment {
			let rhs_expected = lhs_ty;
			if !self.coerce_untyped(&mut rhs_ty, rhs_expected)
				&& !self.type_matches(rhs_ty, rhs_expected)
			{
				self.errors.push(FrontendError::TypeMismatch {
					location: node.location.clone(),
					expected: self.type_name(rhs_expected),
					found: self.type_name(rhs_ty),
				});
			}
			typed_rhs.ty = Some(rhs_ty);
			return self.build_bin_expr(
				node.location.clone(),
				op.clone(),
				typed_lhs,
				typed_rhs,
				has_eq,
				self.builtins.void_id,
			);
		}

		self.build_bin_expr(
			node.location.clone(),
			op.clone(),
			typed_lhs,
			typed_rhs,
			has_eq,
			expected.unwrap_or(self.builtins.unknown_id),
		)
	}

	fn type_initializer_items(
		&mut self,
		ctx: &mut TypeContext,
		items: &[InitializerItem],
		element_ty: Option<TypeId>,
	) -> Vec<TypedInitializerItem> {
		let mut typed_items = Vec::new();
		for item in items {
			match item {
				InitializerItem::Positional(value) => {
					let typed_value = self.type_node(value, ctx, element_ty);
					typed_items.push(TypedInitializerItem::Positional(
						typed_value,
					));
				}
				InitializerItem::Named { name, value } => {
					let typed_value = self.type_node(value, ctx, None);
					typed_items.push(TypedInitializerItem::Named {
						name: name.clone(),
						value: typed_value,
					});
				}
			}
		}
		typed_items
	}

	#[allow(clippy::vec_box)]
	fn build_call(
		&self,
		location: SourceLocation,
		callee: Box<TypedAst>,
		args: Vec<Box<TypedAst>>,
		return_ty: TypeId,
	) -> Box<TypedAst> {
		self.typed_with_ty(location, TypedValue::Call { callee, args }, return_ty)
	}

	fn build_dot_expr(
		&self,
		location: SourceLocation,
		lhs: Box<TypedAst>,
		rhs: Box<TypedAst>,
		ty: TypeId,
	) -> Box<TypedAst> {
		self.typed_with_ty(
			location,
			TypedValue::BinExpr {
				op: crate::frontend::Operator::Dot,
				lhs,
				rhs,
				has_eq: false,
			},
			ty,
		)
	}

	fn build_typed_id(
		&self,
		location: SourceLocation,
		name: String,
		ty: TypeId,
	) -> Box<TypedAst> {
		self.typed_with_ty(location, TypedValue::Id(name), ty)
	}

	fn build_bin_expr(
		&self,
		location: SourceLocation,
		op: crate::frontend::Operator,
		lhs: Box<TypedAst>,
		rhs: Box<TypedAst>,
		has_eq: bool,
		ty: TypeId,
	) -> Box<TypedAst> {
		self.typed_with_ty(
			location,
			TypedValue::BinExpr {
				op,
				lhs,
				rhs,
				has_eq,
			},
			ty,
		)
	}

	fn typed_with_ty(
		&self,
		location: SourceLocation,
		value: TypedValue,
		ty: TypeId,
	) -> Box<TypedAst> {
		let mut out = TypedAst::from(location, value);
		out.ty = Some(ty);
		out
	}

	fn type_node_without_move(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		expected: Option<TypeId>,
	) -> Box<TypedAst> {
		let prev = ctx.suppress_move;
		ctx.suppress_move = true;
		let out = self.type_node(node, ctx, expected);
		ctx.suppress_move = prev;
		out
	}

	#[allow(clippy::vec_box)]
	fn type_args_with_expected(
		&mut self,
		ctx: &mut TypeContext,
		args: &[Box<AST>],
		params: &[TypeId],
		offset: usize,
		explicit_types: &HashMap<String, TypeId>,
		check_constraints: bool,
	) -> Vec<Box<TypedAst>> {
		let mut typed_args = Vec::new();
		for (idx, arg) in args.iter().enumerate() {
			let expected = params.get(idx + offset).copied();
			let mut typed_arg = self.type_node(arg, ctx, expected);
			let mut arg_ty = typed_arg.ty.unwrap_or(self.builtins.unknown_id);
			if let Some(expected) = expected {
				let mut coerced = arg_ty;
				if !self.type_matches(arg_ty, expected)
					&& !self.coerce_untyped(&mut coerced, expected)
				{
					self.errors.push(FrontendError::TypeMismatch {
						location: arg.location.clone(),
						expected: self.type_name(expected),
						found: self.type_name(arg_ty),
					});
				} else {
					arg_ty = coerced;
					typed_arg.ty = Some(arg_ty);
					if check_constraints {
						self.check_type_param_ops(
							expected,
							arg_ty,
							&arg.location,
							explicit_types,
						);
						self.check_type_param_members(
							expected,
							arg_ty,
							&arg.location,
							explicit_types,
						);
					}
				}
			}
			typed_args.push(typed_arg);
		}
		typed_args
	}

	#[allow(clippy::vec_box)]
	fn strip_named_args(&self, args: &[Box<AST>]) -> Vec<Box<AST>> {
		let mut flattened = Vec::new();
		for arg in args {
			if let ASTValue::NamedArg { value, .. } = &arg.v {
				flattened.push(value.clone());
			} else {
				flattened.push(arg.clone());
			}
		}
		flattened
	}

	fn param_entries_from_defs(&self, params: &[crate::frontend::FnParam]) -> Vec<ParamEntry> {
		let mut entries = Vec::new();
		let mut fallback_idx = 0usize;
		for param in params {
			let has_default = param.default.is_some();
			if param.names.is_empty() {
				entries.push(ParamEntry {
					name: format!("param{fallback_idx}"),
					has_default,
				});
				fallback_idx += 1;
				continue;
			}
			for name in &param.names {
				entries.push(ParamEntry {
					name: name.clone(),
					has_default,
				});
				fallback_idx += 1;
			}
		}
		entries
	}

	fn param_count_range(
		&self,
		params: &[crate::frontend::FnParam],
		skip: usize,
	) -> (usize, usize) {
		let entries = self.param_entries_from_defs(params);
		let required = entries
			.iter()
			.skip(skip)
			.filter(|entry| !entry.has_default)
			.count();
		let total = entries.len().saturating_sub(skip);
		(required, total)
	}

	#[allow(clippy::vec_box)]
	fn normalize_call_args(
		&mut self,
		args: &[Box<AST>],
		params: Option<&[crate::frontend::FnParam]>,
		skip: usize,
		callee: &str,
		location: &SourceLocation,
	) -> (Vec<Box<AST>>, bool) {
		let mut saw_named = false;
		let mut positional_after_named = false;
		let mut has_named = false;
		for arg in args {
			if matches!(arg.v, ASTValue::NamedArg { .. }) {
				saw_named = true;
				has_named = true;
			} else if saw_named {
				positional_after_named = true;
			}
		}
		if positional_after_named {
			self.errors
				.push(FrontendError::PositionalAfterNamedArgument {
					location: location.clone(),
					callee: callee.to_string(),
				});
		}
		let Some(params) = params else {
			if has_named {
				self.errors.push(FrontendError::InvalidCall {
					location: location.clone(),
					callee: callee.to_string(),
				});
			}
			return (self.strip_named_args(args), has_named);
		};
		let mut entries = self.param_entries_from_defs(params);
		if skip < entries.len() {
			entries = entries.split_off(skip);
		} else {
			entries.clear();
		}
		let mut entry_index: HashMap<String, usize> = HashMap::new();
		for (idx, entry) in entries.iter().enumerate() {
			entry_index.insert(entry.name.clone(), idx);
		}
		let mut positional_args: Vec<Box<AST>> = Vec::new();
		let mut named_args: HashMap<String, Box<AST>> = HashMap::new();
		for arg in args {
			match &arg.v {
				ASTValue::NamedArg { name, value } => {
					if let Some(index) = entry_index.get(name) {
						if *index < positional_args.len() {
							self.errors.push(
								FrontendError::DuplicateArgument {
									location: arg
										.location
										.clone(),
									callee: callee.to_string(),
									name: name.clone(),
								},
							);
							continue;
						}
					} else {
						self.errors.push(
							FrontendError::UnknownNamedArgument {
								location: arg.location.clone(),
								callee: callee.to_string(),
								name: name.clone(),
							},
						);
						continue;
					}
					if named_args.contains_key(name) {
						self.errors.push(
							FrontendError::DuplicateNamedArgument {
								location: arg.location.clone(),
								callee: callee.to_string(),
								name: name.clone(),
							},
						);
						continue;
					}
					named_args.insert(name.clone(), value.clone());
				}
				_ => positional_args.push(arg.clone()),
			}
		}
		let mut ordered = Vec::new();
		for (idx, entry) in entries.iter().enumerate() {
			if idx < positional_args.len() {
				ordered.push(positional_args[idx].clone());
				continue;
			}
			if let Some(value) = named_args.remove(&entry.name) {
				ordered.push(value);
				continue;
			}
			if entry.has_default {
				continue;
			}
			self.errors.push(FrontendError::MissingNamedArgument {
				location: location.clone(),
				callee: callee.to_string(),
				name: entry.name.clone(),
			});
		}
		(ordered, has_named)
	}

	fn resolve_dot_module_id(&self, lhs: &Box<AST>, ctx: &TypeContext) -> Option<ModuleId> {
		if let ASTValue::Id(name) = &lhs.v {
			if let Some(module_id) = ctx.module.imports.alias_to_module.get(name) {
				return Some(module_id.clone());
			}
		}
		self.resolve_module_path(lhs, ctx)
	}

	fn build_module_path_expr(
		&mut self,
		lhs: &Box<AST>,
		ctx: &mut TypeContext,
	) -> Box<TypedAst> {
		let mut segments: Vec<(SourceLocation, String)> = Vec::new();
		let mut current = lhs.as_ref();
		loop {
			match &current.v {
				ASTValue::Id(name) => {
					segments.push((current.location.clone(), name.clone()));
					break;
				}
				ASTValue::BinExpr {
					op: crate::frontend::Operator::Dot,
					lhs,
					rhs,
					..
				} => {
					if let ASTValue::Id(name) = &rhs.v {
						segments.push((rhs.location.clone(), name.clone()));
						current = lhs.as_ref();
						continue;
					}
					return self.type_node(lhs, ctx, None);
				}
				_ => return self.type_node(lhs, ctx, None),
			}
		}
		segments.reverse();
		let mut iter = segments.into_iter();
		let Some((first_loc, first_name)) = iter.next() else {
			return self.type_node(lhs, ctx, None);
		};
		let mut built =
			self.build_typed_id(first_loc, first_name, self.builtins.unknown_id);
		for (loc, name) in iter {
			let rhs = self.build_typed_id(loc, name, self.builtins.unknown_id);
			let mut combined_loc = built.location.clone();
			combined_loc.range.end = rhs.location.range.end;
			built = self.build_dot_expr(
				combined_loc,
				built,
				rhs,
				self.builtins.unknown_id,
			);
		}
		built
	}

	fn type_dot(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		lhs: &Box<AST>,
		rhs: &Box<AST>,
	) -> Box<TypedAst> {
		let rhs_name = match &rhs.v {
			ASTValue::Id(name) => name.clone(),
			_ => "<member>".to_string(),
		};

		if let Some(module_id) = self.resolve_dot_module_id(lhs, ctx) {
			let value_ty = self
				.modules
				.get(&module_id)
				.and_then(|module| module.values.get(&rhs_name))
				.map(|info| info.ty);
			if let Some(value_ty) = value_ty {
				let typed_lhs = self.build_module_path_expr(lhs, ctx);
				let typed_rhs = self.build_typed_id(
					rhs.location.clone(),
					rhs_name.clone(),
					self.builtins.unknown_id,
				);
				return self.build_dot_expr(
					node.location.clone(),
					typed_lhs,
					typed_rhs,
					value_ty,
				);
			}
		}

		if let ASTValue::Id(name) = &lhs.v {
			if let Some(&type_id) = ctx.module.types.get(name) {
				if let ResolvedType::Enum { variants, .. } = self.arena.get(type_id)
				{
					if variants.iter().any(|variant| variant.name == rhs_name) {
						let typed_lhs = self.type_node(lhs, ctx, None);
						let typed_rhs = self.build_typed_id(
							rhs.location.clone(),
							rhs_name.clone(),
							self.builtins.unknown_id,
						);
						return self.build_dot_expr(
							node.location.clone(),
							typed_lhs,
							typed_rhs,
							type_id,
						);
					}
				}
			}
		}

		let typed_lhs = self.type_node_without_move(lhs, ctx, None);
		let lhs_ty = typed_lhs.ty.unwrap_or(self.builtins.unknown_id);
		if matches!(self.arena.get(lhs_ty), ResolvedType::TypeParam(_)) {
			self.record_type_param_member(lhs_ty, &rhs_name, &rhs.location);
			let typed_rhs = self.build_typed_id(
				rhs.location.clone(),
				rhs_name.clone(),
				self.builtins.unknown_id,
			);
			return self.build_dot_expr(
				node.location.clone(),
				typed_lhs,
				typed_rhs,
				self.builtins.unknown_id,
			);
		}
		match self.lookup_field_access(lhs_ty, &rhs_name, ctx.self_type) {
			MemberAccess::Found(field_ty) => {
				if let ASTValue::Id(name) = &lhs.v {
					if !self.is_copy_type(field_ty) {
						self.mark_moved(ctx, name, &lhs.location);
					}
				}
				let typed_rhs = self.build_typed_id(
					rhs.location.clone(),
					rhs_name.clone(),
					self.builtins.unknown_id,
				);
				return self.build_dot_expr(
					node.location.clone(),
					typed_lhs,
					typed_rhs,
					field_ty,
				);
			}
			MemberAccess::Inaccessible => {
				self.errors.push(FrontendError::InaccessibleMember {
					location: node.location.clone(),
					type_name: self.type_name(lhs_ty),
					member: rhs_name.clone(),
				});
				let typed_rhs = self.build_typed_id(
					rhs.location.clone(),
					rhs_name.clone(),
					self.builtins.unknown_id,
				);
				return self.build_dot_expr(
					node.location.clone(),
					typed_lhs,
					typed_rhs,
					self.builtins.unknown_id,
				);
			}
			MemberAccess::Missing => {}
		}

		match self.lookup_method_access(lhs_ty, &rhs_name, ctx.self_type) {
			MemberAccess::Found((method_ty, _)) => {
				let typed_rhs = self.build_typed_id(
					rhs.location.clone(),
					rhs_name.clone(),
					self.builtins.unknown_id,
				);
				return self.build_dot_expr(
					node.location.clone(),
					typed_lhs,
					typed_rhs,
					method_ty,
				);
			}
			MemberAccess::Inaccessible => {
				self.errors.push(FrontendError::InaccessibleMember {
					location: node.location.clone(),
					type_name: self.type_name(lhs_ty),
					member: rhs_name.clone(),
				});
				let typed_rhs = self.build_typed_id(
					rhs.location.clone(),
					rhs_name.clone(),
					self.builtins.unknown_id,
				);
				return self.build_dot_expr(
					node.location.clone(),
					typed_lhs,
					typed_rhs,
					self.builtins.unknown_id,
				);
			}
			MemberAccess::Missing => {}
		}

		self.errors.push(FrontendError::MissingField {
			location: node.location.clone(),
			type_name: self.type_name(lhs_ty),
			field: rhs_name,
		});
		let typed_rhs = self.build_typed_id(
			rhs.location.clone(),
			"<member>".to_string(),
			self.builtins.unknown_id,
		);
		self.build_dot_expr(
			node.location.clone(),
			typed_lhs,
			typed_rhs,
			self.builtins.unknown_id,
		)
	}

	fn type_call(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		callee: &Box<AST>,
		args: &[Box<AST>],
		expected: Option<TypeId>,
	) -> Box<TypedAst> {
		if let Some((callee_name, callee_ty)) =
			self.runtime_function_call_in_constexpr(ctx, callee.as_ref())
		{
			self.errors.push(FrontendError::RuntimeCallInConstexpr {
				location: node.location.clone(),
				callee: callee_name.clone(),
			});
			let explicit_types = HashMap::new();
			let normalized_args = self.strip_named_args(args);
			let typed_args = self.type_args_with_expected(
				ctx,
				&normalized_args,
				&[],
				0,
				&explicit_types,
				false,
			);
			let typed_callee = self.build_typed_id(
				callee.location.clone(),
				callee_name,
				callee_ty,
			);
			return self.build_call(
				node.location.clone(),
				typed_callee,
				typed_args,
				self.builtins.unknown_id,
			);
		}

		if let ASTValue::GenericApply { target, .. } = &callee.v {
			if let ASTValue::Id(name) = &target.v {
				let param_defs = self.fn_param_defs_from_ast(ctx.module_id, name);
				let (normalized_args, has_named) = self.normalize_call_args(
					args,
					param_defs.as_deref(),
					0,
					name,
					&node.location,
				);
				if let Some(expected_args) =
					self.fn_param_count(ctx.module_id, name)
				{
					let mut count_valid =
						expected_args == normalized_args.len();
					if let Some(param_defs) = param_defs.as_deref() {
						let (required, total) =
							self.param_count_range(param_defs, 0);
						count_valid = normalized_args.len() >= required
							&& normalized_args.len() <= total;
					} else if has_named {
						count_valid = true;
					}
					if !count_valid {
						self.errors.push(FrontendError::InvalidCall {
							location: node.location.clone(),
							callee: name.clone(),
						});
						let explicit_types = HashMap::new();
						let typed_args = self.type_args_with_expected(
							ctx,
							&normalized_args,
							&[],
							0,
							&explicit_types,
							false,
						);
						let typed_callee =
							self.type_node(callee, ctx, None);
						return self.build_call(
							node.location.clone(),
							typed_callee,
							typed_args,
							self.builtins.unknown_id,
						);
					}
				}
			}
		}
		let expected_union =
			expected.and_then(|expected_ty| match self.arena.get(expected_ty) {
				ResolvedType::Union { .. } => Some(expected_ty),
				ResolvedType::GenericInstance { base, .. } => {
					matches!(self.arena.get(*base), ResolvedType::Union { .. })
						.then_some(expected_ty)
				}
				_ => None,
			});
		if let ASTValue::BinExpr {
			op: crate::frontend::Operator::Dot,
			lhs,
			rhs,
			..
		} = &callee.v
		{
			if let ASTValue::Id(method_name) = &rhs.v {
				if let Some(module_id) = self.resolve_dot_module_id(lhs, ctx) {
					let value_ty = self
						.modules
						.get(&module_id)
						.and_then(|module| module.values.get(method_name))
						.map(|info| info.ty);
					if let Some(callee_ty) = value_ty {
						let typed_callee =
							self.type_dot(callee, ctx, lhs, rhs);
						let param_defs = self.fn_param_defs_from_ast(
							&module_id,
							method_name,
						);
						let (normalized_args, has_named) = self
							.normalize_call_args(
								args,
								param_defs.as_deref(),
								0,
								method_name,
								&node.location,
							);
						self.maybe_require_runtimeable_for_call(
							ctx,
							&node.location,
							method_name,
							self.fn_flags_from_ast(
								&module_id,
								method_name,
								"runtimeable",
							),
							&normalized_args,
							None,
						);
						let mut return_ty = self.builtins.unknown_id;
						let typed_args =
							if let ResolvedType::Fn {
								params,
								return_type,
							} = self.arena.get(callee_ty)
							{
								let params = params.clone();
								return_ty = *return_type;
								if let Some(param_defs) =
									param_defs.as_deref()
								{
									let (required, total) =
									self.param_count_range(param_defs, 0);
									if normalized_args.len()
										< required
										|| normalized_args
											.len() > total
									{
										self.errors.push(FrontendError::InvalidCall {
										location: node.location.clone(),
										callee: method_name.clone(),
									});
									}
								} else if !has_named
									&& params.len()
										!= normalized_args
											.len()
								{
									self.errors.push(FrontendError::InvalidCall {
									location: node.location.clone(),
									callee: method_name.clone(),
								});
								}
								let explicit_types = HashMap::new();
								self.type_args_with_expected(
									ctx,
									&normalized_args,
									&params,
									0,
									&explicit_types,
									true,
								)
							} else {
								self.errors
									.push(FrontendError::InvalidCall {
									location: callee
										.location
										.clone(),
									callee: self.type_name(
										callee_ty,
									),
								});
								let explicit_types = HashMap::new();
								self.type_args_with_expected(
									ctx,
									&normalized_args,
									&[],
									0,
									&explicit_types,
									false,
								)
							};
						return self.build_call(
							node.location.clone(),
							typed_callee,
							typed_args,
							return_ty,
						);
					}
				}
				let typed_lhs = self.type_node_without_move(lhs, ctx, None);
				let lhs_ty = typed_lhs.ty.unwrap_or(self.builtins.unknown_id);
				match self.lookup_method_access(lhs_ty, method_name, ctx.self_type)
				{
					MemberAccess::Found((method_ty, _needs_ref)) => {
						let (params, return_type) = match self
							.arena
							.get(method_ty)
						{
							ResolvedType::Fn {
								params,
								return_type,
							} => (params.clone(), *return_type),
							_ => {
								self.errors
									.push(FrontendError::InvalidCall {
									location: node
										.location
										.clone(),
									callee: method_name.clone(),
								});
								let typed_callee = self.type_node(
									callee, ctx, None,
								);
								return self.build_call(
									node.location.clone(),
									typed_callee,
									Vec::new(),
									self.builtins.unknown_id,
								);
							}
						};
						let param_defs = self.method_param_defs_from_ast(
							lhs_ty,
							method_name,
						);
						let (normalized_args, has_named) = self
							.normalize_call_args(
								args,
								param_defs.as_deref(),
								1,
								method_name,
								&node.location,
							);
						self.maybe_require_runtimeable_for_call(
							ctx,
							&node.location,
							method_name,
							self.method_flags_from_ast(
								lhs_ty,
								method_name,
								"runtimeable",
							),
							&normalized_args,
							Some(lhs.as_ref()),
						);
						if let Some(param_defs) = param_defs.as_deref() {
							let (required, total) = self
								.param_count_range(param_defs, 1);
							if normalized_args.len() < required
								|| normalized_args.len() > total
							{
								self.errors
									.push(FrontendError::InvalidCall {
									location: node
										.location
										.clone(),
									callee: method_name.clone(),
								});
							}
						} else if !has_named
							&& params.len().saturating_sub(1)
								!= normalized_args.len()
						{
							self.errors.push(
								FrontendError::InvalidCall {
									location: node
										.location
										.clone(),
									callee: method_name.clone(),
								},
							);
						}
						let this_param = params
							.first()
							.copied()
							.unwrap_or(self.builtins.unknown_id);
						if !matches!(
							self.arena.get(this_param),
							ResolvedType::Reference { .. }
						) {
							if let ASTValue::Id(name) = &lhs.v {
								self.mark_moved(
									ctx,
									name,
									&lhs.location,
								);
							}
						}
						let mut this_ty = lhs_ty;
						if let ResolvedType::Reference { mutable, .. } =
							self.arena.get(this_param)
						{
							if !matches!(
								self.arena.get(this_ty),
								ResolvedType::Reference { .. }
							) {
								this_ty = self.arena.add(
									ResolvedType::Reference {
										mutable: *mutable,
										lifetime: None,
										underlying: lhs_ty,
									},
								);
							}
						}
						if !self.type_matches(this_ty, this_param)
							&& !self.reference_allows_inheritance(
								this_ty, this_param,
							) {
							self.errors.push(
								FrontendError::TypeMismatch {
									location: lhs
										.location
										.clone(),
									expected: self.type_name(
										this_param,
									),
									found: self
										.type_name(this_ty),
								},
							);
						}
						let explicit_types = HashMap::new();
						let typed_args = self.type_args_with_expected(
							ctx,
							&normalized_args,
							&params,
							1,
							&explicit_types,
							true,
						);
						let typed_rhs = self.build_typed_id(
							rhs.location.clone(),
							method_name.clone(),
							self.builtins.unknown_id,
						);
						let typed_callee = self.build_dot_expr(
							callee.location.clone(),
							typed_lhs,
							typed_rhs,
							self.builtins.unknown_id,
						);
						return self.build_call(
							node.location.clone(),
							typed_callee,
							typed_args,
							return_type,
						);
					}
					MemberAccess::Inaccessible => {
						self.errors.push(
							FrontendError::InaccessibleMember {
								location: node.location.clone(),
								type_name: self.type_name(lhs_ty),
								member: method_name.clone(),
							},
						);
						let typed_callee =
							self.type_node(callee, ctx, None);
						let explicit_types = HashMap::new();
						let normalized_args = self.strip_named_args(args);
						let typed_args = self.type_args_with_expected(
							ctx,
							&normalized_args,
							&[],
							0,
							&explicit_types,
							false,
						);
						return self.build_call(
							node.location.clone(),
							typed_callee,
							typed_args,
							self.builtins.unknown_id,
						);
					}
					MemberAccess::Missing => {}
				}
			}
		}

		let typed_callee = self.type_node_without_move(callee, ctx, None);
		let mut callee_ty = typed_callee.ty.unwrap_or(self.builtins.unknown_id);
		if callee_ty == self.builtins.unknown_id {
			if let ASTValue::GenericApply { target, .. } = &callee.v {
				if let ASTValue::Id(name) = &target.v {
					if let Some(info) = ctx.module.values.get(name) {
						callee_ty = info.ty;
					}
				}
			}
		}
		let callee_type_id = if callee_ty == self.builtins.type_id {
			if let ASTValue::Id(name) = &callee.v {
				ctx.module.types.get(name).copied()
			} else {
				None
			}
		} else {
			None
		};
		let mut typed_args = Vec::new();
		let mut return_ty = self.builtins.unknown_id;
		if let Some(type_id) = callee_type_id {
			let method_info = match self.lookup_method_access(
				type_id,
				"__construct",
				ctx.self_type,
			) {
				MemberAccess::Found((method_ty, _)) => {
					match self.arena.get(method_ty) {
						ResolvedType::Fn {
							params,
							return_type,
						} => Some((method_ty, params.clone(), *return_type)),
						_ => None,
					}
				}
				MemberAccess::Inaccessible => {
					self.errors.push(FrontendError::InaccessibleMember {
						location: node.location.clone(),
						type_name: self.type_name(type_id),
						member: "__construct".to_string(),
					});
					None
				}
				MemberAccess::Missing => None,
			};
			if let Some((_method_ty, params, _)) = method_info {
				let type_name = self.type_name(type_id);
				let param_defs =
					self.method_param_defs_from_ast(type_id, "__construct");
				let (normalized_args, has_named) = self.normalize_call_args(
					args,
					param_defs.as_deref(),
					1,
					&type_name,
					&node.location,
				);
				if let Some(param_defs) = param_defs.as_deref() {
					let (required, total) =
						self.param_count_range(param_defs, 1);
					if normalized_args.len() < required
						|| normalized_args.len() > total
					{
						self.errors.push(FrontendError::InvalidCall {
							location: node.location.clone(),
							callee: type_name.clone(),
						});
						let explicit_types = HashMap::new();
						let typed_args = self.type_args_with_expected(
							ctx,
							&normalized_args,
							&[],
							0,
							&explicit_types,
							false,
						);
						return self.build_call(
							node.location.clone(),
							typed_callee,
							typed_args,
							type_id,
						);
					}
				} else if !has_named
					&& params.len().saturating_sub(1) != normalized_args.len()
				{
					self.errors.push(FrontendError::InvalidCall {
						location: node.location.clone(),
						callee: type_name.clone(),
					});
					let explicit_types = HashMap::new();
					let typed_args = self.type_args_with_expected(
						ctx,
						&normalized_args,
						&[],
						0,
						&explicit_types,
						false,
					);
					return self.build_call(
						node.location.clone(),
						typed_callee,
						typed_args,
						type_id,
					);
				}
				let this_param =
					params.first().copied().unwrap_or(self.builtins.unknown_id);
				let mut this_ty = type_id;
				if let ResolvedType::Reference { mutable, .. } =
					self.arena.get(this_param)
				{
					this_ty = self.arena.add(ResolvedType::Reference {
						mutable: *mutable,
						lifetime: None,
						underlying: type_id,
					});
				}
				if !self.type_matches(this_ty, this_param) {
					self.errors.push(FrontendError::TypeMismatch {
						location: callee.location.clone(),
						expected: self.type_name(this_param),
						found: self.type_name(this_ty),
					});
				}
				let explicit_types = HashMap::new();
				typed_args = self.type_args_with_expected(
					ctx,
					&normalized_args,
					&params,
					1,
					&explicit_types,
					false,
				);
				let union_return = expected_union.filter(|union_ty| {
					self.union_accepts_type(*union_ty, type_id)
				});
				if let Some(expected_union) = expected_union
					&& union_return.is_none()
				{
					self.errors.push(FrontendError::TypeMismatch {
						location: node.location.clone(),
						expected: self.type_name(expected_union),
						found: self.type_name(type_id),
					});
				}
				return_ty = union_return.unwrap_or(type_id);
				return self.build_call(
					node.location.clone(),
					typed_callee,
					typed_args,
					return_ty,
				);
			}
		}

		if let ResolvedType::Fn {
			params,
			return_type,
		} = self.arena.get(callee_ty)
		{
			let mut params = params.clone();
			let return_type_value = *return_type;
			let callee_name = match &callee.v {
				ASTValue::Id(name) => Some(name.clone()),
				ASTValue::GenericApply { target, .. } => {
					if let ASTValue::Id(name) = &target.v {
						Some(name.clone())
					} else {
						None
					}
				}
				_ => None,
			};
			let callee_display = callee_name
				.clone()
				.unwrap_or_else(|| self.type_name(callee_ty));
			let param_defs = callee_name
				.as_ref()
				.and_then(|name| self.fn_param_defs_from_ast(ctx.module_id, name));
			let (normalized_args, has_named) = self.normalize_call_args(
				args,
				param_defs.as_deref(),
				0,
				&callee_display,
				&node.location,
			);
			if let Some(name) = callee_name.as_ref() {
				self.maybe_require_runtimeable_for_call(
					ctx,
					&node.location,
					name,
					self.fn_flags_from_ast(ctx.module_id, name, "runtimeable"),
					&normalized_args,
					None,
				);
			}
			if let Some(name) = callee_name.as_ref() {
				if let Some(ast_params) =
					self.fn_param_types_from_ast(ctx.module_id, name)
					&& ast_params.len() == normalized_args.len()
				{
					params = ast_params;
				}
			}
			let expected_count = match &callee.v {
				ASTValue::Id(name) => self.fn_param_count(ctx.module_id, name),
				ASTValue::GenericApply { target, .. } => {
					if let ASTValue::Id(name) = &target.v {
						self.fn_param_count(ctx.module_id, name)
					} else {
						None
					}
				}
				_ => None,
			};
			let expected_count = expected_count.unwrap_or(params.len());
			let mut count_valid = expected_count == normalized_args.len();
			if let Some(param_defs) = param_defs.as_deref() {
				let (required, total) = self.param_count_range(param_defs, 0);
				count_valid = normalized_args.len() >= required
					&& normalized_args.len() <= total;
			} else if has_named {
				count_valid = true;
			}
			if !count_valid {
				self.errors.push(FrontendError::InvalidCall {
					location: node.location.clone(),
					callee: callee_display.clone(),
				});
				let explicit_types = HashMap::new();
				typed_args = self.type_args_with_expected(
					ctx,
					&normalized_args,
					&[],
					0,
					&explicit_types,
					false,
				);
				return self.build_call(
					node.location.clone(),
					typed_callee,
					typed_args,
					return_type_value,
				);
			}
			return_ty = return_type_value;
			let explicit_type_args = if let ASTValue::GenericApply { args, .. } =
				&callee.v
			{
				let mut out = Vec::new();
				for arg in args {
					let typed_arg = match arg {
						GenericArg::Type(ty) => Some(self.resolve_type(
							ctx.module_id,
							ty,
							&ctx.generic_types,
							ctx.self_type,
							&callee.location,
						)),
						GenericArg::Expr(_) => None,
						GenericArg::Name(_) => None,
					};
					if let Some(typed_arg) = typed_arg {
						out.push(typed_arg);
					}
				}
				Some(out)
			} else {
				None
			};
			let mut explicit_type_map: HashMap<String, TypeId> = HashMap::new();
			if let Some(explicit_type_args) = &explicit_type_args {
				let generic_names = match &callee.v {
					ASTValue::GenericApply { target, .. } => {
						if let ASTValue::Id(name) = &target.v {
							self.fn_generic_param_names(
								ctx.module_id,
								name,
							)
						} else {
							None
						}
					}
					_ => None,
				};
				if let Some(generic_names) = generic_names {
					for (idx, name) in generic_names.iter().enumerate() {
						if let Some(explicit) = explicit_type_args.get(idx)
						{
							explicit_type_map
								.insert(name.clone(), *explicit);
						}
					}
				} else {
					let mut idx = 0;
					for param in &params {
						if let ResolvedType::TypeParam(name) =
							self.arena.get(*param)
						{
							if explicit_type_map.contains_key(name) {
								continue;
							}
							if let Some(explicit) =
								explicit_type_args.get(idx)
							{
								explicit_type_map.insert(
									name.clone(),
									*explicit,
								);
								idx += 1;
							}
						}
					}
				}
			}
			let mut type_bindings: HashMap<TypeId, TypeId> = HashMap::new();
			let mut type_name_bindings: HashMap<String, TypeId> = HashMap::new();
			let mut inferred_args: Vec<(Option<TypeId>, TypeId, SourceLocation)> =
				Vec::new();
			let mut checked_type_params: HashSet<String> = HashSet::new();
			for (idx, arg) in normalized_args.iter().enumerate() {
				let mut expected = params.get(idx).copied();
				let mut type_param_name = None;
				if let Some(expected_ty) = expected
					&& let ResolvedType::TypeParam(name) =
						self.arena.get(expected_ty)
				{
					type_param_name = Some(name.clone());
					if let Some(explicit) = explicit_type_map.get(name) {
						expected = Some(*explicit);
					}
				}
				let mut typed_arg = self.type_node(arg, ctx, expected);
				let mut arg_ty = typed_arg.ty.unwrap_or(self.builtins.unknown_id);
				if let Some(expected) = expected {
					if let ResolvedType::TypeParam(name) =
						self.arena.get(expected)
					{
						if let Some(bound) = type_name_bindings.get(name) {
							if !self.type_matches(arg_ty, *bound) {
								self.errors.push(FrontendError::TypeMismatch {
									location: arg.location.clone(),
									expected: self.type_name(*bound),
									found: self.type_name(arg_ty),
								});
							}
						} else {
							type_name_bindings
								.insert(name.clone(), arg_ty);
						}
						if let Some(bound) = type_bindings.get(&expected) {
							if !self.type_matches(arg_ty, *bound) {
								self.errors.push(FrontendError::TypeMismatch {
									location: arg.location.clone(),
									expected: self.type_name(*bound),
									found: self.type_name(arg_ty),
								});
							}
						} else {
							type_bindings.insert(expected, arg_ty);
						}
					}
					let mut coerced = arg_ty;
					if !self.type_matches(arg_ty, expected)
						&& !self.coerce_untyped(&mut coerced, expected)
					{
						self.errors.push(FrontendError::TypeMismatch {
							location: arg.location.clone(),
							expected: self.type_name(expected),
							found: self.type_name(arg_ty),
						});
					} else {
						arg_ty = coerced;
						typed_arg.ty = Some(arg_ty);
						let should_check = match type_param_name.as_ref() {
							Some(name) => checked_type_params
								.insert(name.clone()),
							None => true,
						};
						if should_check {
							self.check_type_param_ops(
								expected,
								arg_ty,
								&arg.location,
								&explicit_type_map,
							);
							self.check_type_param_members(
								expected,
								arg_ty,
								&arg.location,
								&explicit_type_map,
							);
						}
					}
				}
				inferred_args.push((expected, arg_ty, arg.location.clone()));
				typed_args.push(typed_arg);
			}
			if type_bindings.len() == 1 {
				let (_, bound_ty) = type_bindings.iter().next().expect("binding");
				for (expected, arg_ty, location) in &inferred_args {
					if expected.is_none()
						&& !self.type_matches(*arg_ty, *bound_ty)
					{
						self.errors.push(FrontendError::TypeMismatch {
							location: location.clone(),
							expected: self.type_name(*bound_ty),
							found: self.type_name(*arg_ty),
						});
					}
				}
			}
		} else if let MemberAccess::Found((method_ty, _needs_ref)) =
			self.lookup_method_access(callee_ty, "operator()", ctx.self_type)
		{
			let method_info = match self.arena.get(method_ty) {
				ResolvedType::Fn {
					params,
					return_type,
				} => Some((params.clone(), *return_type)),
				_ => None,
			};
			let Some((params, method_return)) = method_info else {
				self.errors.push(FrontendError::InvalidCall {
					location: callee.location.clone(),
					callee: "operator()".to_string(),
				});
				let explicit_types = HashMap::new();
				let normalized_args = self.strip_named_args(args);
				typed_args = self.type_args_with_expected(
					ctx,
					&normalized_args,
					&[],
					0,
					&explicit_types,
					false,
				);
				return self.build_call(
					node.location.clone(),
					typed_callee,
					typed_args,
					self.builtins.unknown_id,
				);
			};
			let param_defs = self.method_param_defs_from_ast(callee_ty, "operator()");
			let (normalized_args, has_named) = self.normalize_call_args(
				args,
				param_defs.as_deref(),
				1,
				"operator()",
				&node.location,
			);
			if let Some(param_defs) = param_defs.as_deref() {
				let (required, total) = self.param_count_range(param_defs, 1);
				if normalized_args.len() < required || normalized_args.len() > total
				{
					self.errors.push(FrontendError::InvalidCall {
						location: node.location.clone(),
						callee: "operator()".to_string(),
					});
				}
			} else if !has_named
				&& params.len().saturating_sub(1) != normalized_args.len()
			{
				self.errors.push(FrontendError::InvalidCall {
					location: node.location.clone(),
					callee: "operator()".to_string(),
				});
			}
			let this_param =
				params.first().copied().unwrap_or(self.builtins.unknown_id);
			if !matches!(self.arena.get(this_param), ResolvedType::Reference { .. }) {
				if let ASTValue::Id(name) = &callee.v {
					self.mark_moved(ctx, name, &callee.location);
				}
			}
			let mut this_ty = callee_ty;
			if let ResolvedType::Reference { mutable, .. } = self.arena.get(this_param)
			{
				if !matches!(
					self.arena.get(this_ty),
					ResolvedType::Reference { .. }
				) {
					this_ty = self.arena.add(ResolvedType::Reference {
						mutable: *mutable,
						lifetime: None,
						underlying: callee_ty,
					});
				}
			}
			if !self.type_matches(this_ty, this_param)
				&& !self.reference_allows_inheritance(this_ty, this_param)
			{
				self.errors.push(FrontendError::TypeMismatch {
					location: callee.location.clone(),
					expected: self.type_name(this_param),
					found: self.type_name(this_ty),
				});
			}
			let mut type_bindings: HashMap<TypeId, TypeId> = HashMap::new();
			let mut type_name_bindings: HashMap<String, TypeId> = HashMap::new();
			let mut inferred_args: Vec<(Option<TypeId>, TypeId, SourceLocation)> =
				Vec::new();
			for (idx, arg) in normalized_args.iter().enumerate() {
				let expected = params.get(idx + 1).copied();
				let mut typed_arg = self.type_node(arg, ctx, expected);
				let mut arg_ty = typed_arg.ty.unwrap_or(self.builtins.unknown_id);
				if let Some(expected) = expected {
					if let ResolvedType::TypeParam(name) =
						self.arena.get(expected)
					{
						if let Some(bound) = type_name_bindings.get(name) {
							if !self.type_matches(arg_ty, *bound) {
								self.errors.push(FrontendError::TypeMismatch {
									location: arg.location.clone(),
									expected: self.type_name(*bound),
									found: self.type_name(arg_ty),
								});
							}
						} else {
							type_name_bindings
								.insert(name.clone(), arg_ty);
						}
						if let Some(bound) = type_bindings.get(&expected) {
							if !self.type_matches(arg_ty, *bound) {
								self.errors.push(FrontendError::TypeMismatch {
									location: arg.location.clone(),
									expected: self.type_name(*bound),
									found: self.type_name(arg_ty),
								});
							}
						} else {
							type_bindings.insert(expected, arg_ty);
						}
					}
					let mut coerced = arg_ty;
					if !self.type_matches(arg_ty, expected)
						&& !self.coerce_untyped(&mut coerced, expected)
					{
						self.errors.push(FrontendError::TypeMismatch {
							location: arg.location.clone(),
							expected: self.type_name(expected),
							found: self.type_name(arg_ty),
						});
					} else {
						arg_ty = coerced;
						typed_arg.ty = Some(arg_ty);
						let explicit_types: HashMap<String, TypeId> =
							HashMap::new();
						self.check_type_param_ops(
							expected,
							arg_ty,
							&arg.location,
							&explicit_types,
						);
						self.check_type_param_members(
							expected,
							arg_ty,
							&arg.location,
							&explicit_types,
						);
					}
				}
				inferred_args.push((expected, arg_ty, arg.location.clone()));
				typed_args.push(typed_arg);
			}
			if type_bindings.len() == 1 {
				let (_, bound_ty) = type_bindings.iter().next().expect("binding");
				for (expected, arg_ty, location) in &inferred_args {
					if expected.is_none()
						&& !self.type_matches(*arg_ty, *bound_ty)
					{
						self.errors.push(FrontendError::TypeMismatch {
							location: location.clone(),
							expected: self.type_name(*bound_ty),
							found: self.type_name(*arg_ty),
						});
					}
				}
			}
			return_ty = method_return;
		} else if let MemberAccess::Inaccessible =
			self.lookup_method_access(callee_ty, "operator()", ctx.self_type)
		{
			self.errors.push(FrontendError::InaccessibleMember {
				location: node.location.clone(),
				type_name: self.type_name(callee_ty),
				member: "operator()".to_string(),
			});
			let explicit_types = HashMap::new();
			let normalized_args = self.strip_named_args(args);
			typed_args = self.type_args_with_expected(
				ctx,
				&normalized_args,
				&[],
				0,
				&explicit_types,
				false,
			);
			return_ty = self.builtins.unknown_id;
		} else if let Some(type_id) = callee_type_id {
			let union_return = expected_union
				.filter(|union_ty| self.union_accepts_type(*union_ty, type_id));
			if let Some(expected_union) = expected_union
				&& union_return.is_none()
			{
				self.errors.push(FrontendError::TypeMismatch {
					location: node.location.clone(),
					expected: self.type_name(expected_union),
					found: self.type_name(type_id),
				});
			}
			let explicit_types = HashMap::new();
			let normalized_args = self.strip_named_args(args);
			typed_args = self.type_args_with_expected(
				ctx,
				&normalized_args,
				&[],
				0,
				&explicit_types,
				false,
			);
			return_ty = union_return.unwrap_or(type_id);
		} else {
			if callee_ty != self.builtins.unknown_id {
				self.errors.push(FrontendError::InvalidCall {
					location: callee.location.clone(),
					callee: self.type_name(callee_ty),
				});
			}
			let explicit_types = HashMap::new();
			let normalized_args = self.strip_named_args(args);
			typed_args = self.type_args_with_expected(
				ctx,
				&normalized_args,
				&[],
				0,
				&explicit_types,
				false,
			);
		}
		self.build_call(node.location.clone(), typed_callee, typed_args, return_ty)
	}

	fn type_match(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		binder: &Option<crate::frontend::MatchBinder>,
		scrutinee: &Box<AST>,
		cases: &[MatchCase],
	) -> Box<TypedAst> {
		let typed_scrutinee = self.type_node(scrutinee, ctx, None);
		self.clear_temporary_borrows(ctx);
		let scrutinee_ty = typed_scrutinee.ty.unwrap_or(self.builtins.unknown_id);
		let mut typed_cases = Vec::new();
		let mut match_ty = None;

		for case in cases {
			self.push_scope(ctx);
			let _typed_binder = binder.as_ref().map(|b| {
				let mut binder_ty = scrutinee_ty;
				if b.by_ref {
					binder_ty = self.arena.add(ResolvedType::Reference {
						mutable: b.mutable,
						lifetime: b.lifetime,
						underlying: scrutinee_ty,
					});
				}
				if b.by_ref {
					if let Some(base) = self.lhs_base_id(scrutinee.as_ref()) {
						let kind = if b.mutable {
							BorrowKind::Mutable
						} else {
							BorrowKind::Shared
						};
						let mut can_borrow = true;
						if b.mutable {
							if let Some(info) =
								self.lookup_value_info(ctx, base)
							{
								if !info.mutable {
									self.errors.push(
										FrontendError::MutBorrowOfImmutable {
											location: node.location.clone(),
											name: base.to_string(),
										},
									);
									can_borrow = false;
								}
							}
						}
						if can_borrow
							&& self.register_borrow(
								ctx,
								base,
								kind,
								&node.location,
							) {
							let is_static = b.lifetime == Some('s');
							if let Some(scope) =
								ctx.ref_scopes.last_mut()
							{
								scope.insert(
									b.name.clone(),
									RefBinding {
										base: base
											.to_string(
											),
										kind,
										is_static,
									},
								);
							}
						}
					}
				}
				ctx.value_scopes.last_mut().expect("scope").insert(
					b.name.clone(),
					ValueInfo {
						ty: binder_ty,
						constexpr: ctx.in_constexpr,
						mutable: b.mutable,
						moved: false,
						moved_at: None,
					},
				);
				TypedMatchBinder {
					by_ref: b.by_ref,
					mutable: b.mutable,
					lifetime: b.lifetime,
					name: b.name.clone(),
				}
			});

			let typed_pattern =
				match &case.pattern {
					MatchCasePattern::Default => TypedMatchCasePattern::Default,
					MatchCasePattern::Exprs(exprs) => {
						let mut typed_exprs = Vec::new();
						for expr in exprs {
							let typed_expr = self.type_node(
								expr,
								ctx,
								Some(scrutinee_ty),
							);
							self.clear_temporary_borrows(ctx);
							let expr_ty = typed_expr.ty.unwrap_or(
								self.builtins.unknown_id,
							);
							if !self.type_matches(expr_ty, scrutinee_ty)
							{
								self.errors.push(FrontendError::TypeMismatch {
									location: expr.location.clone(),
									expected: self.type_name(scrutinee_ty),
									found: self.type_name(expr_ty),
								});
							}
							typed_exprs.push(typed_expr);
						}
						TypedMatchCasePattern::Exprs(typed_exprs)
					}
					MatchCasePattern::Type(ty) => {
						let resolved = self.resolve_type(
							ctx.module_id,
							ty,
							&ctx.generic_types,
							ctx.self_type,
							&node.location,
						);
						TypedMatchCasePattern::Type(resolved)
					}
				};

			let typed_guard = case.guard.as_ref().map(|guard| {
				let typed_guard =
					self.type_node(guard, ctx, Some(self.builtins.bool_id));
				self.clear_temporary_borrows(ctx);
				let guard_ty = typed_guard.ty.unwrap_or(self.builtins.unknown_id);
				if !self.type_matches(guard_ty, self.builtins.bool_id) {
					self.errors.push(FrontendError::NonBoolCondition {
						location: guard.location.clone(),
						found: self.type_name(guard_ty),
					});
				}
				typed_guard
			});

			let typed_body = self.type_node(&case.body, ctx, None);
			self.clear_temporary_borrows(ctx);
			let body_ty = typed_body.ty.unwrap_or(self.builtins.unknown_id);
			if let Some(current) = match_ty {
				if !self.type_matches(body_ty, current) {
					self.errors.push(FrontendError::TypeMismatch {
						location: case.body.location.clone(),
						expected: self.type_name(current),
						found: self.type_name(body_ty),
					});
				}
			} else {
				match_ty = Some(body_ty);
			}

			typed_cases.push(TypedMatchCase {
				pattern: typed_pattern,
				guard: typed_guard,
				body: typed_body,
			});
			self.pop_scope(ctx);
		}

		let mut out = TypedAst::from(
			node.location.clone(),
			TypedValue::Match {
				binder: binder.as_ref().map(|b| TypedMatchBinder {
					by_ref: b.by_ref,
					mutable: b.mutable,
					lifetime: b.lifetime,
					name: b.name.clone(),
				}),
				scrutinee: typed_scrutinee,
				cases: typed_cases,
			},
		);
		out.ty = Some(match_ty.unwrap_or(self.builtins.void_id));
		out
	}

	fn type_if(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		cond: &Box<AST>,
		decl: &Option<Box<AST>>,
		body: &Box<AST>,
		else_: &Option<Box<AST>>,
	) -> Box<TypedAst> {
		self.push_scope(ctx);
		let typed_decl = decl.as_ref().map(|d| self.type_node(d, ctx, None));
		self.clear_temporary_borrows(ctx);
		let typed_cond = self.type_node(cond, ctx, Some(self.builtins.bool_id));
		self.clear_temporary_borrows(ctx);
		let cond_ty = typed_cond.ty.unwrap_or(self.builtins.unknown_id);
		if !self.type_matches(cond_ty, self.builtins.bool_id) {
			self.errors.push(FrontendError::NonBoolCondition {
				location: cond.location.clone(),
				found: self.type_name(cond_ty),
			});
		}
		let typed_body = self.type_node(body, ctx, None);
		self.clear_temporary_borrows(ctx);
		let body_ty = typed_body.ty.unwrap_or(self.builtins.unknown_id);
		self.pop_scope(ctx);

		let typed_else = else_.as_ref().map(|e| self.type_node(e, ctx, None));
		self.clear_temporary_borrows(ctx);
		let out_ty = if let Some(typed_else) = &typed_else {
			let else_ty = typed_else.ty.unwrap_or(self.builtins.unknown_id);
			if !self.type_matches(body_ty, else_ty) {
				self.errors.push(FrontendError::TypeMismatch {
					location: typed_else.location.clone(),
					expected: self.type_name(body_ty),
					found: self.type_name(else_ty),
				});
			}
			body_ty
		} else {
			self.builtins.void_id
		};

		let mut out = TypedAst::from(
			node.location.clone(),
			TypedValue::If {
				cond: typed_cond,
				decl: typed_decl,
				body: typed_body,
				else_: typed_else,
			},
		);
		out.ty = Some(out_ty);
		out
	}

	fn type_while(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		cond: &Box<AST>,
		decl: &Option<Box<AST>>,
		body: &Box<AST>,
	) -> Box<TypedAst> {
		self.push_scope(ctx);
		let typed_decl = decl.as_ref().map(|d| self.type_node(d, ctx, None));
		self.clear_temporary_borrows(ctx);
		let typed_cond = self.type_node(cond, ctx, Some(self.builtins.bool_id));
		self.clear_temporary_borrows(ctx);
		let cond_ty = typed_cond.ty.unwrap_or(self.builtins.unknown_id);
		if !self.type_matches(cond_ty, self.builtins.bool_id) {
			self.errors.push(FrontendError::NonBoolCondition {
				location: cond.location.clone(),
				found: self.type_name(cond_ty),
			});
		}
		let typed_body = self.type_node(body, ctx, None);
		self.clear_temporary_borrows(ctx);
		self.pop_scope(ctx);

		let mut out = TypedAst::from(
			node.location.clone(),
			TypedValue::While {
				cond: typed_cond,
				decl: typed_decl,
				body: typed_body,
			},
		);
		out.ty = Some(self.builtins.void_id);
		out
	}

	fn type_for_loop(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		init: &Option<Box<AST>>,
		cond: &Option<Box<AST>>,
		step: &Option<Box<AST>>,
		body: &Box<AST>,
	) -> Box<TypedAst> {
		self.push_scope(ctx);
		let typed_init = init.as_ref().map(|v| self.type_node(v, ctx, None));
		self.clear_temporary_borrows(ctx);
		let typed_cond = cond.as_ref().map(|v| self.type_node(v, ctx, None));
		self.clear_temporary_borrows(ctx);
		if let Some(typed_cond) = &typed_cond {
			let cond_ty = typed_cond.ty.unwrap_or(self.builtins.unknown_id);
			if !self.type_matches(cond_ty, self.builtins.bool_id) {
				self.errors.push(FrontendError::NonBoolCondition {
					location: typed_cond.location.clone(),
					found: self.type_name(cond_ty),
				});
			}
		}
		let typed_step = step.as_ref().map(|v| self.type_node(v, ctx, None));
		self.clear_temporary_borrows(ctx);
		let typed_body = self.type_node(body, ctx, None);
		self.clear_temporary_borrows(ctx);
		self.pop_scope(ctx);

		let mut out = TypedAst::from(
			node.location.clone(),
			TypedValue::ForLoop {
				init: typed_init,
				cond: typed_cond,
				step: typed_step,
				body: typed_body,
			},
		);
		out.ty = Some(self.builtins.void_id);
		out
	}

	fn type_for(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		bindings: &[Box<AST>],
		iter: &Box<AST>,
		body: &Box<AST>,
	) -> Box<TypedAst> {
		let typed_iter = self.type_node(iter, ctx, None);
		self.clear_temporary_borrows(ctx);
		let iter_ty = typed_iter.ty.unwrap_or(self.builtins.unknown_id);
		self.push_scope(ctx);
		for binding in bindings {
			if let ASTValue::Id(name) = &binding.v {
				ctx.value_scopes.last_mut().expect("scope").insert(
					name.clone(),
					ValueInfo {
						ty: iter_ty,
						constexpr: false,
						mutable: false,
						moved: false,
						moved_at: None,
					},
				);
			}
		}
		let typed_body = self.type_node(body, ctx, None);
		self.clear_temporary_borrows(ctx);
		self.pop_scope(ctx);

		let mut typed_bindings = Vec::new();
		for binding in bindings {
			typed_bindings.push(self.type_node(binding, ctx, None));
		}
		let mut out = TypedAst::from(
			node.location.clone(),
			TypedValue::For {
				bindings: typed_bindings,
				iter: typed_iter,
				body: typed_body,
			},
		);
		out.ty = Some(self.builtins.void_id);
		out
	}

	fn type_assignment(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		name: &str,
		value: &Box<AST>,
	) -> Box<TypedAst> {
		let mut target_ty = None;
		let mut target_mutable = None;
		let mut found_in_scope = false;
		let mut target_found = false;
		if name != "_" {
			if let Some(state) = ctx.borrow_state.get(name) {
				if !state.shared_locations.is_empty()
					|| state.mutable_location.is_some()
				{
					let borrowed_at =
						state.mutable_location.clone().or_else(|| {
							state.shared_locations.first().cloned()
						});
					self.errors.push(FrontendError::AssignWhileBorrowed {
						location: node.location.clone(),
						name: name.to_string(),
						borrowed_at,
					});
				}
			}
		}
		for scope in ctx.value_scopes.iter().rev() {
			if let Some(info) = scope.get(name) {
				target_ty = Some(info.ty);
				target_mutable = Some(info.mutable);
				found_in_scope = true;
				target_found = true;
				break;
			}
		}
		if target_ty.is_none() {
			if let Some(info) = ctx.module.values.get(name) {
				target_ty = Some(info.ty);
				target_mutable = Some(info.mutable);
				target_found = true;
			}
		}
		if target_found && name != "_" {
			self.release_binding_for_name(ctx, name);
		}
		let mut typed_value = self.type_node(value, ctx, target_ty);
		let mut value_ty = typed_value.ty.unwrap_or(self.builtins.unknown_id);
		if let Some(target_mutable) = target_mutable {
			if !target_mutable && name != "_" {
				self.errors.push(FrontendError::AssignToImmutable {
					location: node.location.clone(),
					name: name.to_string(),
				});
			}
			if target_mutable && found_in_scope {
				for scope in ctx.value_scopes.iter_mut().rev() {
					if let Some(info) = scope.get_mut(name) {
						info.moved = false;
						info.moved_at = None;
						break;
					}
				}
			}
		}
		if let Some(expected) = target_ty {
			let mut coerced = value_ty;
			if !self.type_matches(value_ty, expected)
				&& !self.coerce_untyped(&mut coerced, expected)
			{
				self.errors.push(FrontendError::TypeMismatch {
					location: node.location.clone(),
					expected: self.type_name(expected),
					found: self.type_name(value_ty),
				});
			} else {
				value_ty = coerced;
				typed_value.ty = Some(value_ty);
			}
		} else if name != "_" {
			self.errors.push(FrontendError::UnknownValue {
				location: node.location.clone(),
				name: name.to_string(),
			});
		}
		if name != "_" {
			if let Some((ref_mutable, inner)) = self.top_level_ref(value.as_ref()) {
				if let Some(base) = self.lhs_base_id(inner) {
					let kind = if ref_mutable {
						BorrowKind::Mutable
					} else {
						BorrowKind::Shared
					};
					let is_static = target_ty
						.map(|ty| self.is_static_ref_type(ty))
						.unwrap_or(false);
					self.promote_temporary_borrow(
						ctx,
						name,
						RefBinding {
							base: base.to_string(),
							kind,
							is_static,
						},
						&node.location,
					);
				}
			}
		}

		let mut out = TypedAst::from(
			node.location.clone(),
			TypedValue::Set(name.to_string(), typed_value),
		);
		out.ty = Some(self.builtins.void_id);
		out
	}

	fn type_declaration(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		name: &str,
		value: &Box<AST>,
		mutable: bool,
		constexpr: bool,
	) -> Box<TypedAst> {
		let prev_constexpr = ctx.in_constexpr;
		let prev_self_type = ctx.self_type;
		ctx.in_constexpr = constexpr;
		if constexpr {
			if let Some(module) = self.modules.get(ctx.module_id) {
				if let Some(&ty_id) = module.types.get(name) {
					ctx.self_type = Some(ty_id);
				}
			}
		}
		let typed_value = self.type_node(value, ctx, None);
		ctx.in_constexpr = prev_constexpr;
		ctx.self_type = prev_self_type;
		let mut value_ty = typed_value.ty.unwrap_or(self.builtins.unknown_id);
		if !constexpr && !ctx.in_constexpr {
			if value_ty == self.builtins.untyped_int_id {
				value_ty = self.builtins.int_id;
			} else if value_ty == self.builtins.untyped_float_id {
				value_ty = self.builtins.f64_id;
			}
		}
		if mutable && !constexpr && self.is_module_scope(ctx) {
			if !self.is_static_mut_ref(value_ty) {
				self.errors.push(FrontendError::NonStaticModuleMut {
					location: node.location.clone(),
					name: name.to_string(),
				});
			}
		}
		if self.type_contains_pointer(value_ty) && !self.is_unsafe_context(ctx) {
			self.errors.push(FrontendError::PointerRequiresUnsafe {
				location: node.location.clone(),
			});
		}
		if ctx.in_constexpr && self.type_contains_pointer(value_ty) {
			self.errors.push(FrontendError::PointerInConstexpr {
				location: node.location.clone(),
			});
		}
		ctx.value_scopes.last_mut().expect("scope").insert(
			name.to_string(),
			ValueInfo {
				ty: value_ty,
				constexpr,
				mutable,
				moved: false,
				moved_at: None,
			},
		);
		if name != "_" {
			if let Some((ref_mutable, inner)) = self.top_level_ref(value.as_ref()) {
				if let Some(base) = self.lhs_base_id(inner) {
					let kind = if ref_mutable {
						BorrowKind::Mutable
					} else {
						BorrowKind::Shared
					};
					let is_static = self.is_static_ref_type(value_ty);
					self.promote_temporary_borrow(
						ctx,
						name,
						RefBinding {
							base: base.to_string(),
							kind,
							is_static,
						},
						&node.location,
					);
				}
			}
		}
		let mut out = TypedAst::from(
			node.location.clone(),
			if constexpr {
				TypedValue::DeclarationConstexpr(name.to_string(), typed_value)
			} else {
				TypedValue::Declaration {
					name: name.to_string(),
					value: typed_value,
					mutable,
				}
			},
		);
		out.ty = Some(self.builtins.void_id);
		out
	}

	#[allow(clippy::vec_box)]
	#[allow(clippy::too_many_arguments)]
	fn type_multi_declaration(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		names: &[String],
		types: &[Box<Type>],
		values: &Option<Vec<Box<AST>>>,
		constexpr: bool,
		mutable: bool,
	) -> Box<TypedAst> {
		let mut typed_types = Vec::new();
		for ty in types {
			typed_types.push(self.resolve_type(
				ctx.module_id,
				ty,
				&ctx.generic_types,
				ctx.self_type,
				&node.location,
			));
		}
		let mut typed_values = None;
		if let Some(values) = values {
			let mut new_values = Vec::new();
			for (idx, value) in values.iter().enumerate() {
				let expected = typed_types
					.get(idx)
					.copied()
					.or_else(|| typed_types.first().copied());
				new_values.push(self.type_node(value, ctx, expected));
			}
			typed_values = Some(new_values);
		}
		for (idx, name) in names.iter().enumerate() {
			let mut value_ty = None;
			if let Some(values) = &mut typed_values {
				let value = if idx < values.len() {
					values.get_mut(idx)
				} else if values.len() == 1 {
					values.first_mut()
				} else {
					None
				};
				if let Some(value) = value {
					value_ty = value.ty;
				}
			}
			let declared_ty = typed_types
				.get(idx)
				.copied()
				.or_else(|| typed_types.first().copied());
			let mut final_ty =
				declared_ty.unwrap_or(value_ty.unwrap_or(self.builtins.unknown_id));
			if let Some(value_ty) = value_ty {
				if let Some(declared_ty) = declared_ty {
					let mut coerced = value_ty;
					if !self.type_matches(value_ty, declared_ty)
						&& !self.coerce_untyped(&mut coerced, declared_ty)
					{
						self.errors.push(FrontendError::TypeMismatch {
							location: node.location.clone(),
							expected: self.type_name(declared_ty),
							found: self.type_name(value_ty),
						});
					} else if let Some(values) = &mut typed_values {
						let value = if idx < values.len() {
							values.get_mut(idx)
						} else if values.len() == 1 {
							values.first_mut()
						} else {
							None
						};
						if let Some(value) = value {
							value.ty = Some(coerced);
						}
					}
				} else if !constexpr {
					if value_ty == self.builtins.untyped_int_id {
						final_ty = self.builtins.int_id;
					} else if value_ty == self.builtins.untyped_float_id {
						final_ty = self.builtins.f64_id;
					}
				}
			}
			if mutable && !constexpr && self.is_module_scope(ctx) {
				if !self.is_static_mut_ref(final_ty) {
					self.errors.push(FrontendError::NonStaticModuleMut {
						location: node.location.clone(),
						name: name.clone(),
					});
				}
			}
			if self.type_contains_pointer(final_ty) && !self.is_unsafe_context(ctx) {
				self.errors.push(FrontendError::PointerRequiresUnsafe {
					location: node.location.clone(),
				});
			}
			if ctx.in_constexpr && self.type_contains_pointer(final_ty) {
				self.errors.push(FrontendError::PointerInConstexpr {
					location: node.location.clone(),
				});
			}
			ctx.value_scopes.last_mut().expect("scope").insert(
				name.clone(),
				ValueInfo {
					ty: final_ty,
					constexpr,
					mutable,
					moved: false,
					moved_at: None,
				},
			);
			if name != "_" {
				if let Some(values) = values {
					if let Some(value_ast) =
						values.get(idx).or_else(|| values.first())
					{
						if let Some((ref_mutable, inner)) =
							self.top_level_ref(value_ast.as_ref())
						{
							if let Some(base) = self.lhs_base_id(inner)
							{
								let kind = if ref_mutable {
									BorrowKind::Mutable
								} else {
									BorrowKind::Shared
								};
								let is_static = declared_ty
									.map(|ty| {
										self.is_static_ref_type(ty)
									})
									.unwrap_or(false);
								self.promote_temporary_borrow(
									ctx,
									name,
									RefBinding {
										base: base
											.to_string(
											),
										kind,
										is_static,
									},
									&node.location,
								);
							}
						}
					}
				}
			}
		}

		let mut out = TypedAst::from(
			node.location.clone(),
			TypedValue::DeclarationMulti {
				names: names.to_vec(),
				types: typed_types,
				values: typed_values,
				constexpr,
				mutable,
			},
		);
		out.ty = Some(self.builtins.void_id);
		out
	}

	#[allow(clippy::too_many_arguments)]
	fn type_fn(
		&mut self,
		node: &Box<AST>,
		ctx: &mut TypeContext,
		attributes: &[String],
		generics: &[GenericParam],
		params: &[crate::frontend::FnParam],
		return_type: &Option<Box<Type>>,
		pre: &[Box<AST>],
		post: &Option<crate::frontend::PostClause>,
		where_clause: &Option<Box<AST>>,
		ensures: &[crate::frontend::EnsuresClause],
		body: &crate::frontend::FnBody,
	) -> Box<TypedAst> {
		let (typed_generics, generic_map) = self.resolve_generic_params(
			ctx.module_id,
			generics,
			Some(&ctx.generic_types),
			ctx.self_type,
		);
		let fn_is_unsafe = attributes.iter().any(|attr| attr == "unsafe");
		let prev_unsafe_depth = ctx.unsafe_depth;
		if fn_is_unsafe {
			ctx.unsafe_depth += 1;
		}
		let mut seen_default = false;
		for param in params {
			if param.default.is_some() {
				seen_default = true;
				continue;
			}
			if seen_default {
				self.errors.push(FrontendError::DefaultParamOrder {
					location: node.location.clone(),
					name: "<anonymous>".to_string(),
				});
				break;
			}
		}
		let mut typed_params = Vec::new();
		let mut param_types = Vec::new();
		for param in params {
			let mut resolved_ty = None;
			if let Some(ty) = &param.ty {
				resolved_ty = Some(self.resolve_type(
					ctx.module_id,
					ty,
					&generic_map,
					ctx.self_type,
					&node.location,
				));
			}
			let mut typed_default = None;
			if let Some(default) = &param.default {
				typed_default = Some(self.type_node(default, ctx, resolved_ty));
			}
			if let Some(ty) = resolved_ty {
				if ctx.in_constexpr && self.type_contains_pointer(ty) {
					self.errors.push(FrontendError::PointerInConstexpr {
						location: node.location.clone(),
					});
				}
				if self.type_contains_pointer(ty) && !fn_is_unsafe {
					self.errors.push(FrontendError::PointerRequiresUnsafe {
						location: node.location.clone(),
					});
				}
				param_types.push(ty);
			} else {
				param_types.push(self.builtins.unknown_id);
			}
			typed_params.push(TypedFnParam {
				names: param.names.clone(),
				ty: resolved_ty,
				default: typed_default,
			});
		}
		let resolved_return = return_type
			.as_ref()
			.map(|t| {
				self.resolve_type(
					ctx.module_id,
					t,
					&generic_map,
					ctx.self_type,
					&node.location,
				)
			})
			.unwrap_or(self.builtins.void_id);
		if ctx.in_constexpr && self.type_contains_pointer(resolved_return) {
			self.errors.push(FrontendError::PointerInConstexpr {
				location: node.location.clone(),
			});
		}
		if self.type_contains_pointer(resolved_return) && !fn_is_unsafe {
			self.errors.push(FrontendError::PointerRequiresUnsafe {
				location: node.location.clone(),
			});
		}
		let fn_ty = self.arena.add(ResolvedType::Fn {
			params: param_types.clone(),
			return_type: resolved_return,
		});

		let prev_return = ctx.return_type;
		let prev_constexpr = ctx.in_constexpr;
		ctx.return_type = Some(resolved_return);
		ctx.in_constexpr = prev_constexpr;
		self.push_scope(ctx);
		for (idx, param) in params.iter().enumerate() {
			let ty = param_types
				.get(idx)
				.copied()
				.unwrap_or(self.builtins.unknown_id);
			for name in &param.names {
				ctx.value_scopes.last_mut().expect("scope").insert(
					name.clone(),
					ValueInfo {
						ty,
						constexpr: false,
						mutable: false,
						moved: false,
						moved_at: None,
					},
				);
			}
		}
		let typed_pre = pre.iter().map(|p| self.type_node(p, ctx, None)).collect();
		let typed_where = where_clause.as_ref().map(|w| self.type_node(w, ctx, None));
		let typed_post = post.as_ref().map(|p| TypedPostClause {
			return_id: p.return_id.clone(),
			conditions: p
				.conditions
				.iter()
				.map(|c| self.type_node(c, ctx, None))
				.collect(),
		});
		let typed_ensures = ensures
			.iter()
			.map(|e| TypedEnsuresClause {
				binders: e.binders.clone(),
				condition: self.type_node(&e.condition, ctx, None),
			})
			.collect();
		let mut typed_body = match body {
			crate::frontend::FnBody::Block(b) => {
				TypedFnBody::Block(self.type_node(b, ctx, None))
			}
			crate::frontend::FnBody::Expr(e) => {
				TypedFnBody::Expr(self.type_node(e, ctx, None))
			}
			crate::frontend::FnBody::Uninitialized => TypedFnBody::Uninitialized,
		};
		self.clear_temporary_borrows(ctx);
		if return_type.is_some() {
			let (body_ty, body_location) = match &typed_body {
				TypedFnBody::Block(block) => (block.ty, &block.location),
				TypedFnBody::Expr(expr) => (expr.ty, &expr.location),
				TypedFnBody::Uninitialized => (None, &node.location),
			};
			if let Some(mut body_ty) = body_ty {
				if body_ty != self.builtins.unknown_id
					&& !self.type_matches(body_ty, resolved_return)
					&& !self.coerce_untyped(&mut body_ty, resolved_return)
				{
					self.errors.push(FrontendError::TypeMismatch {
						location: body_location.clone(),
						expected: self.type_name(resolved_return),
						found: self.type_name(body_ty),
					});
				} else {
					match &mut typed_body {
						TypedFnBody::Block(block) => {
							block.ty = Some(body_ty);
						}
						TypedFnBody::Expr(expr) => {
							expr.ty = Some(body_ty);
						}
						TypedFnBody::Uninitialized => {}
					}
				}
			}
		} else {
			let (body_ty, body_location) = match &typed_body {
				TypedFnBody::Block(block) => match &block.v {
					TypedValue::ExprList { items, .. }
					| TypedValue::ExprListNoScope { items, .. } => items
						.last()
						.map(|expr| (expr.ty, &expr.location))
						.unwrap_or((block.ty, &block.location)),
					_ => (block.ty, &block.location),
				},
				TypedFnBody::Expr(expr) => (expr.ty, &expr.location),
				TypedFnBody::Uninitialized => (None, &node.location),
			};
			if let Some(body_ty) = body_ty
				&& body_ty != self.builtins.void_id
				&& body_ty != self.builtins.unknown_id
			{
				self.errors.push(FrontendError::UnusedValue {
					location: body_location.clone(),
					hint: "use `_ = <expr>` to ignore the return value"
						.to_string(),
				});
			}
		}
		self.pop_scope(ctx);
		ctx.return_type = prev_return;
		ctx.in_constexpr = prev_constexpr;
		ctx.unsafe_depth = prev_unsafe_depth;

		let mut out = TypedAst::from(
			node.location.clone(),
			TypedValue::Fn {
				attributes: attributes.to_vec(),
				generics: typed_generics,
				params: typed_params,
				return_type: if return_type.is_some() {
					Some(resolved_return)
				} else {
					None
				},
				pre: typed_pre,
				post: typed_post,
				where_clause: typed_where,
				ensures: typed_ensures,
				body: typed_body,
			},
		);
		out.ty = Some(fn_ty);
		out
	}

	fn resolve_type(
		&mut self,
		module_id: &ModuleId,
		ty: &Type,
		generic_map: &HashMap<String, TypeId>,
		self_type: Option<TypeId>,
		location: &SourceLocation,
	) -> TypeId {
		match ty {
			Type::Id(name) => {
				if let Some(id) = generic_map.get(name) {
					return *id;
				}
				if name == "Self" {
					if let Some(self_type) = self_type {
						return self_type;
					}
				}
				if let Some(module) = self.modules.get(module_id) {
					if let Some(id) = module.types.get(name) {
						return *id;
					}
				}
				if let Some((module_path, type_name)) = name.rsplit_once('.') {
					let mut module_id = module_path.to_string();
					if let Some(module) = self.modules.get(&module_id) {
						if let Some(alias) = module
							.imports
							.alias_to_module
							.get(module_path)
						{
							module_id = alias.clone();
						}
					}
					if let Some(module) = self.modules.get(&module_id) {
						if let Some(id) = module.types.get(type_name) {
							return *id;
						}
					}
				}
				let err_loc = self.refine_type_location(location, name);
				let key = (
					err_loc.file.clone(),
					err_loc.range.begin.0,
					err_loc.range.begin.1,
					name.clone(),
				);
				if self.unknown_type_errors.insert(key) {
					let hint = match name.as_str() {
						"u0" | "u1" => Some("use bool instead".to_string()),
						_ => None,
					};
					self.errors.push(FrontendError::UnknownType {
						location: err_loc,
						name: name.clone(),
						hint,
					});
				}
				self.builtins.unknown_id
			}
			Type::Generic { base, args } => {
				let base_ty = self.resolve_type(
					module_id,
					&Type::Id(base.clone()),
					generic_map,
					self_type,
					location,
				);
				let generic_params = self.type_generic_params_for_type_id(base_ty);
				let mut resolved_args = Vec::new();
				for (idx, arg) in args.iter().enumerate() {
					let param = generic_params
						.as_ref()
						.and_then(|params| params.get(idx));
					let resolved = match arg {
						GenericArg::Type(ty) => {
							ResolvedGenericArg::Type(self.resolve_type(
								module_id,
								ty,
								generic_map,
								self_type,
								location,
							))
						}
						GenericArg::Expr(expr) => ResolvedGenericArg::Expr(
							self.canonicalize_type_level_expr(
								module_id,
								expr.as_ref(),
							),
						),
						GenericArg::Name(name) => {
							if matches!(
								param,
								Some(GenericParam::Value { .. })
							) {
								let id_expr = AST::from(
									location.clone(),
									ASTValue::Id(name.clone()),
								);
								ResolvedGenericArg::Expr(
									self.canonicalize_type_level_expr(
										module_id,
										id_expr.as_ref(),
									),
								)
							} else {
								ResolvedGenericArg::Name(
									name.clone(),
								)
							}
						}
					};
					resolved_args.push(resolved);
				}
				self.arena.add(ResolvedType::GenericInstance {
					base: base_ty,
					args: resolved_args,
				})
			}
			Type::Fn {
				params,
				return_type,
			} => {
				let mut resolved_params = Vec::new();
				for param in params {
					resolved_params.push(self.resolve_type(
						module_id,
						param,
						generic_map,
						self_type,
						location,
					));
				}
				let resolved_return = return_type
					.as_ref()
					.map(|t| {
						self.resolve_type(
							module_id,
							t,
							generic_map,
							self_type,
							location,
						)
					})
					.unwrap_or(self.builtins.void_id);
				self.arena.add(ResolvedType::Fn {
					params: resolved_params,
					return_type: resolved_return,
				})
			}
			Type::Integer { bit_size, signed } => {
				self.integer_type_id(*bit_size, *signed)
			}
			Type::Float { bit_size } => match bit_size {
				32 => self.builtins.f32_id,
				64 => self.builtins.f64_id,
				_ => self.builtins.unknown_id,
			},
			Type::Bool => self.builtins.bool_id,
			Type::Rune => self.builtins.rune_id,
			Type::Pointer { underlying } => {
				let resolved = self.resolve_type(
					module_id,
					underlying,
					generic_map,
					self_type,
					location,
				);
				self.arena.add(ResolvedType::Pointer {
					underlying: resolved,
				})
			}
			Type::Slice { underlying } => {
				let resolved = self.resolve_type(
					module_id,
					underlying,
					generic_map,
					self_type,
					location,
				);
				self.arena.add(ResolvedType::Slice {
					underlying: resolved,
				})
			}
			Type::Array { size, underlying } => {
				let resolved = self.resolve_type(
					module_id,
					underlying,
					generic_map,
					self_type,
					location,
				);
				self.arena.add(ResolvedType::Array {
					size: self.canonicalize_type_level_expr(
						module_id,
						size.as_ref(),
					),
					underlying: resolved,
				})
			}
			Type::CArray { underlying } => {
				let resolved = self.resolve_type(
					module_id,
					underlying,
					generic_map,
					self_type,
					location,
				);
				self.arena.add(ResolvedType::CArray {
					underlying: resolved,
				})
			}
			Type::Reference {
				mutable,
				lifetime,
				underlying,
			} => {
				let resolved = self.resolve_type(
					module_id,
					underlying,
					generic_map,
					self_type,
					location,
				);
				self.arena.add(ResolvedType::Reference {
					mutable: *mutable,
					lifetime: *lifetime,
					underlying: resolved,
				})
			}
			Type::Void => self.builtins.void_id,
		}
	}

	fn module_location(&self, module_id: &ModuleId) -> SourceLocation {
		if let Some(module) = self.modules.get(module_id) {
			return SourceLocation::new_from_file(module.file_path.clone());
		}
		SourceLocation::new_from_file(module_id.clone())
	}

	fn refine_type_location(&self, location: &SourceLocation, name: &str) -> SourceLocation {
		let mut out = location.clone();
		let line_no = location.range.begin.1;
		if line_no <= 0 {
			return out;
		}
		let Ok(contents) = read_to_string(&location.file) else {
			return out;
		};
		let Some(line) = contents.lines().nth((line_no - 1) as usize) else {
			return out;
		};
		let mut idx = None;
		if let Some(colon_idx) = line.rfind(':') {
			let tail = &line[(colon_idx + 1)..];
			let trim_start = tail
				.char_indices()
				.find(|(_, ch)| !ch.is_whitespace())
				.map(|(i, _)| i)
				.unwrap_or(0);
			let tail_start = colon_idx + 1 + trim_start;
			if let Some(found) = line[tail_start..].find(name) {
				idx = Some(tail_start + found);
			} else if tail_start < line.len() {
				idx = Some(tail_start);
			}
		}
		if idx.is_none() {
			let mut search = 0;
			while let Some(found) = line[search..].find(name) {
				idx = Some(search + found);
				search += found + name.len();
			}
		}
		if let Some(idx) = idx {
			let col = (idx as i32) + 1;
			out.range.begin = (col, line_no);
			out.range.end = (col + (name.len() as i32), line_no);
		}
		out
	}

	fn type_generic_params_for_type_id(&self, ty_id: TypeId) -> Option<Vec<GenericParam>> {
		let mut base_id = ty_id;
		if let ResolvedType::GenericInstance { base, .. } = self.arena.get(base_id) {
			base_id = *base;
		}
		let (module_id, type_name) = match self.arena.get(base_id) {
			ResolvedType::Struct { module, name, .. }
			| ResolvedType::Union { module, name, .. }
			| ResolvedType::RawUnion { module, name, .. }
			| ResolvedType::Newtype { module, name, .. }
			| ResolvedType::Alias { module, name, .. }
			| ResolvedType::Enum { module, name, .. } => (module, name),
			_ => return None,
		};
		self.type_generic_params_from_ast(module_id, type_name)
	}

	fn type_generic_params_from_ast(
		&self,
		module_id: &ModuleId,
		type_name: &str,
	) -> Option<Vec<GenericParam>> {
		let module = self.modules.get(module_id)?;
		let (ASTValue::ExprList { items, .. } | ASTValue::ExprListNoScope { items, .. }) =
			&module.ast.v
		else {
			return None;
		};
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(name, value)
					if name == type_name =>
				{
					match &value.v {
						ASTValue::Struct { generics, .. }
						| ASTValue::Union { generics, .. }
						| ASTValue::RawUnion { generics, .. } => {
							return Some(generics.clone());
						}
						_ => return Some(Vec::new()),
					}
				}
				ASTValue::DeclarationMulti {
					names,
					values: Some(values),
					constexpr: true,
					..
				} => {
					for (idx, name) in names.iter().enumerate() {
						if name != type_name {
							continue;
						}
						let Some(value) =
							values.get(idx).or_else(|| values.first())
						else {
							continue;
						};
						match &value.v {
							ASTValue::Struct { generics, .. }
							| ASTValue::Union { generics, .. }
							| ASTValue::RawUnion { generics, .. } => {
								return Some(generics.clone());
							}
							_ => return Some(Vec::new()),
						}
					}
				}
				_ => {}
			}
		}
		None
	}

	fn resolve_generic_params(
		&mut self,
		module_id: &ModuleId,
		params: &[GenericParam],
		outer: Option<&HashMap<String, TypeId>>,
		self_type: Option<TypeId>,
	) -> (Vec<TypedGenericParam>, HashMap<String, TypeId>) {
		let mut map = HashMap::new();
		if let Some(outer) = outer {
			for (name, id) in outer {
				map.insert(name.clone(), *id);
			}
		}
		if let Some(self_type) = self_type {
			map.insert("Self".to_string(), self_type);
		}
		let mut typed_params = Vec::new();
		for param in params {
			match param {
				GenericParam::Lifetime(c) => {
					typed_params.push(TypedGenericParam::Lifetime(*c));
				}
				GenericParam::Type { names, constraint } => {
					let constraint_ty = constraint.as_ref().map(|t| {
						self.resolve_type(
							module_id,
							t,
							&map,
							self_type,
							&self.module_location(module_id),
						)
					});
					for name in names {
						let ty_id = self
							.arena
							.add(ResolvedType::TypeParam(name.clone()));
						map.insert(name.clone(), ty_id);
					}
					typed_params.push(TypedGenericParam::Type {
						names: names.clone(),
						constraint: constraint_ty,
					});
				}
				GenericParam::Value { names, ty } => {
					let resolved = self.resolve_type(
						module_id,
						ty,
						&map,
						self_type,
						&self.module_location(module_id),
					);
					typed_params.push(TypedGenericParam::Value {
						names: names.clone(),
						ty: resolved,
					});
				}
			}
		}
		(typed_params, map)
	}

	fn fn_type_from_signature(
		&mut self,
		module_id: &ModuleId,
		params: &[crate::frontend::FnParam],
		return_type: Option<&Box<Type>>,
		generic_map: &HashMap<String, TypeId>,
		self_type: Option<TypeId>,
		location: &SourceLocation,
	) -> TypeId {
		self.fn_type_from_signature_inner(
			module_id,
			params,
			return_type,
			generic_map,
			self_type,
			location,
			false,
		)
	}

	#[allow(clippy::too_many_arguments)]
	fn fn_type_from_signature_inner(
		&mut self,
		module_id: &ModuleId,
		params: &[crate::frontend::FnParam],
		return_type: Option<&Box<Type>>,
		generic_map: &HashMap<String, TypeId>,
		self_type: Option<TypeId>,
		location: &SourceLocation,
		include_self: bool,
	) -> TypeId {
		let mut param_types = Vec::new();
		if include_self {
			param_types.push(self_type.unwrap_or(self.builtins.unknown_id));
		}
		for param in params {
			let ty = param.ty.as_ref().map(|t| {
				self.resolve_type(module_id, t, generic_map, self_type, location)
			});
			let param_ty = ty.unwrap_or(self.builtins.unknown_id);
			let repeat = param.names.len().max(1);
			for _ in 0..repeat {
				param_types.push(param_ty);
			}
		}
		let resolved_return =
			return_type
				.map(|t| {
					self.resolve_type(
						module_id,
						t,
						generic_map,
						self_type,
						location,
					)
				})
				.unwrap_or(self.builtins.void_id);
		self.arena.add(ResolvedType::Fn {
			params: param_types,
			return_type: resolved_return,
		})
	}

	fn resolve_module_path(&self, node: &AST, ctx: &TypeContext) -> Option<ModuleId> {
		let mut parts = Vec::new();
		let mut current = node;
		loop {
			match &current.v {
				ASTValue::Id(name) => {
					parts.push(name.clone());
					break;
				}
				ASTValue::BinExpr {
					op: crate::frontend::Operator::Dot,
					lhs,
					rhs,
					..
				} => {
					if let ASTValue::Id(name) = &rhs.v {
						parts.push(name.clone());
						current = lhs.as_ref();
						continue;
					}
					return None;
				}
				_ => return None,
			}
		}
		parts.reverse();
		let path = parts.join(".");
		if let Some(module_id) = ctx.module.imports.alias_to_module.get(&path) {
			return Some(module_id.clone());
		}
		if self.modules.contains_key(&path) {
			return Some(path);
		}
		None
	}

	fn unwrap_inheritance_type(&self, mut ty: TypeId) -> TypeId {
		loop {
			match self.arena.get(ty) {
				ResolvedType::GenericInstance { base, .. } => {
					ty = *base;
				}
				ResolvedType::Alias { underlying, .. } => {
					ty = *underlying;
				}
				ResolvedType::Reference { underlying, .. }
				| ResolvedType::Pointer { underlying } => {
					ty = *underlying;
				}
				_ => return ty,
			}
		}
	}

	fn push_inheritance_bases(
		&self,
		bases: &[TypeId],
		queue: &mut Vec<TypeId>,
		visited: &mut HashSet<TypeId>,
	) {
		for base in bases {
			let base = self.unwrap_inheritance_type(*base);
			if !visited.contains(&base) {
				queue.push(base);
			}
		}
	}

	fn is_child_of(&self, child: TypeId, parent: TypeId) -> bool {
		let parent = self.unwrap_inheritance_type(parent);
		let mut queue = vec![self.unwrap_inheritance_type(child)];
		let mut visited = HashSet::new();
		while let Some(current) = queue.pop() {
			if current == parent {
				return true;
			}
			if !visited.insert(current) {
				continue;
			}
			if let ResolvedType::Struct { extends, .. } = self.arena.get(current) {
				self.push_inheritance_bases(extends, &mut queue, &mut visited);
			}
		}
		false
	}

	fn can_access_member(&self, owner: TypeId, ctx_self: Option<TypeId>) -> bool {
		ctx_self.is_some_and(|self_ty| self.is_child_of(self_ty, owner))
	}

	fn reference_allows_inheritance(&self, actual: TypeId, expected: TypeId) -> bool {
		match (self.arena.get(actual), self.arena.get(expected)) {
			(
				ResolvedType::Reference {
					mutable: actual_mut,
					underlying: actual_underlying,
					..
				},
				ResolvedType::Reference {
					mutable: expected_mut,
					underlying: expected_underlying,
					..
				},
			) => {
				actual_mut == expected_mut
					&& self.is_child_of(
						*actual_underlying,
						*expected_underlying,
					)
			}
			_ => false,
		}
	}

	fn lookup_field_info(&self, ty: TypeId, name: &str) -> Option<(TypeId, TypeId, bool)> {
		match self.arena.get(ty) {
			ResolvedType::Struct { .. } => {
				let mut queue = vec![ty];
				let mut visited = HashSet::new();
				while let Some(current) = queue.pop() {
					if !visited.insert(current) {
						continue;
					}
					let ResolvedType::Struct {
						fields, extends, ..
					} = self.arena.get(current)
					else {
						continue;
					};
					for field in fields {
						if field.name == name {
							return Some((
								field.ty,
								current,
								field.public,
							));
						}
					}
					self.push_inheritance_bases(
						extends,
						&mut queue,
						&mut visited,
					);
				}
				None
			}
			ResolvedType::RawUnion { fields, .. } => {
				for field in fields {
					if field.name == name {
						return Some((field.ty, ty, field.public));
					}
				}
				None
			}
			ResolvedType::GenericInstance { base, .. } => {
				self.lookup_field_info(*base, name)
			}
			ResolvedType::Reference { underlying, .. }
			| ResolvedType::Pointer { underlying } => self.lookup_field_info(*underlying, name),
			ResolvedType::Alias { underlying, .. } => {
				self.lookup_field_info(*underlying, name)
			}
			_ => None,
		}
	}

	fn lookup_field_access(
		&self,
		ty: TypeId,
		name: &str,
		ctx_self: Option<TypeId>,
	) -> MemberAccess<TypeId> {
		let Some((field_ty, owner, public)) = self.lookup_field_info(ty, name) else {
			return MemberAccess::Missing;
		};
		if public || self.can_access_member(owner, ctx_self) {
			MemberAccess::Found(field_ty)
		} else {
			MemberAccess::Inaccessible
		}
	}

	fn lookup_method_info(&self, ty: TypeId, name: &str) -> Option<(TypeId, TypeId, bool)> {
		match self.arena.get(ty) {
			ResolvedType::Struct { .. } => {
				let mut queue = vec![ty];
				let mut visited = HashSet::new();
				while let Some(current) = queue.pop() {
					if !visited.insert(current) {
						continue;
					}
					let ResolvedType::Struct {
						methods, extends, ..
					} = self.arena.get(current)
					else {
						continue;
					};
					if let Some(method) = methods.get(name) {
						return Some((method.ty, current, method.public));
					}
					self.push_inheritance_bases(
						extends,
						&mut queue,
						&mut visited,
					);
				}
				None
			}
			ResolvedType::Union { methods, .. } => {
				if let Some(method) = methods.get(name) {
					return Some((method.ty, ty, method.public));
				}
				None
			}
			ResolvedType::GenericInstance { base, .. } => {
				self.lookup_method_info(*base, name)
			}
			ResolvedType::Reference { underlying, .. }
			| ResolvedType::Pointer { underlying } => self.lookup_method_info(*underlying, name),
			ResolvedType::Alias { underlying, .. } => {
				self.lookup_method_info(*underlying, name)
			}
			_ => None,
		}
	}

	fn method_needs_ref(&self, method_ty: TypeId) -> bool {
		matches!(
			self.arena.get(method_ty),
			ResolvedType::Fn { params, .. }
				if matches!(
					params.first().map(|id| self.arena.get(*id)),
					Some(ResolvedType::Reference { .. })
				)
		)
	}

	fn lookup_method_access(
		&self,
		ty: TypeId,
		name: &str,
		ctx_self: Option<TypeId>,
	) -> MemberAccess<(TypeId, bool)> {
		let Some((method_ty, owner, public)) = self.lookup_method_info(ty, name) else {
			return MemberAccess::Missing;
		};
		if public || self.can_access_member(owner, ctx_self) {
			MemberAccess::Found((method_ty, self.method_needs_ref(method_ty)))
		} else {
			MemberAccess::Inaccessible
		}
	}

	fn fn_param_defs_from_ast(
		&self,
		module_id: &ModuleId,
		name: &str,
	) -> Option<Vec<crate::frontend::FnParam>> {
		let items = self.module_items(module_id);
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(decl_name, value)
				| ASTValue::Declaration {
					name: decl_name,
					value,
					..
				} if decl_name == name => {
					if let ASTValue::Fn { params, .. } = &value.v {
						return Some(params.clone());
					}
				}
				ASTValue::DeclarationMulti { names, values, .. } => {
					let Some(values) = values else {
						continue;
					};
					for (idx, decl_name) in names.iter().enumerate() {
						if decl_name != name {
							continue;
						}
						if let Some(value) = values.get(idx)
							&& let ASTValue::Fn { params, .. } =
								&value.v
						{
							return Some(params.clone());
						}
					}
				}
				_ => {}
			}
		}
		None
	}

	fn fn_flags_from_ast(
		&self,
		module_id: &ModuleId,
		name: &str,
		attr: &str,
	) -> Option<(bool, bool, SourceLocation)> {
		let declaration_location = self.fn_keyword_location_from_source(module_id, name);
		let items = self.module_items(module_id);
		for item in items {
			let item_location = item.location.clone();
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(decl_name, value)
					if decl_name == name =>
				{
					if let ASTValue::Fn { attributes, .. } = &value.v {
						return Some((
							true,
							attributes.iter().any(|a| a == attr),
							declaration_location
								.clone()
								.unwrap_or(item_location.clone()),
						));
					}
				}
				ASTValue::Declaration {
					name: decl_name,
					value,
					..
				} if decl_name == name => {
					if let ASTValue::Fn { attributes, .. } = &value.v {
						return Some((
							false,
							attributes.iter().any(|a| a == attr),
							declaration_location
								.clone()
								.unwrap_or(item_location.clone()),
						));
					}
				}
				ASTValue::DeclarationMulti {
					names,
					values,
					constexpr,
					..
				} => {
					let Some(values) = values else {
						continue;
					};
					for (idx, decl_name) in names.iter().enumerate() {
						if decl_name != name {
							continue;
						}
						if let Some(value) = values.get(idx)
							&& let ASTValue::Fn { attributes, .. } =
								&value.v
						{
							return Some((
								*constexpr,
								attributes
									.iter()
									.any(|a| a == attr),
								declaration_location
									.clone()
									.unwrap_or(
										item_location
											.clone(),
									),
							));
						}
					}
				}
				_ => {}
			}
		}
		None
	}

	fn call_has_runtime_dependent_args(&self, args: &[Box<AST>], ctx: &TypeContext) -> bool {
		args.iter()
			.any(|arg| self.expr_uses_runtime_value(arg.as_ref(), ctx))
	}

	fn call_depends_on_runtime_values(
		&self,
		args: &[Box<AST>],
		receiver: Option<&AST>,
		ctx: &TypeContext,
	) -> bool {
		receiver.map(|node| self.expr_uses_runtime_value(node, ctx))
			.unwrap_or(false) || self.call_has_runtime_dependent_args(args, ctx)
	}

	fn runtime_function_call_in_constexpr(
		&self,
		ctx: &TypeContext,
		callee: &AST,
	) -> Option<(String, TypeId)> {
		if !ctx.in_constexpr {
			return None;
		}
		let name = match &callee.v {
			ASTValue::Id(name) => Some(name.as_str()),
			ASTValue::GenericApply { target, .. } => {
				if let ASTValue::Id(name) = &target.v {
					Some(name.as_str())
				} else {
					None
				}
			}
			_ => None,
		}?;
		let info = self.lookup_value_info(ctx, name)?;
		if info.constexpr || !matches!(self.arena.get(info.ty), ResolvedType::Fn { .. }) {
			return None;
		}
		Some((name.to_string(), info.ty))
	}

	fn maybe_require_runtimeable_for_call(
		&mut self,
		ctx: &TypeContext,
		call_location: &SourceLocation,
		callee: &str,
		flags: Option<(bool, bool, SourceLocation)>,
		args: &[Box<AST>],
		receiver: Option<&AST>,
	) {
		if ctx.in_constexpr {
			return;
		}
		let Some((is_constexpr, is_runtimeable, declaration_location)) = flags else {
			return;
		};
		if !is_constexpr || is_runtimeable {
			return;
		}
		if !self.call_depends_on_runtime_values(args, receiver, ctx) {
			return;
		}
		self.errors
			.push(FrontendError::ConstexprCallNeedsRuntimeable {
				location: call_location.clone(),
				callee: callee.to_string(),
				declaration_location,
			});
	}

	fn expr_uses_runtime_value(&self, node: &AST, ctx: &TypeContext) -> bool {
		let mut ids = VecDeque::new();
		Self::collect_ids_in_ast(node, &mut ids);
		while let Some(name) = ids.pop_front() {
			if let Some(info) = ctx.value_scopes.iter().rev().find_map(|s| s.get(&name))
			{
				if !info.constexpr {
					return true;
				}
				continue;
			}
			if let Some(info) = ctx.module.values.get(&name)
				&& !info.constexpr
			{
				return true;
			}
		}
		false
	}

	fn method_param_defs_from_ast(
		&self,
		ty_id: TypeId,
		method_name: &str,
	) -> Option<Vec<crate::frontend::FnParam>> {
		let base_ty = match self.arena.get(ty_id) {
			ResolvedType::GenericInstance { base, .. } => *base,
			_ => ty_id,
		};
		let (module_id, type_name) = match self.arena.get(base_ty) {
			ResolvedType::Struct { module, name, .. }
			| ResolvedType::Union { module, name, .. } => (module.clone(), name.clone()),
			_ => return None,
		};
		let module = self.modules.get(&module_id)?;
		let items = self.module_items(&module_id);
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			let (decl_name, value) = match &node.v {
				ASTValue::DeclarationConstexpr(name, value) => (name, value),
				ASTValue::DeclarationMulti {
					names,
					values: Some(values),
					constexpr: true,
					..
				} => {
					let mut found = None;
					for (idx, name) in names.iter().enumerate() {
						if name == &type_name {
							found = values
								.get(idx)
								.map(|value| (name, value));
							break;
						}
					}
					let Some((decl, value)) = found else {
						continue;
					};
					(decl, value)
				}
				_ => continue,
			};
			if decl_name != &type_name {
				continue;
			}
			match &value.v {
				ASTValue::Struct { body, .. } => {
					if let ASTValue::ExprList { items, .. }
					| ASTValue::ExprListNoScope { items, .. } = &body.v
					{
						if let Some(params) = self
							.method_param_defs_from_items(
								items,
								method_name,
							) {
							return Some(params);
						}
					}
				}
				ASTValue::Union { methods, .. } => {
					if let Some(params) = self
						.method_param_defs_from_items(methods, method_name)
					{
						return Some(params);
					}
				}
				_ => {}
			}
		}
		let _ = module;
		None
	}

	fn method_flags_from_ast(
		&self,
		ty_id: TypeId,
		method_name: &str,
		attr: &str,
	) -> Option<(bool, bool, SourceLocation)> {
		let base_ty = match self.arena.get(ty_id) {
			ResolvedType::GenericInstance { base, .. } => *base,
			_ => ty_id,
		};
		let (module_id, type_name) = match self.arena.get(base_ty) {
			ResolvedType::Struct { module, name, .. }
			| ResolvedType::Union { module, name, .. } => (module.clone(), name.clone()),
			_ => return None,
		};
		let items = self.module_items(&module_id);
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			let (decl_name, value) = match &node.v {
				ASTValue::DeclarationConstexpr(name, value) => (name, value),
				ASTValue::DeclarationMulti {
					names,
					values: Some(values),
					constexpr: true,
					..
				} => {
					let mut found = None;
					for (idx, name) in names.iter().enumerate() {
						if name == &type_name {
							found = values
								.get(idx)
								.map(|value| (name, value));
							break;
						}
					}
					let Some((decl, value)) = found else {
						continue;
					};
					(decl, value)
				}
				_ => continue,
			};
			if decl_name != &type_name {
				continue;
			}
			match &value.v {
				ASTValue::Struct { body, .. } => {
					if let ASTValue::ExprList { items, .. }
					| ASTValue::ExprListNoScope { items, .. } = &body.v
					{
						if let Some(flags) = self.method_flags_from_items(
							items,
							method_name,
							attr,
						) {
							return Some(flags);
						}
					}
				}
				ASTValue::Union { methods, .. } => {
					if let Some(flags) = self.method_flags_from_items(
						methods,
						method_name,
						attr,
					) {
						return Some(flags);
					}
				}
				_ => {}
			}
		}
		None
	}

	fn method_flags_from_items(
		&self,
		items: &[Box<AST>],
		method_name: &str,
		attr: &str,
	) -> Option<(bool, bool, SourceLocation)> {
		for item in items {
			let item_location = item.location.clone();
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(name, value)
					if name == method_name =>
				{
					if let ASTValue::Fn { attributes, .. } = &value.v {
						return Some((
							true,
							attributes.iter().any(|a| a == attr),
							item_location.clone(),
						));
					}
				}
				ASTValue::DeclarationMulti {
					names,
					values: Some(values),
					constexpr,
					..
				} => {
					for (idx, name) in names.iter().enumerate() {
						if name != method_name {
							continue;
						}
						if let Some(value) = values.get(idx)
							&& let ASTValue::Fn { attributes, .. } =
								&value.v
						{
							return Some((
								*constexpr,
								attributes
									.iter()
									.any(|a| a == attr),
								item_location.clone(),
							));
						}
					}
				}
				_ => {}
			}
		}
		None
	}

	fn fn_keyword_location_from_source(
		&self,
		module_id: &ModuleId,
		name: &str,
	) -> Option<SourceLocation> {
		let module = self.modules.get(module_id)?;
		let src = read_to_string(&module.file_path).ok()?;
		for (line_idx, line) in src.lines().enumerate() {
			let Some(name_pos) = line.find(name) else {
				continue;
			};
			let after_name = &line[name_pos + name.len()..];
			let Some(fn_rel) = after_name.find("fn") else {
				continue;
			};
			let before_fn = &after_name[..fn_rel];
			if !(before_fn.contains("::") || before_fn.contains(":=")) {
				continue;
			}
			let fn_col = (name_pos + name.len() + fn_rel + 1) as i32;
			let line_no = (line_idx + 1) as i32;
			return Some(SourceLocation {
				file: module.file_path.clone(),
				range: crate::frontend::LocationRange {
					begin: (fn_col, line_no),
					end: (fn_col, line_no),
				},
			});
		}
		None
	}

	fn method_param_defs_from_items(
		&self,
		items: &[Box<AST>],
		method_name: &str,
	) -> Option<Vec<crate::frontend::FnParam>> {
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(name, value)
					if name == method_name =>
				{
					if let ASTValue::Fn { params, .. } = &value.v {
						return Some(params.clone());
					}
				}
				ASTValue::DeclarationMulti { names, values, .. } => {
					let Some(values) = values else {
						continue;
					};
					for (idx, name) in names.iter().enumerate() {
						if name != method_name {
							continue;
						}
						if let Some(value) = values.get(idx)
							&& let ASTValue::Fn { params, .. } =
								&value.v
						{
							return Some(params.clone());
						}
					}
				}
				_ => {}
			}
		}
		None
	}

	fn fn_param_count(&self, module_id: &ModuleId, name: &str) -> Option<usize> {
		let items = self.module_items(module_id);
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(decl_name, value)
				| ASTValue::Declaration {
					name: decl_name,
					value,
					..
				} if decl_name == name => {
					if let ASTValue::Fn { params, .. } = &value.v {
						let mut count = 0;
						for param in params {
							count += param.names.len().max(1);
						}
						return Some(count);
					}
				}
				ASTValue::DeclarationMulti { names, values, .. } => {
					let Some(values) = values else {
						continue;
					};
					for (idx, decl_name) in names.iter().enumerate() {
						if decl_name != name {
							continue;
						}
						if let Some(value) = values.get(idx)
							&& let ASTValue::Fn { params, .. } =
								&value.v
						{
							let mut count = 0;
							for param in params {
								count += param.names.len().max(1);
							}
							return Some(count);
						}
					}
				}
				_ => {}
			}
		}
		None
	}

	fn fn_param_types_from_ast(
		&mut self,
		module_id: &ModuleId,
		name: &str,
	) -> Option<Vec<TypeId>> {
		let items = self.module_items(module_id);
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(decl_name, value)
				| ASTValue::Declaration {
					name: decl_name,
					value,
					..
				} if decl_name == name => {
					if let ASTValue::Fn {
						generics, params, ..
					} = &value.v
					{
						let (_, generic_map) = self.resolve_generic_params(
							module_id, generics, None, None,
						);
						return Some(self.param_types_from_list(
							module_id,
							params,
							&generic_map,
							&node.location,
						));
					}
				}
				ASTValue::DeclarationMulti { names, values, .. } => {
					let Some(values) = values else {
						continue;
					};
					for (idx, decl_name) in names.iter().enumerate() {
						if decl_name != name {
							continue;
						}
						if let Some(value) = values.get(idx)
							&& let ASTValue::Fn {
								generics, params, ..
							} = &value.v
						{
							let (_, generic_map) = self
								.resolve_generic_params(
									module_id, generics, None,
									None,
								);
							return Some(self.param_types_from_list(
								module_id,
								params,
								&generic_map,
								&node.location,
							));
						}
					}
				}
				_ => {}
			}
		}
		None
	}

	fn param_types_from_list(
		&mut self,
		module_id: &ModuleId,
		params: &[crate::frontend::FnParam],
		generic_map: &HashMap<String, TypeId>,
		location: &SourceLocation,
	) -> Vec<TypeId> {
		let mut types = Vec::new();
		for param in params {
			let ty = param.ty.as_ref().map(|t| {
				self.resolve_type(module_id, t, generic_map, None, location)
			});
			let param_ty = ty.unwrap_or(self.builtins.unknown_id);
			let repeat = param.names.len().max(1);
			for _ in 0..repeat {
				types.push(param_ty);
			}
		}
		types
	}

	fn fn_generic_param_names(&self, module_id: &ModuleId, name: &str) -> Option<Vec<String>> {
		let items = self.module_items(module_id);
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationConstexpr(decl_name, value)
				| ASTValue::Declaration {
					name: decl_name,
					value,
					..
				} if decl_name == name => {
					if let ASTValue::Fn { generics, .. } = &value.v {
						return Some(self.flatten_generic_names(generics));
					}
				}
				ASTValue::DeclarationMulti { names, values, .. } => {
					let Some(values) = values else {
						continue;
					};
					for (idx, decl_name) in names.iter().enumerate() {
						if decl_name != name {
							continue;
						}
						if let Some(value) = values.get(idx)
							&& let ASTValue::Fn { generics, .. } =
								&value.v
						{
							return Some(self
								.flatten_generic_names(generics));
						}
					}
				}
				_ => {}
			}
		}
		None
	}

	fn flatten_generic_names(&self, generics: &[GenericParam]) -> Vec<String> {
		let mut names = Vec::new();
		for param in generics {
			if let GenericParam::Type {
				names: param_names, ..
			} = param
			{
				for name in param_names {
					names.push(name.clone());
				}
			}
		}
		names
	}

	fn union_accepts_type(&self, union_ty: TypeId, variant_ty: TypeId) -> bool {
		let variants = match self.arena.get(union_ty) {
			ResolvedType::Union { variants, .. } => variants,
			ResolvedType::GenericInstance { base, .. } => {
				if let ResolvedType::Union { variants, .. } = self.arena.get(*base)
				{
					variants
				} else {
					return false;
				}
			}
			_ => return false,
		};
		for variant in variants {
			if self.type_matches(*variant, variant_ty) {
				return true;
			}
			if let ResolvedType::GenericInstance { base, .. } = self.arena.get(*variant)
			{
				if self.type_matches(*base, variant_ty) {
					return true;
				}
			}
		}
		false
	}

	fn type_matches(&self, a: TypeId, b: TypeId) -> bool {
		if a == b {
			return true;
		}
		match (self.arena.get(a), self.arena.get(b)) {
			(ResolvedType::TypeParam(_), _) | (_, ResolvedType::TypeParam(_)) => true,
			(ResolvedType::GenericInstance { base, .. }, other)
				if !matches!(other, ResolvedType::GenericInstance { .. }) =>
			{
				self.type_matches(*base, b)
			}
			(other, ResolvedType::GenericInstance { base, .. })
				if !matches!(other, ResolvedType::GenericInstance { .. }) =>
			{
				self.type_matches(a, *base)
			}
			(ResolvedType::Alias { underlying, .. }, _) => {
				self.type_matches(*underlying, b)
			}
			(_, ResolvedType::Alias { underlying, .. }) => {
				self.type_matches(a, *underlying)
			}
			(
				ResolvedType::Pointer { underlying: a },
				ResolvedType::Pointer { underlying: b },
			) => self.type_matches(*a, *b),
			(
				ResolvedType::Slice { underlying: a },
				ResolvedType::Slice { underlying: b },
			) => self.type_matches(*a, *b),
			(
				ResolvedType::Array {
					size: sa,
					underlying: a,
				},
				ResolvedType::Array {
					size: sb,
					underlying: b,
				},
			) => sa == sb && self.type_matches(*a, *b),
			(
				ResolvedType::Reference {
					mutable: am,
					underlying: a,
					..
				},
				ResolvedType::Reference {
					mutable: bm,
					underlying: b,
					..
				},
			) => am == bm && self.type_matches(*a, *b),
			(
				ResolvedType::Fn {
					params: ap,
					return_type: ar,
				},
				ResolvedType::Fn {
					params: bp,
					return_type: br,
				},
			) => {
				ap.len() == bp.len()
					&& ap.iter()
						.zip(bp.iter())
						.all(|(a, b)| self.type_matches(*a, *b))
					&& self.type_matches(*ar, *br)
			}
			(
				ResolvedType::GenericInstance { base: ab, args: aa },
				ResolvedType::GenericInstance { base: bb, args: ba },
			) => {
				if !self.type_matches(*ab, *bb) || aa.len() != ba.len() {
					return false;
				}
				for (a, b) in aa.iter().zip(ba.iter()) {
					match (a, b) {
						(
							ResolvedGenericArg::Type(a),
							ResolvedGenericArg::Type(b),
						) => {
							if !self.type_matches(*a, *b) {
								return false;
							}
						}
						(
							ResolvedGenericArg::Expr(a),
							ResolvedGenericArg::Expr(b),
						) => {
							if a != b {
								return false;
							}
						}
						(
							ResolvedGenericArg::Name(a),
							ResolvedGenericArg::Name(b),
						) => {
							if a != b {
								return false;
							}
						}
						_ => return false,
					}
				}
				true
			}
			_ => false,
		}
	}

	fn coerce_untyped(&self, lhs: &mut TypeId, rhs: TypeId) -> bool {
		if *lhs == self.builtins.untyped_int_id
			&& (self.is_numeric_type(rhs) || rhs == self.builtins.untyped_int_id)
		{
			*lhs = rhs;
			return true;
		}
		if *lhs == self.builtins.untyped_float_id
			&& (self.is_float_type(rhs) || rhs == self.builtins.untyped_float_id)
		{
			*lhs = rhs;
			return true;
		}
		false
	}

	fn is_numeric_type(&self, ty: TypeId) -> bool {
		matches!(
			self.arena.get(ty),
			ResolvedType::Builtin(BuiltinType::Integer { .. })
				| ResolvedType::Builtin(BuiltinType::Float { .. })
		)
	}

	fn is_integer_type(&self, ty: TypeId) -> bool {
		matches!(
			self.arena.get(ty),
			ResolvedType::Builtin(BuiltinType::Integer { .. })
		)
	}

	fn is_float_type(&self, ty: TypeId) -> bool {
		matches!(
			self.arena.get(ty),
			ResolvedType::Builtin(BuiltinType::Float { .. })
		)
	}

	fn is_untyped_numeric(&self, ty: TypeId) -> bool {
		ty == self.builtins.untyped_int_id || ty == self.builtins.untyped_float_id
	}

	fn try_operator_overload(
		&self,
		op: &str,
		lhs_ty: TypeId,
		rhs_ty: TypeId,
		ctx_self: Option<TypeId>,
	) -> bool {
		let method = format!("operator{op}");
		if let MemberAccess::Found((method_ty, _)) =
			self.lookup_method_access(lhs_ty, &method, ctx_self)
			&& let ResolvedType::Fn { params, .. } = self.arena.get(method_ty)
			&& params.len() == 2 && self.type_matches(params[1], rhs_ty)
		{
			return true;
		}
		false
	}

	fn type_name(&self, ty: TypeId) -> String {
		match self.arena.get(ty) {
			ResolvedType::Builtin(b) => match b {
				BuiltinType::Integer { name, .. } => name.clone(),
				BuiltinType::Float { name, .. } => name.clone(),
				BuiltinType::Bool => "bool".to_string(),
				BuiltinType::Rune => "rune".to_string(),
				BuiltinType::Void => "void".to_string(),
				BuiltinType::Type => "type".to_string(),
			},
			ResolvedType::Struct { name, .. } => name.clone(),
			ResolvedType::Enum { name, .. } => name.clone(),
			ResolvedType::Union { name, .. } => name.clone(),
			ResolvedType::RawUnion { name, .. } => name.clone(),
			ResolvedType::Newtype { name, .. } => name.clone(),
			ResolvedType::Alias { name, .. } => name.clone(),
			ResolvedType::Pointer { underlying } => {
				format!("^{}", self.type_name(*underlying))
			}
			ResolvedType::Slice { underlying } => {
				format!("[]{}", self.type_name(*underlying))
			}
			ResolvedType::Array { size, underlying } => {
				format!("[{}]{}", size, self.type_name(*underlying))
			}
			ResolvedType::CArray { underlying } => {
				format!("[^]{}", self.type_name(*underlying))
			}
			ResolvedType::Reference {
				mutable,
				lifetime,
				underlying,
			} => {
				let mut out = "&".to_string();
				if let Some(lifetime) = lifetime {
					out.push_str(&format!("'{} ", lifetime));
				}
				if *mutable {
					out.push_str("mut ");
				}
				out.push_str(&self.type_name(*underlying));
				out
			}
			ResolvedType::Fn {
				params,
				return_type,
			} => {
				let mut out = String::new();
				out.push_str("fn(");
				for (idx, param) in params.iter().enumerate() {
					if idx > 0 {
						out.push_str(", ");
					}
					out.push_str(&self.type_name(*param));
				}
				out.push(')');
				out.push_str(" -> ");
				out.push_str(&self.type_name(*return_type));
				out
			}
			ResolvedType::GenericInstance { base, .. } => self.type_name(*base),
			ResolvedType::TypeParam(name) => name.clone(),
			ResolvedType::UntypedInt => "untyped int".to_string(),
			ResolvedType::UntypedFloat => "untyped float".to_string(),
			ResolvedType::Unknown => "unknown".to_string(),
		}
	}

	fn type_param_constraints<'a>(
		&'a self,
		expected: TypeId,
		explicit_types: &HashMap<String, TypeId>,
		by_id: &'a HashMap<TypeId, Vec<(String, SourceLocation)>>,
		by_name: &'a HashMap<String, Vec<(String, SourceLocation)>>,
	) -> Option<&'a Vec<(String, SourceLocation)>> {
		by_id.get(&expected)
			.or_else(|| match self.arena.get(expected) {
				ResolvedType::TypeParam(name) => explicit_types
					.get(name)
					.and_then(|explicit| by_id.get(explicit))
					.or_else(|| by_name.get(name)),
				_ => explicit_types
					.iter()
					.find(|(_, ty)| **ty == expected)
					.and_then(|(name, _)| by_name.get(name)),
			})
	}

	fn check_type_param_ops(
		&mut self,
		expected: TypeId,
		arg_ty: TypeId,
		location: &SourceLocation,
		explicit_types: &HashMap<String, TypeId>,
	) {
		if arg_ty == self.builtins.unknown_id {
			return;
		}
		let ops = self.type_param_constraints(
			expected,
			explicit_types,
			&self.type_param_ops,
			&self.type_param_ops_by_name,
		);
		let Some(ops) = ops else {
			return;
		};
		if self.is_numeric_type(arg_ty) || self.is_untyped_numeric(arg_ty) {
			return;
		}
		for (op, _) in ops {
			if self.try_operator_overload(op, arg_ty, arg_ty, None) {
				return;
			}
		}
		if let Some((op, op_location)) = ops.first() {
			self.errors.push(FrontendError::GenericOperatorConstraint {
				location: op_location.clone(),
				call_location: location.clone(),
				operator: op.clone(),
				lhs: self.type_name(arg_ty),
				rhs: self.type_name(arg_ty),
			});
		}
	}

	fn check_type_param_members(
		&mut self,
		expected: TypeId,
		arg_ty: TypeId,
		location: &SourceLocation,
		explicit_types: &HashMap<String, TypeId>,
	) {
		if arg_ty == self.builtins.unknown_id {
			return;
		}
		let members = self.type_param_constraints(
			expected,
			explicit_types,
			&self.type_param_members,
			&self.type_param_members_by_name,
		);
		let Some(members) = members else {
			return;
		};
		for (member, _) in members {
			if matches!(
				self.lookup_field_access(arg_ty, member, None),
				MemberAccess::Found(_)
			) || matches!(
				self.lookup_method_access(arg_ty, member, None),
				MemberAccess::Found(_)
			) {
				return;
			}
		}
		if let Some((member, member_location)) = members.first() {
			self.errors.push(FrontendError::GenericMemberConstraint {
				location: member_location.clone(),
				call_location: location.clone(),
				member: member.clone(),
				lhs: self.type_name(arg_ty),
			});
		}
	}
}

impl Semantics {
	pub fn module(&self, module_id: &ModuleId) -> Option<&ModuleState> {
		self.modules.get(module_id)
	}

	pub fn module_location(&self, module_id: &ModuleId) -> SourceLocation {
		if let Some(module) = self.modules.get(module_id) {
			return SourceLocation::new_from_file(module.file_path.clone());
		}
		SourceLocation::new_from_file(module_id.clone())
	}

	pub fn type_definition_location(&self, ty_id: TypeId) -> SourceLocation {
		if let Some(location) = self.type_locations.get(&ty_id) {
			return location.clone();
		}
		let module_id = match self.arena.get(ty_id) {
			ResolvedType::Struct { module, .. }
			| ResolvedType::Enum { module, .. }
			| ResolvedType::Union { module, .. }
			| ResolvedType::RawUnion { module, .. }
			| ResolvedType::Newtype { module, .. }
			| ResolvedType::Alias { module, .. } => Some(module.clone()),
			ResolvedType::GenericInstance { base, .. } => match self.arena.get(*base) {
				ResolvedType::Struct { module, .. }
				| ResolvedType::Enum { module, .. }
				| ResolvedType::Union { module, .. }
				| ResolvedType::RawUnion { module, .. }
				| ResolvedType::Newtype { module, .. }
				| ResolvedType::Alias { module, .. } => Some(module.clone()),
				_ => None,
			},
			_ => None,
		};
		module_id
			.map(|module_id| self.module_location(&module_id))
			.unwrap_or_else(|| self.module_location(&self.entry))
	}

	pub fn type_name(&self, ty: TypeId) -> String {
		match self.arena.get(ty) {
			ResolvedType::Builtin(b) => match b {
				BuiltinType::Integer { name, .. } => name.clone(),
				BuiltinType::Float { name, .. } => name.clone(),
				BuiltinType::Bool => "bool".to_string(),
				BuiltinType::Rune => "rune".to_string(),
				BuiltinType::Void => "void".to_string(),
				BuiltinType::Type => "type".to_string(),
			},
			ResolvedType::Struct { name, .. }
			| ResolvedType::Enum { name, .. }
			| ResolvedType::Union { name, .. }
			| ResolvedType::RawUnion { name, .. }
			| ResolvedType::Newtype { name, .. }
			| ResolvedType::Alias { name, .. } => name.clone(),
			ResolvedType::Pointer { underlying } => {
				format!("^{}", self.type_name(*underlying))
			}
			ResolvedType::Slice { underlying } => {
				format!("[]{}", self.type_name(*underlying))
			}
			ResolvedType::Array { size, underlying } => {
				format!("[{}]{}", size, self.type_name(*underlying))
			}
			ResolvedType::CArray { underlying } => {
				format!("[^]{}", self.type_name(*underlying))
			}
			ResolvedType::Reference {
				mutable,
				lifetime,
				underlying,
			} => {
				let mut out = "&".to_string();
				if let Some(lifetime) = lifetime {
					out.push_str(&format!("'{} ", lifetime));
				}
				if *mutable {
					out.push_str("mut ");
				}
				out.push_str(&self.type_name(*underlying));
				out
			}
			ResolvedType::Fn {
				params,
				return_type,
			} => {
				let mut out = String::new();
				out.push_str("fn(");
				for (idx, param) in params.iter().enumerate() {
					if idx > 0 {
						out.push_str(", ");
					}
					out.push_str(&self.type_name(*param));
				}
				out.push(')');
				out.push_str(" -> ");
				out.push_str(&self.type_name(*return_type));
				out
			}
			ResolvedType::GenericInstance { base, .. } => self.type_name(*base),
			ResolvedType::TypeParam(name) => name.clone(),
			ResolvedType::UntypedInt => "untyped int".to_string(),
			ResolvedType::UntypedFloat => "untyped float".to_string(),
			ResolvedType::Unknown => "unknown".to_string(),
		}
	}

	pub fn coerce_untyped(&self, lhs: &mut TypeId, rhs: TypeId) -> bool {
		if *lhs == self.builtins.untyped_int_id
			&& (self.is_numeric_type(rhs) || rhs == self.builtins.untyped_int_id)
		{
			*lhs = rhs;
			return true;
		}
		if *lhs == self.builtins.untyped_float_id
			&& (self.is_float_type(rhs) || rhs == self.builtins.untyped_float_id)
		{
			*lhs = rhs;
			return true;
		}
		false
	}

	pub fn is_numeric_type(&self, ty: TypeId) -> bool {
		matches!(
			self.arena.get(ty),
			ResolvedType::Builtin(BuiltinType::Integer { .. })
				| ResolvedType::Builtin(BuiltinType::Float { .. })
		)
	}

	pub fn is_integer_type(&self, ty: TypeId) -> bool {
		matches!(
			self.arena.get(ty),
			ResolvedType::Builtin(BuiltinType::Integer { .. })
		)
	}

	pub fn is_float_type(&self, ty: TypeId) -> bool {
		matches!(
			self.arena.get(ty),
			ResolvedType::Builtin(BuiltinType::Float { .. })
		)
	}

	pub fn is_untyped_numeric(&self, ty: TypeId) -> bool {
		ty == self.builtins.untyped_int_id || ty == self.builtins.untyped_float_id
	}

	pub fn try_operator_overload(
		&self,
		op: &str,
		lhs_ty: TypeId,
		rhs_ty: TypeId,
		ctx_self: Option<TypeId>,
	) -> bool {
		let method = format!("operator{op}");
		if let MemberAccess::Found((method_ty, _)) =
			self.lookup_method_access(lhs_ty, &method, ctx_self)
			&& let ResolvedType::Fn { params, .. } = self.arena.get(method_ty)
			&& params.len() == 2 && self.type_matches(params[1], rhs_ty)
		{
			return true;
		}
		false
	}

	pub fn union_accepts_type(&self, union_ty: TypeId, variant_ty: TypeId) -> bool {
		let variants = match self.arena.get(union_ty) {
			ResolvedType::Union { variants, .. } => variants,
			ResolvedType::GenericInstance { base, .. } => {
				if let ResolvedType::Union { variants, .. } = self.arena.get(*base)
				{
					variants
				} else {
					return false;
				}
			}
			_ => return false,
		};
		for variant in variants {
			if self.type_matches(*variant, variant_ty) {
				return true;
			}
			if let ResolvedType::GenericInstance { base, .. } = self.arena.get(*variant)
				&& self.type_matches(*base, variant_ty)
			{
				return true;
			}
		}
		false
	}

	pub fn type_matches(&self, a: TypeId, b: TypeId) -> bool {
		if a == b {
			return true;
		}
		match (self.arena.get(a), self.arena.get(b)) {
			(ResolvedType::TypeParam(_), _) | (_, ResolvedType::TypeParam(_)) => true,
			(ResolvedType::GenericInstance { base, .. }, other)
				if !matches!(other, ResolvedType::GenericInstance { .. }) =>
			{
				self.type_matches(*base, b)
			}
			(other, ResolvedType::GenericInstance { base, .. })
				if !matches!(other, ResolvedType::GenericInstance { .. }) =>
			{
				self.type_matches(a, *base)
			}
			(ResolvedType::Alias { underlying, .. }, _) => {
				self.type_matches(*underlying, b)
			}
			(_, ResolvedType::Alias { underlying, .. }) => {
				self.type_matches(a, *underlying)
			}
			(
				ResolvedType::Pointer { underlying: a },
				ResolvedType::Pointer { underlying: b },
			) => self.type_matches(*a, *b),
			(
				ResolvedType::Slice { underlying: a },
				ResolvedType::Slice { underlying: b },
			) => self.type_matches(*a, *b),
			(
				ResolvedType::Array {
					size: sa,
					underlying: a,
				},
				ResolvedType::Array {
					size: sb,
					underlying: b,
				},
			) => sa == sb && self.type_matches(*a, *b),
			(
				ResolvedType::Reference {
					mutable: am,
					underlying: a,
					..
				},
				ResolvedType::Reference {
					mutable: bm,
					underlying: b,
					..
				},
			) => am == bm && self.type_matches(*a, *b),
			(
				ResolvedType::Fn {
					params: ap,
					return_type: ar,
				},
				ResolvedType::Fn {
					params: bp,
					return_type: br,
				},
			) => {
				ap.len() == bp.len()
					&& ap.iter()
						.zip(bp.iter())
						.all(|(a, b)| self.type_matches(*a, *b))
					&& self.type_matches(*ar, *br)
			}
			(
				ResolvedType::Struct { name: a, .. },
				ResolvedType::Struct { name: b, .. },
			)
			| (
				ResolvedType::Enum { name: a, .. },
				ResolvedType::Enum { name: b, .. },
			)
			| (
				ResolvedType::Union { name: a, .. },
				ResolvedType::Union { name: b, .. },
			)
			| (
				ResolvedType::RawUnion { name: a, .. },
				ResolvedType::RawUnion { name: b, .. },
			)
			| (
				ResolvedType::Newtype { name: a, .. },
				ResolvedType::Newtype { name: b, .. },
			) => a == b,
			(
				ResolvedType::GenericInstance { base: a, .. },
				ResolvedType::GenericInstance { base: b, .. },
			) => self.type_matches(*a, *b),
			(ResolvedType::UntypedInt, ResolvedType::UntypedInt)
			| (ResolvedType::UntypedFloat, ResolvedType::UntypedFloat) => true,
			_ => false,
		}
	}

	fn unwrap_inheritance_type(&self, mut ty: TypeId) -> TypeId {
		loop {
			match self.arena.get(ty) {
				ResolvedType::GenericInstance { base, .. } => {
					ty = *base;
				}
				ResolvedType::Alias { underlying, .. } => {
					ty = *underlying;
				}
				ResolvedType::Reference { underlying, .. }
				| ResolvedType::Pointer { underlying } => {
					ty = *underlying;
				}
				_ => return ty,
			}
		}
	}

	fn push_inheritance_bases(
		&self,
		bases: &[TypeId],
		queue: &mut Vec<TypeId>,
		visited: &mut HashSet<TypeId>,
	) {
		for base in bases {
			let base = self.unwrap_inheritance_type(*base);
			if !visited.contains(&base) {
				queue.push(base);
			}
		}
	}

	fn is_child_of(&self, child: TypeId, parent: TypeId) -> bool {
		let parent = self.unwrap_inheritance_type(parent);
		let mut queue = vec![self.unwrap_inheritance_type(child)];
		let mut visited = HashSet::new();
		while let Some(current) = queue.pop() {
			if current == parent {
				return true;
			}
			if !visited.insert(current) {
				continue;
			}
			if let ResolvedType::Struct { extends, .. } = self.arena.get(current) {
				self.push_inheritance_bases(extends, &mut queue, &mut visited);
			}
		}
		false
	}

	fn can_access_member(&self, owner: TypeId, ctx_self: Option<TypeId>) -> bool {
		ctx_self.is_some_and(|self_ty| self.is_child_of(self_ty, owner))
	}

	pub fn reference_allows_inheritance(&self, actual: TypeId, expected: TypeId) -> bool {
		match (self.arena.get(actual), self.arena.get(expected)) {
			(
				ResolvedType::Reference {
					mutable: actual_mut,
					underlying: actual_underlying,
					..
				},
				ResolvedType::Reference {
					mutable: expected_mut,
					underlying: expected_underlying,
					..
				},
			) => {
				actual_mut == expected_mut
					&& self.is_child_of(
						*actual_underlying,
						*expected_underlying,
					)
			}
			_ => false,
		}
	}

	fn lookup_field_info(&self, ty: TypeId, name: &str) -> Option<(TypeId, TypeId, bool)> {
		match self.arena.get(ty) {
			ResolvedType::Struct { .. } => {
				let mut queue = vec![ty];
				let mut visited = HashSet::new();
				while let Some(current) = queue.pop() {
					if !visited.insert(current) {
						continue;
					}
					let ResolvedType::Struct {
						fields, extends, ..
					} = self.arena.get(current)
					else {
						continue;
					};
					for field in fields {
						if field.name == name {
							return Some((
								field.ty,
								current,
								field.public,
							));
						}
					}
					self.push_inheritance_bases(
						extends,
						&mut queue,
						&mut visited,
					);
				}
				None
			}
			ResolvedType::RawUnion { fields, .. } => {
				for field in fields {
					if field.name == name {
						return Some((field.ty, ty, field.public));
					}
				}
				None
			}
			ResolvedType::GenericInstance { base, .. } => {
				self.lookup_field_info(*base, name)
			}
			ResolvedType::Reference { underlying, .. }
			| ResolvedType::Pointer { underlying } => self.lookup_field_info(*underlying, name),
			ResolvedType::Alias { underlying, .. } => {
				self.lookup_field_info(*underlying, name)
			}
			_ => None,
		}
	}

	pub fn lookup_field_access(
		&self,
		ty: TypeId,
		name: &str,
		ctx_self: Option<TypeId>,
	) -> MemberAccess<TypeId> {
		let Some((field_ty, owner, public)) = self.lookup_field_info(ty, name) else {
			return MemberAccess::Missing;
		};
		if public || self.can_access_member(owner, ctx_self) {
			MemberAccess::Found(field_ty)
		} else {
			MemberAccess::Inaccessible
		}
	}

	fn lookup_method_info(&self, ty: TypeId, name: &str) -> Option<(TypeId, TypeId, bool)> {
		match self.arena.get(ty) {
			ResolvedType::Struct { .. } => {
				let mut queue = vec![ty];
				let mut visited = HashSet::new();
				while let Some(current) = queue.pop() {
					if !visited.insert(current) {
						continue;
					}
					let ResolvedType::Struct {
						methods, extends, ..
					} = self.arena.get(current)
					else {
						continue;
					};
					if let Some(method) = methods.get(name) {
						return Some((method.ty, current, method.public));
					}
					self.push_inheritance_bases(
						extends,
						&mut queue,
						&mut visited,
					);
				}
				None
			}
			ResolvedType::Union { methods, .. } => {
				if let Some(method) = methods.get(name) {
					return Some((method.ty, ty, method.public));
				}
				None
			}
			ResolvedType::GenericInstance { base, .. } => {
				self.lookup_method_info(*base, name)
			}
			ResolvedType::Reference { underlying, .. }
			| ResolvedType::Pointer { underlying } => self.lookup_method_info(*underlying, name),
			ResolvedType::Alias { underlying, .. } => {
				self.lookup_method_info(*underlying, name)
			}
			_ => None,
		}
	}

	fn method_needs_ref(&self, method_ty: TypeId) -> bool {
		matches!(
			self.arena.get(method_ty),
			ResolvedType::Fn { params, .. }
				if matches!(
					params.first().map(|id| self.arena.get(*id)),
					Some(ResolvedType::Reference { .. })
				)
		)
	}

	pub fn lookup_method_access(
		&self,
		ty: TypeId,
		name: &str,
		ctx_self: Option<TypeId>,
	) -> MemberAccess<(TypeId, bool)> {
		let Some((method_ty, owner, public)) = self.lookup_method_info(ty, name) else {
			return MemberAccess::Missing;
		};
		if public || self.can_access_member(owner, ctx_self) {
			MemberAccess::Found((method_ty, self.method_needs_ref(method_ty)))
		} else {
			MemberAccess::Inaccessible
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::frontend::{FrontendWarning, Lexer, Parser, run_passes_with_modules};
	use std::fs::read_to_string;
	use std::path::{Path, PathBuf};

	fn parse_entry(entry_file: &Path) -> Box<AST> {
		let input = read_to_string(entry_file).expect("read entry file");
		let mut lexer = Lexer::new(input, entry_file.to_string_lossy().to_string());
		let mut parser = Parser::new(&mut lexer).expect("parser init");
		let ast = parser.parse().expect("parse entry file");
		let errors = parser.take_errors();
		assert!(errors.is_empty(), "parse errors: {errors:?}");
		ast
	}

	fn run_fixture(
		root_name: &str,
		module_paths: Vec<String>,
	) -> (
		TypedProgram,
		ResolvedProgram,
		Vec<FrontendError>,
		Vec<FrontendWarning>,
	) {
		let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(root_name);
		let entry_file = root.join("app/main.he");
		let ast = parse_entry(&entry_file);
		run_passes_with_modules(
			ast,
			entry_file.to_str().expect("entry path"),
			&module_paths,
		)
	}

	fn assert_fixture_error(
		root_name: &str,
		module_paths: Vec<String>,
		check: impl Fn(&FrontendError) -> bool,
	) {
		let (_typed, _resolved, errors, _warnings) = run_fixture(root_name, module_paths);
		let has_error = errors.iter().any(check);
		assert!(has_error, "errors: {errors:?}");
	}

	#[test]
	fn reports_generic_operator_constraints_in_fixture() {
		let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("testdata/pass4");
		let module_root = root.join("modules");
		let module_paths = vec![module_root.to_string_lossy().to_string()];
		let (typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4", module_paths);

		assert!(
			errors.iter().any(|err| matches!(
				err,
				FrontendError::GenericOperatorConstraint { .. }
			)),
			"errors: {errors:?}"
		);
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
		assert!(typed.modules.contains_key("main"));
		assert!(typed.modules.contains_key("some_package"));
	}

	#[test]
	fn reports_missing_operator_self() {
		assert_fixture_error("testdata/pass4_missing_self", Vec::new(), |err| {
			matches!(
				err,
				FrontendError::MissingOperatorSelf { operator, .. }
					if operator == "operator+"
			)
		});
	}

	#[test]
	fn reports_duplicate_union_variant_type() {
		assert_fixture_error("testdata/pass4_duplicate_union", Vec::new(), |err| {
			matches!(err, FrontendError::DuplicateUnionVariantType { .. })
		});
	}

	#[test]
	fn reports_cyclic_type_definition() {
		assert_fixture_error("testdata/pass4_cycle", Vec::new(), |err| {
			matches!(err, FrontendError::CyclicTypeDefinition { .. })
		});
	}

	#[test]
	fn reports_invalid_index_operator() {
		assert_fixture_error("testdata/pass4_invalid_index", Vec::new(), |err| {
			matches!(
				err,
				FrontendError::InvalidOperator { operator, .. }
					if operator == "[]"
			)
		});
	}

	#[test]
	fn resolves_imported_type_names() {
		let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
			.join("testdata/pass4_module_types");
		let module_root = root.join("modules");
		let module_paths = vec![module_root.to_string_lossy().to_string()];
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_module_types", module_paths);

		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn allows_generic_member_usage_when_present() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_generics_ok", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn allows_array_sizes_with_ctfe_equivalent_expressions() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_array_size_ctfe_ok", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn allows_value_generic_args_with_ctfe_equivalent_expressions() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_generic_value_ctfe_ok", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn allows_value_generic_args_with_const_name_equivalence() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_generic_value_ctfe_name_equiv", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn keeps_type_generic_name_args_as_type_level_names() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_generic_type_name_param_ok", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn const_expr_key_is_stable_for_nested_records_and_arrays() {
		use std::collections::HashMap;

		let mut first = HashMap::new();
		first.insert(
			"meta".to_string(),
			ConstValue::Record(HashMap::from([
				("name".to_string(), ConstValue::String("x".to_string())),
				(
					"nums".to_string(),
					ConstValue::Array(vec![
						ConstValue::Integer(1),
						ConstValue::Integer(2),
					]),
				),
			])),
		);
		first.insert("flag".to_string(), ConstValue::Bool(true));

		let mut second = HashMap::new();
		second.insert("flag".to_string(), ConstValue::Bool(true));
		second.insert(
			"meta".to_string(),
			ConstValue::Record(HashMap::from([
				(
					"nums".to_string(),
					ConstValue::Array(vec![
						ConstValue::Integer(1),
						ConstValue::Integer(2),
					]),
				),
				("name".to_string(), ConstValue::String("x".to_string())),
			])),
		);

		let first_key = Pass4State::const_value_key(&ConstValue::Record(first));
		let second_key = Pass4State::const_value_key(&ConstValue::Record(second));
		assert_eq!(first_key, second_key);
	}

	#[test]
	#[should_panic(expected = "unsupported type-level expression shape")]
	fn expr_key_panics_on_unsupported_type_level_shape() {
		let expr = AST::from(
			SourceLocation::new_from_file("<test>".to_string()),
			ASTValue::Return(None),
		);
		let _ = Pass4State::expr_key_from_ast(expr.as_ref());
	}

	#[test]
	fn reports_generic_member_constraint() {
		assert_fixture_error(
			"testdata/pass4_generics_missing_member",
			Vec::new(),
			|err| matches!(err, FrontendError::GenericMemberConstraint { .. }),
		);
	}

	#[test]
	fn reports_unknown_value() {
		assert_fixture_error("testdata/pass4_unknown_value", Vec::new(), |err| {
			matches!(err, FrontendError::UnknownValue { .. })
		});
	}

	#[test]
	fn reports_type_mismatch() {
		assert_fixture_error("testdata/pass4_type_mismatch", Vec::new(), |err| {
			matches!(err, FrontendError::TypeMismatch { .. })
		});
	}

	#[test]
	fn reports_invalid_operator() {
		assert_fixture_error("testdata/pass4_invalid_operator", Vec::new(), |err| {
			matches!(err, FrontendError::InvalidOperator { .. })
		});
	}

	#[test]
	fn reports_invalid_call() {
		assert_fixture_error("testdata/pass4_invalid_call", Vec::new(), |err| {
			matches!(err, FrontendError::InvalidCall { .. })
		});
	}

	#[test]
	fn reports_positional_after_named_argument() {
		assert_fixture_error("testdata/pass4_named_positional_after", Vec::new(), |err| {
			matches!(err, FrontendError::PositionalAfterNamedArgument { .. })
		});
	}

	#[test]
	fn reports_duplicate_argument() {
		assert_fixture_error("testdata/pass4_duplicate_argument", Vec::new(), |err| {
			matches!(err, FrontendError::DuplicateArgument { .. })
		});
	}

	#[test]
	fn reports_duplicate_named_argument() {
		assert_fixture_error(
			"testdata/pass4_duplicate_named_argument",
			Vec::new(),
			|err| matches!(err, FrontendError::DuplicateNamedArgument { .. }),
		);
	}

	#[test]
	fn reports_unknown_named_argument() {
		assert_fixture_error("testdata/pass4_unknown_named_argument", Vec::new(), |err| {
			matches!(err, FrontendError::UnknownNamedArgument { .. })
		});
	}

	#[test]
	fn reports_missing_named_argument() {
		assert_fixture_error("testdata/pass4_missing_named_argument", Vec::new(), |err| {
			matches!(err, FrontendError::MissingNamedArgument { .. })
		});
	}

	#[test]
	fn reports_default_param_order() {
		assert_fixture_error("testdata/pass4_default_param_order", Vec::new(), |err| {
			matches!(err, FrontendError::DefaultParamOrder { .. })
		});
	}

	#[test]
	fn reports_assign_to_immutable() {
		assert_fixture_error("testdata/pass4_assign_immutable", Vec::new(), |err| {
			matches!(err, FrontendError::AssignToImmutable { .. })
		});
	}

	#[test]
	fn reports_assign_to_immutable_deref() {
		assert_fixture_error("testdata/pass4_assign_immutable_deref", Vec::new(), |err| {
			matches!(err, FrontendError::AssignToImmutable { .. })
		});
	}

	#[test]
	fn reports_use_after_move() {
		assert_fixture_error("testdata/pass4_use_after_move", Vec::new(), |err| {
			matches!(err, FrontendError::UseAfterMove { .. })
		});
	}

	#[test]
	fn reports_assign_while_borrowed() {
		assert_fixture_error("testdata/pass4_assign_while_borrowed", Vec::new(), |err| {
			matches!(err, FrontendError::AssignWhileBorrowed { .. })
		});
	}

	#[test]
	fn reports_move_while_borrowed() {
		assert_fixture_error("testdata/pass4_move_while_borrowed", Vec::new(), |err| {
			matches!(err, FrontendError::MoveWhileBorrowed { .. })
		});
	}

	#[test]
	fn reports_borrow_mut_while_shared() {
		assert_fixture_error(
			"testdata/pass4_borrow_mut_while_shared",
			Vec::new(),
			|err| matches!(err, FrontendError::BorrowMutWhileShared { .. }),
		);
	}

	#[test]
	fn reports_borrow_shared_while_mut() {
		assert_fixture_error(
			"testdata/pass4_borrow_shared_while_mut",
			Vec::new(),
			|err| matches!(err, FrontendError::BorrowSharedWhileMut { .. }),
		);
	}

	#[test]
	fn reports_mut_borrow_immutable() {
		assert_fixture_error("testdata/pass4_mut_borrow_immutable", Vec::new(), |err| {
			matches!(err, FrontendError::MutBorrowOfImmutable { .. })
		});
	}

	#[test]
	fn reports_nonstatic_module_mut() {
		assert_fixture_error("testdata/pass4_nonstatic_module_mut", Vec::new(), |err| {
			matches!(err, FrontendError::NonStaticModuleMut { .. })
		});
	}

	#[test]
	fn reports_pointer_in_constexpr() {
		assert_fixture_error("testdata/pass4_pointer_in_constexpr", Vec::new(), |err| {
			matches!(err, FrontendError::PointerInConstexpr { .. })
		});
	}

	#[test]
	fn reports_constexpr_call_needing_runtimeable() {
		assert_fixture_error(
			"testdata/pass4_constexpr_runtimeable_required",
			Vec::new(),
			|err| matches!(err, FrontendError::ConstexprCallNeedsRuntimeable { .. }),
		);
	}

	#[test]
	fn allows_constexpr_call_with_runtimeable() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_constexpr_runtimeable_ok", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn reports_runtime_call_in_constexpr_without_cascade_mismatch() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_constexpr_runtime_call", Vec::new());
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
		assert_eq!(errors.len(), 1, "errors: {errors:?}");
		assert!(matches!(
			errors[0],
			FrontendError::RuntimeCallInConstexpr { .. }
		));
	}

	#[test]
	fn reports_pointer_requires_unsafe_expr() {
		assert_fixture_error(
			"testdata/pass4_pointer_requires_unsafe_expr",
			Vec::new(),
			|err| matches!(err, FrontendError::PointerRequiresUnsafe { .. }),
		);
	}

	#[test]
	fn reports_pointer_requires_unsafe_struct() {
		assert_fixture_error(
			"testdata/pass4_pointer_requires_unsafe_struct",
			Vec::new(),
			|err| matches!(err, FrontendError::PointerRequiresUnsafe { .. }),
		);
	}

	#[test]
	fn reports_pointer_requires_unsafe_fn() {
		assert_fixture_error(
			"testdata/pass4_pointer_requires_unsafe_fn",
			Vec::new(),
			|err| matches!(err, FrontendError::PointerRequiresUnsafe { .. }),
		);
	}

	#[test]
	fn reports_missing_field() {
		assert_fixture_error("testdata/pass4_missing_field", Vec::new(), |err| {
			matches!(err, FrontendError::MissingField { .. })
		});
	}

	#[test]
	fn reports_non_bool_condition() {
		assert_fixture_error("testdata/pass4_non_bool_condition", Vec::new(), |err| {
			matches!(err, FrontendError::NonBoolCondition { .. })
		});
	}

	#[test]
	fn reports_unknown_type() {
		assert_fixture_error("testdata/pass4_unknown_type", Vec::new(), |err| {
			matches!(err, FrontendError::UnknownType { .. })
		});
	}

	#[test]
	fn reports_unused_value() {
		let (_typed, _resolved, errors, _warnings) =
			run_fixture("testdata/pass4_unused_value", Vec::new());
		let unused = errors.iter().find_map(|err| match err {
			FrontendError::UnusedValue { location, .. } => Some(location),
			_ => None,
		});
		let Some(location) = unused else {
			panic!("errors: {errors:?}");
		};
		assert_eq!(location.range.begin.1, 6);
	}

	#[test]
	fn allows_ignored_result_assignment() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_unused_value_ok", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn allows_inherited_access_in_child() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_inheritance_ok", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn allows_self_type_in_struct_methods() {
		let (_typed, _resolved, errors, warnings) =
			run_fixture("testdata/pass4_self_type", Vec::new());
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert!(warnings.is_empty(), "warnings: {warnings:?}");
	}

	#[test]
	fn reports_inaccessible_member() {
		assert_fixture_error("testdata/pass4_inheritance_private", Vec::new(), |err| {
			matches!(err, FrontendError::InaccessibleMember { .. })
		});
	}
}
