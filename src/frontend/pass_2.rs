use std::collections::HashMap;

use crate::frontend::{
	AST, ASTValue, FrontendError, InitializerItem, Type, walk_ast, walk_ast_mut,
};

pub(crate) fn pass_2(ast: &mut Box<AST>) -> Vec<FrontendError> {
	let mut errors = Vec::<FrontendError>::new();

	normalize_assignment_binexpr(ast);

	walk_ast(ast, None, &mut |node, parent| match &node.v {
		ASTValue::Hide(name) => {
			let in_scope = matches!(
				parent.map(|p| &p.v),
				Some(ASTValue::ExprList(_) | ASTValue::ExprListNoScope(_))
			);
			if !in_scope {
				errors.push(FrontendError::HideOutsideScope {
					location: node.location.clone(),
					name: name.clone(),
				});
			}
		}
		ASTValue::ExprList(items) => {
			let is_struct_body = matches!(
				parent.map(|p| &p.v),
				Some(ASTValue::Struct { .. } | ASTValue::RawUnion { .. })
			);
			check_duplicate_declarations(items, is_struct_body, &mut errors);
			if is_struct_body {
				check_struct_members(items, &mut errors);
			}
		}
		ASTValue::DeclarationMulti {
			names,
			types,
			values,
			..
		} => {
			if has_invalid_declaration_arity(names.len(), types.len(), values) {
				errors.push(FrontendError::InvalidDeclarationArity(
					node.location.clone(),
				));
			}
			for ty in types {
				check_inline_struct_type(ty, &node.location, &mut errors);
			}
		}
		ASTValue::TypedInitializerList { ty, items } => {
			check_keyed_initializer_duplicates(items, &mut errors);
			check_inline_struct_type(ty, &node.location, &mut errors);
			if let Type::Array { size, .. } = ty.as_ref()
				&& !is_id_or_dot_id_path(size)
			{
				errors.push(FrontendError::InvalidEnumeratedArrayEnum(
					node.location.clone(),
				));
			}
		}

		ASTValue::InitializerList(items) => {
			check_keyed_initializer_duplicates(items, &mut errors);
		}

		ASTValue::Type(ty) => {
			check_inline_struct_type(ty, &node.location, &mut errors);
		}

		ASTValue::Fn {
			params,
			return_type,
			..
		} => {
			for param in params {
				if let Some(ty) = &param.ty {
					check_inline_struct_type(ty, &node.location, &mut errors);
				}
			}
			if let Some(ty) = return_type {
				check_inline_struct_type(ty, &node.location, &mut errors);
			}
		}

		ASTValue::Struct { extends, .. } => {
			if let Some(ty) = extends {
				check_inline_struct_type(ty, &node.location, &mut errors);
			}
			check_struct_or_union_comptime(parent, &node.location, &mut errors);
		}

		ASTValue::Union { variants, .. } => {
			for ty in variants {
				check_inline_struct_type(ty, &node.location, &mut errors);
			}
			check_struct_or_union_comptime(parent, &node.location, &mut errors);
		}

		ASTValue::RawUnion { .. } => {
			check_struct_or_union_comptime(parent, &node.location, &mut errors);
		}

		ASTValue::Match { cases, .. } => {
			for case in cases {
				if let crate::frontend::MatchCasePattern::Type(ty) = &case.pattern {
					check_inline_struct_type(ty, &node.location, &mut errors);
				}
			}
		}

		ASTValue::Newtype { underlying, .. } | ASTValue::Alias { underlying } => {
			check_inline_struct_type(underlying, &node.location, &mut errors);
		}

		_ => {}
	});

	errors
}

fn normalize_assignment_binexpr(ast: &mut Box<AST>) {
	walk_ast_mut(ast, &mut |node| {
		let ASTValue::BinExpr {
			op,
			lhs,
			rhs,
			has_eq: false,
		} = &node.v
		else {
			return;
		};
		if !matches!(op, crate::frontend::Operator::Set) {
			return;
		}
		match &lhs.v {
			ASTValue::Id(name) => {
				node.v = ASTValue::Set(name.clone(), rhs.clone());
			}
			ASTValue::ExprList(items) => {
				let mut names = Vec::new();
				for item in items {
					if let ASTValue::Id(name) = &item.v {
						names.push(name.clone());
					} else {
						return;
					}
				}
				let ASTValue::ExprList(values) = &rhs.v else {
					return;
				};
				node.v = ASTValue::SetMulti {
					names,
					values: values.clone(),
				};
			}
			_ => {}
		}
	});
}

fn is_id_or_dot_id_path(size: &str) -> bool {
	let mut parts = size.split('.');
	let first = parts.next().unwrap_or("");
	if first.is_empty() {
		return false;
	}
	parts.all(|part| !part.is_empty())
}

fn check_keyed_initializer_duplicates(items: &[InitializerItem], errors: &mut Vec<FrontendError>) {
	let mut first: HashMap<String, crate::frontend::SourceLocation> = HashMap::new();

	for item in items {
		let InitializerItem::Named { name, value } = item else {
			continue;
		};

		let loc = value.location.clone();

		if let Some(first_loc) = first.insert(name.clone(), loc.clone()) {
			errors.push(FrontendError::InitializerListHasDuplicateFields {
				first_found_definition: first_loc,
				conflicting_definition: loc,
			});
		}
	}
}

fn check_duplicate_declarations(
	items: &[Box<AST>],
	struct_members: bool,
	errors: &mut Vec<FrontendError>,
) {
	let mut seen: HashMap<String, crate::frontend::SourceLocation> = HashMap::new();
	for item in items {
		let node = unwrap_pub(item.as_ref());
		if let ASTValue::Hide(name) = &node.v {
			if struct_members {
				errors.push(FrontendError::HideOutsideScope {
					location: node.location.clone(),
					name: name.clone(),
				});
			} else {
				seen.remove(name);
			}
			continue;
		}
		let mut names: Vec<&String> = Vec::new();
		match &node.v {
			ASTValue::DeclarationMulti { names: decls, .. } => {
				names.extend(decls.iter());
			}
			ASTValue::Declaration { name, .. }
			| ASTValue::DeclarationConstexpr(name, _) => {
				names.push(name);
			}
			_ => continue,
		}
		for name in names {
			let loc = node.location.clone();
			if let Some(first_loc) = seen.insert(name.clone(), loc.clone()) {
				if struct_members {
					errors.push(FrontendError::DuplicateFieldDeclaration {
						name: name.clone(),
						first_found_definition: first_loc,
						conflicting_definition: loc,
					});
				} else {
					errors.push(FrontendError::DuplicateValueDeclaration {
						name: name.clone(),
						first_found_definition: first_loc,
						conflicting_definition: loc,
					});
				}
			}
		}
	}
}

fn check_struct_members(items: &[Box<AST>], errors: &mut Vec<FrontendError>) {
	for item in items {
		let node = unwrap_pub(item.as_ref());
		match &node.v {
			ASTValue::DeclarationMulti { .. }
			| ASTValue::Declaration { .. }
			| ASTValue::DeclarationConstexpr(..) => {}
			_ => {
				errors.push(FrontendError::InvalidStructMember {
					location: node.location.clone(),
				});
			}
		}
	}
}

fn check_struct_or_union_comptime(
	parent: Option<&AST>,
	location: &crate::frontend::SourceLocation,
	errors: &mut Vec<FrontendError>,
) {
	if !is_inside_comptime_declaration(parent) {
		errors.push(FrontendError::StructOrUnionNotInComptimeDeclaration(
			location.clone(),
		));
	}
}

fn check_inline_struct_type(
	ty: &Type,
	location: &crate::frontend::SourceLocation,
	errors: &mut Vec<FrontendError>,
) {
	if type_contains_inline_struct(ty) {
		errors.push(FrontendError::InlineStructTypeNotAllowed {
			location: location.clone(),
		});
	}
}

fn type_contains_inline_struct(ty: &Type) -> bool {
	match ty {
		Type::Generic { args, .. } => args.iter().any(|arg| match arg {
			crate::frontend::GenericArg::Expr(expr) => {
				expr.contains("Struct") && expr.contains("ExprList")
			}
			crate::frontend::GenericArg::Type(inner) => {
				type_contains_inline_struct(inner)
			}
			crate::frontend::GenericArg::Name(_) => false,
		}),
		Type::Fn {
			params,
			return_type,
		} => {
			params.iter()
				.any(|param| type_contains_inline_struct(param))
				|| return_type
					.as_ref()
					.is_some_and(|ret| type_contains_inline_struct(ret))
		}
		Type::Pointer { underlying }
		| Type::Slice { underlying }
		| Type::Array { underlying, .. }
		| Type::CArray { underlying }
		| Type::Reference { underlying, .. } => type_contains_inline_struct(underlying),
		_ => false,
	}
}

fn unwrap_pub(node: &AST) -> &AST {
	if let ASTValue::Pub(inner) = &node.v {
		inner.as_ref()
	} else {
		node
	}
}

#[allow(clippy::vec_box)]
fn has_invalid_declaration_arity(
	names_len: usize,
	types_len: usize,
	values: &Option<Vec<Box<AST>>>,
) -> bool {
	let types_ok = types_len == 0 || types_len == 1 || types_len == names_len;
	let values_len = values.as_ref().map_or(0, |vals| vals.len());
	let values_ok = values_len == 0 || values_len == 1 || values_len == names_len;
	!types_ok || !values_ok
}

fn is_inside_comptime_declaration(parent: Option<&AST>) -> bool {
	let Some(parent) = parent else {
		return false;
	};

	matches!(
		parent.v,
		ASTValue::DeclarationMulti {
			constexpr: true,
			..
		} | ASTValue::DeclarationConstexpr(..)
	)
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::frontend::{Lexer, Operator, Parser};

	fn parse_all(src: &str) -> Box<AST> {
		let mut lexer = Lexer::new(src.to_string(), "<test>".to_string());
		let mut parser = Parser::new(&mut lexer).expect("parser init");
		parser.parse().expect("parse ok")
	}

	#[test]
	fn typed_initializer_list_allows_scoped_enum_array() {
		let src = "dirs :: [scope.Direction]Vector2 .{}";
		let ast = parse_all(src);
		let mut ast = ast;
		let errors = pass_2(&mut ast);
		assert!(!errors
			.iter()
			.any(|err| matches!(err, FrontendError::InvalidEnumeratedArrayEnum(_))));
	}

	#[test]
	fn typed_initializer_list_allows_unscoped_enum_array() {
		let src = "dirs :: [Direction]Vector2 .{}";
		let ast = parse_all(src);
		let mut ast = ast;
		let errors = pass_2(&mut ast);
		assert!(!errors
			.iter()
			.any(|err| matches!(err, FrontendError::InvalidEnumeratedArrayEnum(_))));
	}

	#[test]
	fn struct_requires_comptime_declaration() {
		let src = "Point := struct { x: i32; };";
		let ast = parse_all(src);
		let mut ast = ast;
		let errors = pass_2(&mut ast);
		assert!(errors.iter().any(|err| matches!(
			err,
			FrontendError::StructOrUnionNotInComptimeDeclaration(_)
		)));
	}

	#[test]
	fn union_allows_comptime_declaration() {
		let src = "Data :: union { i32; f32; };";
		let ast = parse_all(src);
		let mut ast = ast;
		let errors = pass_2(&mut ast);
		assert!(!errors.iter().any(|err| matches!(
			err,
			FrontendError::StructOrUnionNotInComptimeDeclaration(_)
		)));
	}

	#[test]
	fn inline_struct_type_not_allowed_in_generics() {
		let src = "Result :: union<T; E> { Ok<T> Err<E> }\nmain :: fn { res: Result<i32, struct{}> = Ok(1) }";
		let ast = parse_all(src);
		let mut ast = ast;
		let errors = pass_2(&mut ast);
		assert!(errors.iter().any(|err| matches!(
			err,
			FrontendError::InlineStructTypeNotAllowed { .. }
		)));
	}

	#[test]
	fn normalizes_assignment_binexpr_to_set() {
		let src = "main := fn { a = 1 }";
		let ast = parse_all(src);
		let mut ast = ast;
		let _ = pass_2(&mut ast);
		let mut found = false;
		walk_ast(&ast, None, &mut |node, _| {
			if matches!(node.v, ASTValue::Set(_, _)) {
				found = true;
			}
		});
		assert!(found, "expected assignment to normalize into Set");
	}

	#[test]
	fn normalizes_assignment_binexpr_to_set_multi() {
		let src = "main := fn { a, b = b, a }";
		let ast = parse_all(src);
		let mut ast = ast;
		let _ = pass_2(&mut ast);
		let mut found = false;
		walk_ast(&ast, None, &mut |node, _| {
			if let ASTValue::SetMulti { names, values } = &node.v {
				assert_eq!(names, &["a", "b"]);
				assert_eq!(values.len(), 2);
				found = true;
			}
		});
		assert!(found, "expected assignment to normalize into SetMulti");
	}

	#[test]
	fn preserves_member_assignment_binexpr() {
		let src = "main := fn { obj.field = 1 }";
		let ast = parse_all(src);
		let mut ast = ast;
		let _ = pass_2(&mut ast);
		let mut found = false;
		walk_ast(&ast, None, &mut |node, _| {
			if let ASTValue::BinExpr { op, has_eq, .. } = &node.v {
				if matches!(op, Operator::Set) && !*has_eq {
					found = true;
				}
			}
		});
		assert!(found, "expected member assignment to stay BinExpr");
	}
}
