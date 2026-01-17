use std::collections::HashMap;

use crate::frontend::{walk_ast, ASTValue, FrontendError, InitializerItem, Type, AST};

pub(crate) fn pass_2(ast: &Box<AST>) -> Vec<FrontendError> {
	let mut errors = Vec::<FrontendError>::new();

	walk_ast(ast, None, &mut |node, _parent| match &node.v {
		ASTValue::TypedInitializerList { ty, items } => {
			check_keyed_initializer_duplicates(items, &mut errors);
			if let Type::Array { size, .. } = ty.as_ref() {
				if !is_id_or_dot_id_path(size) {
					errors.push(FrontendError::InvalidEnumeratedArrayEnum(
						node.location.clone(),
					));
				}
			}
		}

		ASTValue::InitializerList(items) => {
			check_keyed_initializer_duplicates(items, &mut errors);
		}

		_ => {}
	});

	errors
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

#[cfg(test)]
mod tests {
	use super::*;
	use crate::frontend::{Lexer, Parser};

	fn parse_all(src: &str) -> Box<AST> {
		let mut lexer = Lexer::new(src.to_string(), "<test>".to_string());
		let mut parser = Parser::new(&mut lexer).expect("parser init");
		parser.parse().expect("parse ok")
	}

	#[test]
	fn typed_initializer_list_allows_scoped_enum_array() {
		let src = "dirs :: [scope.Direction]Vector2 .{}";
		let ast = parse_all(src);
		let errors = pass_2(&ast);
		assert!(!errors
			.iter()
			.any(|err| matches!(err, FrontendError::InvalidEnumeratedArrayEnum(_))));
	}

	#[test]
	fn typed_initializer_list_allows_unscoped_enum_array() {
		let src = "dirs :: [Direction]Vector2 .{}";
		let ast = parse_all(src);
		let errors = pass_2(&ast);
		assert!(!errors
			.iter()
			.any(|err| matches!(err, FrontendError::InvalidEnumeratedArrayEnum(_))));
	}
}
