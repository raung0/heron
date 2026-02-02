use crate::frontend::{AST, ASTValue};

pub(crate) fn pass_1(mut ast: Box<AST>) -> Box<AST> {
	reorder_declarations(&mut ast);
	ast
}

fn reorder_declarations(ast: &mut AST) {
	let ASTValue::ExprList(exprs) = &mut ast.v else {
		return;
	};

	let items = std::mem::take(exprs);
	let mut buckets: [Vec<Box<AST>>; 5] = std::array::from_fn(|_| Vec::new());

	for item in items {
		let bucket = declaration_bucket(&item);
		buckets[bucket].push(item);
	}

	*exprs = buckets.into_iter().flatten().collect();
}

fn declaration_bucket(node: &AST) -> usize {
	match &unwrap_pub(node).v {
		ASTValue::Package { .. } => 0,
		ASTValue::Use { .. } => 1,
		ASTValue::Struct { .. }
		| ASTValue::Enum { .. }
		| ASTValue::Union { .. }
		| ASTValue::RawUnion { .. }
		| ASTValue::Newtype { .. }
		| ASTValue::Alias { .. } => 2,
		ASTValue::DeclarationMulti {
			constexpr: true, ..
		}
		| ASTValue::DeclarationConstexpr(..) => 3,
		_ => 4,
	}
}

fn unwrap_pub<'a>(node: &'a AST) -> &'a AST {
	let mut current = node;
	while let ASTValue::Pub(inner) = &current.v {
		current = inner.as_ref();
	}
	current
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
	fn reorders_top_level_declarations() {
		let src = "value: i32 = 2; struct { }; pub use b; CONST :: 1; use a; package main";
		let ast = parse_all(src);
		let ast = pass_1(ast);

		let ASTValue::ExprList(items) = ast.v else {
			panic!("expected ExprList root");
		};

		let labels: Vec<&'static str> = items
			.iter()
			.map(|item| match &item.v {
				ASTValue::Package { .. } => "package",
				ASTValue::Use { .. } => "use",
				ASTValue::Struct { .. } => "struct",
				ASTValue::DeclarationMulti {
					constexpr: true, ..
				}
				| ASTValue::DeclarationConstexpr(..) => "const",
				ASTValue::DeclarationMulti {
					constexpr: false, ..
				} => "decl",
				ASTValue::Pub(inner) => match &inner.v {
					ASTValue::Use { .. } => "use",
					ASTValue::Struct { .. } => "struct",
					_ => "other",
				},
				_ => "other",
			})
			.collect();

		assert_eq!(
			labels,
			vec!["package", "use", "use", "struct", "const", "decl"]
		);
	}
}
