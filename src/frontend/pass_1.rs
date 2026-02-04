use crate::frontend::{walk_ast_mut, ASTValue, SourceLocation, Trivia, AST};

pub(crate) fn pass_1(mut ast: Box<AST>) -> Box<AST> {
	reorder_declarations(&mut ast);
	normalize_declarations(&mut ast);
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

fn normalize_declarations(ast: &mut Box<AST>) {
	walk_ast_mut(ast, &mut |node| match &mut node.v {
		ASTValue::ExprList(exprs) | ASTValue::ExprListNoScope(exprs) => {
			normalize_list(exprs);
		}
		_ => {}
	});
}

#[allow(clippy::vec_box)]
fn normalize_list(exprs: &mut Vec<Box<AST>>) {
	let items = std::mem::take(exprs);
	let mut normalized: Vec<Box<AST>> = Vec::new();

	for item in items {
		let mut item = item;
		match &mut item.v {
			ASTValue::DeclarationMulti {
				names,
				types,
				values,
				constexpr,
				mutable,
			} => {
				let types_len = types.len();
				let values_len = values.as_ref().map_or(0, |v| v.len());
				if !can_expand_arity(names.len(), types_len, values_len)
					|| (types_len == 0 && values.is_none())
				{
					normalized.push(item);
				} else {
					let names_owned = std::mem::take(names);
					let types_owned = std::mem::take(types);
					let values_owned = values.take();
					let expanded = expand_declaration_multi(
						item.location.clone(),
						item.trivia.clone(),
						names_owned,
						types_owned,
						values_owned,
						*constexpr,
						*mutable,
						false,
					);
					normalized.extend(expanded);
				}
			}
			ASTValue::Pub(inner) => {
				if let ASTValue::DeclarationMulti {
					names,
					types,
					values,
					constexpr,
					mutable,
				} = &mut inner.v
				{
					let types_len = types.len();
					let values_len = values.as_ref().map_or(0, |v| v.len());
					if !can_expand_arity(names.len(), types_len, values_len)
						|| (types_len == 0 && values.is_none())
					{
						normalized.push(item);
					} else {
						let names_owned = std::mem::take(names);
						let types_owned = std::mem::take(types);
						let values_owned = values.take();
						let expanded = expand_declaration_multi(
							item.location.clone(),
							item.trivia.clone(),
							names_owned,
							types_owned,
							values_owned,
							*constexpr,
							*mutable,
							true,
						);
						normalized.extend(expanded);
					}
				} else {
					normalized.push(item);
				}
			}
			_ => normalized.push(item),
		}
	}

	*exprs = normalized;
}

#[allow(clippy::vec_box)]
#[allow(clippy::too_many_arguments)]
fn expand_declaration_multi(
	location: SourceLocation,
	trivia: Vec<Trivia>,
	names: Vec<String>,
	types: Vec<Box<crate::frontend::Type>>,
	values: Option<Vec<Box<AST>>>,
	constexpr: bool,
	mutable: bool,
	wrap_pub: bool,
) -> Vec<Box<AST>> {
	let types_len = types.len();
	let values_len = values.as_ref().map_or(0, |v| v.len());

	let mut out = Vec::new();
	let mut values_iter = values.unwrap_or_default().into_iter();
	let mut shared_value = if values_len == 1 {
		values_iter.next()
	} else {
		None
	};

	for (index, name) in names.into_iter().enumerate() {
		let type_for = if types_len == 0 {
			None
		} else if types_len == 1 {
			Some(types[0].clone())
		} else {
			Some(types[index].clone())
		};
		let value_for = if values_len == 0 {
			None
		} else if values_len == 1 {
			if index == 0 {
				shared_value.take()
			} else {
				shared_value.clone()
			}
		} else {
			values_iter.next()
		};

		let mut decl = if type_for.is_none() {
			let value = value_for.expect("declaration requires value");
			if constexpr {
				AST::from(
					location.clone(),
					ASTValue::DeclarationConstexpr(name.clone(), value),
				)
			} else {
				AST::from(
					location.clone(),
					ASTValue::Declaration {
						name: name.clone(),
						value,
						mutable,
					},
				)
			}
		} else {
			AST::from(
				location.clone(),
				ASTValue::DeclarationMulti {
					names: vec![name.clone()],
					types: vec![type_for.expect("type required")],
					values: value_for.map(|v| vec![v]),
					constexpr,
					mutable,
				},
			)
		};
		decl.trivia = trivia.clone();

		if wrap_pub {
			let mut pub_node = AST::from(location.clone(), ASTValue::Pub(decl));
			pub_node.trivia = trivia.clone();
			out.push(pub_node);
		} else {
			out.push(decl);
		}
	}

	out
}

fn can_expand_arity(names_len: usize, types_len: usize, values_len: usize) -> bool {
	let types_ok = types_len == 0 || types_len == 1 || types_len == names_len;
	let values_ok = values_len == 0 || values_len == 1 || values_len == names_len;
	types_ok && values_ok
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

fn unwrap_pub(node: &AST) -> &AST {
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
