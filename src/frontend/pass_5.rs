use std::collections::{HashMap, HashSet};

use crate::frontend::{
	BuiltinType, ConstExprKey, FieldInfo, ResolvedType, Semantics, TypeArena, TypeId,
	TypeLevelExprKey, TypedAst, TypedModule, TypedProgram, TypedValue,
};

const POINTER_SIZE: usize = 8;

#[derive(Clone, Copy)]
struct Layout {
	align: usize,
	size: usize,
}

struct LayoutEstimator<'a> {
	arena: &'a TypeArena,
	cache: HashMap<TypeId, Layout>,
	visiting: HashSet<TypeId>,
}

impl<'a> LayoutEstimator<'a> {
	fn new(arena: &'a TypeArena) -> Self {
		Self {
			arena,
			cache: HashMap::new(),
			visiting: HashSet::new(),
		}
	}

	fn layout_of(&mut self, ty: TypeId) -> Layout {
		if let Some(layout) = self.cache.get(&ty).copied() {
			return layout;
		}
		if !self.visiting.insert(ty) {
			return Layout { align: 1, size: 1 };
		}

		let layout = match self.arena.get(ty) {
			ResolvedType::Builtin(kind) => self.layout_builtin(kind),
			ResolvedType::Struct { fields, .. } => self.layout_record(fields),
			ResolvedType::RawUnion { fields, .. } => self.layout_union(fields),
			ResolvedType::Enum { .. }
			| ResolvedType::Union { .. }
			| ResolvedType::Pointer { .. }
			| ResolvedType::Reference { .. }
			| ResolvedType::Fn { .. } => Layout {
				align: POINTER_SIZE,
				size: POINTER_SIZE,
			},
			ResolvedType::Slice { .. } => Layout {
				align: POINTER_SIZE,
				size: POINTER_SIZE * 2,
			},
			ResolvedType::Array { size, underlying } => {
				let item = self.layout_of(*underlying);
				let len = self.const_array_len(size).unwrap_or(1);
				Layout {
					align: item.align,
					size: item.size.saturating_mul(len),
				}
			}
			ResolvedType::CArray { underlying } => self.layout_of(*underlying),
			ResolvedType::Alias { underlying, .. }
			| ResolvedType::Newtype { underlying, .. }
			| ResolvedType::GenericInstance {
				base: underlying, ..
			} => self.layout_of(*underlying),
			ResolvedType::TypeParam(_)
			| ResolvedType::UntypedInt
			| ResolvedType::UntypedFloat
			| ResolvedType::Unknown => Layout { align: 1, size: 1 },
		};

		self.visiting.remove(&ty);
		self.cache.insert(ty, layout);
		layout
	}

	fn layout_builtin(&self, kind: &BuiltinType) -> Layout {
		match kind {
			BuiltinType::Integer { bit_size, .. } => {
				let bytes = bit_size
					.map(|bits| (bits as usize).div_ceil(8))
					.unwrap_or(POINTER_SIZE)
					.max(1);
				Layout {
					align: bytes,
					size: bytes,
				}
			}
			BuiltinType::Float { bit_size, .. } => {
				let bytes = bit_size
					.map(|bits| (bits as usize).div_ceil(8))
					.unwrap_or(8)
					.max(1);
				Layout {
					align: bytes,
					size: bytes,
				}
			}
			BuiltinType::Bool => Layout { align: 1, size: 1 },
			BuiltinType::Rune => Layout { align: 4, size: 4 },
			BuiltinType::Void => Layout { align: 1, size: 0 },
			BuiltinType::Type => Layout {
				align: POINTER_SIZE,
				size: POINTER_SIZE,
			},
		}
	}

	fn layout_record(&mut self, fields: &[FieldInfo]) -> Layout {
		let mut offset = 0usize;
		let mut max_align = 1usize;
		for field in fields {
			let field_layout = self.layout_of(field.ty);
			max_align = max_align.max(field_layout.align.max(1));
			offset = align_up(offset, field_layout.align.max(1));
			offset = offset.saturating_add(field_layout.size);
		}
		Layout {
			align: max_align,
			size: align_up(offset, max_align),
		}
	}

	fn layout_union(&mut self, fields: &[FieldInfo]) -> Layout {
		let mut max_align = 1usize;
		let mut max_size = 0usize;
		for field in fields {
			let field_layout = self.layout_of(field.ty);
			max_align = max_align.max(field_layout.align.max(1));
			max_size = max_size.max(field_layout.size);
		}
		Layout {
			align: max_align,
			size: align_up(max_size, max_align),
		}
	}

	fn const_array_len(&self, size: &TypeLevelExprKey) -> Option<usize> {
		let TypeLevelExprKey::Ctfe(ConstExprKey::Integer(value)) = size else {
			return None;
		};
		if *value < 0 {
			return None;
		}
		usize::try_from(*value).ok()
	}
}

pub(crate) fn pass_5(program: &mut TypedProgram, semantics: &mut Semantics) {
	let no_reorder_types = collect_no_reorder_types(program);
	let mut estimator = LayoutEstimator::new(&semantics.arena);
	let mut plans: Vec<(TypeId, Vec<FieldInfo>)> = Vec::new();

	for (type_id, ty) in semantics.arena.types.iter().enumerate() {
		let ResolvedType::Struct { fields, .. } = ty else {
			continue;
		};
		if no_reorder_types.contains(&type_id) || fields.len() < 2 {
			continue;
		}
		let mut indexed: Vec<(usize, FieldInfo)> =
			fields.iter().cloned().enumerate().collect();
		indexed.sort_by(|(left_idx, left), (right_idx, right)| {
			let left_layout = estimator.layout_of(left.ty);
			let right_layout = estimator.layout_of(right.ty);
			right_layout
				.align
				.cmp(&left_layout.align)
				.then_with(|| right_layout.size.cmp(&left_layout.size))
				.then_with(|| left_idx.cmp(right_idx))
		});
		plans.push((
			type_id,
			indexed.into_iter().map(|(_, field)| field).collect(),
		));
	}

	for (type_id, new_fields) in plans {
		if let ResolvedType::Struct { fields, .. } = semantics.arena.get_mut(type_id) {
			*fields = new_fields;
		}
	}
}

fn collect_no_reorder_types(program: &TypedProgram) -> HashSet<TypeId> {
	let mut no_reorder = HashSet::new();
	for module in program.modules.values() {
		let TypedValue::ExprList { items, .. } = &module.ast.v else {
			continue;
		};
		for item in items {
			collect_no_reorder_from_item(item, module, &mut no_reorder);
		}
	}
	no_reorder
}

fn collect_no_reorder_from_item(node: &TypedAst, module: &TypedModule, out: &mut HashSet<TypeId>) {
	let node = unwrap_pub(node);
	match &node.v {
		TypedValue::DeclarationConstexpr(name, value)
		| TypedValue::Declaration { name, value, .. } => {
			mark_no_reorder(name, value.as_ref(), module, out);
		}
		TypedValue::DeclarationMulti {
			names,
			values: Some(values),
			..
		} => {
			for (idx, name) in names.iter().enumerate() {
				if let Some(value) = values.get(idx).or_else(|| values.first()) {
					mark_no_reorder(name, value.as_ref(), module, out);
				}
			}
		}
		_ => {}
	}
}

fn mark_no_reorder(name: &str, value: &TypedAst, module: &TypedModule, out: &mut HashSet<TypeId>) {
	let TypedValue::Struct { attributes, .. } = &value.v else {
		return;
	};
	if !attributes.iter().any(|attr| attr == "no_reorder") {
		return;
	}
	if let Some(type_id) = module.types.get(name) {
		out.insert(*type_id);
	}
}

fn unwrap_pub(node: &TypedAst) -> &TypedAst {
	if let TypedValue::Pub(inner) = &node.v {
		inner.as_ref()
	} else {
		node
	}
}

fn align_up(offset: usize, align: usize) -> usize {
	let align = align.max(1);
	(offset + align - 1) & !(align - 1)
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::frontend::{
		Lexer, Parser, ResolvedProgram, pass_1::pass_1, pass_2::pass_2, pass_3::pass_3,
		pass_4::pass_4,
	};

	fn run_to_pass5(
		src: &str,
	) -> (TypedProgram, Semantics, Vec<crate::frontend::FrontendError>) {
		let mut lexer = Lexer::new(src.to_string(), "<test>".to_string());
		let mut parser = Parser::new(&mut lexer).expect("parser init");
		let ast = parser.parse().expect("parse");
		let parse_errors = parser.take_errors();
		assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");

		let mut ast = pass_1(ast);
		let mut errors = pass_2(&mut ast);
		let pass3 = pass_3(ast, "<test>", &Vec::new());
		errors.extend(pass3.errors);
		let mut pass4 = pass_4(ResolvedProgram {
			entry: pass3.program.entry,
			modules: pass3.program.modules,
		});
		errors.append(&mut pass4.errors);
		pass_5(&mut pass4.program, &mut pass4.semantics);
		(pass4.program, pass4.semantics, errors)
	}

	fn struct_field_names(
		semantics: &Semantics,
		module_id: &str,
		struct_name: &str,
	) -> Vec<String> {
		let module = semantics.modules.get(module_id).expect("module");
		let struct_ty = *module.types.get(struct_name).expect("struct type");
		let ResolvedType::Struct { fields, .. } = semantics.arena.get(struct_ty) else {
			panic!("expected struct type");
		};
		fields.iter().map(|field| field.name.clone()).collect()
	}

	#[test]
	fn reorders_struct_fields_for_less_padding() {
		let src = r#"
package main

Data :: struct {
	a: i8
	b: i64
	c: i32
}
"#;
		let (_program, semantics, errors) = run_to_pass5(src);
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert_eq!(
			struct_field_names(&semantics, "main", "Data"),
			vec!["b", "c", "a"]
		);
	}

	#[test]
	fn respects_no_reorder_attribute() {
		let src = r#"
package main

Data :: struct [[no_reorder]] {
	a: i8
	b: i64
	c: i32
}
"#;
		let (_program, semantics, errors) = run_to_pass5(src);
		assert!(errors.is_empty(), "errors: {errors:?}");
		assert_eq!(
			struct_field_names(&semantics, "main", "Data"),
			vec!["a", "b", "c"]
		);
	}
}
