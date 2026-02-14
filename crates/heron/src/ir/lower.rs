use std::collections::{BTreeMap, HashMap, HashSet};

use crate::frontend::{
	ASTValue, BuiltinType, ConstExprKey, ConstValue, CtfeEngine, Operator, ResolvedType,
	Semantics, TypeId, TypeLevelExprKey, TypedFnBody, TypedProgram, TypedValue,
};
use crate::ir::{
	IrBinOp, IrBlock, IrBlockId, IrCastKind, IrCmpOp, IrConst, IrFunction, IrFunctionTypeMap,
	IrInst, IrInstKind, IrLinkage, IrModule, IrParam, IrProgram, IrTerminator, IrType,
	IrTypeId, IrTypeTable, IrValueDef, IrValueId,
};

#[derive(Clone, Debug)]
pub struct IrLowerError {
	pub module_id: String,
	pub message: String,
}

#[derive(Default)]
struct FunctionBuildIds {
	next_value: u32,
	next_block: u32,
}

impl FunctionBuildIds {
	fn fresh_value(&mut self) -> IrValueId {
		let id = IrValueId(self.next_value);
		self.next_value += 1;
		id
	}

	fn fresh_block(&mut self) -> IrBlockId {
		let id = IrBlockId(self.next_block);
		self.next_block += 1;
		id
	}
}

struct TypeLowerContext<'a> {
	semantics: &'a Semantics,
	map: HashMap<TypeId, IrTypeId>,
	visiting: HashSet<TypeId>,
}

impl<'a> TypeLowerContext<'a> {
	fn new(semantics: &'a Semantics) -> Self {
		Self {
			semantics,
			map: HashMap::new(),
			visiting: HashSet::new(),
		}
	}

	fn lower(&mut self, type_id: TypeId, ir_types: &mut IrTypeTable) -> IrTypeId {
		if let Some(existing) = self.map.get(&type_id).copied() {
			return existing;
		}

		if !self.visiting.insert(type_id) {
			return ir_types.add(IrType::Opaque {
				name: format!("recursive_type_{type_id}"),
			});
		}

		let lowered = match self.semantics.arena.get(type_id) {
			ResolvedType::Builtin(builtin) => lower_builtin(builtin, ir_types),
			ResolvedType::Pointer { underlying }
			| ResolvedType::Reference { underlying, .. } => {
				let underlying = self.lower(*underlying, ir_types);
				ir_types.add(IrType::Ptr { to: underlying })
			}
			ResolvedType::Slice { underlying } => {
				let elem = self.lower(*underlying, ir_types);
				ir_types.add(IrType::Slice { elem })
			}
			ResolvedType::Array { size, underlying } => {
				let elem = self.lower(*underlying, ir_types);
				ir_types.add(IrType::Array {
					elem,
					len: const_array_len(size).unwrap_or(1),
				})
			}
			ResolvedType::CArray { underlying } => {
				let elem = self.lower(*underlying, ir_types);
				ir_types.add(IrType::Ptr { to: elem })
			}
			ResolvedType::Struct {
				name,
				extends,
				fields,
				..
			} => {
				let placeholder = ir_types.add(IrType::Opaque {
					name: format!("struct:{name}"),
				});
				self.map.insert(type_id, placeholder);

				let lowered = IrType::Struct(crate::ir::IrStructType {
					name: Some(name.clone()),
					fields: fields
						.iter()
						.map(|field| crate::ir::IrFieldType {
							name: field.name.clone(),
							ty: self.lower(field.ty, ir_types),
							public: field.public,
						})
						.collect(),
					extends: extends
						.iter()
						.map(|t| self.lower(*t, ir_types))
						.collect(),
				});
				ir_types.types[placeholder.0 as usize] = lowered;
				placeholder
			}
			ResolvedType::RawUnion { name, fields, .. } => {
				let placeholder = ir_types.add(IrType::Opaque {
					name: format!("raw_union:{name}"),
				});
				self.map.insert(type_id, placeholder);
				ir_types.types[placeholder.0 as usize] =
					IrType::RawUnion(crate::ir::IrRawUnionType {
						name: Some(name.clone()),
						fields: fields
							.iter()
							.map(|field| crate::ir::IrFieldType {
								name: field.name.clone(),
								ty: self.lower(field.ty, ir_types),
								public: field.public,
							})
							.collect(),
					});
				placeholder
			}
			ResolvedType::Union { name, variants, .. } => {
				let lowered_variants =
					variants.iter().map(|v| self.lower(*v, ir_types)).collect();
				ir_types.add(IrType::Union(crate::ir::IrTaggedUnionType {
					name: Some(name.clone()),
					variants: lowered_variants,
				}))
			}
			ResolvedType::Enum { name, variants, .. } => {
				let lowered_variants = variants
					.iter()
					.map(|v| crate::ir::IrEnumVariantType {
						name: v.name.clone(),
						payload: v.ty.map(|payload| {
							self.lower(payload, ir_types)
						}),
					})
					.collect();
				ir_types.add(IrType::Enum(crate::ir::IrEnumType {
					name: Some(name.clone()),
					variants: lowered_variants,
				}))
			}
			ResolvedType::Interface { name, methods, .. } => {
				let mut ordered: Vec<_> = methods.iter().collect();
				ordered.sort_by(|(a, _), (b, _)| a.cmp(b));
				let lowered_methods = ordered
					.into_iter()
					.map(|(method_name, info)| {
						crate::ir::IrInterfaceMethodSig {
							name: method_name.clone(),
							sig: self.lower(info.ty, ir_types),
						}
					})
					.collect();
				ir_types.add(IrType::Interface(crate::ir::IrInterfaceType {
					name: Some(name.clone()),
					methods: lowered_methods,
				}))
			}
			ResolvedType::Fn {
				params,
				return_type,
			} => {
				let lowered_params =
					params.iter().map(|p| self.lower(*p, ir_types)).collect();
				let lowered_ret = self.lower(*return_type, ir_types);
				ir_types.add(IrType::FnSig(crate::ir::IrFnSig {
					params: lowered_params,
					ret: lowered_ret,
					variadic: false,
				}))
			}
			ResolvedType::Alias { underlying, .. }
			| ResolvedType::Newtype { underlying, .. } => self.lower(*underlying, ir_types),
			ResolvedType::GenericInstance { base, .. } => self.lower(*base, ir_types),
			ResolvedType::TypeParam(name) => ir_types.add(IrType::Opaque {
				name: format!("typeparam:{name}"),
			}),
			ResolvedType::UntypedInt => ir_types.add(IrType::Int {
				bits: 64,
				signed: true,
			}),
			ResolvedType::UntypedFloat => ir_types.add(IrType::Float64),
			ResolvedType::Unknown => ir_types.add(IrType::Opaque {
				name: "unknown".to_string(),
			}),
		};

		self.visiting.remove(&type_id);
		self.map.entry(type_id).or_insert(lowered);
		lowered
	}
}

fn lower_builtin(builtin: &BuiltinType, ir_types: &mut IrTypeTable) -> IrTypeId {
	match builtin {
		BuiltinType::Bool => ir_types.add(IrType::Bool),
		BuiltinType::Rune => ir_types.add(IrType::Int {
			bits: 32,
			signed: true,
		}),
		BuiltinType::Void => ir_types.add(IrType::Void),
		BuiltinType::Type => ir_types.add(IrType::Opaque {
			name: "type".to_string(),
		}),
		BuiltinType::Integer {
			bit_size, signed, ..
		} => ir_types.add(IrType::Int {
			bits: u16::from(bit_size.unwrap_or(64)),
			signed: *signed,
		}),
		BuiltinType::Float { bit_size, .. } => match bit_size.unwrap_or(64) {
			32 => ir_types.add(IrType::Float32),
			_ => ir_types.add(IrType::Float64),
		},
	}
}

fn const_array_len(expr: &TypeLevelExprKey) -> Option<usize> {
	let TypeLevelExprKey::Ctfe(ConstExprKey::Integer(v)) = expr else {
		return None;
	};
	if *v < 0 {
		return None;
	}
	usize::try_from(*v).ok()
}

pub fn lower_typed_program_to_ir(
	program: &TypedProgram,
	semantics: &Semantics,
) -> Result<IrProgram, Vec<IrLowerError>> {
	let mut type_ctx = TypeLowerContext::new(semantics);
	let mut errors = Vec::new();
	let mut out = IrProgram {
		modules: Vec::new(),
		types: IrTypeTable::new(),
		layouts: Default::default(),
	};

	let module_ids: BTreeMap<_, _> = program.modules.iter().collect();
	let builtin_void_ty = semantics.builtins.void_id;
	for (module_id, module) in module_ids {
		let module_comptime_consts = collect_module_comptime_consts(semantics, module_id);
		let module_runtime_ctfe_consts =
			collect_module_runtime_ctfe_consts(semantics, module_id);
		let mut ir_module = IrModule {
			id: module_id.clone(),
			source_file: module.file_path.clone(),
			..IrModule::default()
		};

		for value_ty in module.values.values() {
			let _ = type_ctx.lower(*value_ty, &mut out.types);
		}
		for type_ty in module.types.values() {
			let _ = type_ctx.lower(*type_ty, &mut out.types);
		}
		let module_lowering_data = ModuleLoweringData {
			module_id,
			module,
			module_comptime_consts: &module_comptime_consts,
			module_runtime_ctfe_consts: &module_runtime_ctfe_consts,
			builtin_void_ty,
		};

		if let TypedValue::ExprList { items, .. }
		| TypedValue::ExprListNoScope { items, .. } = &module.ast.v
		{
			for item in items {
				let mut target = item.as_ref();
				if let TypedValue::Pub(inner) = &target.v {
					target = inner.as_ref();
				}

				match &target.v {
					TypedValue::Declaration { name, value, .. }
					| TypedValue::DeclarationComptime(name, value) => {
						match maybe_collect_function(
							name,
							value.as_ref(),
							&module_lowering_data,
							&mut type_ctx,
							&mut out.types,
						) {
							Ok(Some(function)) => {
								ir_module.functions.push(function)
							}
							Ok(None) => {}
							Err(err) => errors.push(err),
						}
					}
					TypedValue::DeclarationMulti {
						names,
						values: Some(values),
						..
					} => {
						for (idx, name) in names.iter().enumerate() {
							let Some(value) = values
								.get(idx)
								.or_else(|| values.first())
							else {
								continue;
							};
							match maybe_collect_function(
								name,
								value.as_ref(),
								&module_lowering_data,
								&mut type_ctx,
								&mut out.types,
							) {
								Ok(Some(function)) => ir_module
									.functions
									.push(function),
								Ok(None) => {}
								Err(err) => errors.push(err),
							}
						}
					}
					_ => {}
				}
			}
		}

		out.modules.push(ir_module);
	}

	if errors.is_empty() {
		Ok(out)
	} else {
		Err(errors)
	}
}

fn collect_module_comptime_consts(
	semantics: &Semantics,
	module_id: &str,
) -> HashMap<String, ConstValue> {
	let mut out = HashMap::new();
	let Some(module) = semantics.modules.get(module_id) else {
		return out;
	};
	let (ASTValue::ExprList { items, .. } | ASTValue::ExprListNoScope { items, .. }) =
		&module.ast.v
	else {
		return out;
	};
	let required_comptimes = collect_required_comptime_names(items);
	let mut engine = CtfeEngine::new(crate::frontend::CtfeLimits::default());
	for item in items {
		engine.register_function_decls(unwrap_pub(item.as_ref()));
	}

	for item in items {
		let target = unwrap_pub(item.as_ref());
		match &target.v {
			ASTValue::DeclarationComptime(name, value) => {
				if required_comptimes.contains(name)
					&& should_eval_comptime_value(value.as_ref())
					&& let Ok(evaluated) = engine.eval_expr(value.as_ref())
				{
					engine.bind_const(name, evaluated.clone());
					out.insert(name.clone(), evaluated);
				}
			}
			ASTValue::DeclarationMulti {
				names,
				values: Some(values),
				comptime: true,
				..
			} => {
				for (idx, name) in names.iter().enumerate() {
					if !required_comptimes.contains(name) {
						continue;
					}
					if let Some(value) =
						values.get(idx).or_else(|| values.first())
						&& should_eval_comptime_value(value.as_ref())
						&& let Ok(evaluated) =
							engine.eval_expr(value.as_ref())
					{
						engine.bind_const(name, evaluated.clone());
						out.insert(name.clone(), evaluated);
					}
				}
			}
			_ => {}
		}
	}

	out
}

fn collect_module_runtime_ctfe_consts(
	semantics: &Semantics,
	module_id: &str,
) -> HashMap<String, ConstValue> {
	let mut out = HashMap::new();
	let Some(module) = semantics.modules.get(module_id) else {
		return out;
	};
	let (ASTValue::ExprList { items, .. } | ASTValue::ExprListNoScope { items, .. }) =
		&module.ast.v
	else {
		return out;
	};
	let required_comptimes = collect_required_comptime_names(items);
	let known_fn_names = collect_module_function_names(items);
	let mut engine = CtfeEngine::new(crate::frontend::CtfeLimits::default());
	for item in items {
		engine.register_function_decls(unwrap_pub(item.as_ref()));
	}
	for item in items {
		let target = unwrap_pub(item.as_ref());
		match &target.v {
			ASTValue::DeclarationComptime(name, value) => {
				if required_comptimes.contains(name)
					&& should_eval_comptime_value(value.as_ref())
					&& let Ok(evaluated) = engine.eval_expr(value.as_ref())
				{
					engine.bind_const(name, evaluated);
				}
			}
			ASTValue::DeclarationMulti {
				names,
				values: Some(values),
				comptime: true,
				..
			} => {
				for (idx, name) in names.iter().enumerate() {
					if !required_comptimes.contains(name) {
						continue;
					}
					if let Some(value) =
						values.get(idx).or_else(|| values.first())
						&& should_eval_comptime_value(value.as_ref())
						&& let Ok(evaluated) =
							engine.eval_expr(value.as_ref())
					{
						engine.bind_const(name, evaluated);
					}
				}
			}
			_ => {}
		}
	}

	let runtime_scopes = vec![HashSet::new()];
	for item in items {
		let target = unwrap_pub(item.as_ref());
		collect_runtime_ctfe_candidates(
			&mut engine,
			target,
			&runtime_scopes,
			&known_fn_names,
			&mut out,
		);
	}

	out
}

fn collect_module_function_names(items: &[Box<crate::frontend::AST>]) -> HashSet<String> {
	let mut out = HashSet::new();
	for item in items {
		let target = unwrap_pub(item.as_ref());
		match &target.v {
			ASTValue::Declaration { name, value, .. }
			| ASTValue::DeclarationComptime(name, value) => {
				if matches!(value.v, ASTValue::Fn { .. }) {
					out.insert(name.clone());
				}
			}
			ASTValue::DeclarationMulti {
				names,
				values: Some(values),
				..
			} => {
				for (idx, name) in names.iter().enumerate() {
					if let Some(value) =
						values.get(idx).or_else(|| values.first()) && matches!(
						value.v,
						ASTValue::Fn { .. }
					) {
						out.insert(name.clone());
					}
				}
			}
			_ => {}
		}
	}
	out
}

fn collect_runtime_ctfe_candidates(
	engine: &mut CtfeEngine,
	node: &crate::frontend::AST,
	runtime_scopes: &[HashSet<String>],
	known_fn_names: &HashSet<String>,
	out: &mut HashMap<String, ConstValue>,
) {
	match &node.v {
		ASTValue::Declaration {
			name,
			value,
			mutable: _,
		} => {
			if let ASTValue::Fn { body, .. } = &value.v {
				let mut nested_scopes = runtime_scopes.to_vec();
				nested_scopes.push(HashSet::new());
				collect_fn_body_runtime_ctfe(
					engine,
					body,
					&mut nested_scopes,
					known_fn_names,
					out,
				);
			} else {
				collect_runtime_decl_value_ctfe(
					engine,
					name,
					value.as_ref(),
					runtime_scopes,
					known_fn_names,
					out,
				);
			}
		}
		ASTValue::DeclarationMulti {
			names,
			values: Some(values),
			comptime: false,
			..
		} => {
			for (idx, name) in names.iter().enumerate() {
				if let Some(value) = values.get(idx).or_else(|| values.first()) {
					collect_runtime_decl_value_ctfe(
						engine,
						name,
						value.as_ref(),
						runtime_scopes,
						known_fn_names,
						out,
					);
				}
			}
		}
		_ => {}
	}
}

fn collect_runtime_decl_value_ctfe(
	engine: &mut CtfeEngine,
	_name: &str,
	value: &crate::frontend::AST,
	runtime_scopes: &[HashSet<String>],
	known_fn_names: &HashSet<String>,
	out: &mut HashMap<String, ConstValue>,
) {
	if let ASTValue::Fn { body, .. } = &value.v {
		let mut nested_scopes = runtime_scopes.to_vec();
		nested_scopes.push(HashSet::new());
		collect_fn_body_runtime_ctfe(engine, body, &mut nested_scopes, known_fn_names, out);
		return;
	}
	if expr_depends_on_runtime_locals(value, runtime_scopes) {
		return;
	}
	if !expr_contains_call(value) {
		return;
	}
	if !expr_calls_only_known_functions(value, known_fn_names) {
		return;
	}
	if !should_eval_comptime_value(value) {
		return;
	}
	if let Ok(result) = engine.eval_expr(value) {
		out.insert(source_location_key(&value.location), result);
	}
}

fn collect_fn_body_runtime_ctfe(
	engine: &mut CtfeEngine,
	body: &crate::frontend::FnBody,
	runtime_scopes: &mut Vec<HashSet<String>>,
	known_fn_names: &HashSet<String>,
	out: &mut HashMap<String, ConstValue>,
) {
	match body {
		crate::frontend::FnBody::Block(ast) | crate::frontend::FnBody::Expr(ast) => {
			collect_ast_runtime_ctfe(
				engine,
				ast.as_ref(),
				runtime_scopes,
				known_fn_names,
				out,
			)
		}
		crate::frontend::FnBody::Uninitialized => {}
	}
}

fn collect_ast_runtime_ctfe(
	engine: &mut CtfeEngine,
	node: &crate::frontend::AST,
	runtime_scopes: &mut Vec<HashSet<String>>,
	known_fn_names: &HashSet<String>,
	out: &mut HashMap<String, ConstValue>,
) {
	match &node.v {
		ASTValue::ExprList { items, .. } | ASTValue::ExprListNoScope { items, .. } => {
			runtime_scopes.push(HashSet::new());
			for item in items {
				collect_ast_runtime_ctfe(
					engine,
					item.as_ref(),
					runtime_scopes,
					known_fn_names,
					out,
				);
			}
			let _ = runtime_scopes.pop();
		}
		ASTValue::Declaration { name, value, .. } => {
			collect_runtime_decl_value_ctfe(
				engine,
				name,
				value.as_ref(),
				runtime_scopes,
				known_fn_names,
				out,
			);
			if let Some(scope) = runtime_scopes.last_mut() {
				scope.insert(name.clone());
			}
		}
		ASTValue::DeclarationMulti {
			names,
			values: Some(values),
			comptime: false,
			..
		} => {
			for (idx, name) in names.iter().enumerate() {
				if let Some(value) = values.get(idx).or_else(|| values.first()) {
					collect_runtime_decl_value_ctfe(
						engine,
						name,
						value.as_ref(),
						runtime_scopes,
						known_fn_names,
						out,
					);
				}
				if let Some(scope) = runtime_scopes.last_mut() {
					scope.insert(name.clone());
				}
			}
		}
		ASTValue::DeclarationMulti {
			names,
			comptime: false,
			..
		} => {
			if let Some(scope) = runtime_scopes.last_mut() {
				for name in names {
					scope.insert(name.clone());
				}
			}
		}
		ASTValue::If {
			decl, body, else_, ..
		} => {
			if let Some(decl) = decl {
				collect_ast_runtime_ctfe(
					engine,
					decl.as_ref(),
					runtime_scopes,
					known_fn_names,
					out,
				);
			}
			collect_ast_runtime_ctfe(
				engine,
				body.as_ref(),
				runtime_scopes,
				known_fn_names,
				out,
			);
			if let Some(else_) = else_ {
				collect_ast_runtime_ctfe(
					engine,
					else_.as_ref(),
					runtime_scopes,
					known_fn_names,
					out,
				);
			}
		}
		ASTValue::While { decl, body, .. } => {
			if let Some(decl) = decl {
				collect_ast_runtime_ctfe(
					engine,
					decl.as_ref(),
					runtime_scopes,
					known_fn_names,
					out,
				);
			}
			collect_ast_runtime_ctfe(
				engine,
				body.as_ref(),
				runtime_scopes,
				known_fn_names,
				out,
			);
		}
		ASTValue::ForLoop {
			init, step, body, ..
		} => {
			if let Some(init) = init {
				collect_ast_runtime_ctfe(
					engine,
					init.as_ref(),
					runtime_scopes,
					known_fn_names,
					out,
				);
			}
			if let Some(step) = step {
				collect_ast_runtime_ctfe(
					engine,
					step.as_ref(),
					runtime_scopes,
					known_fn_names,
					out,
				);
			}
			collect_ast_runtime_ctfe(
				engine,
				body.as_ref(),
				runtime_scopes,
				known_fn_names,
				out,
			);
		}
		ASTValue::For { body, .. } => collect_ast_runtime_ctfe(
			engine,
			body.as_ref(),
			runtime_scopes,
			known_fn_names,
			out,
		),
		ASTValue::Match { cases, .. } => {
			for case in cases {
				collect_ast_runtime_ctfe(
					engine,
					case.body.as_ref(),
					runtime_scopes,
					known_fn_names,
					out,
				);
			}
		}
		_ => {}
	}
}

fn expr_depends_on_runtime_locals(
	node: &crate::frontend::AST,
	runtime_scopes: &[HashSet<String>],
) -> bool {
	let mut ids = std::collections::VecDeque::new();
	collect_ids_in_ast(node, &mut ids);
	while let Some(id) = ids.pop_front() {
		if runtime_scopes.iter().rev().any(|scope| scope.contains(&id)) {
			return true;
		}
	}
	false
}

fn expr_contains_call(node: &crate::frontend::AST) -> bool {
	match &node.v {
		ASTValue::Call { .. } => true,
		ASTValue::Pub(inner)
		| ASTValue::UnaryPlus(inner)
		| ASTValue::UnaryMinus(inner)
		| ASTValue::Not(inner)
		| ASTValue::Deref(inner)
		| ASTValue::Mut(inner)
		| ASTValue::PtrOf(inner)
		| ASTValue::Defer(inner)
		| ASTValue::Ref { v: inner, .. }
		| ASTValue::DeclarationComptime(_, inner)
		| ASTValue::Set(_, inner)
		| ASTValue::Cast { value: inner, .. }
		| ASTValue::Transmute { value: inner, .. }
		| ASTValue::Declaration { value: inner, .. }
		| ASTValue::NamedArg { value: inner, .. }
		| ASTValue::Return(Some(inner)) => expr_contains_call(inner.as_ref()),
		ASTValue::BinExpr { lhs, rhs, .. } => {
			expr_contains_call(lhs.as_ref()) || expr_contains_call(rhs.as_ref())
		}
		ASTValue::DeclarationMulti {
			values: Some(values),
			..
		}
		| ASTValue::ExprList { items: values, .. }
		| ASTValue::ExprListNoScope { items: values, .. }
		| ASTValue::SetMulti { values, .. } => values.iter().any(|v| expr_contains_call(v.as_ref())),
		ASTValue::GenericApply { target, .. } => expr_contains_call(target.as_ref()),
		ASTValue::If {
			cond,
			decl,
			body,
			else_,
		} => {
			expr_contains_call(cond.as_ref())
				|| decl.as_ref()
					.is_some_and(|d| expr_contains_call(d.as_ref()))
				|| expr_contains_call(body.as_ref())
				|| else_.as_ref()
					.is_some_and(|e| expr_contains_call(e.as_ref()))
		}
		ASTValue::While { cond, decl, body } => {
			expr_contains_call(cond.as_ref())
				|| decl.as_ref()
					.is_some_and(|d| expr_contains_call(d.as_ref()))
				|| expr_contains_call(body.as_ref())
		}
		ASTValue::ForLoop {
			init,
			cond,
			step,
			body,
		} => {
			init.as_ref()
				.is_some_and(|i| expr_contains_call(i.as_ref()))
				|| cond.as_ref()
					.is_some_and(|c| expr_contains_call(c.as_ref()))
				|| step.as_ref()
					.is_some_and(|s| expr_contains_call(s.as_ref()))
				|| expr_contains_call(body.as_ref())
		}
		ASTValue::For { iter, body, .. } => {
			expr_contains_call(iter.as_ref()) || expr_contains_call(body.as_ref())
		}
		ASTValue::InitializerList(items) | ASTValue::TypedInitializerList { items, .. } => {
			items.iter().any(|item| match item {
				crate::frontend::InitializerItem::Positional(value)
				| crate::frontend::InitializerItem::Named { value, .. } => expr_contains_call(value.as_ref()),
			})
		}
		ASTValue::Index { target, indices } => {
			expr_contains_call(target.as_ref())
				|| indices.iter().any(|idx| expr_contains_call(idx.as_ref()))
		}
		ASTValue::Match {
			scrutinee, cases, ..
		} => {
			expr_contains_call(scrutinee.as_ref())
				|| cases.iter().any(|case| {
					(case.guard
						.as_ref()
						.is_some_and(|g| expr_contains_call(g.as_ref())))
						|| expr_contains_call(case.body.as_ref())
				})
		}
		ASTValue::Fn {
			pre,
			where_clause,
			ensures,
			body,
			..
		} => {
			pre.iter().any(|p| expr_contains_call(p.as_ref()))
				|| where_clause
					.as_ref()
					.is_some_and(|w| expr_contains_call(w.as_ref()))
				|| ensures
					.iter()
					.any(|e| expr_contains_call(e.condition.as_ref()))
				|| match body {
					crate::frontend::FnBody::Expr(expr)
					| crate::frontend::FnBody::Block(expr) => expr_contains_call(expr.as_ref()),
					crate::frontend::FnBody::Uninitialized => false,
				}
		}
		_ => false,
	}
}

fn expr_calls_only_known_functions(
	node: &crate::frontend::AST,
	known_fn_names: &HashSet<String>,
) -> bool {
	match &node.v {
		ASTValue::Call { callee, args } => {
			let callee_ok = match &callee.v {
				ASTValue::Id(name) | ASTValue::DotId(name) => {
					known_fn_names.contains(name)
				}
				ASTValue::GenericApply { target, .. } => match &target.v {
					ASTValue::Id(name) | ASTValue::DotId(name) => {
						known_fn_names.contains(name)
					}
					_ => false,
				},
				_ => false,
			};
			callee_ok
				&& args.iter().all(|arg| {
					expr_calls_only_known_functions(
						arg.as_ref(),
						known_fn_names,
					)
				})
		}
		ASTValue::Pub(inner)
		| ASTValue::UnaryPlus(inner)
		| ASTValue::UnaryMinus(inner)
		| ASTValue::Not(inner)
		| ASTValue::Deref(inner)
		| ASTValue::Mut(inner)
		| ASTValue::PtrOf(inner)
		| ASTValue::Defer(inner)
		| ASTValue::Ref { v: inner, .. }
		| ASTValue::DeclarationComptime(_, inner)
		| ASTValue::Set(_, inner)
		| ASTValue::Cast { value: inner, .. }
		| ASTValue::Transmute { value: inner, .. }
		| ASTValue::Declaration { value: inner, .. }
		| ASTValue::NamedArg { value: inner, .. }
		| ASTValue::Return(Some(inner)) => {
			expr_calls_only_known_functions(inner.as_ref(), known_fn_names)
		}
		ASTValue::BinExpr { lhs, rhs, .. } => {
			expr_calls_only_known_functions(lhs.as_ref(), known_fn_names)
				&& expr_calls_only_known_functions(rhs.as_ref(), known_fn_names)
		}
		ASTValue::DeclarationMulti {
			values: Some(values),
			..
		}
		| ASTValue::ExprList { items: values, .. }
		| ASTValue::ExprListNoScope { items: values, .. }
		| ASTValue::SetMulti { values, .. } => values
			.iter()
			.all(|v| expr_calls_only_known_functions(v.as_ref(), known_fn_names)),
		ASTValue::GenericApply { target, .. } => {
			expr_calls_only_known_functions(target.as_ref(), known_fn_names)
		}
		ASTValue::If {
			cond,
			decl,
			body,
			else_,
		} => {
			expr_calls_only_known_functions(cond.as_ref(), known_fn_names)
				&& decl.as_ref().is_none_or(|d| {
					expr_calls_only_known_functions(d.as_ref(), known_fn_names)
				}) && expr_calls_only_known_functions(body.as_ref(), known_fn_names)
				&& else_.as_ref().is_none_or(|e| {
					expr_calls_only_known_functions(e.as_ref(), known_fn_names)
				})
		}
		ASTValue::While { cond, decl, body } => {
			expr_calls_only_known_functions(cond.as_ref(), known_fn_names)
				&& decl.as_ref().is_none_or(|d| {
					expr_calls_only_known_functions(d.as_ref(), known_fn_names)
				}) && expr_calls_only_known_functions(body.as_ref(), known_fn_names)
		}
		ASTValue::ForLoop {
			init,
			cond,
			step,
			body,
		} => {
			init.as_ref().is_none_or(|i| {
				expr_calls_only_known_functions(i.as_ref(), known_fn_names)
			}) && cond.as_ref().is_none_or(|c| {
				expr_calls_only_known_functions(c.as_ref(), known_fn_names)
			}) && step.as_ref().is_none_or(|s| {
				expr_calls_only_known_functions(s.as_ref(), known_fn_names)
			}) && expr_calls_only_known_functions(body.as_ref(), known_fn_names)
		}
		ASTValue::For { iter, body, .. } => {
			expr_calls_only_known_functions(iter.as_ref(), known_fn_names)
				&& expr_calls_only_known_functions(body.as_ref(), known_fn_names)
		}
		ASTValue::InitializerList(items) | ASTValue::TypedInitializerList { items, .. } => {
			items.iter().all(|item| match item {
				crate::frontend::InitializerItem::Positional(value)
				| crate::frontend::InitializerItem::Named { value, .. } => expr_calls_only_known_functions(
					value.as_ref(),
					known_fn_names,
				),
			})
		}
		ASTValue::Index { target, indices } => {
			expr_calls_only_known_functions(target.as_ref(), known_fn_names)
				&& indices.iter().all(|idx| {
					expr_calls_only_known_functions(
						idx.as_ref(),
						known_fn_names,
					)
				})
		}
		ASTValue::Match {
			scrutinee, cases, ..
		} => {
			expr_calls_only_known_functions(scrutinee.as_ref(), known_fn_names)
				&& cases.iter().all(|case| {
					case.guard.as_ref().is_none_or(|guard| {
						expr_calls_only_known_functions(
							guard.as_ref(),
							known_fn_names,
						)
					}) && expr_calls_only_known_functions(
						case.body.as_ref(),
						known_fn_names,
					)
				})
		}
		ASTValue::Fn {
			pre,
			where_clause,
			ensures,
			body,
			..
		} => {
			pre.iter().all(|p| {
				expr_calls_only_known_functions(p.as_ref(), known_fn_names)
			}) && where_clause.as_ref().is_none_or(|w| {
				expr_calls_only_known_functions(w.as_ref(), known_fn_names)
			}) && ensures.iter().all(|e| {
				expr_calls_only_known_functions(
					e.condition.as_ref(),
					known_fn_names,
				)
			}) && match body {
				crate::frontend::FnBody::Expr(expr)
				| crate::frontend::FnBody::Block(expr) => expr_calls_only_known_functions(
					expr.as_ref(),
					known_fn_names,
				),
				crate::frontend::FnBody::Uninitialized => true,
			}
		}
		_ => true,
	}
}

fn source_location_key(location: &crate::frontend::SourceLocation) -> String {
	format!(
		"{}:{}:{}:{}:{}",
		location.file,
		location.range.begin.0,
		location.range.begin.1,
		location.range.end.0,
		location.range.end.1
	)
}

fn unwrap_pub(node: &crate::frontend::AST) -> &crate::frontend::AST {
	if let ASTValue::Pub(inner) = &node.v {
		inner.as_ref()
	} else {
		node
	}
}

fn should_eval_comptime_value(value: &crate::frontend::AST) -> bool {
	!matches!(
		value.v,
		ASTValue::Fn { .. }
			| ASTValue::Struct { .. }
			| ASTValue::Enum { .. }
			| ASTValue::Union { .. }
			| ASTValue::RawUnion { .. }
			| ASTValue::Newtype { .. }
			| ASTValue::Alias { .. }
	)
}

fn collect_required_comptime_names(items: &[Box<crate::frontend::AST>]) -> HashSet<String> {
	let mut comptime_values: HashMap<String, Box<crate::frontend::AST>> = HashMap::new();
	let mut queue: std::collections::VecDeque<String> = std::collections::VecDeque::new();

	for item in items {
		let node = unwrap_pub(item.as_ref());
		match &node.v {
			ASTValue::DeclarationComptime(name, value) => {
				comptime_values.insert(name.clone(), value.clone());
			}
			ASTValue::DeclarationMulti {
				names,
				values: Some(values),
				comptime: true,
				..
			} => {
				for (idx, name) in names.iter().enumerate() {
					if let Some(value) =
						values.get(idx).or_else(|| values.first())
					{
						comptime_values.insert(name.clone(), value.clone());
					}
				}
			}
			ASTValue::Declaration { value, .. } => {
				collect_ids_in_ast(value.as_ref(), &mut queue);
			}
			ASTValue::DeclarationMulti {
				values: Some(values),
				comptime: false,
				..
			} => {
				for value in values {
					collect_ids_in_ast(value.as_ref(), &mut queue);
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
		if let Some(value) = comptime_values.get(&name) {
			collect_ids_in_ast(value.as_ref(), &mut queue);
		}
	}
	required
}

fn collect_ids_in_ast(node: &crate::frontend::AST, out: &mut std::collections::VecDeque<String>) {
	match &node.v {
		ASTValue::Id(name) | ASTValue::DotId(name) => out.push_back(name.clone()),
		ASTValue::Pub(inner)
		| ASTValue::UnaryPlus(inner)
		| ASTValue::UnaryMinus(inner)
		| ASTValue::Not(inner)
		| ASTValue::Deref(inner)
		| ASTValue::Mut(inner)
		| ASTValue::PtrOf(inner)
		| ASTValue::Defer(inner) => collect_ids_in_ast(inner.as_ref(), out),
		ASTValue::Ref { v, .. }
		| ASTValue::DeclarationComptime(_, v)
		| ASTValue::Set(_, v) => collect_ids_in_ast(v.as_ref(), out),
		ASTValue::Cast { value, .. } | ASTValue::Transmute { value, .. } => {
			collect_ids_in_ast(value.as_ref(), out)
		}
		ASTValue::BinExpr { lhs, rhs, .. } => {
			collect_ids_in_ast(lhs.as_ref(), out);
			collect_ids_in_ast(rhs.as_ref(), out);
		}
		ASTValue::Declaration { value, .. } => collect_ids_in_ast(value.as_ref(), out),
		ASTValue::DeclarationMulti {
			values: Some(values),
			..
		}
		| ASTValue::ExprList { items: values, .. }
		| ASTValue::ExprListNoScope { items: values, .. }
		| ASTValue::SetMulti { values, .. } => {
			for value in values {
				collect_ids_in_ast(value.as_ref(), out);
			}
		}
		ASTValue::Call { callee, args } => {
			collect_ids_in_ast(callee.as_ref(), out);
			for arg in args {
				collect_ids_in_ast(arg.as_ref(), out);
			}
		}
		ASTValue::NamedArg { value, .. } | ASTValue::Return(Some(value)) => {
			collect_ids_in_ast(value.as_ref(), out)
		}
		ASTValue::If {
			cond,
			decl,
			body,
			else_,
		} => {
			collect_ids_in_ast(cond.as_ref(), out);
			if let Some(decl) = decl {
				collect_ids_in_ast(decl.as_ref(), out);
			}
			collect_ids_in_ast(body.as_ref(), out);
			if let Some(else_) = else_ {
				collect_ids_in_ast(else_.as_ref(), out);
			}
		}
		ASTValue::While { cond, decl, body } => {
			collect_ids_in_ast(cond.as_ref(), out);
			if let Some(decl) = decl {
				collect_ids_in_ast(decl.as_ref(), out);
			}
			collect_ids_in_ast(body.as_ref(), out);
		}
		ASTValue::ForLoop {
			init,
			cond,
			step,
			body,
		} => {
			if let Some(init) = init {
				collect_ids_in_ast(init.as_ref(), out);
			}
			if let Some(cond) = cond {
				collect_ids_in_ast(cond.as_ref(), out);
			}
			if let Some(step) = step {
				collect_ids_in_ast(step.as_ref(), out);
			}
			collect_ids_in_ast(body.as_ref(), out);
		}
		ASTValue::For { iter, body, .. } => {
			collect_ids_in_ast(iter.as_ref(), out);
			collect_ids_in_ast(body.as_ref(), out);
		}
		ASTValue::InitializerList(items) | ASTValue::TypedInitializerList { items, .. } => {
			for item in items {
				match item {
					crate::frontend::InitializerItem::Positional(value)
					| crate::frontend::InitializerItem::Named {
						value, ..
					} => collect_ids_in_ast(value.as_ref(), out),
				}
			}
		}
		ASTValue::Index { target, indices } => {
			collect_ids_in_ast(target.as_ref(), out);
			for idx in indices {
				collect_ids_in_ast(idx.as_ref(), out);
			}
		}
		ASTValue::Match {
			scrutinee, cases, ..
		} => {
			collect_ids_in_ast(scrutinee.as_ref(), out);
			for case in cases {
				if let crate::frontend::MatchCasePattern::Exprs(exprs) =
					&case.pattern
				{
					for expr in exprs {
						collect_ids_in_ast(expr.as_ref(), out);
					}
				}
				if let Some(guard) = &case.guard {
					collect_ids_in_ast(guard.as_ref(), out);
				}
				collect_ids_in_ast(case.body.as_ref(), out);
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
				collect_ids_in_ast(pre.as_ref(), out);
			}
			if let Some(where_clause) = where_clause {
				collect_ids_in_ast(where_clause.as_ref(), out);
			}
			for ensure in ensures {
				collect_ids_in_ast(ensure.condition.as_ref(), out);
			}
			match body {
				crate::frontend::FnBody::Expr(expr)
				| crate::frontend::FnBody::Block(expr) => collect_ids_in_ast(expr.as_ref(), out),
				crate::frontend::FnBody::Uninitialized => {}
			}
		}
		_ => {}
	}
}

struct ModuleLoweringData<'a> {
	module_id: &'a str,
	module: &'a crate::frontend::TypedModule,
	module_comptime_consts: &'a HashMap<String, ConstValue>,
	module_runtime_ctfe_consts: &'a HashMap<String, ConstValue>,
	builtin_void_ty: TypeId,
}

fn maybe_collect_function(
	name: &str,
	value: &crate::frontend::TypedAst,
	module_data: &ModuleLoweringData,
	type_ctx: &mut TypeLowerContext,
	ir_types: &mut IrTypeTable,
) -> Result<Option<IrFunction>, IrLowerError> {
	let TypedValue::Fn {
		generics,
		params,
		return_type,
		body,
		..
	} = &value.v
	else {
		return Ok(None);
	};

	if !generics.is_empty() {
		return Ok(None);
	}

	let Some(fn_ty) = module_data.module.values.get(name).copied() else {
		return Ok(None);
	};

	let sig = type_ctx.lower(fn_ty, ir_types);
	let lowerer_module_data = FunctionLowererModuleData {
		module_id: module_data.module_id,
		builtin_void_ty: module_data.builtin_void_ty,
		module_values: &module_data.module.values,
		module_types: &module_data.module.types,
		module_comptime_consts: module_data.module_comptime_consts,
		runtime_ctfe_consts: module_data.module_runtime_ctfe_consts,
	};
	let mut lowerer = FunctionLowerer::new(name, lowerer_module_data, type_ctx, ir_types);
	let entry = lowerer.create_block();
	lowerer.switch_to(entry);
	let mut out_params = Vec::new();
	let sig_param_tys = match lowerer.ir_types.get(sig) {
		IrType::FnSig(fn_sig) => Some(fn_sig.params.clone()),
		_ => None,
	};
	let flat_param_names: Vec<String> = params
		.iter()
		.flat_map(|param| param.names.iter().cloned())
		.collect();

	if let Some(sig_param_tys) = sig_param_tys
		&& !flat_param_names.is_empty()
		&& flat_param_names.len() == sig_param_tys.len()
	{
		for (name, ty) in flat_param_names.into_iter().zip(sig_param_tys.into_iter()) {
			let value = lowerer.ids.fresh_value();
			out_params.push(IrParam {
				name: name.clone(),
				ty,
				value,
			});
			lowerer.bind_local(name, value, ty);
			lowerer.value_types.values.insert(value, ty);
		}
	} else {
		for (idx, param) in params.iter().enumerate() {
			let Some(param_ty) = param.ty else {
				continue;
			};
			let ty = lowerer.type_ctx.lower(param_ty, lowerer.ir_types);
			let value = lowerer.ids.fresh_value();
			out_params.push(IrParam {
				name: param
					.names
					.first()
					.cloned()
					.unwrap_or_else(|| format!("arg{idx}")),
				ty,
				value,
			});
			for param_name in &param.names {
				lowerer.bind_local(param_name.clone(), value, ty);
			}
			lowerer.value_types.values.insert(value, ty);
		}
	}

	let ret_ty = return_type
		.map(|ty| lowerer.type_ctx.lower(ty, lowerer.ir_types))
		.unwrap_or_else(|| {
			lowerer.type_ctx
				.lower(module_data.builtin_void_ty, lowerer.ir_types)
		});
	let _term = lowerer
		.lower_body(body, ret_ty)
		.map_err(|message| IrLowerError {
			module_id: module_data.module_id.to_string(),
			message: format!("{}: {message}", name),
		})?;

	Ok(Some(IrFunction {
		name: name.to_string(),
		linkage: IrLinkage::Private,
		signature: sig,
		params: out_params,
		blocks: lowerer.blocks,
		entry: Some(entry),
		value_types: lowerer.value_types,
		local_names: lowerer.debug_value_names,
		location: Some(value.location.clone()),
	}))
}

struct FunctionLowerer<'a, 'b> {
	module_id: &'a str,
	function_name: &'a str,
	builtin_void_ty: TypeId,
	module_values: &'a HashMap<String, TypeId>,
	module_types: &'a HashMap<String, TypeId>,
	module_comptime_consts: &'a HashMap<String, ConstValue>,
	runtime_ctfe_consts: &'a HashMap<String, ConstValue>,
	ids: FunctionBuildIds,
	type_ctx: &'a mut TypeLowerContext<'b>,
	ir_types: &'a mut IrTypeTable,
	blocks: Vec<IrBlock>,
	current_block: Option<IrBlockId>,
	value_types: IrFunctionTypeMap,
	locals: HashMap<String, (IrValueId, IrTypeId)>,
	debug_value_names: HashMap<IrValueId, String>,
}

struct FunctionLowererModuleData<'a> {
	module_id: &'a str,
	builtin_void_ty: TypeId,
	module_values: &'a HashMap<String, TypeId>,
	module_types: &'a HashMap<String, TypeId>,
	module_comptime_consts: &'a HashMap<String, ConstValue>,
	runtime_ctfe_consts: &'a HashMap<String, ConstValue>,
}

impl<'a, 'b> FunctionLowerer<'a, 'b> {
	fn new(
		function_name: &'a str,
		module_data: FunctionLowererModuleData<'a>,
		type_ctx: &'a mut TypeLowerContext<'b>,
		ir_types: &'a mut IrTypeTable,
	) -> Self {
		Self {
			module_id: module_data.module_id,
			function_name,
			builtin_void_ty: module_data.builtin_void_ty,
			module_values: module_data.module_values,
			module_types: module_data.module_types,
			module_comptime_consts: module_data.module_comptime_consts,
			runtime_ctfe_consts: module_data.runtime_ctfe_consts,
			ids: FunctionBuildIds::default(),
			type_ctx,
			ir_types,
			blocks: Vec::new(),
			current_block: None,
			value_types: IrFunctionTypeMap::new(),
			locals: HashMap::new(),
			debug_value_names: HashMap::new(),
		}
	}

	fn bind_local(&mut self, name: String, value: IrValueId, ty: IrTypeId) {
		self.locals.insert(name.clone(), (value, ty));
		if name != "_" {
			self.debug_value_names.insert(value, name);
		}
	}

	fn create_block(&mut self) -> IrBlockId {
		let id = self.ids.fresh_block();
		self.blocks.push(IrBlock {
			id,
			params: Vec::new(),
			insts: Vec::new(),
			term: None,
		});
		id
	}

	fn switch_to(&mut self, block: IrBlockId) {
		self.current_block = Some(block);
	}

	fn block_position(&self, block: IrBlockId) -> Option<usize> {
		self.blocks.iter().position(|b| b.id == block)
	}

	fn current_block_mut(&mut self) -> Result<&mut IrBlock, String> {
		let current = self
			.current_block
			.ok_or_else(|| "no active block while lowering function".to_string())?;
		let Some(position) = self.block_position(current) else {
			return Err(format!("active block {:?} not found", current));
		};
		Ok(&mut self.blocks[position])
	}

	fn current_is_terminated(&self) -> bool {
		let Some(current) = self.current_block else {
			return true;
		};
		self.block_position(current)
			.and_then(|idx| self.blocks[idx].term.as_ref())
			.is_some()
	}

	fn add_block_param(
		&mut self,
		block: IrBlockId,
		name_hint: Option<String>,
		ty: IrTypeId,
	) -> Result<IrValueId, String> {
		let value = self.ids.fresh_value();
		let Some(position) = self.block_position(block) else {
			return Err(format!("block {:?} not found for block param", block));
		};
		self.blocks[position].params.push(crate::ir::IrBlockParam {
			value,
			ty,
			name_hint: name_hint.clone(),
		});
		self.value_types.values.insert(value, ty);
		if let Some(name_hint) = name_hint
			&& name_hint != "_"
		{
			self.debug_value_names.insert(value, name_hint);
		}
		Ok(value)
	}

	fn patch_edge_args(
		&mut self,
		from: IrBlockId,
		to: IrBlockId,
		args: Vec<IrValueId>,
	) -> Result<(), String> {
		let Some(position) = self.block_position(from) else {
			return Err(format!("source block {:?} not found for edge patch", from));
		};
		let Some(term) = self.blocks[position].term.as_mut() else {
			return Err(format!("source block {:?} has no terminator", from));
		};
		match term {
			IrTerminator::Br(edge) => {
				if edge.to != to {
					return Err(format!(
						"edge patch target mismatch: {:?} -> {:?}",
						from, to
					));
				}
				edge.args = args;
				Ok(())
			}
			IrTerminator::CondBr {
				then_edge,
				else_edge,
				..
			} => {
				if then_edge.to == to {
					then_edge.args = args;
					return Ok(());
				}
				if else_edge.to == to {
					else_edge.args = args;
					return Ok(());
				}
				Err(format!(
					"edge patch target not found in condbr: {:?} -> {:?}",
					from, to
				))
			}
			_ => Err(format!(
				"cannot patch edges for terminator in block {:?}",
				from
			)),
		}
	}

	fn terminate_current(&mut self, term: IrTerminator) -> Result<(), String> {
		let block = self.current_block_mut()?;
		if block.term.is_some() {
			return Err(format!("block {:?} already terminated", block.id));
		}
		block.term = Some(term);
		Ok(())
	}

	fn current_block_id(&self) -> Result<IrBlockId, String> {
		self.current_block
			.ok_or_else(|| "no active block while lowering function".to_string())
	}

	fn lower_body(
		&mut self,
		body: &TypedFnBody,
		ret_ty: IrTypeId,
	) -> Result<IrTerminator, String> {
		match body {
			TypedFnBody::Uninitialized => {
				self.terminate_current(IrTerminator::Unreachable)?;
				Ok(IrTerminator::Unreachable)
			}
			TypedFnBody::Expr(expr) => {
				let value = self.lower_expr(expr.as_ref())?;
				let term = self.finalize_return(
					value,
					ret_ty,
					Some(expr.location.clone()),
				)?;
				self.terminate_current(term.clone())?;
				Ok(term)
			}
			TypedFnBody::Block(block) => {
				let mut last = None;
				self.lower_stmt(block.as_ref(), &mut last)?;
				if self.current_is_terminated() {
					Ok(IrTerminator::Unreachable)
				} else {
					let term = self.finalize_return(
						last,
						ret_ty,
						Some(block.location.clone()),
					)?;
					self.terminate_current(term.clone())?;
					Ok(term)
				}
			}
		}
	}

	fn finalize_return(
		&mut self,
		value: Option<(IrValueId, IrTypeId)>,
		ret_ty: IrTypeId,
		location: Option<crate::frontend::SourceLocation>,
	) -> Result<IrTerminator, String> {
		if matches!(self.ir_types.get(ret_ty), IrType::Void) {
			return Ok(IrTerminator::Ret(None));
		}
		if let Some((value, _)) = value {
			return Ok(IrTerminator::Ret(Some(value)));
		}
		let undef = self.push_value(IrInstKind::Const(IrConst::Undef), ret_ty, location)?;
		Ok(IrTerminator::Ret(Some(undef)))
	}

	fn lower_stmt(
		&mut self,
		node: &crate::frontend::TypedAst,
		last_value: &mut Option<(IrValueId, IrTypeId)>,
	) -> Result<(), String> {
		if self.current_is_terminated() {
			return Ok(());
		}
		match &node.v {
			TypedValue::ExprList { items, .. }
			| TypedValue::ExprListNoScope { items, .. } => {
				for item in items {
					self.lower_stmt(item.as_ref(), last_value)?;
					if self.current_is_terminated() {
						break;
					}
				}
			}
			TypedValue::If {
				cond,
				decl,
				body,
				else_,
			} => {
				let incoming_locals = self.locals.clone();
				if let Some(decl) = decl {
					self.lower_stmt(decl.as_ref(), last_value)?;
				}
				let branch_base_locals = self.locals.clone();
				let Some((cond_val, _)) = self.lower_expr(cond.as_ref())? else {
					return Err("if condition produced no value".to_string());
				};
				let then_bb = self.create_block();
				let else_bb = self.create_block();
				self.terminate_current(IrTerminator::CondBr {
					cond: cond_val,
					then_edge: crate::ir::IrEdge {
						to: then_bb,
						args: Vec::new(),
					},
					else_edge: crate::ir::IrEdge {
						to: else_bb,
						args: Vec::new(),
					},
				})?;

				let mut then_last = None;
				self.switch_to(then_bb);
				self.locals = branch_base_locals.clone();
				self.lower_stmt(body.as_ref(), &mut then_last)?;
				let then_falls = !self.current_is_terminated();
				let then_locals = self.locals.clone();

				let mut else_last = None;
				self.switch_to(else_bb);
				self.locals = branch_base_locals;
				if let Some(else_node) = else_ {
					self.lower_stmt(else_node.as_ref(), &mut else_last)?;
				}
				let else_falls = !self.current_is_terminated();
				let else_locals = self.locals.clone();

				if then_falls || else_falls {
					let merge_bb = self.create_block();
					let mut merge_locals: HashMap<
						String,
						(IrValueId, IrTypeId),
					> = HashMap::new();
					let mut merge_params: Vec<(String, IrValueId, IrValueId)> =
						Vec::new();
					let mut merge_expr_param: Option<(IrValueId, IrValueId)> =
						None;
					let mut merge_expr_value: Option<(IrValueId, IrTypeId)> =
						None;

					if then_falls && else_falls {
						let mut names: Vec<_> = then_locals
							.keys()
							.filter(|name| {
								else_locals.contains_key(*name)
							})
							.cloned()
							.collect();
						names.sort();
						for name in names {
							let (then_val, then_ty) =
								then_locals[&name];
							let (else_val, else_ty) =
								else_locals[&name];
							if then_ty != else_ty {
								continue;
							}
							if then_val == else_val {
								merge_locals.insert(
									name,
									(then_val, then_ty),
								);
							} else {
								let phi = self.add_block_param(
									merge_bb,
									Some(name.clone()),
									then_ty,
								)?;
								merge_locals.insert(
									name.clone(),
									(phi, then_ty),
								);
								merge_params.push((
									name, then_val, else_val,
								));
							}
						}
						match (then_last, else_last) {
							(
								Some((then_value, then_ty)),
								Some((else_value, else_ty)),
							) if then_ty == else_ty => {
								if then_value == else_value {
									merge_expr_value = Some((
										then_value, then_ty,
									));
								} else {
									let phi = self
										.add_block_param(
										merge_bb,
										Some("if.result"
											.to_string(
											)),
										then_ty,
									)?;
									merge_expr_param = Some((
										then_value,
										else_value,
									));
									merge_expr_value = Some((
										phi, then_ty,
									));
								}
							}
							_ => {}
						}
					} else if then_falls {
						merge_locals = then_locals.clone();
						merge_expr_value = then_last;
					} else {
						merge_locals = else_locals.clone();
						merge_expr_value = else_last;
					}

					if then_falls {
						self.switch_to(then_bb);
						self.terminate_current(IrTerminator::Br(
							crate::ir::IrEdge {
								to: merge_bb,
								args: Vec::new(),
							},
						))?;
						if then_falls && else_falls {
							let mut args: Vec<IrValueId> = merge_params
								.iter()
								.map(|(_, then_val, _)| *then_val)
								.collect();
							if let Some((then_value, _)) =
								merge_expr_param
							{
								args.push(then_value);
							}
							self.patch_edge_args(
								then_bb, merge_bb, args,
							)?;
						}
					}
					if else_falls {
						self.switch_to(else_bb);
						self.terminate_current(IrTerminator::Br(
							crate::ir::IrEdge {
								to: merge_bb,
								args: Vec::new(),
							},
						))?;
						if then_falls && else_falls {
							let mut args: Vec<IrValueId> = merge_params
								.iter()
								.map(|(_, _, else_val)| *else_val)
								.collect();
							if let Some((_, else_value)) =
								merge_expr_param
							{
								args.push(else_value);
							}
							self.patch_edge_args(
								else_bb, merge_bb, args,
							)?;
						}
					}
					self.switch_to(merge_bb);
					self.locals = merge_locals;
					*last_value = merge_expr_value;
				} else {
					self.locals = incoming_locals;
					*last_value = None;
				}
			}
			TypedValue::While { cond, decl, body } => {
				let incoming_locals = self.locals.clone();
				let cond_bb = self.create_block();
				let body_bb = self.create_block();
				let end_bb = self.create_block();
				let mut param_names: Vec<String> =
					incoming_locals.keys().cloned().collect();
				param_names.sort();
				let mut cond_param_map: HashMap<String, (IrValueId, IrTypeId)> =
					HashMap::new();
				for name in &param_names {
					let (_incoming, ty) = incoming_locals[name];
					let value = self.add_block_param(
						cond_bb,
						Some(name.clone()),
						ty,
					)?;
					cond_param_map.insert(name.clone(), (value, ty));
				}
				let entry_args: Vec<IrValueId> = param_names
					.iter()
					.map(|name| incoming_locals[name].0)
					.collect();
				self.terminate_current(IrTerminator::Br(crate::ir::IrEdge {
					to: cond_bb,
					args: entry_args,
				}))?;

				self.switch_to(cond_bb);
				self.locals = cond_param_map.clone();
				if let Some(decl) = decl {
					self.lower_stmt(decl.as_ref(), &mut None)?;
				}
				let cond_state = self.locals.clone();
				let Some((cond_val, _)) = self.lower_expr(cond.as_ref())? else {
					return Err("while condition produced no value".to_string());
				};
				self.terminate_current(IrTerminator::CondBr {
					cond: cond_val,
					then_edge: crate::ir::IrEdge {
						to: body_bb,
						args: Vec::new(),
					},
					else_edge: crate::ir::IrEdge {
						to: end_bb,
						args: Vec::new(),
					},
				})?;

				self.switch_to(body_bb);
				self.locals = cond_state.clone();
				self.lower_stmt(body.as_ref(), &mut None)?;
				if !self.current_is_terminated() {
					let back_args: Vec<IrValueId> = param_names
						.iter()
						.map(|name| {
							self.locals
								.get(name)
								.map(|(v, _)| *v)
								.unwrap_or_else(|| {
									incoming_locals[name].0
								})
						})
						.collect();
					self.terminate_current(IrTerminator::Br(
						crate::ir::IrEdge {
							to: cond_bb,
							args: back_args,
						},
					))?;
				}

				self.switch_to(end_bb);
				self.locals = cond_state;
				*last_value = None;
			}
			TypedValue::ForLoop {
				init,
				cond,
				step,
				body,
			} => {
				if let Some(init) = init {
					self.lower_stmt(init.as_ref(), &mut None)?;
				}
				let cond_bb = self.create_block();
				let body_bb = self.create_block();
				let step_bb = self.create_block();
				let end_bb = self.create_block();
				let incoming_locals = self.locals.clone();
				let mut param_names: Vec<String> =
					incoming_locals.keys().cloned().collect();
				param_names.sort();
				let mut cond_param_map: HashMap<String, (IrValueId, IrTypeId)> =
					HashMap::new();
				for name in &param_names {
					let (_incoming, ty) = incoming_locals[name];
					let value = self.add_block_param(
						cond_bb,
						Some(name.clone()),
						ty,
					)?;
					cond_param_map.insert(name.clone(), (value, ty));
				}
				let entry_args: Vec<IrValueId> = param_names
					.iter()
					.map(|name| incoming_locals[name].0)
					.collect();
				self.terminate_current(IrTerminator::Br(crate::ir::IrEdge {
					to: cond_bb,
					args: entry_args,
				}))?;

				self.switch_to(cond_bb);
				self.locals = cond_param_map.clone();
				let cond_state = self.locals.clone();
				if let Some(cond) = cond {
					let Some((cond_val, _)) = self.lower_expr(cond.as_ref())?
					else {
						return Err("for-loop condition produced no value"
							.to_string());
					};
					self.terminate_current(IrTerminator::CondBr {
						cond: cond_val,
						then_edge: crate::ir::IrEdge {
							to: body_bb,
							args: Vec::new(),
						},
						else_edge: crate::ir::IrEdge {
							to: end_bb,
							args: Vec::new(),
						},
					})?;
				} else {
					self.terminate_current(IrTerminator::Br(
						crate::ir::IrEdge {
							to: body_bb,
							args: Vec::new(),
						},
					))?;
				}

				self.switch_to(body_bb);
				self.locals = cond_state.clone();
				self.lower_stmt(body.as_ref(), &mut None)?;
				if !self.current_is_terminated() {
					self.terminate_current(IrTerminator::Br(
						crate::ir::IrEdge {
							to: step_bb,
							args: Vec::new(),
						},
					))?;
				}

				self.switch_to(step_bb);
				if let Some(step) = step {
					self.lower_stmt(step.as_ref(), &mut None)?;
				}
				if !self.current_is_terminated() {
					let back_args: Vec<IrValueId> = param_names
						.iter()
						.map(|name| {
							self.locals
								.get(name)
								.map(|(v, _)| *v)
								.unwrap_or_else(|| {
									incoming_locals[name].0
								})
						})
						.collect();
					self.terminate_current(IrTerminator::Br(
						crate::ir::IrEdge {
							to: cond_bb,
							args: back_args,
						},
					))?;
				}

				self.switch_to(end_bb);
				self.locals = cond_state;
				*last_value = None;
			}
			TypedValue::Match { .. } => {
				*last_value = self.lower_expr(node)?;
			}
			TypedValue::Declaration { name, value, .. }
			| TypedValue::DeclarationComptime(name, value) => {
				if let Some(const_value) = self
					.runtime_ctfe_consts
					.get(&source_location_key(&value.location))
				{
					let ty = self.node_ir_type(value.as_ref());
					if let Some(ir_const) =
						self.const_value_to_ir_const(const_value, ty)
					{
						let value_id = self.push_value(
							IrInstKind::Const(ir_const),
							ty,
							Some(value.location.clone()),
						)?;
						self.bind_local(name.clone(), value_id, ty);
						return Ok(());
					}
				}
				if let Some((val, ty)) = self.lower_expr(value.as_ref())? {
					self.bind_local(name.clone(), val, ty);
				}
			}
			TypedValue::DeclarationMulti {
				names,
				values: Some(values),
				..
			} => {
				for (idx, name) in names.iter().enumerate() {
					let Some(value) =
						values.get(idx).or_else(|| values.first())
					else {
						continue;
					};
					if let Some(const_value) = self
						.runtime_ctfe_consts
						.get(&source_location_key(&value.location))
					{
						let ty = self.node_ir_type(value.as_ref());
						if let Some(ir_const) = self
							.const_value_to_ir_const(const_value, ty)
						{
							let value_id = self.push_value(
								IrInstKind::Const(ir_const),
								ty,
								Some(value.location.clone()),
							)?;
							self.bind_local(name.clone(), value_id, ty);
							continue;
						}
					}
					if let Some((val, ty)) = self.lower_expr(value.as_ref())? {
						self.bind_local(name.clone(), val, ty);
					}
				}
			}
			TypedValue::Set(name, value) => {
				if name == "_" {
					let _ = self.lower_expr(value.as_ref())?;
				} else if let Some((val, ty)) = self.lower_expr(value.as_ref())? {
					self.bind_local(name.clone(), val, ty);
				}
			}
			TypedValue::SetMulti { names, values } => {
				for (idx, name) in names.iter().enumerate() {
					let Some(value) =
						values.get(idx).or_else(|| values.first())
					else {
						continue;
					};
					if name == "_" {
						let _ = self.lower_expr(value.as_ref())?;
					} else if let Some((val, ty)) =
						self.lower_expr(value.as_ref())?
					{
						self.bind_local(name.clone(), val, ty);
					}
				}
			}
			TypedValue::Return(value) => match value {
				Some(expr) => {
					let Some((val, _)) = self.lower_expr(expr.as_ref())? else {
						return Err("return expression produced no value"
							.to_string());
					};
					self.terminate_current(IrTerminator::Ret(Some(val)))?;
				}
				None => {
					self.terminate_current(IrTerminator::Ret(None))?;
				}
			},
			TypedValue::Pub(inner) => self.lower_stmt(inner.as_ref(), last_value)?,
			_ => {
				*last_value = self.lower_expr(node)?;
			}
		}
		Ok(())
	}

	fn lower_expr(
		&mut self,
		node: &crate::frontend::TypedAst,
	) -> Result<Option<(IrValueId, IrTypeId)>, String> {
		match &node.v {
			TypedValue::TypedInitializerList { ty, items } => {
				let ir_ty = self.type_ctx.lower(*ty, self.ir_types);
				let canonical = self.canonical_type(*ty);
				match self.type_ctx.semantics.arena.get(canonical) {
					ResolvedType::Struct { fields, .. } => {
						let mut lowered_fields: Vec<
							Option<(IrValueId, IrTypeId)>,
						> = vec![None; fields.len()];
						let mut positional = 0usize;
						for item in items {
							match item {
								crate::frontend::TypedInitializerItem::Positional(value) => {
									if positional >= fields.len() {
										continue;
									}
									lowered_fields[positional] =
										self.lower_expr(value.as_ref())?;
									positional += 1;
								}
								crate::frontend::TypedInitializerItem::Named {
									name,
									value,
								} => {
									if let Some((idx, _)) = fields
										.iter()
										.enumerate()
										.find(|(_, f)| f.name == *name)
									{
										lowered_fields[idx] =
											self.lower_expr(value.as_ref())?;
									}
								}
							}
						}
						let mut field_values =
							Vec::with_capacity(fields.len());
						for (idx, field) in fields.iter().enumerate() {
							if let Some((value, _)) =
								lowered_fields[idx]
							{
								field_values.push(value);
							} else {
								let field_ty = self.type_ctx.lower(
									field.ty,
									self.ir_types,
								);
								let undef = self.push_value(
									IrInstKind::Const(
										IrConst::Undef,
									),
									field_ty,
									Some(node.location.clone()),
								)?;
								field_values.push(undef);
							}
						}
						let value = self.push_value(
							IrInstKind::MakeStruct {
								ty: ir_ty,
								fields: field_values,
							},
							ir_ty,
							Some(node.location.clone()),
						)?;
						Ok(Some((value, ir_ty)))
					}
					ResolvedType::Union { variants, .. } => {
						let mut selected: Option<(u32, IrValueId)> = None;
						for item in items {
							let value_node = match item {
								crate::frontend::TypedInitializerItem::Positional(v) => {
									v.as_ref()
								}
								crate::frontend::TypedInitializerItem::Named {
									value,
									..
								} => value.as_ref(),
							};
							let Some((value, value_ir_ty)) =
								self.lower_expr(value_node)?
							else {
								continue;
							};
							let value_sem_ty = value_node
								.ty
								.unwrap_or(self.builtin_void_ty);
							let value_sem_canon =
								self.canonical_type(value_sem_ty);
							if let Some(tag_idx) =
								variants.iter()
									.enumerate()
									.find_map(
										|(idx, variant)| {
											let variant_canon = self.canonical_type(*variant);
											(variant_canon == value_sem_canon).then_some(idx as u32)
										},
									) {
								let _ = value_ir_ty;
								selected = Some((tag_idx, value));
								break;
							}
						}
						let Some((variant, payload)) = selected else {
							return Err(
								"union initializer does not match any variant".to_string(),
							);
						};
						let value = self.push_value(
							IrInstKind::MakeUnion {
								ty: ir_ty,
								variant,
								payload,
							},
							ir_ty,
							Some(node.location.clone()),
						)?;
						Ok(Some((value, ir_ty)))
					}
					_ => Err("unsupported typed initializer target".to_string()),
				}
			}
			TypedValue::InitializerList(_) => {
				Err("untyped initializer is not supported in IR lowering"
					.to_string())
			}
			TypedValue::ExprList { items, .. } => {
				let saved = self.locals.clone();
				let mut last = None;
				for item in items {
					self.lower_stmt(item.as_ref(), &mut last)?;
					if self.current_is_terminated() {
						break;
					}
				}
				self.locals = saved;
				Ok(last)
			}
			TypedValue::ExprListNoScope { items, .. } => {
				let mut last = None;
				for item in items {
					self.lower_stmt(item.as_ref(), &mut last)?;
					if self.current_is_terminated() {
						break;
					}
				}
				Ok(last)
			}
			TypedValue::Integer(value) => {
				let ty = self.node_ir_type(node);
				let const_kind = match self.ir_types.get(ty) {
					IrType::Int { signed: true, .. } => {
						IrConst::Int(*value as i128)
					}
					_ => IrConst::UInt((*value).into()),
				};
				let value = self.push_value(
					IrInstKind::Const(const_kind),
					ty,
					Some(node.location.clone()),
				)?;
				Ok(Some((value, ty)))
			}
			TypedValue::Float(value) => {
				let ty = self.node_ir_type(node);
				let const_kind = match self.ir_types.get(ty) {
					IrType::Float32 => IrConst::Float32(*value as f32),
					_ => IrConst::Float64(*value),
				};
				let value = self.push_value(
					IrInstKind::Const(const_kind),
					ty,
					Some(node.location.clone()),
				)?;
				Ok(Some((value, ty)))
			}
			TypedValue::Char(value) => {
				let ty = self.node_ir_type(node);
				let value = self.push_value(
					IrInstKind::Const(IrConst::Int(i128::from(u32::from(
						*value,
					)))),
					ty,
					Some(node.location.clone()),
				)?;
				Ok(Some((value, ty)))
			}
			TypedValue::Id(name) => {
				if name == "true" || name == "false" {
					let ty = self.type_ctx.lower(
						self.type_ctx.semantics.builtins.bool_id,
						self.ir_types,
					);
					let value = self.push_value(
						IrInstKind::Const(IrConst::Bool(name == "true")),
						ty,
						Some(node.location.clone()),
					)?;
					return Ok(Some((value, ty)));
				}
				let ty = self.node_ir_type(node);
				if let Some(local) = self.locals.get(name).copied() {
					return Ok(Some(local));
				}
				if let Some(const_value) = self.module_comptime_consts.get(name)
					&& let Some(ir_const) =
						self.const_value_to_ir_const(const_value, ty)
				{
					let value = self.push_value(
						IrInstKind::Const(ir_const),
						ty,
						Some(node.location.clone()),
					)?;
					self.bind_local(name.clone(), value, ty);
					return Ok(Some((value, ty)));
				}
				Ok(None)
			}
			TypedValue::UnaryMinus(inner) => {
				let Some((inner_value, inner_ty)) =
					self.lower_expr(inner.as_ref())?
				else {
					return Err(
						"unary minus operand produced no value".to_string()
					);
				};
				let zero = self.push_value(
					IrInstKind::Const(IrConst::Int(0)),
					inner_ty,
					Some(node.location.clone()),
				)?;
				let out = self.push_value(
					IrInstKind::BinOp {
						op: IrBinOp::Sub,
						lhs: zero,
						rhs: inner_value,
					},
					inner_ty,
					Some(node.location.clone()),
				)?;
				Ok(Some((out, inner_ty)))
			}
			TypedValue::Deref(inner) => {
				let Some((ptr, ptr_ty)) = self.lower_expr(inner.as_ref())? else {
					return Err("deref operand produced no value".to_string());
				};
				if !matches!(self.ir_types.get(ptr_ty), IrType::Ptr { .. }) {
					return Err(
						"deref operand is not a pointer/reference value"
							.to_string(),
					);
				}
				let out_ty = self.node_ir_type(node);
				let out = self.push_value(
					IrInstKind::Load { ptr, ty: out_ty },
					out_ty,
					Some(node.location.clone()),
				)?;
				Ok(Some((out, out_ty)))
			}
			TypedValue::BinExpr {
				op,
				lhs,
				rhs,
				has_eq,
			} => {
				if matches!(op, Operator::Set) && !*has_eq {
					match &lhs.v {
						TypedValue::Id(name) => {
							let Some((rhs_val, rhs_ty)) =
								self.lower_expr(rhs.as_ref())?
							else {
								return Err("assignment rhs produced no value".to_string());
							};
							if name != "_" {
								self.bind_local(
									name.clone(),
									rhs_val,
									rhs_ty,
								);
							}
							return Ok(None);
						}
						TypedValue::Deref(ptr_expr) => {
							let Some((ptr, ptr_ty)) =
								self.lower_expr(ptr_expr.as_ref())?
							else {
								return Err(
									"assignment deref target produced no value"
										.to_string(),
								);
							};
							if !matches!(
								self.ir_types.get(ptr_ty),
								IrType::Ptr { .. }
							) {
								return Err(
									"assignment deref target is not a pointer/reference value"
										.to_string(),
								);
							}
							let Some((rhs_val, _rhs_ty)) =
								self.lower_expr(rhs.as_ref())?
							else {
								return Err("assignment rhs produced no value".to_string());
							};
							self.push_effect(
								IrInstKind::Store {
									ptr,
									value: rhs_val,
								},
								Some(rhs.location.clone()),
							);
							return Ok(None);
						}
						_ => {
							return Err(format!(
								"unsupported assignment target in IR lowering for {}::{}: {:?}",
								self.module_id,
								self.function_name,
								lhs.v
							));
						}
					}
				}
				if matches!(op, Operator::Dot) && !*has_eq {
					let field_name = match &rhs.v {
						TypedValue::Id(name) | TypedValue::DotId(name) => name,
						_ => {
							return Err(
								"dot rhs must be identifier in current IR lowering"
									.to_string(),
							)
						}
					};
					let lhs_sem_ty = lhs.ty.unwrap_or(self.builtin_void_ty);
					let canonical = self.canonical_type(lhs_sem_ty);
					if let ResolvedType::Struct { fields, .. } =
						self.type_ctx.semantics.arena.get(canonical)
					{
						let Some((field_idx, field_info)) = fields
							.iter()
							.enumerate()
							.find(|(_, f)| f.name == *field_name)
						else {
							return Err(format!(
								"unknown field `{field_name}` on struct access"
							));
						};
						let Some((lhs_value, _)) =
							self.lower_expr(lhs.as_ref())?
						else {
							return Err("dot lhs produced no value"
								.to_string());
						};
						let field_ty = self
							.type_ctx
							.lower(field_info.ty, self.ir_types);
						let out = self.push_value(
							IrInstKind::ExtractField {
								agg: lhs_value,
								field: field_idx,
							},
							field_ty,
							Some(node.location.clone()),
						)?;
						return Ok(Some((out, field_ty)));
					}
				}
				let Some((lhs_val, lhs_ty)) = self.lower_expr(lhs.as_ref())? else {
					return Err("binary lhs produced no value".to_string());
				};
				let Some((rhs_val, _rhs_ty)) = self.lower_expr(rhs.as_ref())?
				else {
					return Err("binary rhs produced no value".to_string());
				};
				if let Some(cmp_op) = map_cmp(op, *has_eq) {
					let bool_ty = self.type_ctx.lower(
						self.type_ctx.semantics.builtins.bool_id,
						self.ir_types,
					);
					let out = self.push_value(
						IrInstKind::Cmp {
							op: cmp_op,
							lhs: lhs_val,
							rhs: rhs_val,
						},
						bool_ty,
						Some(node.location.clone()),
					)?;
					return Ok(Some((out, bool_ty)));
				}
				let Some(bin_op) = map_bin(op) else {
					return Err(format!(
						"unsupported binary operator in IR lowering: {op:?}"
					));
				};
				let out_ty = self.node_ir_type(node);
				let out = self.push_value(
					IrInstKind::BinOp {
						op: bin_op,
						lhs: lhs_val,
						rhs: rhs_val,
					},
					out_ty,
					Some(node.location.clone()),
				)?;
				let _ = lhs_ty;
				Ok(Some((out, out_ty)))
			}
			TypedValue::Cast { ty, value } | TypedValue::Transmute { ty, value } => {
				let Some((src, _src_ty)) = self.lower_expr(value.as_ref())? else {
					return Err("cast operand produced no value".to_string());
				};
				let semantic_target = ty
					.or(node.ty)
					.unwrap_or(self.type_ctx.semantics.builtins.unknown_id);
				let canonical_target = self.canonical_type(semantic_target);
				let target = ty
					.map(|t| self.type_ctx.lower(t, self.ir_types))
					.unwrap_or_else(|| self.node_ir_type(node));
				if matches!(
					self.type_ctx.semantics.arena.get(canonical_target),
					ResolvedType::Interface { .. }
				) {
					let src_ty = self.node_ir_type(value.as_ref());
					let ptr_ty = self.ir_types.add(IrType::Ptr { to: src_ty });
					let stack_slot = self.push_value(
						IrInstKind::StackSlot { ty: src_ty },
						ptr_ty,
						Some(node.location.clone()),
					)?;
					self.push_effect(
						IrInstKind::Store {
							ptr: stack_slot,
							value: src,
						},
						Some(node.location.clone()),
					);
					let src_sem = value.ty.unwrap_or(self.builtin_void_ty);
					let vtable = format!(
						"vtbl::{}::{}",
						canonical_target,
						self.canonical_type(src_sem)
					);
					let packed = self.push_value(
						IrInstKind::PackInterface {
							data_ptr: stack_slot,
							vtable_global: vtable,
						},
						target,
						Some(node.location.clone()),
					)?;
					return Ok(Some((packed, target)));
				}
				let out = self.push_value(
					IrInstKind::Cast {
						kind: IrCastKind::Bitcast,
						value: src,
						to: target,
					},
					target,
					Some(node.location.clone()),
				)?;
				Ok(Some((out, target)))
			}
			TypedValue::Call { callee, args } => {
				if let TypedValue::Id(callee_name) | TypedValue::DotId(callee_name) =
					&callee.v && let Some(value) =
					self.lower_type_constructor_call(callee_name, args, node)?
				{
					return Ok(Some(value));
				}

				if let TypedValue::BinExpr {
					op: Operator::Dot,
					lhs,
					rhs,
					has_eq: false,
				} = &callee.v
				{
					let method_name = match &rhs.v {
						TypedValue::Id(name) | TypedValue::DotId(name) => {
							name
						}
						_ => return Err(
							"method call dot rhs must be identifier"
								.to_string(),
						),
					};
					let lhs_sem_ty = lhs.ty.unwrap_or(self.builtin_void_ty);
					let canonical_lhs = self.canonical_type(lhs_sem_ty);
					if matches!(
						self.type_ctx.semantics.arena.get(canonical_lhs),
						ResolvedType::Interface { .. }
					) {
						let methods = self
							.interface_methods_sorted(canonical_lhs)?;
						let Some((method_index, (_name, method_sem_ty))) =
							methods.iter().enumerate().find(
								|(_, (name, _))| {
									name == method_name
								},
							)
						else {
							return Err(format!(
								"interface method `{method_name}` not found"
							));
						};
						let Some((iface_value, _)) =
							self.lower_expr(lhs.as_ref())?
						else {
							return Err("interface call lhs produced no value".to_string());
						};
						let method_sig = self
							.type_ctx
							.lower(*method_sem_ty, self.ir_types);
						let fn_ptr_ty = self
							.ir_types
							.add(IrType::Ptr { to: method_sig });
						let fn_ptr = self.push_value(
							IrInstKind::InterfaceMethodPtr {
								iface: iface_value,
								method_index: method_index as u32,
								sig: method_sig,
							},
							fn_ptr_ty,
							Some(node.location.clone()),
						)?;
						let int_ty = self.type_ctx.lower(
							self.type_ctx.semantics.builtins.int_id,
							self.ir_types,
						);
						let data_ptr_ty = self
							.ir_types
							.add(IrType::Ptr { to: int_ty });
						let data_ptr = self.push_value(
							IrInstKind::ExtractField {
								agg: iface_value,
								field: 0,
							},
							data_ptr_ty,
							Some(node.location.clone()),
						)?;
						let mut call_args = vec![data_ptr];
						for arg in args {
							let Some((arg_value, _)) =
								self.lower_expr(arg.as_ref())?
							else {
								return Err(
									"interface call argument produced no value".to_string(),
								);
							};
							call_args.push(arg_value);
						}
						let out_ty = self.node_ir_type(node);
						if matches!(self.ir_types.get(out_ty), IrType::Void)
						{
							self.push_effect(
								IrInstKind::CallIndirect {
									callee: fn_ptr,
									sig: method_sig,
									args: call_args,
								},
								Some(node.location.clone()),
							);
							return Ok(None);
						}
						let out = self.push_value(
							IrInstKind::CallIndirect {
								callee: fn_ptr,
								sig: method_sig,
								args: call_args,
							},
							out_ty,
							Some(node.location.clone()),
						)?;
						return Ok(Some((out, out_ty)));
					}

					let lhs_lowered = self.lower_expr(lhs.as_ref())?;
					if lhs_lowered.is_none()
						&& matches!(
							lhs.v,
							TypedValue::Id(_) | TypedValue::DotId(_)
						) {
						let mut lowered_args =
							Vec::with_capacity(args.len());
						for arg in args {
							let Some((arg_value, _)) =
								self.lower_expr(arg.as_ref())?
							else {
								return Err(
									"module function call argument produced no value".to_string(),
								);
							};
							lowered_args.push(arg_value);
						}
						let sig_ty = self.resolve_direct_call_signature(
							callee.as_ref(),
							node,
						)?;
						let out_ty = self.node_ir_type(node);
						if matches!(self.ir_types.get(out_ty), IrType::Void)
						{
							self.push_effect(
								IrInstKind::CallDirect {
									callee: method_name.clone(),
									sig: sig_ty,
									args: lowered_args,
								},
								Some(node.location.clone()),
							);
							return Ok(None);
						}
						let out = self.push_value(
							IrInstKind::CallDirect {
								callee: method_name.clone(),
								sig: sig_ty,
								args: lowered_args,
							},
							out_ty,
							Some(node.location.clone()),
						)?;
						return Ok(Some((out, out_ty)));
					}

					let Some((lhs_value, lhs_ir_ty)) = lhs_lowered else {
						return Err("method call lhs produced no value"
							.to_string());
					};
					let method_sem_ty = self
						.resolve_method_semantic_type(
							lhs_sem_ty,
							method_name,
							callee.ty,
						)
						.ok_or_else(|| {
							format!(
								"unable to resolve method type for `{method_name}`"
							)
						})?;
					let method_sig = self
						.resolve_fn_sig_type_from_semantic(method_sem_ty)
						.ok_or_else(|| {
							format!(
								"unable to resolve method signature for `{method_name}`"
							)
						})?;

					let mut call_args = Vec::with_capacity(args.len() + 1);
					let self_arg = match self
						.type_ctx
						.semantics
						.arena
						.get(method_sem_ty)
					{
						ResolvedType::Fn { params, .. } => {
							if let Some(first_param) =
								params.first().copied()
							{
								match self
									.type_ctx
									.semantics
									.arena
									.get(self.canonical_type(first_param))
								{
									ResolvedType::Reference { .. }
									| ResolvedType::Pointer { .. } => {
										let ptr_ty = self
											.ir_types
											.add(IrType::Ptr { to: lhs_ir_ty });
										let slot = self.push_value(
											IrInstKind::StackSlot { ty: lhs_ir_ty },
											ptr_ty,
											Some(node.location.clone()),
										)?;
										self.push_effect(
											IrInstKind::Store {
												ptr: slot,
												value: lhs_value,
											},
											Some(node.location.clone()),
										);
										slot
									}
									_ => lhs_value,
								}
							} else {
								lhs_value
							}
						}
						_ => lhs_value,
					};
					call_args.push(self_arg);
					for arg in args {
						let Some((arg_value, _)) =
							self.lower_expr(arg.as_ref())?
						else {
							return Err(
								"method call argument produced no value".to_string(),
							);
						};
						call_args.push(arg_value);
					}

					let out_ty = self.node_ir_type(node);
					if matches!(self.ir_types.get(out_ty), IrType::Void) {
						self.push_effect(
							IrInstKind::CallDirect {
								callee: method_name.clone(),
								sig: method_sig,
								args: call_args,
							},
							Some(node.location.clone()),
						);
						return Ok(None);
					}
					let out = self.push_value(
						IrInstKind::CallDirect {
							callee: method_name.clone(),
							sig: method_sig,
							args: call_args,
						},
						out_ty,
						Some(node.location.clone()),
					)?;
					return Ok(Some((out, out_ty)));
				}
				let callee_name = match &callee.v {
					TypedValue::Id(name) => name.clone(),
					_ => {
						return Err(
							"only direct identifier calls are supported by current IR lowering"
								.to_string(),
						)
					}
				};
				let mut lowered_args = Vec::with_capacity(args.len());
				for arg in args {
					let Some((arg_val, _arg_ty)) =
						self.lower_expr(arg.as_ref())?
					else {
						return Err("call argument produced no value"
							.to_string());
					};
					lowered_args.push(arg_val);
				}
				let sig_ty =
					self.resolve_direct_call_signature(callee.as_ref(), node)?;
				let out_ty = self.node_ir_type(node);
				if matches!(self.ir_types.get(out_ty), IrType::Void) {
					self.push_effect(
						IrInstKind::CallDirect {
							callee: callee_name,
							sig: sig_ty,
							args: lowered_args,
						},
						Some(node.location.clone()),
					);
					return Ok(None);
				}
				let out = self.push_value(
					IrInstKind::CallDirect {
						callee: callee_name,
						sig: sig_ty,
						args: lowered_args,
					},
					out_ty,
					Some(node.location.clone()),
				)?;
				Ok(Some((out, out_ty)))
			}
			TypedValue::Match {
				binder,
				scrutinee,
				cases,
			} => self.lower_match_expr(node, binder.as_ref(), scrutinee.as_ref(), cases),
			TypedValue::Pub(inner) => self.lower_expr(inner.as_ref()),
			_ => Err(format!(
				"unsupported expression in IR lowering for {}::{}: {:?}",
				self.module_id, self.function_name, node.v
			)),
		}
	}

	fn lower_match_expr(
		&mut self,
		node: &crate::frontend::TypedAst,
		binder: Option<&crate::frontend::TypedMatchBinder>,
		scrutinee: &crate::frontend::TypedAst,
		cases: &[crate::frontend::TypedMatchCase],
	) -> Result<Option<(IrValueId, IrTypeId)>, String> {
		let Some((scrutinee_val, _)) = self.lower_expr(scrutinee)? else {
			return Err("match scrutinee produced no value".to_string());
		};
		let scrutinee_ty = scrutinee.ty.unwrap_or(self.builtin_void_ty);
		let match_ty = self.node_ir_type(node);
		let bool_ty = self
			.type_ctx
			.lower(self.type_ctx.semantics.builtins.bool_id, self.ir_types);
		let tag_ty = self
			.type_ctx
			.lower(self.type_ctx.semantics.builtins.int_id, self.ir_types);

		let merge_bb = self.create_block();
		let mut merge_value = None;
		if !matches!(self.ir_types.get(match_ty), IrType::Void) {
			merge_value = Some(self.add_block_param(
				merge_bb,
				Some("match.result".to_string()),
				match_ty,
			)?);
		}

		let mut test_bb = self.current_block_id()?;
		let mut default_seen = false;
		for (idx, case) in cases.iter().enumerate() {
			let case_bb = self.create_block();
			let next_bb = self.create_block();

			self.switch_to(test_bb);
			match &case.pattern {
				crate::frontend::TypedMatchCasePattern::Default => {
					default_seen = true;
					self.terminate_current(IrTerminator::Br(
						crate::ir::IrEdge {
							to: case_bb,
							args: Vec::new(),
						},
					))?;
				}
				crate::frontend::TypedMatchCasePattern::Exprs(exprs) => {
					let mut cond_value = None;
					for expr in exprs {
						let Some((pattern_value, _)) =
							self.lower_expr(expr.as_ref())?
						else {
							return Err(
								"match expression pattern produced no value".to_string(),
							);
						};
						let eq = self.push_value(
							IrInstKind::Cmp {
								op: IrCmpOp::Eq,
								lhs: scrutinee_val,
								rhs: pattern_value,
							},
							bool_ty,
							Some(expr.location.clone()),
						)?;
						cond_value = Some(if let Some(acc) = cond_value {
							self.push_value(
								IrInstKind::BinOp {
									op: IrBinOp::Or,
									lhs: acc,
									rhs: eq,
								},
								bool_ty,
								Some(expr.location.clone()),
							)?
						} else {
							eq
						});
					}
					let Some(cond_value) = cond_value else {
						return Err(
							"match expression pattern list is empty"
								.to_string(),
						);
					};
					self.terminate_current(IrTerminator::CondBr {
						cond: cond_value,
						then_edge: crate::ir::IrEdge {
							to: case_bb,
							args: Vec::new(),
						},
						else_edge: crate::ir::IrEdge {
							to: next_bb,
							args: Vec::new(),
						},
					})?;
				}
				crate::frontend::TypedMatchCasePattern::Type(case_ty) => {
					let Some(variant) =
						self.union_variant_index(scrutinee_ty, *case_ty)
					else {
						return Err(
							"match type pattern does not map to union variant".to_string(),
						);
					};
					let tag = self.push_value(
						IrInstKind::GetTag {
							tagged: scrutinee_val,
						},
						tag_ty,
						Some(node.location.clone()),
					)?;
					let expected = self.push_value(
						IrInstKind::Const(IrConst::UInt(u128::from(
							variant,
						))),
						tag_ty,
						Some(node.location.clone()),
					)?;
					let cond = self.push_value(
						IrInstKind::Cmp {
							op: IrCmpOp::Eq,
							lhs: tag,
							rhs: expected,
						},
						bool_ty,
						Some(node.location.clone()),
					)?;
					self.terminate_current(IrTerminator::CondBr {
						cond,
						then_edge: crate::ir::IrEdge {
							to: case_bb,
							args: Vec::new(),
						},
						else_edge: crate::ir::IrEdge {
							to: next_bb,
							args: Vec::new(),
						},
					})?;
				}
			}

			let previous_locals = self.locals.clone();
			self.switch_to(case_bb);
			if let Some(binder) = binder {
				let binder_ty = if binder.by_ref {
					self.type_ctx.lower(
						scrutinee.ty.unwrap_or(scrutinee_ty),
						self.ir_types,
					)
				} else {
					self.node_ir_type(scrutinee)
				};
				self.locals
					.insert(binder.name.clone(), (scrutinee_val, binder_ty));
			}

			let mut guard_ok = true;
			if let Some(guard) = &case.guard {
				let Some((guard_value, _)) = self.lower_expr(guard.as_ref())?
				else {
					return Err("match guard produced no value".to_string());
				};
				if idx + 1 >= cases.len() {
					return Err(
						"guarded final match case must have fallback case"
							.to_string(),
					);
				}
				let post_guard_bb = self.create_block();
				let fail_guard_bb = next_bb;
				self.terminate_current(IrTerminator::CondBr {
					cond: guard_value,
					then_edge: crate::ir::IrEdge {
						to: post_guard_bb,
						args: Vec::new(),
					},
					else_edge: crate::ir::IrEdge {
						to: fail_guard_bb,
						args: Vec::new(),
					},
				})?;
				self.switch_to(post_guard_bb);
				guard_ok = false;
			}

			let body_value = self.lower_expr(case.body.as_ref())?;
			if !self.current_is_terminated() {
				let args =
					if let Some(result) = merge_value {
						let Some((value, _)) = body_value else {
							return Err("non-void match case produced no value".to_string());
						};
						let _ = result;
						vec![value]
					} else {
						Vec::new()
					};
				self.terminate_current(IrTerminator::Br(crate::ir::IrEdge {
					to: merge_bb,
					args,
				}))?;
			}

			self.locals = previous_locals;
			if !default_seen {
				test_bb = next_bb;
			}
			let _ = guard_ok;
		}

		if !default_seen {
			self.switch_to(test_bb);
			if !self.current_is_terminated() {
				let fail = self.create_block();
				self.terminate_current(IrTerminator::Br(crate::ir::IrEdge {
					to: fail,
					args: Vec::new(),
				}))?;
				self.switch_to(fail);
				self.terminate_current(IrTerminator::Unreachable)?;
			}
		}

		self.switch_to(merge_bb);
		if let Some(result) = merge_value {
			Ok(Some((result, match_ty)))
		} else {
			Ok(None)
		}
	}

	fn union_variant_index(&self, mut union_ty: TypeId, variant_ty: TypeId) -> Option<u32> {
		loop {
			match self.type_ctx.semantics.arena.get(union_ty) {
				ResolvedType::Alias { underlying, .. }
				| ResolvedType::Reference { underlying, .. }
				| ResolvedType::Pointer { underlying } => {
					union_ty = *underlying;
				}
				ResolvedType::GenericInstance { base, .. } => {
					union_ty = *base;
				}
				ResolvedType::Union { variants, .. } => {
					for (idx, candidate) in variants.iter().enumerate() {
						if self.type_ctx
							.semantics
							.type_matches(*candidate, variant_ty) || self
							.type_ctx
							.semantics
							.type_matches(variant_ty, *candidate)
						{
							return Some(idx as u32);
						}
					}
					return None;
				}
				_ => return None,
			}
		}
	}

	fn node_ir_type(&mut self, node: &crate::frontend::TypedAst) -> IrTypeId {
		let ty = node.ty.unwrap_or(self.builtin_void_ty);
		self.type_ctx.lower(ty, self.ir_types)
	}

	fn canonical_type(&self, mut ty: TypeId) -> TypeId {
		loop {
			match self.type_ctx.semantics.arena.get(ty) {
				ResolvedType::Alias { underlying, .. }
				| ResolvedType::Reference { underlying, .. }
				| ResolvedType::Pointer { underlying } => {
					ty = *underlying;
				}
				ResolvedType::GenericInstance { base, .. } => {
					ty = *base;
				}
				_ => return ty,
			}
		}
	}

	fn const_value_to_ir_const(&self, value: &ConstValue, ty: IrTypeId) -> Option<IrConst> {
		match value {
			ConstValue::Void => Some(IrConst::Undef),
			ConstValue::Bool(v) => Some(IrConst::Bool(*v)),
			ConstValue::Integer(v) => match self.ir_types.get(ty) {
				IrType::Int { signed: true, .. } => Some(IrConst::Int(*v)),
				IrType::Int { signed: false, .. } => {
					Some(IrConst::UInt(*v as u128))
				}
				_ => Some(IrConst::Int(*v)),
			},
			ConstValue::Float(v) => match self.ir_types.get(ty) {
				IrType::Float32 => Some(IrConst::Float32(*v as f32)),
				_ => Some(IrConst::Float64(*v)),
			},
			ConstValue::Char(v) => Some(IrConst::Int(i128::from(u32::from(*v)))),
			ConstValue::String(_)
			| ConstValue::Array(_)
			| ConstValue::Record(_)
			| ConstValue::Slice(_)
			| ConstValue::Ref(_) => None,
		}
	}

	fn interface_methods_sorted(
		&self,
		interface_ty: TypeId,
	) -> Result<Vec<(String, TypeId)>, String> {
		let ResolvedType::Interface { methods, .. } =
			self.type_ctx.semantics.arena.get(interface_ty)
		else {
			return Err("expected interface type for method lookup".to_string());
		};
		let mut ordered: Vec<(String, TypeId)> = methods
			.iter()
			.map(|(name, info)| (name.clone(), info.ty))
			.collect();
		ordered.sort_by(|a, b| a.0.cmp(&b.0));
		Ok(ordered)
	}

	fn resolve_fn_sig_type_from_semantic(&mut self, sem_ty: TypeId) -> Option<IrTypeId> {
		let canonical = self.canonical_type(sem_ty);
		if matches!(
			self.type_ctx.semantics.arena.get(canonical),
			ResolvedType::Fn { .. }
		) {
			Some(self.type_ctx.lower(canonical, self.ir_types))
		} else {
			None
		}
	}

	fn resolve_direct_call_signature(
		&mut self,
		callee: &crate::frontend::TypedAst,
		node: &crate::frontend::TypedAst,
	) -> Result<IrTypeId, String> {
		if let Some(callee_ty) = callee.ty
			&& let Some(sig) = self.resolve_fn_sig_type_from_semantic(callee_ty)
		{
			return Ok(sig);
		}
		if let TypedValue::Id(name) | TypedValue::DotId(name) = &callee.v
			&& let Some(sem_ty) = self.module_values.get(name).copied()
			&& let Some(sig) = self.resolve_fn_sig_type_from_semantic(sem_ty)
		{
			return Ok(sig);
		}
		Err(format!(
			"unable to resolve function signature for direct call at {}::{} ({:?})",
			self.module_id, self.function_name, node.location
		))
	}

	fn lookup_method_type_on_value_type(
		&self,
		ty: TypeId,
		method_name: &str,
	) -> Option<TypeId> {
		match self.type_ctx.semantics.arena.get(ty) {
			ResolvedType::Struct {
				methods, extends, ..
			} => {
				if let Some(method) = methods.get(method_name) {
					return Some(method.ty);
				}
				for base in extends {
					if let Some(found) = self.lookup_method_type_on_value_type(
						*base,
						method_name,
					) {
						return Some(found);
					}
				}
				None
			}
			ResolvedType::Union { methods, .. }
			| ResolvedType::Interface { methods, .. } => methods.get(method_name).map(|method| method.ty),
			ResolvedType::Alias { underlying, .. }
			| ResolvedType::Newtype { underlying, .. }
			| ResolvedType::Reference { underlying, .. }
			| ResolvedType::Pointer { underlying }
			| ResolvedType::GenericInstance {
				base: underlying, ..
			} => self.lookup_method_type_on_value_type(*underlying, method_name),
			_ => None,
		}
	}

	fn resolve_method_semantic_type(
		&self,
		lhs_sem_ty: TypeId,
		method_name: &str,
		callee_ty: Option<TypeId>,
	) -> Option<TypeId> {
		if let Some(callee_ty) = callee_ty
			&& !matches!(
				self.type_ctx.semantics.arena.get(callee_ty),
				ResolvedType::Unknown
			) {
			return Some(callee_ty);
		}
		if let Some(from_lhs) =
			self.lookup_method_type_on_value_type(lhs_sem_ty, method_name)
		{
			return Some(from_lhs);
		}
		if let Some(from_values) = self.module_values.get(method_name).copied()
			&& !matches!(
				self.type_ctx.semantics.arena.get(from_values),
				ResolvedType::Unknown
			) {
			return Some(from_values);
		}
		let suffix = format!(".{method_name}");
		self.module_values.iter().find_map(|(name, ty)| {
			(name.ends_with(&suffix)
				&& !matches!(
					self.type_ctx.semantics.arena.get(*ty),
					ResolvedType::Unknown
				))
			.then_some(*ty)
		})
	}

	fn lower_type_constructor_call(
		&mut self,
		callee_name: &str,
		args: &[Box<crate::frontend::TypedAst>],
		node: &crate::frontend::TypedAst,
	) -> Result<Option<(IrValueId, IrTypeId)>, String> {
		let Some(type_id) = self.module_types.get(callee_name).copied() else {
			return Ok(None);
		};
		let canonical = self.canonical_type(type_id);
		let ir_ty = self.type_ctx.lower(canonical, self.ir_types);
		match self.type_ctx.semantics.arena.get(canonical) {
			ResolvedType::Struct { fields, .. } => {
				if args.len() != fields.len() {
					return Err(format!(
						"constructor `{callee_name}` expects {} args, got {}",
						fields.len(),
						args.len()
					));
				}
				let mut lowered_fields = Vec::with_capacity(args.len());
				for arg in args {
					let Some((value, _)) = self.lower_expr(arg.as_ref())?
					else {
						return Err(format!(
							"constructor arg in `{callee_name}` produced no value"
						));
					};
					lowered_fields.push(value);
				}
				let out = self.push_value(
					IrInstKind::MakeStruct {
						ty: ir_ty,
						fields: lowered_fields,
					},
					ir_ty,
					Some(node.location.clone()),
				)?;
				Ok(Some((out, ir_ty)))
			}
			ResolvedType::Union { variants, .. } => {
				if args.len() != 1 {
					return Err(format!(
						"union constructor `{callee_name}` expects 1 arg, got {}",
						args.len()
					));
				}
				let arg_node = args[0].as_ref();
				let Some((payload, _)) = self.lower_expr(arg_node)? else {
					return Err(format!(
						"union constructor arg in `{callee_name}` produced no value"
					));
				};
				let arg_ty = arg_node.ty.unwrap_or(self.builtin_void_ty);
				let arg_canonical = self.canonical_type(arg_ty);
				let Some(variant) =
					variants.iter().enumerate().find_map(|(idx, candidate)| {
						let candidate_canonical =
							self.canonical_type(*candidate);
						(self.type_ctx.semantics.type_matches(
							candidate_canonical,
							arg_canonical,
						) || self.type_ctx.semantics.type_matches(
							arg_canonical,
							candidate_canonical,
						))
						.then_some(idx as u32)
					})
				else {
					return Err(format!(
						"union constructor `{callee_name}` arg type does not match a variant"
					));
				};
				let out = self.push_value(
					IrInstKind::MakeUnion {
						ty: ir_ty,
						variant,
						payload,
					},
					ir_ty,
					Some(node.location.clone()),
				)?;
				Ok(Some((out, ir_ty)))
			}
			_ => Ok(None),
		}
	}

	fn push_effect(
		&mut self,
		kind: IrInstKind,
		location: Option<crate::frontend::SourceLocation>,
	) {
		let block = self
			.current_block_mut()
			.expect("active block for effect emission");
		block.insts.push(IrInst {
			result: None,
			kind,
			location,
		});
	}

	fn push_value(
		&mut self,
		kind: IrInstKind,
		ty: IrTypeId,
		location: Option<crate::frontend::SourceLocation>,
	) -> Result<IrValueId, String> {
		let id = self.ids.fresh_value();
		self.value_types.values.insert(id, ty);
		self.current_block_mut()?.insts.push(IrInst {
			result: Some(IrValueDef { id, ty }),
			kind,
			location,
		});
		Ok(id)
	}
}

fn map_bin(op: &Operator) -> Option<IrBinOp> {
	match op {
		Operator::Add => Some(IrBinOp::Add),
		Operator::Sub => Some(IrBinOp::Sub),
		Operator::Mul => Some(IrBinOp::Mul),
		Operator::Divide => Some(IrBinOp::Div),
		Operator::And => Some(IrBinOp::And),
		Operator::Or => Some(IrBinOp::Or),
		Operator::BinAnd => Some(IrBinOp::BitAnd),
		Operator::BinOr => Some(IrBinOp::BitOr),
		Operator::BinXOR => Some(IrBinOp::BitXor),
		_ => None,
	}
}

fn map_cmp(op: &Operator, has_eq: bool) -> Option<IrCmpOp> {
	match (op, has_eq) {
		(Operator::LessThan, false) => Some(IrCmpOp::Lt),
		(Operator::LessThan, true) => Some(IrCmpOp::Le),
		(Operator::GreaterThan, false) => Some(IrCmpOp::Gt),
		(Operator::GreaterThan, true) => Some(IrCmpOp::Ge),
		(Operator::Set, true) => Some(IrCmpOp::Eq),
		(Operator::Not, true) => Some(IrCmpOp::Ne),
		_ => None,
	}
}

#[cfg(test)]
mod tests {
	use super::lower_typed_program_to_ir;
	use crate::frontend::{
		Lexer, Parser, ResolvedType, Semantics, TypedProgram, run_passes_with_modules_timed,
	};
	use crate::ir::{IrBinOp, IrInstKind, IrTerminator, IrType};
	use std::fs::read_to_string;
	use std::path::{Path, PathBuf};

	fn parse_entry(entry_file: &Path) -> Box<crate::frontend::AST> {
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
	) -> (TypedProgram, Semantics, Vec<crate::frontend::FrontendError>) {
		let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(root_name);
		let entry_file = root.join("app/main.he");
		let ast = parse_entry(&entry_file);
		let (typed, semantics, _resolved, errors, _warnings, _timings) =
			run_passes_with_modules_timed(
				ast,
				entry_file.to_str().expect("entry path"),
				&Vec::new(),
			);
		(typed, semantics, errors)
	}

	#[test]
	fn lowers_void_function_with_entry_terminator() {
		let (typed, semantics, errors) = run_fixture("testdata/ir_lowering_void");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let module = ir
			.modules
			.iter()
			.find(|m| m.id == "main")
			.expect("main module lowered");
		let func = module
			.functions
			.iter()
			.find(|f| f.name == "main")
			.expect("main function lowered");

		let entry = func.entry.expect("entry block exists");
		let entry_block = func
			.blocks
			.iter()
			.find(|b| b.id == entry)
			.expect("entry block present");
		assert!(matches!(entry_block.term, Some(IrTerminator::Ret(None))));
	}

	#[test]
	fn lowers_interface_methods_in_deterministic_order() {
		let (typed, semantics, errors) =
			run_fixture("testdata/ir_lowering_interface_order");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let ops_ty = semantics
			.modules
			.get("main")
			.expect("module semantics")
			.types
			.get("Ops")
			.copied()
			.expect("Ops type present");

		let lowered_iface =
			ir.types.types
				.iter()
				.find_map(|ty| match ty {
					IrType::Interface(iface)
						if iface.name.as_deref() == Some("Ops") =>
					{
						Some(iface)
					}
					_ => None,
				})
				.expect("Ops interface lowered");

		let method_names: Vec<_> = lowered_iface
			.methods
			.iter()
			.map(|m| m.name.as_str())
			.collect();
		assert_eq!(method_names, vec!["add", "zed"]);

		assert!(matches!(
			semantics.arena.get(ops_ty),
			ResolvedType::Interface { .. }
		));
	}

	#[test]
	fn lowers_deref_assignment_to_store_instruction() {
		let (typed, semantics, errors) = run_fixture("testdata/ir_lowering_deref_store");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let module = ir
			.modules
			.iter()
			.find(|m| m.id == "main")
			.expect("main module lowered");
		let func = module
			.functions
			.iter()
			.find(|f| f.name == "main")
			.expect("main function lowered");

		let has_store = func.blocks.iter().any(|block| {
			block.insts
				.iter()
				.any(|inst| matches!(inst.kind, IrInstKind::Store { .. }))
		});
		assert!(has_store, "expected deref assignment to lower to store");
	}

	#[test]
	fn lowers_function_parameter_metadata() {
		let (typed, semantics, errors) = run_fixture("testdata/ir_lowering_params");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let module = ir
			.modules
			.iter()
			.find(|m| m.id == "main")
			.expect("main module lowered");
		let sum = module
			.functions
			.iter()
			.find(|f| f.name == "sum")
			.expect("sum function lowered");

		assert_eq!(sum.params.len(), 2);
		assert_eq!(sum.params[0].name, "a");
		assert_eq!(sum.params[1].name, "b");

		let sig_ty = ir.types.get(sum.signature);
		let IrType::FnSig(sig) = sig_ty else {
			panic!("sum signature must lower to function signature");
		};
		assert_eq!(sig.params.len(), 2);

		let entry = sum.entry.expect("entry block exists");
		let entry_block = sum
			.blocks
			.iter()
			.find(|b| b.id == entry)
			.expect("entry block present");
		assert!(entry_block.insts.iter().any(|inst| matches!(
			inst.kind,
			IrInstKind::BinOp {
				op: IrBinOp::Add,
				..
			}
		)));
		assert!(!entry_block.insts.iter().any(|inst| matches!(
			inst.kind,
			IrInstKind::Const(crate::ir::IrConst::Undef)
		)));
	}

	#[test]
	fn lowers_struct_and_inheritance_type_metadata() {
		let (typed, semantics, errors) = run_fixture("testdata/ir_lowering_inheritance");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");

		let base = ir
			.types
			.types
			.iter()
			.find_map(|ty| match ty {
				IrType::Struct(s) if s.name.as_deref() == Some("Base") => Some(s),
				_ => None,
			})
			.expect("Base lowered");
		assert!(base.fields.iter().any(|f| f.name == "x"));

		let child = ir
			.types
			.types
			.iter()
			.find_map(|ty| match ty {
				IrType::Struct(s) if s.name.as_deref() == Some("Child") => Some(s),
				_ => None,
			})
			.expect("Child lowered");
		assert!(child.fields.iter().any(|f| f.name == "y"));
		assert_eq!(child.extends.len(), 1);
	}

	#[test]
	fn lowers_if_and_while_to_cfg_blocks() {
		let (typed, semantics, errors) = run_fixture("testdata/backend_smoke");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let module = ir
			.modules
			.iter()
			.find(|m| m.id == "main")
			.expect("main module lowered");
		let main_fn = module
			.functions
			.iter()
			.find(|f| f.name == "main")
			.expect("main function lowered");

		assert!(
			main_fn.blocks.len() >= 5,
			"expected CFG expansion for if/while"
		);
		let condbr_count = main_fn
			.blocks
			.iter()
			.filter(|block| matches!(block.term, Some(IrTerminator::CondBr { .. })))
			.count();
		assert!(
			condbr_count >= 2,
			"expected conditional branches for if/while"
		);
		assert!(
			main_fn.blocks.iter().any(|block| !block.params.is_empty()),
			"expected block parameters for loop-carried SSA values"
		);
	}

	#[test]
	fn lowers_if_merge_with_block_parameters() {
		let (typed, semantics, errors) = run_fixture("testdata/ir_lowering_if_merge");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let module = ir
			.modules
			.iter()
			.find(|m| m.id == "main")
			.expect("main module lowered");
		let main_fn = module
			.functions
			.iter()
			.find(|f| f.name == "main")
			.expect("main function lowered");

		let merge_blocks: Vec<_> = main_fn
			.blocks
			.iter()
			.filter(|block| !block.params.is_empty())
			.collect();
		assert!(!merge_blocks.is_empty(), "expected merge block parameters");

		let has_nonempty_br_args = main_fn.blocks.iter().any(|block| {
			matches!(
				block.term,
				Some(IrTerminator::Br(crate::ir::IrEdge { ref args, .. })) if !args.is_empty()
			)
		});
		assert!(
			has_nonempty_br_args,
			"expected predecessor edges to pass block arguments"
		);
	}

	#[test]
	fn lowers_union_match_to_tag_and_cfg() {
		let (typed, semantics, errors) = run_fixture("testdata/backend_match");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let module = ir
			.modules
			.iter()
			.find(|m| m.id == "main")
			.expect("main module lowered");
		let is_a = module
			.functions
			.iter()
			.find(|f| f.name == "is_a")
			.expect("is_a function lowered");

		let has_get_tag = is_a.blocks.iter().any(|block| {
			block.insts
				.iter()
				.any(|inst| matches!(inst.kind, IrInstKind::GetTag { .. }))
		});
		assert!(has_get_tag, "expected union match to read tag");

		let condbr_count = is_a
			.blocks
			.iter()
			.filter(|block| matches!(block.term, Some(IrTerminator::CondBr { .. })))
			.count();
		assert!(condbr_count >= 1, "expected branching for match cases");

		let has_match_result_param =
			is_a.blocks.iter().any(|block| !block.params.is_empty());
		assert!(
			has_match_result_param,
			"expected match result block parameter in non-void match"
		);
	}

	#[test]
	fn lowers_interface_cast_and_dispatch_ops() {
		let (typed, semantics, errors) =
			run_fixture("testdata/backend_interface_multi_methods");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let module = ir
			.modules
			.iter()
			.find(|m| m.id == "main")
			.expect("main module lowered");
		let main_fn = module
			.functions
			.iter()
			.find(|f| f.name == "main")
			.expect("main function lowered");

		let has_make_struct = main_fn.blocks.iter().any(|block| {
			block.insts
				.iter()
				.any(|inst| matches!(inst.kind, IrInstKind::MakeStruct { .. }))
		});
		assert!(has_make_struct, "expected struct initializer lowering");

		let has_pack_iface = main_fn.blocks.iter().any(|block| {
			block.insts
				.iter()
				.any(|inst| matches!(inst.kind, IrInstKind::PackInterface { .. }))
		});
		assert!(has_pack_iface, "expected interface pack lowering for cast");

		let method_ptr_count = main_fn
			.blocks
			.iter()
			.flat_map(|block| block.insts.iter())
			.filter(|inst| matches!(inst.kind, IrInstKind::InterfaceMethodPtr { .. }))
			.count();
		assert!(
			method_ptr_count >= 2,
			"expected interface method pointer loads for add/mul calls"
		);
	}

	#[test]
	fn lowers_ctor_fixture_call_signature_arity() {
		let (typed, semantics, errors) = run_fixture("testdata/backend_ctor_return");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let ir = lower_typed_program_to_ir(&typed, &semantics).expect("ir lowering");
		let module = ir
			.modules
			.iter()
			.find(|m| m.id == "main")
			.expect("main module lowered");
		let main_fn = module
			.functions
			.iter()
			.find(|f| f.name == "main")
			.expect("main function lowered");

		let call_sig = main_fn
			.blocks
			.iter()
			.flat_map(|block| block.insts.iter())
			.find_map(|inst| match &inst.kind {
				IrInstKind::CallDirect { callee, sig, .. }
					if callee == "make_pair" =>
				{
					Some(*sig)
				}
				_ => None,
			})
			.expect("make_pair call lowered");
		let IrType::FnSig(sig) = ir.types.get(call_sig) else {
			panic!("call signature should be fn signature");
		};
		assert_eq!(sig.params.len(), 1);
	}
}
