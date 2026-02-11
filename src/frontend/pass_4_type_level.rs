use super::*;

impl Pass4State {
	pub(super) fn build_module_ctfe_engine(&self, module_id: &ModuleId) -> CtfeEngine {
		let mut engine = CtfeEngine::new(CtfeLimits::default());
		let Some(module) = self.modules.get(module_id) else {
			return engine;
		};
		let (ASTValue::ExprList { items, .. } | ASTValue::ExprListNoScope { items, .. }) =
			&module.ast.v
		else {
			return engine;
		};
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			engine.register_function_decls(node);
		}
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationComptime(name, value) => {
					if !Self::should_eval_comptime_value(value.as_ref()) {
						continue;
					}
					if let Ok(evaluated) = engine.eval_expr(value.as_ref()) {
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
						let Some(value) =
							values.get(idx).or_else(|| values.first())
						else {
							continue;
						};
						if !Self::should_eval_comptime_value(value.as_ref())
						{
							continue;
						}
						if let Ok(evaluated) =
							engine.eval_expr(value.as_ref())
						{
							engine.bind_const(name, evaluated);
						}
					}
				}
				_ => {}
			}
		}
		engine
	}

	pub(super) fn const_value_key(value: &ConstValue) -> Option<ConstExprKey> {
		match value {
			ConstValue::Void => Some(ConstExprKey::Void),
			ConstValue::Bool(v) => Some(ConstExprKey::Bool(*v)),
			ConstValue::Integer(v) => Some(ConstExprKey::Integer(*v)),
			ConstValue::Float(v) => Some(ConstExprKey::Float(v.to_bits())),
			ConstValue::Char(v) => Some(ConstExprKey::Char(*v)),
			ConstValue::String(v) => Some(ConstExprKey::String(v.clone())),
			ConstValue::Array(values) => {
				let mut out = Vec::with_capacity(values.len());
				for value in values {
					out.push(Self::const_value_key(value)?);
				}
				Some(ConstExprKey::Array(out))
			}
			ConstValue::Slice(values) => {
				let mut out = Vec::with_capacity(values.len());
				for value in values {
					out.push(Self::const_value_key(value)?);
				}
				Some(ConstExprKey::Slice(out))
			}
			ConstValue::Record(fields) => {
				let mut entries: Vec<(&String, &ConstValue)> =
					fields.iter().collect();
				entries.sort_by(|a, b| a.0.cmp(b.0));
				let mut out = Vec::with_capacity(entries.len());
				for (name, value) in entries {
					out.push((name.clone(), Self::const_value_key(value)?));
				}
				Some(ConstExprKey::Record(out))
			}
			ConstValue::Ref(_) => None,
		}
	}

	pub(super) fn canonicalize_type_level_expr(
		&self,
		module_id: &ModuleId,
		expr: &AST,
	) -> TypeLevelExprKey {
		if !self.type_level_expr_is_ctfe_evaluable(module_id, expr) {
			return TypeLevelExprKey::Expr(Self::expr_key_from_ast(expr));
		}
		let mut engine = self.build_module_ctfe_engine(module_id);
		if let Ok(value) = engine.eval_expr(expr)
			&& let Some(key) = Self::const_value_key(&value)
		{
			return TypeLevelExprKey::Ctfe(key);
		}
		TypeLevelExprKey::Expr(Self::expr_key_from_ast(expr))
	}

	pub(super) fn type_level_expr_is_ctfe_evaluable(
		&self,
		module_id: &ModuleId,
		expr: &AST,
	) -> bool {
		let Some(module) = self.modules.get(module_id) else {
			return false;
		};
		let (ASTValue::ExprList { items, .. } | ASTValue::ExprListNoScope { items, .. }) =
			&module.ast.v
		else {
			return false;
		};

		let mut comptime_names = HashSet::new();
		for item in items {
			let node = Self::unwrap_pub(item.as_ref());
			match &node.v {
				ASTValue::DeclarationComptime(name, value) => {
					if Self::is_ctfe_bindable_value(value.as_ref()) {
						comptime_names.insert(name.clone());
					}
				}
				ASTValue::DeclarationMulti {
					names,
					values: Some(values),
					comptime: true,
					..
				} => {
					for (idx, name) in names.iter().enumerate() {
						let Some(value) =
							values.get(idx).or_else(|| values.first())
						else {
							continue;
						};
						if Self::is_ctfe_bindable_value(value.as_ref()) {
							comptime_names.insert(name.clone());
						}
					}
				}
				_ => {}
			}
		}

		let mut ids = VecDeque::new();
		Self::collect_ids_in_ast(expr, &mut ids);
		while let Some(name) = ids.pop_front() {
			if name == "true" || name == "false" {
				continue;
			}
			if !comptime_names.contains(&name) {
				return false;
			}
		}
		true
	}

	pub(super) fn expr_key_from_ast(expr: &AST) -> ExprKey {
		match &expr.v {
			ASTValue::Id(name) => ExprKey::Id(name.clone()),
			ASTValue::DotId(name) => ExprKey::DotId(name.clone()),
			ASTValue::Integer(value) => ExprKey::Integer(*value),
			ASTValue::Float(value) => ExprKey::Float(value.to_bits()),
			ASTValue::Char(value) => ExprKey::Char(*value),
			ASTValue::String(value) => ExprKey::String(value.clone()),
			ASTValue::UnaryPlus(inner) => {
				ExprKey::UnaryPlus(Box::new(Self::expr_key_from_ast(inner)))
			}
			ASTValue::UnaryMinus(inner) => {
				ExprKey::UnaryMinus(Box::new(Self::expr_key_from_ast(inner)))
			}
			ASTValue::Not(inner) => {
				ExprKey::Not(Box::new(Self::expr_key_from_ast(inner)))
			}
			ASTValue::BinExpr {
				op,
				has_eq,
				lhs,
				rhs,
			} => ExprKey::Bin {
				op: op.clone(),
				has_eq: *has_eq,
				lhs: Box::new(Self::expr_key_from_ast(lhs)),
				rhs: Box::new(Self::expr_key_from_ast(rhs)),
			},
			ASTValue::Call { callee, args } => ExprKey::Call {
				callee: Box::new(Self::expr_key_from_ast(callee)),
				args: args
					.iter()
					.map(|arg| Self::expr_key_from_ast(arg))
					.collect(),
			},
			ASTValue::GenericApply { target, args } => ExprKey::GenericApply {
				target: Box::new(Self::expr_key_from_ast(target)),
				args: args
					.iter()
					.map(|arg| match arg {
						GenericArg::Type(ty) => {
							GenericExprArgKey::Type(ty.to_string())
						}
						GenericArg::Expr(expr) => GenericExprArgKey::Expr(
							Self::expr_key_from_ast(expr),
						),
						GenericArg::Name(name) => {
							GenericExprArgKey::Name(name.clone())
						}
					})
					.collect(),
			},
			ASTValue::Index { target, indices } => ExprKey::Index {
				target: Box::new(Self::expr_key_from_ast(target)),
				indices: indices
					.iter()
					.map(|index| Self::expr_key_from_ast(index))
					.collect(),
			},
			_ => {
				panic!(
					"internal compiler error: unsupported type-level expression shape in pass4 canonicalization: {}",
					expr
				)
			}
		}
	}
}
