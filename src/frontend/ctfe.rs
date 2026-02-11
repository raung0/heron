use std::collections::HashMap;

use crate::frontend::{
	AST, ASTValue, FnBody, FnParam, InitializerItem, MatchCasePattern, Operator, SourceLocation,
};

#[derive(Clone, Debug, PartialEq)]
pub struct CtfeLimits {
	pub fuel: u64,
	pub recursion_limit: u32,
}

impl Default for CtfeLimits {
	fn default() -> Self {
		Self {
			fuel: 100_000,
			recursion_limit: 128,
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefValue {
	pub slot: usize,
	pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstValue {
	Void,
	Bool(bool),
	Integer(i128),
	Float(f64),
	Char(char),
	String(String),
	Array(Vec<ConstValue>),
	Record(HashMap<String, ConstValue>),
	Slice(Vec<ConstValue>),
	Ref(RefValue),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtfeErrorKind {
	RecursionLimitExceeded,
	FuelExceeded,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CtfeError {
	pub location: SourceLocation,
	pub kind: CtfeErrorKind,
}

#[derive(Clone)]
struct FunctionValue {
	params: Vec<ParamSpec>,
	body: FnBody,
}

#[derive(Clone)]
struct ParamSpec {
	name: String,
	default: Option<Box<AST>>,
}

#[derive(Clone)]
struct Binding {
	slot: usize,
	mutable: bool,
}

#[derive(Clone)]
struct Scope {
	bindings: HashMap<String, Binding>,
	functions: HashMap<String, FunctionValue>,
}

#[derive(Clone)]
struct Slot {
	value: ConstValue,
}

enum EvalFlow {
	Continue(ConstValue),
	Return(ConstValue),
}

#[derive(Clone)]
struct LValuePath {
	base: String,
	fields: Vec<String>,
}

pub struct CtfeEngine {
	limits: CtfeLimits,
	remaining_fuel: u64,
	recursion_depth: u32,
	scopes: Vec<Scope>,
	slots: Vec<Slot>,
	current_location: SourceLocation,
}

impl CtfeEngine {
	pub fn new(limits: CtfeLimits) -> Self {
		Self {
			remaining_fuel: limits.fuel,
			limits,
			recursion_depth: 0,
			scopes: vec![Scope {
				bindings: HashMap::new(),
				functions: HashMap::new(),
			}],
			slots: Vec::new(),
			current_location: SourceLocation::new_from_file("<ctfe>".to_string()),
		}
	}

	fn err_here(&self, kind: CtfeErrorKind) -> CtfeError {
		CtfeError {
			location: self.current_location.clone(),
			kind,
		}
	}

	pub fn eval_module_const(
		&mut self,
		module: &AST,
		name: &str,
	) -> Result<ConstValue, CtfeError> {
		if let ASTValue::ExprList { items, .. } | ASTValue::ExprListNoScope { items, .. } =
			&module.v
		{
			for item in items {
				self.register_function_decls(item.as_ref());
			}
			for item in items {
				let target = Self::unwrap_pub(item.as_ref());
				match &target.v {
					ASTValue::DeclarationConstexpr(_, _)
					| ASTValue::DeclarationMulti {
						constexpr: true, ..
					} => {
						let flow = self.eval_node(target)?;
						Self::assert_no_module_scope_return(flow);
					}
					_ => {}
				}
			}
		}
		self.resolve_value(name)
	}

	pub fn eval_expr(&mut self, expr: &AST) -> Result<ConstValue, CtfeError> {
		match self.eval_node(expr)? {
			EvalFlow::Continue(value) | EvalFlow::Return(value) => Ok(value),
		}
	}

	pub fn register_function_decls(&mut self, node: &AST) {
		let target = Self::unwrap_pub(node);
		match &target.v {
			ASTValue::DeclarationConstexpr(name, value) => {
				self.register_function_value(name, value.as_ref());
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
						self.register_function_value(name, value.as_ref());
					}
				}
			}
			_ => {}
		}
	}

	fn register_function_value(&mut self, name: &str, value: &AST) {
		if let ASTValue::Fn { params, body, .. } = &value.v {
			let fn_value = Self::build_function(params, body);
			self.declare_function(name.to_string(), fn_value);
		}
	}

	pub fn bind_const(&mut self, name: &str, value: ConstValue) {
		self.declare_value(name.to_string(), value, false);
	}

	fn consume_fuel(&mut self) -> Result<(), CtfeError> {
		if self.remaining_fuel == 0 {
			return Err(self.err_here(CtfeErrorKind::FuelExceeded));
		}
		self.remaining_fuel -= 1;
		Ok(())
	}

	fn push_scope(&mut self) {
		self.scopes.push(Scope {
			bindings: HashMap::new(),
			functions: HashMap::new(),
		});
	}

	fn pop_scope(&mut self) {
		let _ = self.scopes.pop();
	}

	fn declare_value(&mut self, name: String, value: ConstValue, mutable: bool) {
		let slot = self.slots.len();
		self.slots.push(Slot { value });
		if let Some(scope) = self.scopes.last_mut() {
			scope.bindings.insert(name, Binding { slot, mutable });
		}
	}

	fn declare_function(&mut self, name: String, fn_value: FunctionValue) {
		if let Some(scope) = self.scopes.last_mut() {
			scope.functions.insert(name, fn_value);
		}
	}

	fn resolve_binding(&self, name: &str) -> Option<&Binding> {
		for scope in self.scopes.iter().rev() {
			if let Some(binding) = scope.bindings.get(name) {
				return Some(binding);
			}
		}
		None
	}

	fn resolve_function(&self, name: &str) -> Option<FunctionValue> {
		for scope in self.scopes.iter().rev() {
			if let Some(function) = scope.functions.get(name) {
				return Some(function.clone());
			}
		}
		None
	}

	fn resolve_value(&self, name: &str) -> Result<ConstValue, CtfeError> {
		if name == "true" {
			return Ok(ConstValue::Bool(true));
		}
		if name == "false" {
			return Ok(ConstValue::Bool(false));
		}
		if let Some(binding) = self.resolve_binding(name) {
			return Ok(self.slots[binding.slot].value.clone());
		}
		panic!(
			"internal compiler error: pass4 allowed unresolved value in CTFE: {}",
			name
		)
	}

	fn eval_node(&mut self, node: &AST) -> Result<EvalFlow, CtfeError> {
		self.current_location = node.location.clone();
		self.consume_fuel()?;
		match &node.v {
			ASTValue::Integer(v) => {
				Ok(EvalFlow::Continue(ConstValue::Integer(*v as i128)))
			}
			ASTValue::Float(v) => Ok(EvalFlow::Continue(ConstValue::Float(*v))),
			ASTValue::Char(v) => Ok(EvalFlow::Continue(ConstValue::Char(*v))),
			ASTValue::String(v) => {
				Ok(EvalFlow::Continue(ConstValue::String(v.clone())))
			}
			ASTValue::Id(name) => Ok(EvalFlow::Continue(self.resolve_value(name)?)),
			ASTValue::UnaryPlus(inner) => self.eval_node(inner),
			ASTValue::UnaryMinus(inner) => {
				let value = self.eval_continue(inner)?;
				match value {
					ConstValue::Integer(v) => {
						Ok(EvalFlow::Continue(ConstValue::Integer(-v)))
					}
					ConstValue::Float(v) => {
						Ok(EvalFlow::Continue(ConstValue::Float(-v)))
					}
					_ => panic!(
						"internal compiler error: pass4 allowed unary minus on non-number in CTFE"
					),
				}
			}
			ASTValue::Not(inner) => {
				let cond = self.eval_continue(inner)?;
				let as_bool = self.as_bool(&cond)?;
				Ok(EvalFlow::Continue(ConstValue::Bool(!as_bool)))
			}
			ASTValue::BinExpr {
				op,
				lhs,
				rhs,
				has_eq,
			} => self.eval_binary(op, lhs, rhs, *has_eq),
			ASTValue::Ref { mutable, v } => {
				let ASTValue::Id(name) = &v.v else {
					panic!(
						"internal compiler error: pass4 allowed non-id reference in CTFE"
					);
				};
				let Some(binding) = self.resolve_binding(name) else {
					panic!(
						"internal compiler error: pass4 allowed unresolved reference in CTFE: {}",
						name
					);
				};
				if *mutable && !binding.mutable {
					panic!(
						"internal compiler error: pass4 allowed mutable reference to immutable value in CTFE: {}",
						name
					);
				}
				Ok(EvalFlow::Continue(ConstValue::Ref(RefValue {
					slot: binding.slot,
					mutable: *mutable,
				})))
			}
			ASTValue::Deref(inner) => {
				let value = self.eval_continue(inner)?;
				let ConstValue::Ref(reference) = value else {
					panic!(
						"internal compiler error: pass4 allowed deref of non-reference in CTFE"
					);
				};
				Ok(EvalFlow::Continue(self.slots[reference.slot].value.clone()))
			}
			ASTValue::Mut(inner) => self.eval_node(inner),
			ASTValue::Declaration {
				name,
				value,
				mutable,
			} => {
				self.declare_from_value(name, value.as_ref(), *mutable)?;
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			ASTValue::DeclarationConstexpr(name, value) => {
				self.declare_from_value(name, value.as_ref(), false)?;
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			ASTValue::DeclarationMulti {
				names,
				values,
				mutable,
				..
			} => {
				let Some(values) = values else {
					return Ok(EvalFlow::Continue(ConstValue::Void));
				};
				for (idx, name) in names.iter().enumerate() {
					let value = values.get(idx).or_else(|| values.first());
					let Some(value) = value else {
						continue;
					};
					self.declare_from_value(name, value.as_ref(), *mutable)?;
				}
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			ASTValue::Set(name, value) => {
				let value = self.eval_continue(value)?;
				let Some(binding) = self.resolve_binding(name).cloned() else {
					panic!(
						"internal compiler error: pass4 allowed assignment to unresolved value in CTFE: {}",
						name
					);
				};
				if !binding.mutable {
					panic!(
						"internal compiler error: pass4 allowed assignment to immutable value in CTFE: {}",
						name
					);
				}
				self.slots[binding.slot].value = value;
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			ASTValue::SetMulti { names, values } => {
				for (idx, name) in names.iter().enumerate() {
					let value_ast = values.get(idx).or_else(|| values.first());
					let Some(value_ast) = value_ast else {
						continue;
					};
					let value = self.eval_continue(value_ast)?;
					let Some(binding) = self.resolve_binding(name).cloned()
					else {
						panic!(
							"internal compiler error: pass4 allowed multi-assignment to unresolved value in CTFE: {}",
							name
						);
					};
					if !binding.mutable {
						panic!(
							"internal compiler error: pass4 allowed multi-assignment to immutable value in CTFE: {}",
							name
						);
					}
					self.slots[binding.slot].value = value;
				}
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			ASTValue::ExprList { items, .. } => {
				self.push_scope();
				let flow = self.eval_items(items);
				self.pop_scope();
				flow
			}
			ASTValue::ExprListNoScope { items, .. } => self.eval_items(items),
			ASTValue::If {
				cond,
				decl,
				body,
				else_,
			} => {
				self.push_scope();
				if let Some(decl) = decl {
					let flow = self.eval_node(decl)?;
					if matches!(flow, EvalFlow::Return(_)) {
						self.pop_scope();
						return Ok(flow);
					}
				}
				let condition_value = self.eval_continue(cond)?;
				let condition = self.as_bool(&condition_value)?;
				let result = if condition {
					self.eval_node(body)?
				} else if let Some(else_node) = else_ {
					self.eval_node(else_node)?
				} else {
					EvalFlow::Continue(ConstValue::Void)
				};
				self.pop_scope();
				Ok(result)
			}
			ASTValue::While { cond, decl, body } => {
				self.push_scope();
				if let Some(decl) = decl {
					let flow = self.eval_node(decl)?;
					if matches!(flow, EvalFlow::Return(_)) {
						self.pop_scope();
						return Ok(flow);
					}
				}
				loop {
					let condition_value = self.eval_continue(cond)?;
					let condition = self.as_bool(&condition_value)?;
					if !condition {
						break;
					}
					let flow = self.eval_node(body)?;
					if matches!(flow, EvalFlow::Return(_)) {
						self.pop_scope();
						return Ok(flow);
					}
				}
				self.pop_scope();
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			ASTValue::ForLoop {
				init,
				cond,
				step,
				body,
			} => {
				self.push_scope();
				if let Some(init) = init {
					let flow = self.eval_node(init)?;
					if matches!(flow, EvalFlow::Return(_)) {
						self.pop_scope();
						return Ok(flow);
					}
				}
				loop {
					if let Some(cond) = cond {
						let condition_value = self.eval_continue(cond)?;
						let condition = self.as_bool(&condition_value)?;
						if !condition {
							break;
						}
					}
					let flow = self.eval_node(body)?;
					if matches!(flow, EvalFlow::Return(_)) {
						self.pop_scope();
						return Ok(flow);
					}
					if let Some(step) = step {
						let flow = self.eval_node(step)?;
						if matches!(flow, EvalFlow::Return(_)) {
							self.pop_scope();
							return Ok(flow);
						}
					}
				}
				self.pop_scope();
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			ASTValue::For {
				bindings,
				iter,
				body,
			} => {
				let iterable = self.eval_continue(iter)?;
				let elements = match iterable {
					ConstValue::Array(values) | ConstValue::Slice(values) => {
						values
					}
					_ => panic!(
						"internal compiler error: pass4 allowed for-loop over non-iterable in CTFE"
					),
				};
				if bindings.len() != 1 {
					panic!(
						"internal compiler error: pass4 allowed unsupported for-binding arity in CTFE"
					);
				}
				let ASTValue::Id(binding_name) = &bindings[0].v else {
					panic!(
						"internal compiler error: pass4 allowed non-id for-binding in CTFE"
					);
				};
				for element in elements {
					self.push_scope();
					self.declare_value(binding_name.clone(), element, false);
					let flow = self.eval_node(body)?;
					self.pop_scope();
					if matches!(flow, EvalFlow::Return(_)) {
						return Ok(flow);
					}
				}
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			ASTValue::Return(value) => {
				let value = match value {
					Some(value) => self.eval_continue(value)?,
					None => ConstValue::Void,
				};
				Ok(EvalFlow::Return(value))
			}
			ASTValue::Call { callee, args } => {
				let value = self.eval_call(callee.as_ref(), args)?;
				Ok(EvalFlow::Continue(value))
			}
			ASTValue::InitializerList(items)
			| ASTValue::TypedInitializerList { items, .. } => {
				let mut values = Vec::new();
				let mut named = HashMap::new();
				let mut has_named = false;
				for item in items {
					match item {
						InitializerItem::Positional(value) => {
							values.push(self.eval_continue(value)?)
						}
						InitializerItem::Named { name, value } => {
							has_named = true;
							named.insert(
								name.clone(),
								self.eval_continue(value)?,
							);
						}
					}
				}
				if has_named {
					Ok(EvalFlow::Continue(ConstValue::Record(named)))
				} else {
					Ok(EvalFlow::Continue(ConstValue::Array(values)))
				}
			}
			ASTValue::Index { target, indices } => {
				if indices.len() != 1 {
					panic!(
						"internal compiler error: pass4 allowed multidimensional index in CTFE"
					);
				}
				let target = self.eval_continue(target)?;
				let index = self.eval_continue(&indices[0])?;
				let index = match index {
					ConstValue::Integer(v) if v >= 0 => v as usize,
					_ => panic!(
						"internal compiler error: pass4 allowed invalid CTFE index expression"
					),
				};
				match target {
					ConstValue::Array(values) | ConstValue::Slice(values) => {
						values.get(index)
							.cloned()
							.map(EvalFlow::Continue)
							.ok_or_else(|| {
								panic!(
									"internal compiler error: pass4 allowed out-of-bounds CTFE index"
								)
							})
					}
					_ => panic!(
						"internal compiler error: pass4 allowed indexing non-indexable CTFE target"
					),
				}
			}
			ASTValue::Match {
				scrutinee, cases, ..
			} => {
				let scrutinee = self.eval_continue(scrutinee)?;
				for case in cases {
					let pattern_matches = match &case.pattern {
						MatchCasePattern::Default => true,
						MatchCasePattern::Exprs(exprs) => {
							let mut matched = false;
							for expr in exprs {
								let candidate =
									self.eval_continue(expr)?;
								if candidate == scrutinee {
									matched = true;
									break;
								}
							}
							matched
						}
						MatchCasePattern::Type(_) => false,
					};
					if !pattern_matches {
						continue;
					}
					if let Some(guard) = &case.guard {
						let guard = self.eval_continue(guard)?;
						if !self.as_bool(&guard)? {
							continue;
						}
					}
					return self.eval_node(&case.body);
				}
				Ok(EvalFlow::Continue(ConstValue::Void))
			}
			_ => panic!(
				"internal compiler error: pass4 allowed unsupported AST node in CTFE"
			),
		}
	}

	fn eval_items(&mut self, items: &[Box<AST>]) -> Result<EvalFlow, CtfeError> {
		let mut last = ConstValue::Void;
		for item in items {
			let flow = self.eval_node(item)?;
			match flow {
				EvalFlow::Continue(value) => {
					last = value;
				}
				EvalFlow::Return(value) => return Ok(EvalFlow::Return(value)),
			}
		}
		Ok(EvalFlow::Continue(last))
	}

	fn eval_continue(&mut self, node: &AST) -> Result<ConstValue, CtfeError> {
		match self.eval_node(node)? {
			EvalFlow::Continue(value) => Ok(value),
			EvalFlow::Return(value) => Ok(value),
		}
	}

	fn eval_call(&mut self, callee: &AST, args: &[Box<AST>]) -> Result<ConstValue, CtfeError> {
		let (function, mut positional) = self.resolve_call_target(callee)?;
		let mut named = HashMap::new();
		for arg in args {
			if let ASTValue::NamedArg { name, value } = &arg.v {
				named.insert(name.clone(), value.clone());
			} else {
				positional.push(self.eval_continue(arg.as_ref())?);
			}
		}
		self.eval_call_function(function, positional, named)
	}

	fn resolve_call_target(
		&mut self,
		callee: &AST,
	) -> Result<(FunctionValue, Vec<ConstValue>), CtfeError> {
		let target = match &callee.v {
			ASTValue::Id(name) => {
				let function = self.resolve_function(name).unwrap_or_else(|| {
					panic!(
						"internal compiler error: pass4 allowed unresolved function in CTFE: {}",
						name
					)
				});
				(function, Vec::new())
			}
			ASTValue::Fn { params, body, .. } => {
				(Self::build_function(params, body), Vec::new())
			}
			ASTValue::BinExpr {
				op: Operator::Dot,
				lhs,
				rhs,
				has_eq: false,
			} => {
				let method_name = match &rhs.v {
					ASTValue::Id(name) | ASTValue::DotId(name) => name,
					_ => {
						panic!(
							"internal compiler error: pass4 allowed non-id method name in CTFE"
						);
					}
				};
				let receiver = self.eval_continue(lhs)?;
				let function =
					self.resolve_function(method_name).unwrap_or_else(|| {
						panic!(
							"internal compiler error: pass4 allowed unresolved method in CTFE: {}",
							method_name
						)
					});
				(function, vec![receiver])
			}
			_ => {
				panic!(
					"internal compiler error: pass4 allowed unsupported call target in CTFE"
				);
			}
		};
		Ok(target)
	}

	fn eval_call_named_values(
		&mut self,
		name: &str,
		positional: Vec<ConstValue>,
	) -> ConstValue {
		let function = self.resolve_function(name).unwrap_or_else(|| {
			panic!(
				"internal compiler error: pass4 allowed unresolved overloaded operator in CTFE: {}",
				name
			)
		});
		self.eval_call_function(function, positional, HashMap::new())
			.unwrap_or_else(|err| {
				panic!(
					"internal compiler error: ctfe overloaded operator call failed: {:?}",
					err
				)
			})
	}

	fn eval_call_function(
		&mut self,
		function: FunctionValue,
		positional: Vec<ConstValue>,
		mut named: HashMap<String, Box<AST>>,
	) -> Result<ConstValue, CtfeError> {
		if self.recursion_depth >= self.limits.recursion_limit {
			return Err(self.err_here(CtfeErrorKind::RecursionLimitExceeded));
		}

		self.recursion_depth += 1;
		self.push_scope();
		for (idx, param) in function.params.iter().enumerate() {
			let value = if let Some(arg_value) = positional.get(idx).cloned() {
				arg_value
			} else if let Some(arg_node) = named.remove(&param.name) {
				self.eval_continue(&arg_node)?
			} else if let Some(default) = &param.default {
				self.eval_continue(default)?
			} else {
				self.pop_scope();
				self.recursion_depth -= 1;
				panic!(
					"internal compiler error: pass4 allowed call with missing argument in CTFE"
				);
			};
			self.declare_value(param.name.clone(), value, false);
		}

		let result = match &function.body {
			FnBody::Block(body) => self.eval_node(body),
			FnBody::Expr(expr) => self.eval_node(expr),
			FnBody::Uninitialized => {
				panic!(
					"internal compiler error: pass4 allowed uninitialized function body in CTFE"
				)
			}
		};
		self.pop_scope();
		self.recursion_depth -= 1;
		match result? {
			EvalFlow::Continue(value) | EvalFlow::Return(value) => Ok(value),
		}
	}

	fn declare_from_value(
		&mut self,
		name: &str,
		value: &AST,
		mutable: bool,
	) -> Result<(), CtfeError> {
		if let ASTValue::Fn { params, body, .. } = &value.v {
			let fn_value = Self::build_function(params, body);
			self.declare_function(name.to_string(), fn_value);
			return Ok(());
		}
		if Self::is_type_decl_expr(value) {
			self.declare_value(name.to_string(), ConstValue::Void, mutable);
			return Ok(());
		}
		let evaluated = self.eval_continue(value)?;
		self.declare_value(name.to_string(), evaluated, mutable);
		Ok(())
	}

	fn unwrap_pub(node: &AST) -> &AST {
		match &node.v {
			ASTValue::Pub(inner) => inner.as_ref(),
			_ => node,
		}
	}

	fn assert_no_module_scope_return(flow: EvalFlow) {
		if let EvalFlow::Return(_) = flow {
			panic!(
				"internal compiler error: pass4 allowed return at module scope in CTFE"
			);
		}
	}

	fn build_function(params: &[FnParam], body: &FnBody) -> FunctionValue {
		let mut out = Vec::new();
		for param in params {
			for name in &param.names {
				out.push(ParamSpec {
					name: name.clone(),
					default: param.default.clone(),
				});
			}
		}
		FunctionValue {
			params: out,
			body: body.clone(),
		}
	}

	fn as_bool(&self, value: &ConstValue) -> Result<bool, CtfeError> {
		match value {
			ConstValue::Bool(v) => Ok(*v),
			_ => panic!(
				"internal compiler error: pass4 allowed non-bool condition in CTFE"
			),
		}
	}

	fn is_type_decl_expr(node: &AST) -> bool {
		matches!(
			node.v,
			ASTValue::Struct { .. }
				| ASTValue::Enum { .. } | ASTValue::Union { .. }
				| ASTValue::RawUnion { .. } | ASTValue::Newtype { .. }
				| ASTValue::Alias { .. }
		)
	}

	fn extract_lvalue_path(node: &AST) -> Option<LValuePath> {
		match &node.v {
			ASTValue::Id(name) => Some(LValuePath {
				base: name.clone(),
				fields: Vec::new(),
			}),
			ASTValue::BinExpr {
				op: Operator::Dot,
				lhs,
				rhs,
				has_eq: false,
			} => {
				let mut path = Self::extract_lvalue_path(lhs)?;
				match &rhs.v {
					ASTValue::Id(name) | ASTValue::DotId(name) => {
						path.fields.push(name.clone());
						Some(path)
					}
					_ => None,
				}
			}
			_ => None,
		}
	}

	fn assign_path(value: &mut ConstValue, fields: &[String], new_value: ConstValue) {
		if fields.is_empty() {
			*value = new_value;
			return;
		}
		match value {
			ConstValue::Record(map) => {
				if fields.len() == 1 {
					map.insert(fields[0].clone(), new_value);
					return;
				}
				let Some(field) = map.get_mut(&fields[0]) else {
					panic!(
						"internal compiler error: pass4 allowed assignment to missing record field in CTFE"
					);
				};
				Self::assign_path(field, &fields[1..], new_value)
			}
			_ => panic!(
				"internal compiler error: pass4 allowed member assignment on non-record in CTFE"
			),
		}
	}

	fn eval_binary(
		&mut self,
		op: &Operator,
		lhs: &AST,
		rhs: &AST,
		has_eq: bool,
	) -> Result<EvalFlow, CtfeError> {
		if matches!(op, Operator::Set) && !has_eq {
			let Some(path) = Self::extract_lvalue_path(lhs) else {
				panic!(
					"internal compiler error: pass4 allowed invalid assignment lhs in CTFE"
				);
			};
			let value = self.eval_continue(rhs)?;
			let Some(binding) = self.resolve_binding(&path.base).cloned() else {
				panic!(
					"internal compiler error: pass4 allowed assignment through unresolved base in CTFE: {}",
					path.base
				);
			};
			if !binding.mutable {
				panic!(
					"internal compiler error: pass4 allowed member assignment to immutable value in CTFE: {}",
					path.base
				);
			}
			Self::assign_path(&mut self.slots[binding.slot].value, &path.fields, value);
			return Ok(EvalFlow::Continue(ConstValue::Void));
		}

		if matches!(op, Operator::Dot) && !has_eq {
			let target = self.eval_continue(lhs)?;
			let field = match &rhs.v {
				ASTValue::Id(name) | ASTValue::DotId(name) => name.as_str(),
				_ => {
					panic!(
						"internal compiler error: pass4 allowed non-id dot rhs in CTFE"
					);
				}
			};
			return match target {
				ConstValue::Record(fields) => Ok(fields
					.get(field)
					.cloned()
					.map(EvalFlow::Continue)
					.unwrap_or_else(|| {
						panic!(
							"internal compiler error: pass4 allowed access to missing record field in CTFE"
						)
					})),
				_ => panic!(
					"internal compiler error: pass4 allowed dot access on non-record in CTFE"
				),
			};
		}

		let lhs = self.eval_continue(lhs)?;
		let rhs = self.eval_continue(rhs)?;
		let overloaded =
			|this: &mut Self, op_name: &str, lhs: ConstValue, rhs: ConstValue| {
				let name = format!("operator{op_name}");
				let value =
					this.eval_call_named_values(name.as_str(), vec![lhs, rhs]);
				Ok(EvalFlow::Continue(value))
			};
		match op {
			Operator::Add => match (lhs, rhs) {
				(ConstValue::Integer(a), ConstValue::Integer(b)) => {
					Ok(EvalFlow::Continue(ConstValue::Integer(a + b)))
				}
				(ConstValue::Float(a), ConstValue::Float(b)) => {
					Ok(EvalFlow::Continue(ConstValue::Float(a + b)))
				}
				(lhs, rhs) => overloaded(self, "+", lhs, rhs),
			},
			Operator::Sub => match (lhs, rhs) {
				(ConstValue::Integer(a), ConstValue::Integer(b)) => {
					Ok(EvalFlow::Continue(ConstValue::Integer(a - b)))
				}
				(ConstValue::Float(a), ConstValue::Float(b)) => {
					Ok(EvalFlow::Continue(ConstValue::Float(a - b)))
				}
				(lhs, rhs) => overloaded(self, "-", lhs, rhs),
			},
			Operator::Mul => match (lhs, rhs) {
				(ConstValue::Integer(a), ConstValue::Integer(b)) => {
					Ok(EvalFlow::Continue(ConstValue::Integer(a * b)))
				}
				(ConstValue::Float(a), ConstValue::Float(b)) => {
					Ok(EvalFlow::Continue(ConstValue::Float(a * b)))
				}
				(lhs, rhs) => overloaded(self, "*", lhs, rhs),
			},
			Operator::Divide => match (lhs, rhs) {
				(ConstValue::Integer(a), ConstValue::Integer(b)) => {
					Ok(EvalFlow::Continue(ConstValue::Integer(a / b)))
				}
				(ConstValue::Float(a), ConstValue::Float(b)) => {
					Ok(EvalFlow::Continue(ConstValue::Float(a / b)))
				}
				(lhs, rhs) => overloaded(self, "/", lhs, rhs),
			},
			Operator::LessThan => match (lhs, rhs) {
				(ConstValue::Integer(a), ConstValue::Integer(b)) => {
					let value = if has_eq { a <= b } else { a < b };
					Ok(EvalFlow::Continue(ConstValue::Bool(value)))
				}
				(ConstValue::Float(a), ConstValue::Float(b)) => {
					let value = if has_eq { a <= b } else { a < b };
					Ok(EvalFlow::Continue(ConstValue::Bool(value)))
				}
				(lhs, rhs) => overloaded(self, "<", lhs, rhs),
			},
			Operator::GreaterThan => match (lhs, rhs) {
				(ConstValue::Integer(a), ConstValue::Integer(b)) => {
					let value = if has_eq { a >= b } else { a > b };
					Ok(EvalFlow::Continue(ConstValue::Bool(value)))
				}
				(ConstValue::Float(a), ConstValue::Float(b)) => {
					let value = if has_eq { a >= b } else { a > b };
					Ok(EvalFlow::Continue(ConstValue::Bool(value)))
				}
				(lhs, rhs) => overloaded(self, ">", lhs, rhs),
			},
			Operator::Set if has_eq => {
				Ok(EvalFlow::Continue(ConstValue::Bool(lhs == rhs)))
			}
			Operator::Not if has_eq => {
				Ok(EvalFlow::Continue(ConstValue::Bool(lhs != rhs)))
			}
			Operator::And => {
				let lhs = self.as_bool(&lhs)?;
				let rhs = self.as_bool(&rhs)?;
				Ok(EvalFlow::Continue(ConstValue::Bool(lhs && rhs)))
			}
			Operator::Or => {
				let lhs = self.as_bool(&lhs)?;
				let rhs = self.as_bool(&rhs)?;
				Ok(EvalFlow::Continue(ConstValue::Bool(lhs || rhs)))
			}
			_ => panic!(
				"internal compiler error: pass4 allowed unsupported operator in CTFE"
			),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::frontend::{Lexer, Parser};
	use std::fs;
	use std::path::PathBuf;

	fn parse_module(src: &str) -> Box<AST> {
		let mut lexer = Lexer::new(src.to_string(), "<test>".to_string());
		let mut parser = Parser::new(&mut lexer).expect("parser init");
		let ast = parser.parse().expect("parse ok");
		let errors = parser.take_errors();
		assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");
		ast
	}

	fn parse_fixture(fixture: &str) -> Box<AST> {
		let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
			.join("testdata")
			.join(fixture)
			.join("app")
			.join("main.he");
		let src = fs::read_to_string(&path).unwrap_or_else(|e| {
			panic!("failed to read fixture {}: {e}", path.display())
		});
		let mut lexer = Lexer::new(src, path.display().to_string());
		let mut parser = Parser::new(&mut lexer).expect("parser init");
		let ast = parser.parse().expect("parse ok");
		let errors = parser.take_errors();
		assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");
		ast
	}

	fn assert_fixture_ctfe_error(fixture: &str, expected: fn(&CtfeErrorKind) -> bool) {
		assert_fixture_ctfe_error_with_limits(fixture, CtfeLimits::default(), expected);
	}

	fn assert_fixture_ctfe_error_with_limits(
		fixture: &str,
		limits: CtfeLimits,
		expected: fn(&CtfeErrorKind) -> bool,
	) {
		let ast = parse_fixture(fixture);
		let mut engine = CtfeEngine::new(limits);
		let err = engine
			.eval_module_const(ast.as_ref(), "bad")
			.err()
			.unwrap_or_else(|| panic!("expected ctfe error in fixture `{fixture}`"));
		assert!(
			expected(&err.kind),
			"unexpected ctfe error for fixture `{fixture}`: {:?}",
			err.kind
		);
	}

	#[test]
	fn evaluates_arithmetic_constant() {
		let ast = parse_module("result :: 1 + 2 * 3");
		let mut engine = CtfeEngine::new(CtfeLimits::default());
		let value = engine
			.eval_module_const(ast.as_ref(), "result")
			.expect("ctfe eval");
		assert_eq!(value, ConstValue::Integer(7));
	}

	#[test]
	fn evaluates_function_with_control_flow() {
		let ast = parse_module(
			"sum_to :: fn(n: int) -> int { mut i := 0; mut acc := 0; while i < n { acc = acc + i; i = i + 1; }; acc }\nresult :: sum_to(5)",
		);
		let mut engine = CtfeEngine::new(CtfeLimits::default());
		let value = engine
			.eval_module_const(ast.as_ref(), "result")
			.expect("ctfe eval");
		assert_eq!(value, ConstValue::Integer(10));
	}

	#[test]
	fn supports_refs_and_deref() {
		let ast = parse_module("x :: 41\ny :: &x\nz :: (y^) + 1");
		let mut engine = CtfeEngine::new(CtfeLimits::default());
		let value = engine
			.eval_module_const(ast.as_ref(), "z")
			.expect("ctfe eval");
		assert_eq!(value, ConstValue::Integer(42));
	}

	#[test]
	fn recursion_limit_errors() {
		let ast = parse_module(
			"loop :: fn(n: int) -> int { loop(n + 1) }\nresult :: loop(0)",
		);
		let mut engine = CtfeEngine::new(CtfeLimits {
			fuel: 10_000,
			recursion_limit: 8,
		});
		let err = engine
			.eval_module_const(ast.as_ref(), "result")
			.err()
			.expect("expected recursion limit error");
		assert!(matches!(err.kind, CtfeErrorKind::RecursionLimitExceeded));
	}

	#[test]
	fn fuel_limit_errors() {
		let ast =
			parse_module("spin :: fn -> int { while true { } ; 0 }\nresult :: spin()");
		let mut engine = CtfeEngine::new(CtfeLimits {
			fuel: 200,
			recursion_limit: 16,
		});
		let err = engine
			.eval_module_const(ast.as_ref(), "result")
			.err()
			.expect("expected fuel error");
		assert!(matches!(err.kind, CtfeErrorKind::FuelExceeded));
	}

	#[test]
	fn supports_member_assignment() {
		let ast = parse_module(
			"update :: fn -> int { mut p := .{ .x = 1 }; p.x = 4; p.x }\nresult :: update()",
		);
		let mut engine = CtfeEngine::new(CtfeLimits::default());
		let value = engine
			.eval_module_const(ast.as_ref(), "result")
			.expect("ctfe eval");
		assert_eq!(value, ConstValue::Integer(4));
	}

	#[test]
	fn supports_anonymous_function_call() {
		let ast = parse_module("bad :: (fn(x: int) -> int do x + 1)(41)");
		let mut engine = CtfeEngine::new(CtfeLimits::default());
		let value = engine
			.eval_module_const(ast.as_ref(), "bad")
			.expect("ctfe eval");
		assert_eq!(value, ConstValue::Integer(42));
	}

	#[test]
	fn supports_member_function_call() {
		let ast = parse_module(
			"inc :: fn(self: int; by: int) -> int do self + by\nx :: 40\nbad :: x.inc(2)",
		);
		let mut engine = CtfeEngine::new(CtfeLimits::default());
		let value = engine
			.eval_module_const(ast.as_ref(), "bad")
			.expect("ctfe eval");
		assert_eq!(value, ConstValue::Integer(42));
	}

	#[test]
	fn supports_operator_overload_call() {
		let ast = parse_module(
			"operator+ :: fn(a; b) -> int do a.x + b.x\nbad :: .{ .x = 40 } + .{ .x = 2 }",
		);
		let mut engine = CtfeEngine::new(CtfeLimits::default());
		let value = engine
			.eval_module_const(ast.as_ref(), "bad")
			.expect("ctfe eval");
		assert_eq!(value, ConstValue::Integer(42));
	}

	#[test]
	fn fixture_recursion_limit_error() {
		assert_fixture_ctfe_error_with_limits(
			"ctfe_err_recursion_limit",
			CtfeLimits {
				fuel: 10_000,
				recursion_limit: 16,
			},
			|kind| matches!(kind, CtfeErrorKind::RecursionLimitExceeded),
		);
	}

	#[test]
	fn fixture_fuel_error() {
		assert_fixture_ctfe_error("ctfe_err_fuel", |kind| {
			matches!(kind, CtfeErrorKind::FuelExceeded)
		});
	}
}
