use std::collections::HashMap;
use std::fmt;

use crate::frontend::{ModuleExports, ModuleId, ModuleImports, Operator, SourceLocation, Trivia};

pub type TypeId = usize;

#[derive(Clone, Debug)]
pub enum BuiltinType {
	Integer {
		name: String,
		bit_size: Option<u8>,
		signed: bool,
	},
	Float {
		name: String,
		bit_size: Option<u8>,
	},
	Bool,
	Rune,
	Void,
	Type,
}

#[derive(Clone, Debug)]
pub enum ResolvedType {
	Builtin(BuiltinType),
	Struct {
		name: String,
		module: ModuleId,
		extends: Vec<TypeId>,
		fields: Vec<FieldInfo>,
		methods: HashMap<String, MethodInfo>,
	},
	Enum {
		name: String,
		module: ModuleId,
		variants: Vec<EnumVariantInfo>,
	},
	Union {
		name: String,
		module: ModuleId,
		variants: Vec<TypeId>,
		methods: HashMap<String, MethodInfo>,
	},
	RawUnion {
		name: String,
		module: ModuleId,
		fields: Vec<FieldInfo>,
	},
	Newtype {
		name: String,
		module: ModuleId,
		underlying: TypeId,
	},
	Alias {
		name: String,
		module: ModuleId,
		underlying: TypeId,
	},
	Pointer {
		underlying: TypeId,
	},
	Slice {
		underlying: TypeId,
	},
	Array {
		size: TypeLevelExprKey,
		underlying: TypeId,
	},
	CArray {
		underlying: TypeId,
	},
	Reference {
		mutable: bool,
		lifetime: Option<char>,
		underlying: TypeId,
	},
	Fn {
		params: Vec<TypeId>,
		return_type: TypeId,
	},
	GenericInstance {
		base: TypeId,
		args: Vec<ResolvedGenericArg>,
	},
	TypeParam(String),
	UntypedInt,
	UntypedFloat,
	Unknown,
}

#[derive(Clone, Debug)]
pub enum ResolvedGenericArg {
	Type(TypeId),
	Expr(TypeLevelExprKey),
	Name(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeLevelExprKey {
	Ctfe(ConstExprKey),
	Expr(ExprKey),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKey {
	Id(String),
	DotId(String),
	Integer(u64),
	Float(u64),
	Char(char),
	String(String),
	UnaryPlus(Box<ExprKey>),
	UnaryMinus(Box<ExprKey>),
	Not(Box<ExprKey>),
	Bin {
		op: Operator,
		has_eq: bool,
		lhs: Box<ExprKey>,
		rhs: Box<ExprKey>,
	},
	Call {
		callee: Box<ExprKey>,
		args: Vec<ExprKey>,
	},
	GenericApply {
		target: Box<ExprKey>,
		args: Vec<GenericExprArgKey>,
	},
	Index {
		target: Box<ExprKey>,
		indices: Vec<ExprKey>,
	},
}

#[derive(Clone, Debug, PartialEq)]
pub enum GenericExprArgKey {
	Type(String),
	Expr(ExprKey),
	Name(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstExprKey {
	Void,
	Bool(bool),
	Integer(i128),
	Float(u64),
	Char(char),
	String(String),
	Array(Vec<ConstExprKey>),
	Slice(Vec<ConstExprKey>),
	Record(Vec<(String, ConstExprKey)>),
}

impl fmt::Display for ConstExprKey {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ConstExprKey::Void => write!(f, "void"),
			ConstExprKey::Bool(v) => write!(f, "bool:{v}"),
			ConstExprKey::Integer(v) => write!(f, "int:{v}"),
			ConstExprKey::Float(bits) => write!(f, "float:{bits}"),
			ConstExprKey::Char(v) => write!(f, "char:{v:?}"),
			ConstExprKey::String(v) => write!(f, "string:{v:?}"),
			ConstExprKey::Array(values) => {
				write!(f, "array:[")?;
				for (idx, value) in values.iter().enumerate() {
					if idx > 0 {
						write!(f, ",")?;
					}
					write!(f, "{value}")?;
				}
				write!(f, "]")
			}
			ConstExprKey::Slice(values) => {
				write!(f, "slice:[")?;
				for (idx, value) in values.iter().enumerate() {
					if idx > 0 {
						write!(f, ",")?;
					}
					write!(f, "{value}")?;
				}
				write!(f, "]")
			}
			ConstExprKey::Record(entries) => {
				write!(f, "record:{{")?;
				for (idx, (name, value)) in entries.iter().enumerate() {
					if idx > 0 {
						write!(f, ",")?;
					}
					write!(f, "{name}={value}")?;
				}
				write!(f, "}}")
			}
		}
	}
}

impl fmt::Display for TypeLevelExprKey {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TypeLevelExprKey::Ctfe(value) => write!(f, "ctfe:{value}"),
			TypeLevelExprKey::Expr(expr) => write!(f, "{expr}"),
		}
	}
}

impl fmt::Display for GenericExprArgKey {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			GenericExprArgKey::Type(ty) => write!(f, "{ty}"),
			GenericExprArgKey::Expr(expr) => write!(f, "{expr}"),
			GenericExprArgKey::Name(name) => write!(f, "{name}"),
		}
	}
}

impl fmt::Display for ExprKey {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ExprKey::Id(name) | ExprKey::DotId(name) => write!(f, "{name}"),
			ExprKey::Integer(value) => write!(f, "{value}"),
			ExprKey::Float(bits) => write!(f, "float:{bits}"),
			ExprKey::Char(value) => write!(f, "'{value}'"),
			ExprKey::String(value) => write!(f, "{value:?}"),
			ExprKey::UnaryPlus(inner) => write!(f, "(+{inner})"),
			ExprKey::UnaryMinus(inner) => write!(f, "(-{inner})"),
			ExprKey::Not(inner) => write!(f, "(!{inner})"),
			ExprKey::Bin {
				op,
				has_eq,
				lhs,
				rhs,
			} => {
				let mut op_text = format!("{op}");
				if *has_eq {
					op_text.push('=');
				}
				write!(f, "({lhs} {op_text} {rhs})")
			}
			ExprKey::Call { callee, args } => {
				write!(f, "{callee}(")?;
				for (idx, arg) in args.iter().enumerate() {
					if idx > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{arg}")?;
				}
				write!(f, ")")
			}
			ExprKey::GenericApply { target, args } => {
				write!(f, "{target}<")?;
				for (idx, arg) in args.iter().enumerate() {
					if idx > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{arg}")?;
				}
				write!(f, ">")
			}
			ExprKey::Index { target, indices } => {
				write!(f, "{target}[")?;
				for (idx, index) in indices.iter().enumerate() {
					if idx > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{index}")?;
				}
				write!(f, "]")
			}
		}
	}
}

#[derive(Clone, Debug)]
pub struct FieldInfo {
	pub name: String,
	pub ty: TypeId,
	pub public: bool,
}

#[derive(Clone, Debug)]
pub struct MethodInfo {
	pub ty: TypeId,
	pub public: bool,
}

#[derive(Clone, Debug)]
pub struct EnumVariantInfo {
	pub name: String,
	pub ty: Option<TypeId>,
}

#[derive(Clone, Debug)]
pub struct TypeArena {
	pub types: Vec<ResolvedType>,
}

impl TypeArena {
	pub fn new() -> Self {
		Self { types: Vec::new() }
	}

	pub fn add(&mut self, ty: ResolvedType) -> TypeId {
		let id = self.types.len();
		self.types.push(ty);
		id
	}

	pub fn get(&self, id: TypeId) -> &ResolvedType {
		&self.types[id]
	}

	pub fn get_mut(&mut self, id: TypeId) -> &mut ResolvedType {
		&mut self.types[id]
	}
}

impl Default for TypeArena {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Clone, Debug)]
pub enum TypedGenericArg {
	Type(TypeId),
	Expr(String),
	Name(String),
}

#[derive(Clone, Debug)]
pub enum TypedGenericParam {
	Lifetime(char),
	Type {
		names: Vec<String>,
		constraint: Option<TypeId>,
	},
	Value {
		names: Vec<String>,
		ty: TypeId,
	},
}

#[derive(Clone, Debug)]
pub struct TypedFnParam {
	pub names: Vec<String>,
	pub ty: Option<TypeId>,
	pub default: Option<Box<TypedAst>>,
}

#[derive(Clone, Debug)]
pub struct TypedEnsuresClause {
	pub binders: Vec<String>,
	pub condition: Box<TypedAst>,
}

#[derive(Clone, Debug)]
pub struct TypedPostClause {
	pub return_id: Option<String>,
	pub conditions: Vec<Box<TypedAst>>,
}

#[derive(Clone, Debug)]
pub struct TypedMatchBinder {
	pub by_ref: bool,
	pub mutable: bool,
	pub lifetime: Option<char>,
	pub name: String,
}

#[derive(Clone, Debug)]
pub enum TypedMatchCasePattern {
	Default,
	Exprs(Vec<Box<TypedAst>>),
	Type(TypeId),
}

#[derive(Clone, Debug)]
pub struct TypedMatchCase {
	pub pattern: TypedMatchCasePattern,
	pub guard: Option<Box<TypedAst>>,
	pub body: Box<TypedAst>,
}

#[derive(Clone, Debug)]
pub enum TypedFnBody {
	Block(Box<TypedAst>),
	Expr(Box<TypedAst>),
	Uninitialized,
}

#[derive(Clone, Debug)]
pub enum TypedInitializerItem {
	Positional(Box<TypedAst>),
	Named { name: String, value: Box<TypedAst> },
}

#[derive(Clone, Debug)]
pub struct TypedEnumVariant {
	pub name: String,
	pub value: Option<Box<TypedAst>>,
}

#[derive(Clone, Debug)]
pub enum TypedValue {
	Package {
		path: Vec<String>,
	},
	Use {
		path: Vec<String>,
		alias: Option<String>,
	},
	Id(String),
	String(String),
	Char(char),
	Integer(u64),
	Float(f64),
	BinExpr {
		op: Operator,
		lhs: Box<TypedAst>,
		rhs: Box<TypedAst>,
		has_eq: bool,
	},
	Not(Box<TypedAst>),
	UnaryPlus(Box<TypedAst>),
	UnaryMinus(Box<TypedAst>),
	Ref {
		mutable: bool,
		v: Box<TypedAst>,
	},
	Deref(Box<TypedAst>),
	Mut(Box<TypedAst>),
	Call {
		callee: Box<TypedAst>,
		args: Vec<Box<TypedAst>>,
	},
	NamedArg {
		name: String,
		value: Box<TypedAst>,
	},
	GenericApply {
		target: Box<TypedAst>,
		args: Vec<TypedGenericArg>,
	},
	InitializerList(Vec<TypedInitializerItem>),
	TypedInitializerList {
		ty: TypeId,
		items: Vec<TypedInitializerItem>,
	},
	PtrOf(Box<TypedAst>),
	Cast {
		ty: Option<TypeId>,
		value: Box<TypedAst>,
	},
	Transmute {
		ty: Option<TypeId>,
		value: Box<TypedAst>,
	},
	Index {
		target: Box<TypedAst>,
		indices: Vec<Box<TypedAst>>,
	},
	ExprList {
		items: Vec<Box<TypedAst>>,
		attributes: Vec<String>,
	},
	ExprListNoScope {
		items: Vec<Box<TypedAst>>,
		attributes: Vec<String>,
	},
	Return(Option<Box<TypedAst>>),
	Hide(String),
	Defer(Box<TypedAst>),
	DotId(String),
	Match {
		binder: Option<TypedMatchBinder>,
		scrutinee: Box<TypedAst>,
		cases: Vec<TypedMatchCase>,
	},
	If {
		cond: Box<TypedAst>,
		decl: Option<Box<TypedAst>>,
		body: Box<TypedAst>,
		else_: Option<Box<TypedAst>>,
	},
	While {
		cond: Box<TypedAst>,
		decl: Option<Box<TypedAst>>,
		body: Box<TypedAst>,
	},
	ForLoop {
		init: Option<Box<TypedAst>>,
		cond: Option<Box<TypedAst>>,
		step: Option<Box<TypedAst>>,
		body: Box<TypedAst>,
	},
	For {
		bindings: Vec<Box<TypedAst>>,
		iter: Box<TypedAst>,
		body: Box<TypedAst>,
	},
	Pub(Box<TypedAst>),
	Set(String, Box<TypedAst>),
	Declaration {
		name: String,
		value: Box<TypedAst>,
		mutable: bool,
	},
	DeclarationComptime(String, Box<TypedAst>),
	SetMulti {
		names: Vec<String>,
		values: Vec<Box<TypedAst>>,
	},
	DeclarationMulti {
		names: Vec<String>,
		types: Vec<TypeId>,
		values: Option<Vec<Box<TypedAst>>>,
		comptime: bool,
		mutable: bool,
	},
	Type(TypeId),
	Fn {
		attributes: Vec<String>,
		generics: Vec<TypedGenericParam>,
		params: Vec<TypedFnParam>,
		return_type: Option<TypeId>,
		pre: Vec<Box<TypedAst>>,
		post: Option<TypedPostClause>,
		where_clause: Option<Box<TypedAst>>,
		ensures: Vec<TypedEnsuresClause>,
		body: TypedFnBody,
	},
	Struct {
		attributes: Vec<String>,
		generics: Vec<TypedGenericParam>,
		extends: Vec<TypeId>,
		implements: Vec<TypeId>,
		body: Box<TypedAst>,
	},
	Interface {
		attributes: Vec<String>,
		generics: Vec<TypedGenericParam>,
		body: Box<TypedAst>,
	},
	Enum {
		variants: Vec<TypedEnumVariant>,
		implements: Vec<TypeId>,
	},
	Union {
		generics: Vec<TypedGenericParam>,
		variants: Vec<TypeId>,
		implements: Vec<TypeId>,
	},
	RawUnion {
		generics: Vec<TypedGenericParam>,
		body: Box<TypedAst>,
		implements: Vec<TypeId>,
	},
	Newtype {
		underlying: TypeId,
		constraint: Option<String>,
	},
	Alias {
		underlying: TypeId,
	},
}

#[derive(Clone, Debug)]
pub struct TypedAst {
	// TypedAst is owned so the typed program can outlive the parser AST without
	// propagating lifetimes through all later compiler passes.
	pub location: SourceLocation,
	pub trivia: Vec<Trivia>,
	pub v: TypedValue,
	pub ty: Option<TypeId>,
}

impl TypedAst {
	pub fn from(location: SourceLocation, v: TypedValue) -> Box<Self> {
		Box::new(Self {
			location,
			trivia: Vec::new(),
			v,
			ty: None,
		})
	}
}

#[derive(Clone)]
pub struct TypedModule {
	pub id: ModuleId,
	pub file_path: String,
	pub package_path: Vec<String>,
	pub ast: Box<TypedAst>,
	pub imports: ModuleImports,
	pub exports: ModuleExports,
	pub types: HashMap<String, TypeId>,
	pub values: HashMap<String, TypeId>,
}

#[derive(Clone)]
pub struct TypedProgram {
	pub entry: ModuleId,
	pub modules: HashMap<ModuleId, TypedModule>,
}
