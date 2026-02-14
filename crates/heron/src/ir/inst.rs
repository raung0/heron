use crate::frontend::SourceLocation;
use crate::ir::{IrBlockId, IrTypeId, IrValueId};

#[derive(Clone, Debug)]
pub struct IrValueDef {
	pub id: IrValueId,
	pub ty: IrTypeId,
}

#[derive(Clone, Debug)]
pub struct IrInst {
	pub result: Option<IrValueDef>,
	pub kind: IrInstKind,
	pub location: Option<SourceLocation>,
}

#[derive(Clone, Debug)]
pub enum IrInstKind {
	Const(IrConst),
	Move {
		src: IrValueId,
	},
	BinOp {
		op: IrBinOp,
		lhs: IrValueId,
		rhs: IrValueId,
	},
	Cmp {
		op: IrCmpOp,
		lhs: IrValueId,
		rhs: IrValueId,
	},
	Cast {
		kind: IrCastKind,
		value: IrValueId,
		to: IrTypeId,
	},
	StackSlot {
		ty: IrTypeId,
	},
	Load {
		ptr: IrValueId,
		ty: IrTypeId,
	},
	Store {
		ptr: IrValueId,
		value: IrValueId,
	},
	GepField {
		base_ptr: IrValueId,
		field: usize,
	},
	GepIndex {
		base_ptr: IrValueId,
		index: IrValueId,
	},
	MakeStruct {
		ty: IrTypeId,
		fields: Vec<IrValueId>,
	},
	ExtractField {
		agg: IrValueId,
		field: usize,
	},
	InsertField {
		agg: IrValueId,
		field: usize,
		value: IrValueId,
	},
	MakeEnum {
		ty: IrTypeId,
		variant: u32,
		payload: Option<IrValueId>,
	},
	MakeUnion {
		ty: IrTypeId,
		variant: u32,
		payload: IrValueId,
	},
	GetTag {
		tagged: IrValueId,
	},
	GetPayloadPtr {
		tagged_ptr: IrValueId,
	},
	CallDirect {
		callee: String,
		sig: IrTypeId,
		args: Vec<IrValueId>,
	},
	CallIndirect {
		callee: IrValueId,
		sig: IrTypeId,
		args: Vec<IrValueId>,
	},
	PackInterface {
		data_ptr: IrValueId,
		vtable_global: String,
	},
	InterfaceMethodPtr {
		iface: IrValueId,
		method_index: u32,
		sig: IrTypeId,
	},
	MemCpy {
		dst: IrValueId,
		src: IrValueId,
		bytes: IrValueId,
		align: usize,
	},
	MemSet {
		dst: IrValueId,
		byte: IrValueId,
		bytes: IrValueId,
		align: usize,
	},
	MemCmp {
		lhs: IrValueId,
		rhs: IrValueId,
		bytes: IrValueId,
	},
}

#[derive(Clone, Debug)]
pub enum IrConst {
	Bool(bool),
	Int(i128),
	UInt(u128),
	Float32(f32),
	Float64(f64),
	Null,
	Undef,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IrBinOp {
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	BitAnd,
	BitOr,
	BitXor,
	Shl,
	Shr,
	And,
	Or,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IrCmpOp {
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IrCastKind {
	IntToInt,
	IntToFloat,
	FloatToInt,
	FloatToFloat,
	PtrToInt,
	IntToPtr,
	PtrToPtr,
	Bitcast,
}

#[derive(Clone, Debug)]
pub struct IrBlock {
	pub id: IrBlockId,
	pub params: Vec<IrBlockParam>,
	pub insts: Vec<IrInst>,
	pub term: Option<IrTerminator>,
}

#[derive(Clone, Debug)]
pub struct IrBlockParam {
	pub value: IrValueId,
	pub ty: IrTypeId,
	pub name_hint: Option<String>,
}

#[derive(Clone, Debug)]
pub enum IrTerminator {
	Br(IrEdge),
	CondBr {
		cond: IrValueId,
		then_edge: IrEdge,
		else_edge: IrEdge,
	},
	SwitchInt {
		discr: IrValueId,
		arms: Vec<(u128, IrEdge)>,
		default: IrEdge,
	},
	Ret(Option<IrValueId>),
	Unreachable,
}

#[derive(Clone, Debug)]
pub struct IrEdge {
	pub to: IrBlockId,
	pub args: Vec<IrValueId>,
}

impl IrInstKind {
	pub fn operands(&self) -> Vec<IrValueId> {
		match self {
			Self::Const(_) | Self::StackSlot { .. } => Vec::new(),
			Self::Move { src }
			| Self::Load { ptr: src, .. }
			| Self::GetTag { tagged: src }
			| Self::GetPayloadPtr { tagged_ptr: src } => vec![*src],
			Self::BinOp { lhs, rhs, .. }
			| Self::Cmp { lhs, rhs, .. }
			| Self::Store {
				ptr: lhs,
				value: rhs,
			}
			| Self::GepIndex {
				base_ptr: lhs,
				index: rhs,
			} => vec![*lhs, *rhs],
			Self::MemCmp { lhs, rhs, bytes } => vec![*lhs, *rhs, *bytes],
			Self::Cast { value, .. } => vec![*value],
			Self::GepField { base_ptr, .. } => vec![*base_ptr],
			Self::MakeStruct { fields, .. } => fields.clone(),
			Self::ExtractField { agg, .. } => vec![*agg],
			Self::InsertField { agg, value, .. } => vec![*agg, *value],
			Self::MakeEnum { payload, .. } => payload.iter().copied().collect(),
			Self::MakeUnion { payload, .. } => vec![*payload],
			Self::CallDirect { args, .. } => args.clone(),
			Self::CallIndirect { callee, args, .. } => {
				let mut out = Vec::with_capacity(args.len() + 1);
				out.push(*callee);
				out.extend_from_slice(args);
				out
			}
			Self::PackInterface { data_ptr, .. } => vec![*data_ptr],
			Self::InterfaceMethodPtr { iface, .. } => vec![*iface],
			Self::MemCpy {
				dst, src, bytes, ..
			} => vec![*dst, *src, *bytes],
			Self::MemSet {
				dst, byte, bytes, ..
			} => vec![*dst, *byte, *bytes],
		}
	}
}

impl IrTerminator {
	pub fn successors(&self) -> Vec<IrEdge> {
		match self {
			Self::Br(edge) => vec![edge.clone()],
			Self::CondBr {
				then_edge,
				else_edge,
				..
			} => vec![then_edge.clone(), else_edge.clone()],
			Self::SwitchInt { arms, default, .. } => {
				let mut out = Vec::with_capacity(arms.len() + 1);
				for (_, edge) in arms {
					out.push(edge.clone());
				}
				out.push(default.clone());
				out
			}
			Self::Ret(_) | Self::Unreachable => Vec::new(),
		}
	}

	pub fn operands(&self) -> Vec<IrValueId> {
		match self {
			Self::Br(edge) => edge.args.clone(),
			Self::CondBr {
				cond,
				then_edge,
				else_edge,
			} => {
				let mut out = vec![*cond];
				out.extend_from_slice(&then_edge.args);
				out.extend_from_slice(&else_edge.args);
				out
			}
			Self::SwitchInt {
				discr,
				arms,
				default,
			} => {
				let mut out = vec![*discr];
				for (_, edge) in arms {
					out.extend_from_slice(&edge.args);
				}
				out.extend_from_slice(&default.args);
				out
			}
			Self::Ret(value) => value.iter().copied().collect(),
			Self::Unreachable => Vec::new(),
		}
	}
}
