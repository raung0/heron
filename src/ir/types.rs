use std::collections::HashMap;

use crate::ir::{IrTypeId, IrValueId};

#[derive(Clone, Debug)]
pub struct IrTypeTable {
	pub types: Vec<IrType>,
}

impl IrTypeTable {
	pub fn new() -> Self {
		Self { types: Vec::new() }
	}

	pub fn add(&mut self, ty: IrType) -> IrTypeId {
		let id = IrTypeId(self.types.len() as u32);
		self.types.push(ty);
		id
	}

	pub fn get(&self, id: IrTypeId) -> &IrType {
		&self.types[id.0 as usize]
	}
}

impl Default for IrTypeTable {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Clone, Debug)]
pub enum IrType {
	Void,
	Bool,
	Int { bits: u16, signed: bool },
	Float32,
	Float64,
	Ptr { to: IrTypeId },
	Array { elem: IrTypeId, len: usize },
	Slice { elem: IrTypeId },
	Struct(IrStructType),
	RawUnion(IrRawUnionType),
	Union(IrTaggedUnionType),
	Enum(IrEnumType),
	Interface(IrInterfaceType),
	FnSig(IrFnSig),
	Opaque { name: String },
}

#[derive(Clone, Debug)]
pub struct IrStructType {
	pub name: Option<String>,
	pub fields: Vec<IrFieldType>,
	pub extends: Vec<IrTypeId>,
}

#[derive(Clone, Debug)]
pub struct IrRawUnionType {
	pub name: Option<String>,
	pub fields: Vec<IrFieldType>,
}

#[derive(Clone, Debug)]
pub struct IrTaggedUnionType {
	pub name: Option<String>,
	pub variants: Vec<IrTypeId>,
}

#[derive(Clone, Debug)]
pub struct IrEnumType {
	pub name: Option<String>,
	pub variants: Vec<IrEnumVariantType>,
}

#[derive(Clone, Debug)]
pub struct IrInterfaceType {
	pub name: Option<String>,
	pub methods: Vec<IrInterfaceMethodSig>,
}

#[derive(Clone, Debug)]
pub struct IrFieldType {
	pub name: String,
	pub ty: IrTypeId,
	pub public: bool,
}

#[derive(Clone, Debug)]
pub struct IrEnumVariantType {
	pub name: String,
	pub payload: Option<IrTypeId>,
}

#[derive(Clone, Debug)]
pub struct IrInterfaceMethodSig {
	pub name: String,
	pub sig: IrTypeId,
}

#[derive(Clone, Debug)]
pub struct IrFnSig {
	pub params: Vec<IrTypeId>,
	pub ret: IrTypeId,
	pub variadic: bool,
}

#[derive(Clone, Debug)]
pub struct IrLayoutTable {
	pub by_type: HashMap<IrTypeId, IrLayout>,
}

impl IrLayoutTable {
	pub fn new() -> Self {
		Self {
			by_type: HashMap::new(),
		}
	}
}

impl Default for IrLayoutTable {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Clone, Debug)]
pub struct IrLayout {
	pub size: usize,
	pub align: usize,
	pub fields: Vec<IrFieldLayout>,
	pub tag: Option<IrTagLayout>,
	pub payload_size: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct IrFieldLayout {
	pub field: String,
	pub offset: usize,
	pub ty: IrTypeId,
}

#[derive(Clone, Debug)]
pub struct IrTagLayout {
	pub offset: usize,
	pub bits: u16,
}

#[derive(Clone, Debug)]
pub struct IrFunctionTypeMap {
	pub values: HashMap<IrValueId, IrTypeId>,
}

impl IrFunctionTypeMap {
	pub fn new() -> Self {
		Self {
			values: HashMap::new(),
		}
	}
}

impl Default for IrFunctionTypeMap {
	fn default() -> Self {
		Self::new()
	}
}
