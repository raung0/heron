use std::collections::HashMap;

use crate::frontend::SourceLocation;
use crate::ir::{
	IrBlock, IrBlockId, IrFunctionTypeMap, IrGlobalId, IrLayoutTable, IrTypeId, IrTypeTable,
	IrVTableId, IrValueId,
};

#[derive(Clone, Debug, Default)]
pub struct IrProgram {
	pub modules: Vec<IrModule>,
	pub types: IrTypeTable,
	pub layouts: IrLayoutTable,
}

#[derive(Clone, Debug, Default)]
pub struct IrModule {
	pub id: String,
	pub source_file: String,
	pub functions: Vec<IrFunction>,
	pub globals: Vec<IrGlobal>,
	pub externs: Vec<IrExtern>,
	pub vtables: Vec<IrVTableDecl>,
}

#[derive(Clone, Debug, Default)]
pub struct IrFunction {
	pub name: String,
	pub linkage: IrLinkage,
	pub signature: IrTypeId,
	pub params: Vec<IrParam>,
	pub blocks: Vec<IrBlock>,
	pub entry: Option<IrBlockId>,
	pub value_types: IrFunctionTypeMap,
	pub local_names: HashMap<IrValueId, String>,
	pub location: Option<SourceLocation>,
}

#[derive(Clone, Debug)]
pub struct IrParam {
	pub name: String,
	pub ty: IrTypeId,
	pub value: crate::ir::IrValueId,
}

#[derive(Clone, Debug)]
pub struct IrGlobal {
	pub id: IrGlobalId,
	pub name: String,
	pub ty: IrTypeId,
	pub init: Option<IrGlobalInit>,
	pub mutable: bool,
	pub linkage: IrLinkage,
}

#[derive(Clone, Debug)]
pub enum IrGlobalInit {
	Zeroed,
	Bytes(Vec<u8>),
	VTableRef(IrVTableId),
}

#[derive(Clone, Debug)]
pub struct IrExtern {
	pub name: String,
	pub sig: IrTypeId,
}

#[derive(Clone, Debug)]
pub struct IrVTableDecl {
	pub id: IrVTableId,
	pub name: String,
	pub interface_ty: IrTypeId,
	pub concrete_ty: IrTypeId,
	pub methods: Vec<IrVTableMethod>,
}

#[derive(Clone, Debug)]
pub struct IrVTableMethod {
	pub name: String,
	pub sig: IrTypeId,
	pub impl_symbol: String,
	pub slot: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum IrLinkage {
	#[default]
	Private,
	Public,
	External,
}

#[derive(Clone, Debug, Default)]
pub struct IrSymbolIndex {
	pub functions: HashMap<String, usize>,
	pub globals: HashMap<String, usize>,
	pub vtables: HashMap<String, usize>,
}

impl IrModule {
	pub fn symbol_index(&self) -> IrSymbolIndex {
		let mut index = IrSymbolIndex::default();
		for (idx, f) in self.functions.iter().enumerate() {
			index.functions.insert(f.name.clone(), idx);
		}
		for (idx, g) in self.globals.iter().enumerate() {
			index.globals.insert(g.name.clone(), idx);
		}
		for (idx, vt) in self.vtables.iter().enumerate() {
			index.vtables.insert(vt.name.clone(), idx);
		}
		index
	}
}
