use std::path::PathBuf;

use crate::frontend::{Semantics, SourceLocation, TypedProgram};
use crate::ir::IrProgram;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OptimizationLevel {
	None,
	Size,
	Optimized,
	Aggressive,
}

impl OptimizationLevel {
	pub fn as_str(self) -> &'static str {
		match self {
			Self::None => "none",
			Self::Size => "size",
			Self::Optimized => "optimized",
			Self::Aggressive => "aggressive",
		}
	}
}

#[derive(Clone, Debug)]
pub struct BackendOptions {
	pub emit_obj: bool,
	pub emit_llvm: bool,
	pub debug_info: bool,
	pub emit_dir: PathBuf,
	pub target_triple: Option<String>,
	pub optimization_level: OptimizationLevel,
}

impl Default for BackendOptions {
	fn default() -> Self {
		Self {
			emit_obj: false,
			emit_llvm: false,
			debug_info: false,
			emit_dir: PathBuf::from("target/heron-out"),
			target_triple: None,
			optimization_level: OptimizationLevel::None,
		}
	}
}

#[derive(Clone, Debug, Default)]
pub struct ModuleArtifact {
	pub module_id: String,
	pub object_path: Option<PathBuf>,
	pub llvm_ir_path: Option<PathBuf>,
	pub llvm_ir_text: Option<String>,
}

#[derive(Clone, Debug)]
pub struct BackendError {
	pub module_id: String,
	pub message: String,
	pub location: Option<SourceLocation>,
}

pub trait Backend {
	fn name(&self) -> &'static str;

	fn supports_ir(&self) -> bool {
		false
	}

	fn compile_module(
		&mut self,
		module_id: &str,
		program: &TypedProgram,
		semantics: &Semantics,
		options: &BackendOptions,
	) -> Result<ModuleArtifact, Vec<BackendError>>;

	fn compile_module_ir(
		&mut self,
		_module_id: &str,
		_program: &IrProgram,
		_semantics: &Semantics,
		_options: &BackendOptions,
	) -> Result<ModuleArtifact, Vec<BackendError>> {
		Err(vec![BackendError {
			module_id: "<backend>".to_string(),
			message: "backend does not implement IR compilation".to_string(),
			location: None,
		}])
	}
}
