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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LinkMode {
	Executable,
	SharedLibrary,
	StaticLibrary,
}

impl LinkMode {
	pub fn as_str(self) -> &'static str {
		match self {
			Self::Executable => "executable",
			Self::SharedLibrary => "shared-library",
			Self::StaticLibrary => "static-library",
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryFormat {
	Auto,
	Pe,
	Elf,
	MachO,
}

impl BinaryFormat {
	pub fn as_str(self) -> &'static str {
		match self {
			Self::Auto => "auto",
			Self::Pe => "pe",
			Self::Elf => "elf",
			Self::MachO => "mach-o",
		}
	}
}

pub fn resolve_binary_format(requested: BinaryFormat, target_triple: Option<&str>) -> BinaryFormat {
	if requested != BinaryFormat::Auto {
		return requested;
	}
	let triple = target_triple.unwrap_or("").to_ascii_lowercase();
	if triple.contains("windows") || triple.contains("mingw") || triple.contains("msvc") {
		BinaryFormat::Pe
	} else if triple.contains("apple")
		|| triple.contains("darwin")
		|| triple.contains("macos")
		|| triple.contains("ios")
	{
		BinaryFormat::MachO
	} else if triple.is_empty() {
		match std::env::consts::OS {
			"windows" => BinaryFormat::Pe,
			"macos" | "ios" => BinaryFormat::MachO,
			_ => BinaryFormat::Elf,
		}
	} else {
		BinaryFormat::Elf
	}
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
	pub link_mode: Option<LinkMode>,
	pub binary_format: BinaryFormat,
	pub out_path: Option<PathBuf>,
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
			link_mode: None,
			binary_format: BinaryFormat::Auto,
			out_path: None,
		}
	}
}

#[derive(Clone, Debug, Default)]
pub struct ModuleArtifact {
	pub module_id: String,
	pub object_path: Option<PathBuf>,
	pub llvm_ir_path: Option<PathBuf>,
	pub llvm_ir_text: Option<String>,
	pub linked_output_path: Option<PathBuf>,
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
