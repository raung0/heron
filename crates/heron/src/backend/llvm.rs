use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::ffi::{CStr, CString};
use std::fs;
use std::path::PathBuf;
use std::ptr;

use llvm_sys::core::*;
use llvm_sys::debuginfo::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;

use crate::backend::{Backend, BackendError, BackendOptions, ModuleArtifact, OptimizationLevel};
use crate::frontend::{Semantics, SourceLocation, TypedProgram};
use crate::ir::{
	IrBlockId, IrConst, IrInstKind, IrProgram, IrTerminator, IrType, IrTypeId, IrValueId,
};

pub struct LlvmBackend {
	initialized: bool,
}

thread_local! {
	static TARGET_POINTER_SIZE: Cell<usize> = const { Cell::new(std::mem::size_of::<*const ()>()) };
}

#[allow(unsafe_op_in_unsafe_fn)]
impl LlvmBackend {
	pub fn new() -> Self {
		Self { initialized: false }
	}

	fn init_targets(&mut self) {
		if self.initialized {
			return;
		}
		unsafe {
			LLVM_InitializeAllTargetInfos();
			LLVM_InitializeAllTargets();
			LLVM_InitializeAllTargetMCs();
			LLVM_InitializeAllAsmParsers();
			LLVM_InitializeAllAsmPrinters();
		}
		self.initialized = true;
	}

	fn module_object_path(options: &BackendOptions, module_id: &str) -> PathBuf {
		let file = format!("{}.o", module_id.replace('.', "__"));
		options.emit_dir.join("obj").join(file)
	}

	fn module_llvm_path(options: &BackendOptions, module_id: &str) -> PathBuf {
		let file = format!("{}.ll", module_id.replace('.', "__"));
		options.emit_dir.join("llvm").join(file)
	}

	fn function_symbol(module_id: &str, name: &str) -> String {
		format!("{}::{}", module_id, name)
	}

	fn split_source_path(file_path: &str) -> (String, String) {
		let path = std::path::Path::new(file_path);
		let filename = path
			.file_name()
			.map(|v| v.to_string_lossy().into_owned())
			.unwrap_or_else(|| file_path.to_string());
		let directory = path
			.parent()
			.map(|v| v.to_string_lossy().into_owned())
			.unwrap_or_else(|| ".".to_string());
		(filename, directory)
	}

	fn sanitized_name(name: &str) -> CString {
		CString::new(name)
			.unwrap_or_else(|_| CString::new("invalid").expect("valid static"))
	}

	fn target_triple(options: &BackendOptions) -> Result<CString, String> {
		if let Some(triple) = &options.target_triple {
			return CString::new(triple.as_str()).map_err(|_| {
				"target triple contains interior null byte".to_string()
			});
		}
		unsafe {
			let triple_ptr = LLVMGetDefaultTargetTriple();
			if triple_ptr.is_null() {
				return Err("failed to get default target triple".to_string());
			}
			let triple = CStr::from_ptr(triple_ptr).to_string_lossy().into_owned();
			LLVMDisposeMessage(triple_ptr);
			CString::new(triple).map_err(|_| {
				"default target triple contains interior null byte".to_string()
			})
		}
	}

	fn set_target_pointer_size(pointer_size: usize) {
		TARGET_POINTER_SIZE.with(|size| size.set(pointer_size.max(1)));
	}

	fn target_pointer_size() -> usize {
		TARGET_POINTER_SIZE.with(Cell::get)
	}

	fn align_up(value: usize, align: usize) -> usize {
		if align <= 1 {
			return value;
		}
		value.div_ceil(align).saturating_mul(align)
	}

	unsafe fn map_ir_type_to_debug_type(
		di_builder: LLVMDIBuilderRef,
		program: &IrProgram,
		ty: IrTypeId,
		cache: &mut HashMap<IrTypeId, LLVMMetadataRef>,
	) -> LLVMMetadataRef {
		if let Some(existing) = cache.get(&ty).copied() {
			return existing;
		}
		const DW_ATE_ADDRESS: LLVMDWARFTypeEncoding = 0x01;
		const DW_ATE_BOOLEAN: LLVMDWARFTypeEncoding = 0x02;
		const DW_ATE_FLOAT: LLVMDWARFTypeEncoding = 0x04;
		const DW_ATE_SIGNED: LLVMDWARFTypeEncoding = 0x05;
		const DW_ATE_UNSIGNED: LLVMDWARFTypeEncoding = 0x07;

		let mapped = match program.types.get(ty) {
			IrType::Bool => LLVMDIBuilderCreateBasicType(
				di_builder,
				Self::sanitized_name("bool").as_ptr(),
				4,
				8,
				DW_ATE_BOOLEAN,
				LLVMDIFlagZero,
			),
			IrType::Int { bits, signed } => {
				let name = if *signed {
					format!("i{bits}")
				} else {
					format!("u{bits}")
				};
				let name_c = Self::sanitized_name(&name);
				LLVMDIBuilderCreateBasicType(
					di_builder,
					name_c.as_ptr(),
					name.len(),
					u64::from(*bits),
					if *signed {
						DW_ATE_SIGNED
					} else {
						DW_ATE_UNSIGNED
					},
					LLVMDIFlagZero,
				)
			}
			IrType::Float32 => LLVMDIBuilderCreateBasicType(
				di_builder,
				Self::sanitized_name("f32").as_ptr(),
				3,
				32,
				DW_ATE_FLOAT,
				LLVMDIFlagZero,
			),
			IrType::Float64 => LLVMDIBuilderCreateBasicType(
				di_builder,
				Self::sanitized_name("f64").as_ptr(),
				3,
				64,
				DW_ATE_FLOAT,
				LLVMDIFlagZero,
			),
			IrType::Ptr { to } => {
				let pointee = Self::map_ir_type_to_debug_type(
					di_builder, program, *to, cache,
				);
				LLVMDIBuilderCreatePointerType(
					di_builder,
					pointee,
					64,
					64,
					0,
					Self::sanitized_name("ptr").as_ptr(),
					3,
				)
			}
			_ => LLVMDIBuilderCreateBasicType(
				di_builder,
				Self::sanitized_name("heron.value").as_ptr(),
				11,
				64,
				DW_ATE_ADDRESS,
				LLVMDIFlagZero,
			),
		};
		cache.insert(ty, mapped);
		mapped
	}

	fn approx_ir_layout(
		program: &IrProgram,
		ty: IrTypeId,
		cache: &mut HashMap<IrTypeId, (usize, usize)>,
		visiting: &mut HashSet<IrTypeId>,
		pointer_size: usize,
	) -> (usize, usize) {
		if let Some(layout) = cache.get(&ty).copied() {
			return layout;
		}
		if !visiting.insert(ty) {
			return (pointer_size, pointer_size);
		}

		let layout = match program.types.get(ty) {
			IrType::Void => (0, 1),
			IrType::Bool => (1, 1),
			IrType::Int { bits, .. } => {
				let size = usize::from(*bits).div_ceil(8).max(1);
				(size, size.min(16))
			}
			IrType::Float32 => (4, 4),
			IrType::Float64 => (8, 8),
			IrType::Ptr { .. } | IrType::FnSig(_) | IrType::Opaque { .. } => {
				(pointer_size, pointer_size)
			}
			IrType::Array { elem, len } => {
				let (elem_size, elem_align) = Self::approx_ir_layout(
					program,
					*elem,
					cache,
					visiting,
					pointer_size,
				);
				(elem_size.saturating_mul(*len), elem_align)
			}
			IrType::Slice { .. } | IrType::Interface(_) => {
				(pointer_size.saturating_mul(2), pointer_size)
			}
			IrType::Struct(s) => {
				let mut offset = 0usize;
				let mut max_align = 1usize;
				for field in &s.fields {
					let (field_size, field_align) = Self::approx_ir_layout(
						program,
						field.ty,
						cache,
						visiting,
						pointer_size,
					);
					max_align = max_align.max(field_align.max(1));
					offset = Self::align_up(offset, field_align.max(1));
					offset = offset.saturating_add(field_size);
				}
				(Self::align_up(offset, max_align), max_align)
			}
			IrType::RawUnion(u) => {
				let mut max_size = 1usize;
				let mut max_align = 1usize;
				for field in &u.fields {
					let (field_size, field_align) = Self::approx_ir_layout(
						program,
						field.ty,
						cache,
						visiting,
						pointer_size,
					);
					max_size = max_size.max(field_size);
					max_align = max_align.max(field_align.max(1));
				}
				(Self::align_up(max_size, max_align), max_align)
			}
			IrType::Union(u) => {
				let mut payload_size = 1usize;
				let mut payload_align = 1usize;
				for variant in &u.variants {
					let (variant_size, variant_align) = Self::approx_ir_layout(
						program,
						*variant,
						cache,
						visiting,
						pointer_size,
					);
					payload_size = payload_size.max(variant_size);
					payload_align = payload_align.max(variant_align.max(1));
				}
				let tag_size = 4usize;
				let tag_align = 4usize;
				let payload_offset = Self::align_up(tag_size, payload_align);
				let align = tag_align.max(payload_align);
				(
					Self::align_up(
						payload_offset.saturating_add(payload_size),
						align,
					),
					align,
				)
			}
			IrType::Enum(_) => (4, 4),
		};

		visiting.remove(&ty);
		cache.insert(ty, layout);
		layout
	}

	unsafe fn union_storage_type_for_variants(
		context: LLVMContextRef,
		program: &IrProgram,
		variant_tys: &[IrTypeId],
		cache: &mut HashMap<IrTypeId, LLVMTypeRef>,
		visiting: &mut HashSet<IrTypeId>,
	) -> LLVMTypeRef {
		let pointer_size = Self::target_pointer_size();
		if variant_tys.is_empty() {
			return LLVMInt8TypeInContext(context);
		}

		let mut layout_cache = HashMap::new();
		let mut layout_visiting = HashSet::new();
		let mut max_size = 1usize;
		let mut anchor_ty = variant_tys[0];
		let mut anchor_align = 1usize;
		for ty in variant_tys {
			let (size, align) = Self::approx_ir_layout(
				program,
				*ty,
				&mut layout_cache,
				&mut layout_visiting,
				pointer_size,
			);
			if align > anchor_align {
				anchor_ty = *ty;
				anchor_align = align;
			}
			max_size = max_size.max(size);
		}

		let anchor_llvm = Self::map_ir_type(context, program, anchor_ty, cache, visiting);
		let (anchor_size, _) = Self::approx_ir_layout(
			program,
			anchor_ty,
			&mut layout_cache,
			&mut layout_visiting,
			pointer_size,
		);
		if anchor_size >= max_size {
			return anchor_llvm;
		}

		let pad_len = max_size - anchor_size;
		LLVMStructTypeInContext(
			context,
			[
				anchor_llvm,
				LLVMArrayType2(LLVMInt8TypeInContext(context), pad_len as u64),
			]
			.as_mut_ptr(),
			2,
			0,
		)
	}

	unsafe fn map_ir_type(
		context: LLVMContextRef,
		program: &IrProgram,
		ty: IrTypeId,
		cache: &mut HashMap<IrTypeId, LLVMTypeRef>,
		visiting: &mut HashSet<IrTypeId>,
	) -> LLVMTypeRef {
		if let Some(cached) = cache.get(&ty).copied() {
			return cached;
		}
		if !visiting.insert(ty) {
			return LLVMPointerType(LLVMInt8TypeInContext(context), 0);
		}

		let mapped = match program.types.get(ty) {
			IrType::Void => LLVMVoidTypeInContext(context),
			IrType::Bool => LLVMInt1TypeInContext(context),
			IrType::Int { bits, .. } => LLVMIntTypeInContext(context, u32::from(*bits)),
			IrType::Float32 => LLVMFloatTypeInContext(context),
			IrType::Float64 => LLVMDoubleTypeInContext(context),
			IrType::Ptr { to } => LLVMPointerType(
				Self::map_ir_type(context, program, *to, cache, visiting),
				0,
			),
			IrType::Array { elem, len } => LLVMArrayType2(
				Self::map_ir_type(context, program, *elem, cache, visiting),
				u64::try_from(*len).unwrap_or(u64::MAX),
			),
			IrType::Slice { elem } => LLVMStructTypeInContext(
				context,
				[
					LLVMPointerType(
						Self::map_ir_type(
							context, program, *elem, cache, visiting,
						),
						0,
					),
					LLVMInt64TypeInContext(context),
				]
				.as_mut_ptr(),
				2,
				0,
			),
			IrType::Struct(s) => {
				let name = Self::sanitized_name(
					s.name.as_deref().unwrap_or("ir.struct"),
				);
				let opaque = LLVMStructCreateNamed(context, name.as_ptr());
				cache.insert(ty, opaque);
				let mut fields: Vec<LLVMTypeRef> = s
					.fields
					.iter()
					.map(|f| {
						Self::map_ir_type(
							context, program, f.ty, cache, visiting,
						)
					})
					.collect();
				LLVMStructSetBody(
					opaque,
					fields.as_mut_ptr(),
					fields.len() as u32,
					0,
				);
				opaque
			}
			IrType::RawUnion(raw) => {
				let field_tys: Vec<IrTypeId> =
					raw.fields.iter().map(|f| f.ty).collect();
				Self::union_storage_type_for_variants(
					context, program, &field_tys, cache, visiting,
				)
			}
			IrType::Union(tagged) => {
				let payload_ty = Self::union_storage_type_for_variants(
					context,
					program,
					&tagged.variants,
					cache,
					visiting,
				);
				LLVMStructTypeInContext(
					context,
					[LLVMInt32TypeInContext(context), payload_ty].as_mut_ptr(),
					2,
					0,
				)
			}
			IrType::Enum(_) => LLVMInt32TypeInContext(context),
			IrType::Interface(_) => LLVMStructTypeInContext(
				context,
				[
					LLVMPointerType(LLVMInt8TypeInContext(context), 0),
					LLVMPointerType(LLVMInt8TypeInContext(context), 0),
				]
				.as_mut_ptr(),
				2,
				0,
			),
			IrType::FnSig(sig) => {
				let mut params: Vec<LLVMTypeRef> = sig
					.params
					.iter()
					.map(|p| {
						Self::map_ir_type(
							context, program, *p, cache, visiting,
						)
					})
					.collect();
				LLVMFunctionType(
					Self::map_ir_type(
						context, program, sig.ret, cache, visiting,
					),
					params.as_mut_ptr(),
					params.len() as u32,
					i32::from(sig.variadic),
				)
			}
			IrType::Opaque { .. } => LLVMPointerType(LLVMInt8TypeInContext(context), 0),
		};

		cache.insert(ty, mapped);
		visiting.remove(&ty);
		mapped
	}

	unsafe fn lower_ir_const(
		context: LLVMContextRef,
		llvm_ty: LLVMTypeRef,
		c: &IrConst,
	) -> LLVMValueRef {
		match c {
			IrConst::Bool(v) => {
				LLVMConstInt(LLVMInt1TypeInContext(context), u64::from(*v), 0)
			}
			IrConst::Int(v) => LLVMConstInt(llvm_ty, *v as u64, 1),
			IrConst::UInt(v) => LLVMConstInt(llvm_ty, *v as u64, 0),
			IrConst::Float32(v) => LLVMConstReal(llvm_ty, f64::from(*v)),
			IrConst::Float64(v) => LLVMConstReal(llvm_ty, *v),
			IrConst::Null => LLVMConstNull(llvm_ty),
			IrConst::Undef => LLVMGetUndef(llvm_ty),
		}
	}

	unsafe fn get_or_create_ir_vtable_global(
		llvm_module: LLVMModuleRef,
		name: &str,
	) -> LLVMValueRef {
		let name_c = Self::sanitized_name(name);
		let existing = LLVMGetNamedGlobal(llvm_module, name_c.as_ptr());
		if !existing.is_null() {
			return existing;
		}
		let table_ty = LLVMArrayType2(LLVMPointerType(LLVMInt8Type(), 0), 256);
		let global = LLVMAddGlobal(llvm_module, table_ty, name_c.as_ptr());
		LLVMSetLinkage(global, llvm_sys::LLVMLinkage::LLVMExternalLinkage);
		LLVMSetInitializer(global, LLVMConstNull(table_ty));
		global
	}

	fn codegen_opt_level(options: &BackendOptions) -> LLVMCodeGenOptLevel {
		match options.optimization_level {
			OptimizationLevel::None => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
			OptimizationLevel::Size => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
			OptimizationLevel::Optimized => {
				LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault
			}
			OptimizationLevel::Aggressive => {
				LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive
			}
		}
	}

	unsafe fn set_debug_location_for_source(
		&self,
		context: LLVMContextRef,
		builder: LLVMBuilderRef,
		function: LLVMValueRef,
		location: &SourceLocation,
	) {
		let subprogram = LLVMGetSubprogram(function);
		if subprogram.is_null() {
			return;
		}
		let line = location.range.begin.1.max(1) as u32;
		let col = location.range.begin.0.max(1) as u32;
		let loc = LLVMDIBuilderCreateDebugLocation(
			context,
			line,
			col,
			subprogram,
			ptr::null_mut(),
		);
		LLVMSetCurrentDebugLocation2(builder, loc);
	}
}

impl Default for LlvmBackend {
	fn default() -> Self {
		Self::new()
	}
}

impl Backend for LlvmBackend {
	fn name(&self) -> &'static str {
		"llvm"
	}

	fn supports_ir(&self) -> bool {
		true
	}

	fn compile_module_ir(
		&mut self,
		module_id: &str,
		program: &IrProgram,
		_semantics: &Semantics,
		options: &BackendOptions,
	) -> Result<ModuleArtifact, Vec<BackendError>> {
		self.init_targets();

		let Some(module) = program.modules.iter().find(|m| m.id == module_id) else {
			return Err(vec![BackendError {
				module_id: module_id.to_string(),
				message: "module not found in IR program".to_string(),
				location: None,
			}]);
		};

		if let Err(err) = fs::create_dir_all(options.emit_dir.join("obj")) {
			return Err(vec![BackendError {
				module_id: module_id.to_string(),
				message: format!("failed to create object output dir: {err}"),
				location: None,
			}]);
		}
		if options.emit_llvm
			&& let Err(err) = fs::create_dir_all(options.emit_dir.join("llvm"))
		{
			return Err(vec![BackendError {
				module_id: module_id.to_string(),
				message: format!("failed to create LLVM output dir: {err}"),
				location: None,
			}]);
		}

		let module_name = Self::sanitized_name(module_id);
		let file_name = Self::sanitized_name(&module.source_file);
		let mut errors = Vec::new();

		unsafe {
			let context = LLVMContextCreate();
			let llvm_module =
				LLVMModuleCreateWithNameInContext(module_name.as_ptr(), context);
			LLVMSetSourceFileName(
				llvm_module,
				file_name.as_ptr(),
				module.source_file.len(),
			);
			let builder = LLVMCreateBuilderInContext(context);

			let mut di_builder = ptr::null_mut();
			let mut di_file = ptr::null_mut();
			if options.debug_info {
				di_builder = LLVMCreateDIBuilder(llvm_module);
				let debug_info_version = LLVMConstInt(
					LLVMInt32TypeInContext(context),
					u64::from(LLVMDebugMetadataVersion()),
					0,
				);
				LLVMAddModuleFlag(
					llvm_module,
					llvm_sys::LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorWarning,
					b"Debug Info Version".as_ptr().cast(),
					"Debug Info Version".len(),
					LLVMValueAsMetadata(debug_info_version),
				);
				let dwarf_version =
					LLVMConstInt(LLVMInt32TypeInContext(context), 4, 0);
				LLVMAddModuleFlag(
					llvm_module,
					llvm_sys::LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorWarning,
					b"Dwarf Version".as_ptr().cast(),
					"Dwarf Version".len(),
					LLVMValueAsMetadata(dwarf_version),
				);
				let (filename, directory) =
					Self::split_source_path(&module.source_file);
				let filename_c = Self::sanitized_name(&filename);
				let directory_c = Self::sanitized_name(&directory);
				di_file = LLVMDIBuilderCreateFile(
					di_builder,
					filename_c.as_ptr(),
					filename.len(),
					directory_c.as_ptr(),
					directory.len(),
				);
				let producer = Self::sanitized_name("heronc");
				let flags = Self::sanitized_name("");
				let split_name = Self::sanitized_name("");
				let sysroot = Self::sanitized_name("");
				let sdk = Self::sanitized_name("");
				let _compile_unit = LLVMDIBuilderCreateCompileUnit(
					di_builder,
					LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
					di_file,
					producer.as_ptr(),
					"heronc".len(),
					0,
					flags.as_ptr(),
					0,
					0,
					split_name.as_ptr(),
					0,
					LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
					0,
					0,
					0,
					sysroot.as_ptr(),
					0,
					sdk.as_ptr(),
					0,
				);
			}

			let triple = match Self::target_triple(options) {
				Ok(v) => v,
				Err(err) => {
					LLVMDisposeBuilder(builder);
					LLVMDisposeModule(llvm_module);
					LLVMContextDispose(context);
					return Err(vec![BackendError {
						module_id: module_id.to_string(),
						message: err,
						location: None,
					}]);
				}
			};
			LLVMSetTarget(llvm_module, triple.as_ptr());

			let mut target = ptr::null_mut();
			let mut target_err = ptr::null_mut();
			if LLVMGetTargetFromTriple(triple.as_ptr(), &mut target, &mut target_err)
				!= 0
			{
				let msg = if target_err.is_null() {
					"failed to resolve target from triple".to_string()
				} else {
					let s = CStr::from_ptr(target_err)
						.to_string_lossy()
						.into_owned();
					LLVMDisposeMessage(target_err);
					s
				};
				LLVMDisposeBuilder(builder);
				LLVMDisposeModule(llvm_module);
				LLVMContextDispose(context);
				return Err(vec![BackendError {
					module_id: module_id.to_string(),
					message: msg,
					location: None,
				}]);
			}

			let cpu = CString::new("generic").expect("valid static");
			let features = CString::new("").expect("valid static");
			let target_machine = LLVMCreateTargetMachine(
				target,
				triple.as_ptr(),
				cpu.as_ptr(),
				features.as_ptr(),
				Self::codegen_opt_level(options),
				LLVMRelocMode::LLVMRelocDefault,
				LLVMCodeModel::LLVMCodeModelDefault,
			);
			if target_machine.is_null() {
				LLVMDisposeBuilder(builder);
				LLVMDisposeModule(llvm_module);
				LLVMContextDispose(context);
				return Err(vec![BackendError {
					module_id: module_id.to_string(),
					message: "failed to create target machine".to_string(),
					location: None,
				}]);
			}

			let target_data = LLVMCreateTargetDataLayout(target_machine);
			let data_layout = LLVMCopyStringRepOfTargetData(target_data);
			if !data_layout.is_null() {
				LLVMSetDataLayout(llvm_module, data_layout);
				LLVMDisposeMessage(data_layout);
			}
			Self::set_target_pointer_size(LLVMPointerSize(target_data) as usize);

			let mut type_cache: HashMap<IrTypeId, LLVMTypeRef> = HashMap::new();
			let mut type_visiting: HashSet<IrTypeId> = HashSet::new();

			let mut function_map: HashMap<String, LLVMValueRef> = HashMap::new();
			let mut function_debug_locals: HashMap<
				String,
				HashMap<IrValueId, LLVMMetadataRef>,
			> = HashMap::new();
			for function in &module.functions {
				let llvm_fn_ty = Self::map_ir_type(
					context,
					program,
					function.signature,
					&mut type_cache,
					&mut type_visiting,
				);
				let symbol = Self::function_symbol(module_id, &function.name);
				let symbol_c = Self::sanitized_name(&symbol);
				let fn_val =
					LLVMAddFunction(llvm_module, symbol_c.as_ptr(), llvm_fn_ty);
				if options.debug_info && !di_builder.is_null() {
					let function_line = function
						.location
						.as_ref()
						.map(|loc| loc.range.begin.1.max(1) as u32)
						.unwrap_or(1);
					let function_col = function
						.location
						.as_ref()
						.map(|loc| loc.range.begin.0.max(1) as u32)
						.unwrap_or(1);
					let subroutine_type = LLVMDIBuilderCreateSubroutineType(
						di_builder,
						di_file,
						ptr::null_mut(),
						0,
						LLVMDIFlagZero,
					);
					let fn_name_c = Self::sanitized_name(&function.name);
					let linkage_c = Self::sanitized_name(&symbol);
					let subprogram = LLVMDIBuilderCreateFunction(
						di_builder,
						di_file,
						fn_name_c.as_ptr(),
						function.name.len(),
						linkage_c.as_ptr(),
						symbol.len(),
						di_file,
						function_line,
						subroutine_type,
						1,
						1,
						function_line,
						LLVMDIFlagZero,
						0,
					);
					if !subprogram.is_null() {
						LLVMSetSubprogram(fn_val, subprogram);
						let loc = LLVMDIBuilderCreateDebugLocation(
							context,
							function_line,
							function_col,
							subprogram,
							ptr::null_mut(),
						);
						LLVMSetCurrentDebugLocation2(builder, loc);
						let mut local_var_map = HashMap::new();
						let mut debug_type_cache: HashMap<
							IrTypeId,
							LLVMMetadataRef,
						> = HashMap::new();
						for (idx, param) in
							function.params.iter().enumerate()
						{
							let param_name_c =
								Self::sanitized_name(&param.name);
							let var_info = LLVMDIBuilderCreateParameterVariable(
								di_builder,
								subprogram,
								param_name_c.as_ptr(),
								param.name.len(),
								(idx + 1) as u32,
								di_file,
								function_line,
								Self::map_ir_type_to_debug_type(
									di_builder,
									program,
									param.ty,
									&mut debug_type_cache,
								),
								1,
								LLVMDIFlagZero,
							);
							local_var_map.insert(param.value, var_info);
						}
						function_debug_locals.insert(
							function.name.clone(),
							local_var_map,
						);
					}
				}
				function_map.insert(function.name.clone(), fn_val);
			}

			for function in &module.functions {
				let Some(&fn_val) = function_map.get(&function.name) else {
					continue;
				};
				let mut block_map: HashMap<IrBlockId, LLVMBasicBlockRef> =
					HashMap::new();
				for block in &function.blocks {
					let bb_name =
						Self::sanitized_name(&format!("bb{}", block.id.0));
					let bb = LLVMAppendBasicBlockInContext(
						context,
						fn_val,
						bb_name.as_ptr(),
					);
					block_map.insert(block.id, bb);
				}

				let mut value_map: HashMap<IrValueId, LLVMValueRef> =
					HashMap::new();
				let mut block_param_phis: HashMap<IrBlockId, Vec<LLVMValueRef>> =
					HashMap::new();
				let mut debug_shadow_slots: HashMap<IrValueId, LLVMValueRef> =
					HashMap::new();
				let mut debug_type_cache: HashMap<IrTypeId, LLVMMetadataRef> =
					HashMap::new();
				let mut empty_debug_local_map = HashMap::new();
				let debug_local_map = function_debug_locals
					.get_mut(&function.name)
					.unwrap_or(&mut empty_debug_local_map);
				let subprogram = if options.debug_info && !di_builder.is_null() {
					LLVMGetSubprogram(fn_val)
				} else {
					ptr::null_mut()
				};
				let di_expr = if options.debug_info && !di_builder.is_null() {
					LLVMDIBuilderCreateExpression(
						di_builder,
						ptr::null_mut(),
						0,
					)
				} else {
					ptr::null_mut()
				};
				let function_line = function
					.location
					.as_ref()
					.map(|loc| loc.range.begin.1.max(1) as u32)
					.unwrap_or(1);
				let function_col = function
					.location
					.as_ref()
					.map(|loc| loc.range.begin.0.max(1) as u32)
					.unwrap_or(1);
				let entry_bb = function
					.entry
					.and_then(|entry_id| block_map.get(&entry_id).copied());

				for (idx, param) in function.params.iter().enumerate() {
					let p = LLVMGetParam(fn_val, idx as u32);
					value_map.insert(param.value, p);
				}
				if options.debug_info
					&& !di_builder.is_null() && !subprogram.is_null()
					&& !function.params.is_empty() && let Some(entry_bb) = entry_bb
				{
					for param in &function.params {
						let Some(&param_val) = value_map.get(&param.value)
						else {
							continue;
						};
						let Some(&var_info) =
							debug_local_map.get(&param.value)
						else {
							continue;
						};
						let loc = LLVMDIBuilderCreateDebugLocation(
							context,
							function_line,
							function_col,
							subprogram,
							ptr::null_mut(),
						);
						LLVMDIBuilderInsertDbgValueRecordAtEnd(
							di_builder, param_val, var_info, di_expr,
							loc, entry_bb,
						);
					}
				}

				for block in &function.blocks {
					let Some(&bb) = block_map.get(&block.id) else {
						continue;
					};
					LLVMPositionBuilderAtEnd(builder, bb);
					let mut phis = Vec::new();
					for param in &block.params {
						let phi_name = Self::sanitized_name("phi");
						let phi_ty = Self::map_ir_type(
							context,
							program,
							param.ty,
							&mut type_cache,
							&mut type_visiting,
						);
						let phi = LLVMBuildPhi(
							builder,
							phi_ty,
							phi_name.as_ptr(),
						);
						value_map.insert(param.value, phi);
						phis.push(phi);
					}
					block_param_phis.insert(block.id, phis);
				}

				for block in &function.blocks {
					let Some(&bb) = block_map.get(&block.id) else {
						continue;
					};
					LLVMPositionBuilderAtEnd(builder, bb);

					for inst in &block.insts {
						if options.debug_info
							&& let Some(location) = &inst.location
						{
							self.set_debug_location_for_source(
								context, builder, fn_val, location,
							);
						}
						let lowered = match &inst.kind {
							IrInstKind::Const(c) => {
								inst.result.as_ref().map(|result| {
									let llvm_ty = Self::map_ir_type(
									context,
									program,
									result.ty,
									&mut type_cache,
									&mut type_visiting,
								);
									Self::lower_ir_const(
										context, llvm_ty, c,
									)
								})
							}
							IrInstKind::Move { src } => {
								value_map.get(src).copied()
							}
							IrInstKind::BinOp { op, lhs, rhs } => {
								let (Some(&lhs), Some(&rhs)) = (
									value_map.get(lhs),
									value_map.get(rhs),
								) else {
									errors.push(BackendError {
										module_id: module_id.to_string(),
										message: "missing operand for binop".to_string(),
										location: inst.location.clone(),
									});
									continue;
								};
								let name = Self::sanitized_name(
									"bintmp",
								);
								Some(match op {
									crate::ir::IrBinOp::Add => LLVMBuildAdd(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::Sub => LLVMBuildSub(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::Mul => LLVMBuildMul(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::Div => LLVMBuildSDiv(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::Rem => LLVMBuildSRem(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::BitAnd => LLVMBuildAnd(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::BitOr => LLVMBuildOr(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::BitXor => LLVMBuildXor(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::Shl => LLVMBuildShl(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::Shr => LLVMBuildLShr(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::And => LLVMBuildAnd(builder, lhs, rhs, name.as_ptr()),
									crate::ir::IrBinOp::Or => LLVMBuildOr(builder, lhs, rhs, name.as_ptr()),
								})
							}
							IrInstKind::Cmp { op, lhs, rhs } => {
								let (Some(&lhs), Some(&rhs)) = (
									value_map.get(lhs),
									value_map.get(rhs),
								) else {
									errors.push(BackendError {
										module_id: module_id.to_string(),
										message: "missing operand for cmp".to_string(),
										location: inst.location.clone(),
									});
									continue;
								};
								let pred = match op {
									crate::ir::IrCmpOp::Eq => llvm_sys::LLVMIntPredicate::LLVMIntEQ,
									crate::ir::IrCmpOp::Ne => llvm_sys::LLVMIntPredicate::LLVMIntNE,
									crate::ir::IrCmpOp::Lt => llvm_sys::LLVMIntPredicate::LLVMIntSLT,
									crate::ir::IrCmpOp::Le => llvm_sys::LLVMIntPredicate::LLVMIntSLE,
									crate::ir::IrCmpOp::Gt => llvm_sys::LLVMIntPredicate::LLVMIntSGT,
									crate::ir::IrCmpOp::Ge => llvm_sys::LLVMIntPredicate::LLVMIntSGE,
								};
								let name = Self::sanitized_name(
									"cmptmp",
								);
								Some(LLVMBuildICmp(
									builder,
									pred,
									lhs,
									rhs,
									name.as_ptr(),
								))
							}
							IrInstKind::Cast { value, to, .. } => {
								let Some(&value) =
									value_map.get(value)
								else {
									continue;
								};
								let to_ty = Self::map_ir_type(
									context,
									program,
									*to,
									&mut type_cache,
									&mut type_visiting,
								);
								Some(LLVMBuildBitCast(
									builder,
									value,
									to_ty,
									Self::sanitized_name(
										"cast",
									)
									.as_ptr(),
								))
							}
							IrInstKind::StackSlot { ty } => {
								let slot_ty = Self::map_ir_type(
									context,
									program,
									*ty,
									&mut type_cache,
									&mut type_visiting,
								);
								Some(LLVMBuildAlloca(
									builder,
									slot_ty,
									Self::sanitized_name(
										"slot",
									)
									.as_ptr(),
								))
							}
							IrInstKind::Load { ptr, ty } => {
								let Some(&ptr) = value_map.get(ptr)
								else {
									continue;
								};
								let load_ty = Self::map_ir_type(
									context,
									program,
									*ty,
									&mut type_cache,
									&mut type_visiting,
								);
								Some(LLVMBuildLoad2(
									builder,
									load_ty,
									ptr,
									Self::sanitized_name(
										"load",
									)
									.as_ptr(),
								))
							}
							IrInstKind::Store { ptr, value } => {
								if let (Some(&ptr), Some(&value)) = (
									value_map.get(ptr),
									value_map.get(value),
								) {
									LLVMBuildStore(
										builder, value, ptr,
									);
								}
								None
							}
							IrInstKind::GepField {
								base_ptr,
								field,
							} => {
								let Some(&base_ptr) =
									value_map.get(base_ptr)
								else {
									continue;
								};
								Some(LLVMBuildStructGEP2(
									builder,
									LLVMGetElementType(
										LLVMTypeOf(
											base_ptr,
										),
									),
									base_ptr,
									*field as u32,
									Self::sanitized_name(
										"gepfield",
									)
									.as_ptr(),
								))
							}
							IrInstKind::GepIndex {
								base_ptr,
								index,
							} => {
								let (Some(&base_ptr), Some(&index)) = (
									value_map.get(base_ptr),
									value_map.get(index),
								) else {
									continue;
								};
								let elem_ty = LLVMGetElementType(
									LLVMTypeOf(base_ptr),
								);
								Some(LLVMBuildGEP2(
									builder,
									elem_ty,
									base_ptr,
									[index].as_mut_ptr(),
									1,
									Self::sanitized_name(
										"gepidx",
									)
									.as_ptr(),
								))
							}
							IrInstKind::MakeStruct { ty, fields } => {
								let struct_ty = Self::map_ir_type(
									context,
									program,
									*ty,
									&mut type_cache,
									&mut type_visiting,
								);
								let mut agg =
									LLVMGetUndef(struct_ty);
								for (idx, field_val) in
									fields.iter().enumerate()
								{
									let Some(&field_val) =
										value_map.get(
											field_val,
										)
									else {
										continue;
									};
									agg = LLVMBuildInsertValue(builder, agg, field_val, idx as u32, Self::sanitized_name("insfield").as_ptr());
								}
								Some(agg)
							}
							IrInstKind::ExtractField { agg, field } => {
								let Some(&agg) = value_map.get(agg)
								else {
									continue;
								};
								Some(LLVMBuildExtractValue(
									builder,
									agg,
									*field as u32,
									Self::sanitized_name(
										"extfield",
									)
									.as_ptr(),
								))
							}
							IrInstKind::InsertField {
								agg,
								field,
								value,
							} => {
								let (Some(&agg), Some(&value)) = (
									value_map.get(agg),
									value_map.get(value),
								) else {
									continue;
								};
								Some(LLVMBuildInsertValue(
									builder,
									agg,
									value,
									*field as u32,
									Self::sanitized_name(
										"insfield",
									)
									.as_ptr(),
								))
							}
							IrInstKind::MakeUnion {
								ty,
								variant,
								payload,
							} => {
								let Some(&payload) =
									value_map.get(payload)
								else {
									continue;
								};
								let union_ty = Self::map_ir_type(
									context,
									program,
									*ty,
									&mut type_cache,
									&mut type_visiting,
								);
								let slot = LLVMBuildAlloca(
									builder,
									union_ty,
									Self::sanitized_name(
										"union.slot",
									)
									.as_ptr(),
								);
								let tag_ptr = LLVMBuildStructGEP2(
									builder,
									union_ty,
									slot,
									0,
									Self::sanitized_name(
										"union.tag.ptr",
									)
									.as_ptr(),
								);
								LLVMBuildStore(
									builder,
									LLVMConstInt(LLVMInt32TypeInContext(context), u64::from(*variant), 0),
									tag_ptr,
								);
								let payload_ptr = LLVMBuildStructGEP2(
									builder,
									union_ty,
									slot,
									1,
									Self::sanitized_name("union.payload.ptr").as_ptr(),
								);
								let payload_store_ptr = LLVMBuildBitCast(
									builder,
									payload_ptr,
									LLVMPointerType(LLVMTypeOf(payload), 0),
									Self::sanitized_name("union.payload.cast").as_ptr(),
								);
								LLVMBuildStore(
									builder,
									payload,
									payload_store_ptr,
								);
								Some(LLVMBuildLoad2(
									builder,
									union_ty,
									slot,
									Self::sanitized_name(
										"union.load",
									)
									.as_ptr(),
								))
							}
							IrInstKind::MakeEnum {
								ty: _,
								variant: _,
								payload: Some(_),
							} => {
								errors.push(BackendError {
									module_id: module_id.to_string(),
									message: "enum payloads are not supported in LLVM lowering"
										.to_string(),
									location: inst.location.clone(),
								});
								continue;
							}
							IrInstKind::MakeEnum {
								ty,
								variant,
								payload: None,
							} => {
								let enum_ty = Self::map_ir_type(
									context,
									program,
									*ty,
									&mut type_cache,
									&mut type_visiting,
								);
								Some(LLVMConstInt(
									enum_ty,
									u64::from(*variant),
									0,
								))
							}
							IrInstKind::GetTag { tagged } => {
								let Some(&tagged) =
									value_map.get(tagged)
								else {
									continue;
								};
								Some(LLVMBuildExtractValue(
									builder,
									tagged,
									0,
									Self::sanitized_name("tag")
										.as_ptr(),
								))
							}
							IrInstKind::GetPayloadPtr {
								tagged_ptr,
							} => {
								let Some(&tagged_ptr) =
									value_map.get(tagged_ptr)
								else {
									continue;
								};
								let tagged_elem =
									LLVMGetElementType(
										LLVMTypeOf(
											tagged_ptr,
										),
									);
								Some(LLVMBuildStructGEP2(
									builder,
									tagged_elem,
									tagged_ptr,
									1,
									Self::sanitized_name(
										"payload.ptr",
									)
									.as_ptr(),
								))
							}
							IrInstKind::PackInterface {
								data_ptr,
								vtable_global,
							} => {
								let Some(&data_ptr) =
									value_map.get(data_ptr)
								else {
									continue;
								};
								let vtable = Self::get_or_create_ir_vtable_global(llvm_module, vtable_global);
								let data_i8 = LLVMBuildBitCast(
									builder,
									data_ptr,
									LLVMPointerType(LLVMInt8TypeInContext(context), 0),
									Self::sanitized_name("ifc.data").as_ptr(),
								);
								let vtable_i8 = LLVMBuildBitCast(
									builder,
									vtable,
									LLVMPointerType(LLVMInt8TypeInContext(context), 0),
									Self::sanitized_name("ifc.vtbl").as_ptr(),
								);
								let iface_ty =
									if let Some(result) =
										inst.result.as_ref()
									{
										Self::map_ir_type(context, program, result.ty, &mut type_cache, &mut type_visiting)
									} else {
										LLVMStructTypeInContext(
										context,
										[
											LLVMPointerType(LLVMInt8TypeInContext(context), 0),
											LLVMPointerType(LLVMInt8TypeInContext(context), 0),
										]
										.as_mut_ptr(),
										2,
										0,
									)
									};
								let mut iface =
									LLVMGetUndef(iface_ty);
								iface = LLVMBuildInsertValue(
									builder,
									iface,
									data_i8,
									0,
									Self::sanitized_name(
										"ifc.ins.data",
									)
									.as_ptr(),
								);
								iface = LLVMBuildInsertValue(
									builder,
									iface,
									vtable_i8,
									1,
									Self::sanitized_name(
										"ifc.ins.vtbl",
									)
									.as_ptr(),
								);
								Some(iface)
							}
							IrInstKind::InterfaceMethodPtr {
								iface,
								method_index,
								sig,
							} => {
								let Some(&iface) =
									value_map.get(iface)
								else {
									continue;
								};
								let vtable_i8 = LLVMBuildExtractValue(builder, iface, 1, Self::sanitized_name("ifc.vtbl").as_ptr());
								let vtable_ptr = LLVMBuildBitCast(
									builder,
									vtable_i8,
									LLVMPointerType(LLVMPointerType(LLVMInt8TypeInContext(context), 0), 0),
									Self::sanitized_name("ifc.vtbl.ptr").as_ptr(),
								);
								let slot_ptr = LLVMBuildGEP2(
									builder,
									LLVMPointerType(LLVMInt8TypeInContext(context), 0),
									vtable_ptr,
									[LLVMConstInt(LLVMInt64TypeInContext(context), u64::from(*method_index), 0)].as_mut_ptr(),
									1,
									Self::sanitized_name("ifc.slot.ptr").as_ptr(),
								);
								let raw_fn = LLVMBuildLoad2(
									builder,
									LLVMPointerType(LLVMInt8TypeInContext(context), 0),
									slot_ptr,
									Self::sanitized_name("ifc.fn.raw").as_ptr(),
								);
								let fn_ty = Self::map_ir_type(
									context,
									program,
									*sig,
									&mut type_cache,
									&mut type_visiting,
								);
								Some(LLVMBuildBitCast(
									builder,
									raw_fn,
									LLVMPointerType(fn_ty, 0),
									Self::sanitized_name(
										"ifc.fn",
									)
									.as_ptr(),
								))
							}
							IrInstKind::MemCpy {
								dst,
								src,
								bytes,
								..
							} => {
								if let (
									Some(&dst),
									Some(&src),
									Some(&bytes),
								) = (
									value_map.get(dst),
									value_map.get(src),
									value_map.get(bytes),
								) {
									let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
									let dst_i8 = LLVMBuildBitCast(builder, dst, i8_ptr, Self::sanitized_name("memcpy.dst").as_ptr());
									let src_i8 = LLVMBuildBitCast(builder, src, i8_ptr, Self::sanitized_name("memcpy.src").as_ptr());
									let mut params = [i8_ptr, i8_ptr, LLVMInt64TypeInContext(context)];
									let memcpy_ty = LLVMFunctionType(LLVMVoidTypeInContext(context), params.as_mut_ptr(), 3, 0);
									let memcpy = LLVMAddFunction(llvm_module, Self::sanitized_name("memcpy").as_ptr(), memcpy_ty);
									let mut args = [
										dst_i8, src_i8,
										bytes,
									];
									LLVMBuildCall2(builder, memcpy_ty, memcpy, args.as_mut_ptr(), 3, Self::sanitized_name("memcpy.call").as_ptr());
								}
								None
							}
							IrInstKind::MemSet {
								dst,
								byte,
								bytes,
								..
							} => {
								if let (
									Some(&dst),
									Some(&byte),
									Some(&bytes),
								) = (
									value_map.get(dst),
									value_map.get(byte),
									value_map.get(bytes),
								) {
									let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
									let dst_i8 = LLVMBuildBitCast(builder, dst, i8_ptr, Self::sanitized_name("memset.dst").as_ptr());
									let mut params = [i8_ptr, LLVMInt32TypeInContext(context), LLVMInt64TypeInContext(context)];
									let memset_ty = LLVMFunctionType(LLVMVoidTypeInContext(context), params.as_mut_ptr(), 3, 0);
									let memset = LLVMAddFunction(llvm_module, Self::sanitized_name("memset").as_ptr(), memset_ty);
									let byte_i32 = LLVMBuildIntCast(builder, byte, LLVMInt32TypeInContext(context), Self::sanitized_name("memset.byte").as_ptr());
									let mut args = [
										dst_i8, byte_i32,
										bytes,
									];
									LLVMBuildCall2(builder, memset_ty, memset, args.as_mut_ptr(), 3, Self::sanitized_name("memset.call").as_ptr());
								}
								None
							}
							IrInstKind::MemCmp { lhs, rhs, bytes } => {
								let (
									Some(&lhs),
									Some(&rhs),
									Some(&bytes),
								) = (
									value_map.get(lhs),
									value_map.get(rhs),
									value_map.get(bytes),
								)
								else {
									continue;
								};
								let i8_ptr = LLVMPointerType(
									LLVMInt8TypeInContext(
										context,
									),
									0,
								);
								let lhs_i8 = LLVMBuildBitCast(
									builder,
									lhs,
									i8_ptr,
									Self::sanitized_name(
										"memcmp.lhs",
									)
									.as_ptr(),
								);
								let rhs_i8 = LLVMBuildBitCast(
									builder,
									rhs,
									i8_ptr,
									Self::sanitized_name(
										"memcmp.rhs",
									)
									.as_ptr(),
								);
								let mut params = [
									i8_ptr,
									i8_ptr,
									LLVMInt64TypeInContext(
										context,
									),
								];
								let memcmp_ty = LLVMFunctionType(
									LLVMInt32TypeInContext(
										context,
									),
									params.as_mut_ptr(),
									3,
									0,
								);
								let memcmp = LLVMAddFunction(
									llvm_module,
									Self::sanitized_name(
										"memcmp",
									)
									.as_ptr(),
									memcmp_ty,
								);
								let mut args =
									[lhs_i8, rhs_i8, bytes];
								Some(LLVMBuildCall2(
									builder,
									memcmp_ty,
									memcmp,
									args.as_mut_ptr(),
									3,
									Self::sanitized_name(
										"memcmp.call",
									)
									.as_ptr(),
								))
							}
							IrInstKind::CallDirect {
								callee,
								sig,
								args,
							} => {
								let target_fn =
									if let Some(&target_fn) =
										function_map
											.get(callee)
									{
										target_fn
									} else {
										let fn_ty = Self::map_ir_type(
										context,
										program,
										*sig,
										&mut type_cache,
										&mut type_visiting,
									);
										let symbol =
											if callee
												.contains(
													"::",
												) {
												callee.clone()
											} else {
												Self::function_symbol(module_id, callee)
											};
										let symbol_c = Self::sanitized_name(&symbol);
										let declared = LLVMAddFunction(llvm_module, symbol_c.as_ptr(), fn_ty);
										function_map.insert(callee.clone(), declared);
										declared
									};
								let mut lowered_args: Vec<
									LLVMValueRef,
								> =
									args.iter()
										.filter_map(|arg| {
											value_map.get(arg).copied()
										})
										.collect();
								let fn_ty = Self::map_ir_type(
									context,
									program,
									*sig,
									&mut type_cache,
									&mut type_visiting,
								);
								Some(LLVMBuildCall2(
									builder,
									fn_ty,
									target_fn,
									lowered_args.as_mut_ptr(),
									lowered_args.len() as u32,
									Self::sanitized_name(
										"call",
									)
									.as_ptr(),
								))
							}
							IrInstKind::CallIndirect {
								callee,
								sig,
								args,
							} => {
								let Some(&callee) =
									value_map.get(callee)
								else {
									continue;
								};
								let mut lowered_args: Vec<
									LLVMValueRef,
								> =
									args.iter()
										.filter_map(|arg| {
											value_map.get(arg).copied()
										})
										.collect();
								let fn_ty = Self::map_ir_type(
									context,
									program,
									*sig,
									&mut type_cache,
									&mut type_visiting,
								);
								Some(LLVMBuildCall2(
									builder,
									fn_ty,
									callee,
									lowered_args.as_mut_ptr(),
									lowered_args.len() as u32,
									Self::sanitized_name(
										"icall",
									)
									.as_ptr(),
								))
							}
						};

						if let (Some(result), Some(value)) =
							(inst.result.as_ref(), lowered)
						{
							value_map.insert(result.id, value);
							if options.debug_info
								&& !di_builder.is_null() && !subprogram
								.is_null() && !di_expr.is_null()
								&& let Some(name) = function
									.local_names
									.get(&result.id)
							{
								let var_info =
									if let Some(&existing) =
										debug_local_map.get(
											&result.id,
										) {
										existing
									} else {
										let name_c = Self::sanitized_name(name);
										let line = inst
										.location
										.as_ref()
										.map(|loc| loc.range.begin.1.max(1) as u32)
										.unwrap_or(function_line);
										let created = LLVMDIBuilderCreateAutoVariable(
										di_builder,
										subprogram,
										name_c.as_ptr(),
										name.len(),
										di_file,
										line,
										Self::map_ir_type_to_debug_type(
											di_builder,
											program,
											result.ty,
											&mut debug_type_cache,
										),
										1,
										LLVMDIFlagZero,
										0,
									);
										debug_local_map
											.insert(
											result.id,
											created,
										);
										created
									};
								let loc =
									if let Some(location) =
										&inst.location
									{
										LLVMDIBuilderCreateDebugLocation(
										context,
										location.range.begin.1.max(1) as u32,
										location.range.begin.0.max(1) as u32,
										subprogram,
										ptr::null_mut(),
									)
									} else {
										LLVMGetCurrentDebugLocation2(builder)
									};
								if !loc.is_null() {
									let shadow_slot =
										if let Some(&slot) =
											debug_shadow_slots.get(&result.id)
										{
											slot
										} else {
											let entry_builder =
												LLVMCreateBuilderInContext(context);
											let Some(entry_bb) = entry_bb else {
												LLVMDisposeBuilder(entry_builder);
												continue;
											};
											let first = LLVMGetFirstInstruction(entry_bb);
											if first.is_null() {
												LLVMPositionBuilderAtEnd(entry_builder, entry_bb);
											} else {
												LLVMPositionBuilderBefore(entry_builder, first);
											}
											let llvm_ty = Self::map_ir_type(
												context,
												program,
												result.ty,
												&mut type_cache,
												&mut type_visiting,
											);
											let slot_name =
												Self::sanitized_name(&format!("dbg.slot.{name}"));
											let slot = LLVMBuildAlloca(
												entry_builder,
												llvm_ty,
												slot_name.as_ptr(),
											);
											LLVMDIBuilderInsertDeclareRecordAtEnd(
												di_builder,
												slot,
												var_info,
												di_expr,
												loc,
												entry_bb,
											);
											LLVMDisposeBuilder(entry_builder);
											debug_shadow_slots.insert(result.id, slot);
											slot
										};
									LLVMBuildStore(
										builder,
										value,
										shadow_slot,
									);
									LLVMDIBuilderInsertDbgValueRecordAtEnd(
										di_builder,
										value,
										var_info,
										di_expr,
										loc,
										bb,
									);
								}
							}
						}
					}

					if let Some(term) = &block.term {
						if options.debug_info
							&& let Some(location) = block
								.insts
								.last()
								.and_then(|inst| {
									inst.location.as_ref()
								}) {
							self.set_debug_location_for_source(
								context, builder, fn_val, location,
							);
						}
						match term {
							IrTerminator::Br(edge) => {
								let Some(&target_bb) =
									block_map.get(&edge.to)
								else {
									continue;
								};
								LLVMBuildBr(builder, target_bb);
								if let Some(phis) = block_param_phis
									.get(&edge.to)
								{
									for (idx, phi) in phis
										.iter()
										.enumerate()
									{
										let Some(arg) = edge.args.get(idx).and_then(|id| value_map.get(id)).copied() else { continue; };
										let mut val_arr =
											[arg];
										let mut bb_arr =
											[bb];
										LLVMAddIncoming(*phi, val_arr.as_mut_ptr(), bb_arr.as_mut_ptr(), 1);
									}
								}
							}
							IrTerminator::CondBr {
								cond,
								then_edge,
								else_edge,
							} => {
								let Some(&cond) =
									value_map.get(cond)
								else {
									continue;
								};
								let (
									Some(&then_bb),
									Some(&else_bb),
								) = (
									block_map
										.get(&then_edge.to),
									block_map
										.get(&else_edge.to),
								)
								else {
									continue;
								};
								LLVMBuildCondBr(
									builder, cond, then_bb,
									else_bb,
								);
								for edge in [then_edge, else_edge] {
									if let Some(phis) =
										block_param_phis
											.get(&edge
												.to)
									{
										for (idx, phi) in
											phis.iter()
												.enumerate(
												) {
											let Some(arg) = edge.args.get(idx).and_then(|id| value_map.get(id)).copied() else { continue; };
											let mut
											val_arr = [arg];
											let mut
											bb_arr = [bb];
											LLVMAddIncoming(*phi, val_arr.as_mut_ptr(), bb_arr.as_mut_ptr(), 1);
										}
									}
								}
							}
							IrTerminator::Ret(value) => {
								if let Some(value) = value {
									if let Some(&ret_val) =
										value_map.get(value)
									{
										LLVMBuildRet(
											builder,
											ret_val,
										);
									} else {
										LLVMBuildRetVoid(
											builder,
										);
									}
								} else {
									LLVMBuildRetVoid(builder);
								}
							}
							IrTerminator::Unreachable => {
								LLVMBuildUnreachable(builder);
							}
							IrTerminator::SwitchInt {
								discr,
								arms,
								default,
							} => {
								let Some(&discr_val) =
									value_map.get(discr)
								else {
									continue;
								};
								let Some(&default_bb) =
									block_map.get(&default.to)
								else {
									continue;
								};
								let switch = LLVMBuildSwitch(
									builder,
									discr_val,
									default_bb,
									arms.len() as u32,
								);
								for (value, edge) in arms {
									if let Some(&target_bb) =
										block_map
											.get(&edge
												.to)
									{
										LLVMAddCase(
											switch,
											LLVMConstInt(LLVMTypeOf(discr_val), *value as u64, 0),
											target_bb,
										);
										if let Some(phis) = block_param_phis.get(&edge.to) {
											for (idx, phi) in phis.iter().enumerate() {
												let Some(arg) = edge.args.get(idx).and_then(|id| value_map.get(id)).copied() else { continue; };
												let mut val_arr = [arg];
												let mut bb_arr = [bb];
												LLVMAddIncoming(*phi, val_arr.as_mut_ptr(), bb_arr.as_mut_ptr(), 1);
											}
										}
									}
								}
								if let Some(phis) = block_param_phis
									.get(&default.to)
								{
									for (idx, phi) in phis
										.iter()
										.enumerate()
									{
										let Some(arg) = default.args.get(idx).and_then(|id| value_map.get(id)).copied() else { continue; };
										let mut val_arr =
											[arg];
										let mut bb_arr =
											[bb];
										LLVMAddIncoming(*phi, val_arr.as_mut_ptr(), bb_arr.as_mut_ptr(), 1);
									}
								}
							}
						}
					}
				}
			}

			if !errors.is_empty() {
				LLVMDisposeTargetData(target_data);
				LLVMDisposeTargetMachine(target_machine);
				LLVMDisposeBuilder(builder);
				if options.debug_info && !di_builder.is_null() {
					LLVMDisposeDIBuilder(di_builder);
				}
				LLVMDisposeModule(llvm_module);
				LLVMContextDispose(context);
				return Err(errors);
			}

			let mut artifact = ModuleArtifact {
				module_id: module_id.to_string(),
				..ModuleArtifact::default()
			};

			if options.debug_info && !di_builder.is_null() {
				LLVMDIBuilderFinalize(di_builder);
			}

			if options.emit_llvm {
				let llvm_text_ptr = LLVMPrintModuleToString(llvm_module);
				if !llvm_text_ptr.is_null() {
					let llvm_text = CStr::from_ptr(llvm_text_ptr)
						.to_string_lossy()
						.into_owned();
					LLVMDisposeMessage(llvm_text_ptr);
					let out_path = Self::module_llvm_path(options, module_id);
					if let Err(err) = fs::write(&out_path, &llvm_text) {
						errors.push(BackendError {
							module_id: module_id.to_string(),
							message: format!(
								"failed to write LLVM IR file: {err}"
							),
							location: None,
						});
					} else {
						artifact.llvm_ir_path = Some(out_path);
						artifact.llvm_ir_text = Some(llvm_text);
					}
				}
			}

			if options.emit_obj {
				let out_path = Self::module_object_path(options, module_id);
				if let Some(parent) = out_path.parent()
					&& let Err(err) = fs::create_dir_all(parent)
				{
					errors.push(BackendError {
						module_id: module_id.to_string(),
						message: format!(
							"failed to create object dir: {err}"
						),
						location: None,
					});
				}
				let mut out_file = out_path.to_string_lossy().as_bytes().to_vec();
				out_file.push(0);
				let mut emit_err = ptr::null_mut();
				let emit_res = LLVMTargetMachineEmitToFile(
					target_machine,
					llvm_module,
					out_file.as_mut_ptr().cast(),
					LLVMCodeGenFileType::LLVMObjectFile,
					&mut emit_err,
				);
				if emit_res != 0 {
					let msg = if emit_err.is_null() {
						"failed to emit object file".to_string()
					} else {
						let s = CStr::from_ptr(emit_err)
							.to_string_lossy()
							.into_owned();
						LLVMDisposeMessage(emit_err);
						s
					};
					errors.push(BackendError {
						module_id: module_id.to_string(),
						message: msg,
						location: None,
					});
				} else {
					artifact.object_path = Some(out_path);
				}
			}

			LLVMDisposeTargetData(target_data);
			LLVMDisposeTargetMachine(target_machine);
			LLVMDisposeBuilder(builder);
			if options.debug_info && !di_builder.is_null() {
				LLVMDisposeDIBuilder(di_builder);
			}
			LLVMDisposeModule(llvm_module);
			LLVMContextDispose(context);

			if errors.is_empty() {
				Ok(artifact)
			} else {
				Err(errors)
			}
		}
	}

	fn compile_module(
		&mut self,
		module_id: &str,
		_program: &TypedProgram,
		_semantics: &Semantics,
		_options: &BackendOptions,
	) -> Result<ModuleArtifact, Vec<BackendError>> {
		Err(vec![BackendError {
			module_id: module_id.to_string(),
			message: "LLVM backend no longer supports Typed AST lowering; use Heron IR pipeline".to_string(),
			location: None,
		}])
	}
}
