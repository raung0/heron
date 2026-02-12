use crate::backend::{Backend, BackendError, BackendOptions, BuildCache, ModuleArtifact};
use crate::frontend::{Semantics, TypedProgram};
use crate::ir::{lower_typed_program_to_ir, verify_program};
use std::time::{Duration, Instant};

#[derive(Clone, Debug, Default)]
pub struct BackendTimings {
	pub total: Duration,
	pub cache_load: Duration,
	pub cache_plan: Duration,
	pub ir_lower: Duration,
	pub ir_verify: Duration,
	pub module_compile: Duration,
	pub module_reuse: Duration,
	pub cache_save: Duration,
	pub modules_compiled: usize,
	pub modules_reused: usize,
}

pub fn compile_modules<B: Backend>(
	backend: &mut B,
	program: &TypedProgram,
	semantics: &Semantics,
	options: &BackendOptions,
) -> Result<Vec<ModuleArtifact>, Vec<BackendError>> {
	compile_modules_timed(backend, program, semantics, options).map(|(artifacts, _)| artifacts)
}

pub fn compile_modules_timed<B: Backend>(
	backend: &mut B,
	program: &TypedProgram,
	semantics: &Semantics,
	options: &BackendOptions,
) -> Result<(Vec<ModuleArtifact>, BackendTimings), Vec<BackendError>> {
	let total_start = Instant::now();
	let mut timings = BackendTimings::default();

	let cache_load_start = Instant::now();
	let mut cache = BuildCache::load_or_create(options, backend.name());
	timings.cache_load = cache_load_start.elapsed();

	let cache_plan_start = Instant::now();
	cache.prune_modules(program);
	let plan = cache.compute_build_plan(program, semantics, backend.name(), options);
	timings.cache_plan = cache_plan_start.elapsed();

	let cache_trace = std::env::var("HERON_CACHE_TRACE").ok().as_deref() == Some("1");
	let ir_program = if backend.supports_ir() {
		let ir_lower_start = Instant::now();
		match lower_typed_program_to_ir(program, semantics) {
			Ok(ir) => {
				timings.ir_lower = ir_lower_start.elapsed();
				let ir_verify_start = Instant::now();
				if let Err(verify_errors) = verify_program(&ir) {
					timings.ir_verify = ir_verify_start.elapsed();
					timings.total = total_start.elapsed();
					let errors = verify_errors
						.into_iter()
						.map(|err| BackendError {
							module_id: err.module_id,
							message: if let Some(function) =
								err.function
							{
								format!(
									"IR verify in `{function}`: {}",
									err.message
								)
							} else {
								err.message
							},
							location: None,
						})
						.collect();
					return Err(errors);
				}
				timings.ir_verify = ir_verify_start.elapsed();
				Some(ir)
			}
			Err(ir_errors) => {
				timings.ir_lower = ir_lower_start.elapsed();
				timings.total = total_start.elapsed();
				let errors = ir_errors
					.into_iter()
					.map(|err| BackendError {
						module_id: err.module_id,
						message: err.message,
						location: None,
					})
					.collect();
				return Err(errors);
			}
		}
	} else {
		None
	};

	let mut artifacts = Vec::new();
	let mut errors = Vec::new();

	for module in plan {
		let must_compile = (options.emit_obj && module.needs_compile) || options.emit_llvm;
		if cache_trace {
			eprintln!(
				"[cache] {} {}",
				if must_compile { "compile" } else { "reuse" },
				module.module_id
			);
		}
		if must_compile {
			timings.modules_compiled += 1;
			let module_compile_start = Instant::now();
			let result = if let Some(ir_program) = ir_program.as_ref() {
				backend.compile_module_ir(
					&module.module_id,
					ir_program,
					semantics,
					options,
				)
			} else {
				backend.compile_module(
					&module.module_id,
					program,
					semantics,
					options,
				)
			};
			timings.module_compile += module_compile_start.elapsed();
			match result {
				Ok(artifact) => {
					if options.emit_obj
						&& let Some(object_path) =
							artifact.object_path.as_ref()
					{
						cache.update_module(
							&module.module_id,
							module.fingerprint.clone(),
							object_path,
						);
					}
					artifacts.push(artifact);
				}
				Err(mut module_errors) => {
					errors.append(&mut module_errors);
				}
			}
		} else {
			timings.modules_reused += 1;
			let module_reuse_start = Instant::now();
			let mut artifact = ModuleArtifact {
				module_id: module.module_id.clone(),
				..ModuleArtifact::default()
			};
			if options.emit_obj {
				artifact.object_path = Some(module.object_path.clone());
			}
			artifacts.push(artifact);
			timings.module_reuse += module_reuse_start.elapsed();
		}
	}

	if errors.is_empty() {
		let cache_save_start = Instant::now();
		if options.emit_obj
			&& let Err(err) = cache.save()
		{
			timings.cache_save = cache_save_start.elapsed();
			timings.total = total_start.elapsed();
			return Err(vec![BackendError {
				module_id: "<build-cache>".to_string(),
				message: err,
				location: None,
			}]);
		}
		timings.cache_save = cache_save_start.elapsed();
		timings.total = total_start.elapsed();
		Ok((artifacts, timings))
	} else {
		timings.total = total_start.elapsed();
		Err(errors)
	}
}

#[cfg(test)]
mod tests {
	use std::path::{Path, PathBuf};
	use std::time::{SystemTime, UNIX_EPOCH};

	use crate::backend::{
		Backend, BackendOptions, LlvmBackend, ModuleArtifact, compile_modules,
	};
	use crate::frontend::{
		Lexer, Parser, Semantics, TypedProgram, run_passes_with_modules_timed,
	};
	use crate::ir::IrProgram;

	fn load_typed_program(
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

	fn parse_entry(entry_file: &Path) -> Box<crate::frontend::AST> {
		let input = std::fs::read_to_string(entry_file).expect("read entry file");
		let mut lexer = Lexer::new(input, entry_file.to_string_lossy().to_string());
		let mut parser = Parser::new(&mut lexer).expect("parser init");
		let ast = parser.parse().expect("parse entry file");
		let errors = parser.take_errors();
		assert!(errors.is_empty(), "parse errors: {errors:?}");
		ast
	}

	fn compile_fixture_to_llvm_text(fixture: &str) -> String {
		let (typed, semantics, errors) = load_typed_program(fixture);
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let mut backend = LlvmBackend::new();
		let options = BackendOptions {
			emit_llvm: true,
			emit_obj: false,
			..BackendOptions::default()
		};
		let artifacts = compile_modules(&mut backend, &typed, &semantics, &options)
			.expect("llvm IR-path compile succeeds");
		artifacts
			.iter()
			.find_map(|artifact| artifact.llvm_ir_text.as_ref().cloned())
			.expect("llvm text emitted")
	}

	#[derive(Default)]
	struct MockBackend {
		support_ir: bool,
		ir_calls: usize,
		typed_calls: usize,
	}

	impl Backend for MockBackend {
		fn name(&self) -> &'static str {
			"mock"
		}

		fn supports_ir(&self) -> bool {
			self.support_ir
		}

		fn compile_module(
			&mut self,
			module_id: &str,
			_program: &TypedProgram,
			_semantics: &Semantics,
			_options: &BackendOptions,
		) -> Result<ModuleArtifact, Vec<crate::backend::BackendError>> {
			self.typed_calls += 1;
			Ok(ModuleArtifact {
				module_id: module_id.to_string(),
				..ModuleArtifact::default()
			})
		}

		fn compile_module_ir(
			&mut self,
			module_id: &str,
			_program: &IrProgram,
			_semantics: &Semantics,
			_options: &BackendOptions,
		) -> Result<ModuleArtifact, Vec<crate::backend::BackendError>> {
			self.ir_calls += 1;
			Ok(ModuleArtifact {
				module_id: module_id.to_string(),
				..ModuleArtifact::default()
			})
		}
	}

	#[test]
	fn uses_ir_backend_path_when_supported() {
		let (typed, semantics, errors) = load_typed_program("testdata/ir_lowering_void");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let mut backend = MockBackend {
			support_ir: true,
			..MockBackend::default()
		};
		let options = BackendOptions {
			emit_llvm: true,
			..BackendOptions::default()
		};

		let artifacts = compile_modules(&mut backend, &typed, &semantics, &options)
			.expect("compile through IR path");
		assert!(!artifacts.is_empty());
		assert!(backend.ir_calls > 0);
		assert_eq!(backend.typed_calls, 0);
	}

	#[test]
	fn uses_typed_backend_path_when_ir_not_supported() {
		let (typed, semantics, errors) = load_typed_program("testdata/ir_lowering_void");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let mut backend = MockBackend::default();
		let options = BackendOptions {
			emit_llvm: true,
			..BackendOptions::default()
		};

		let artifacts = compile_modules(&mut backend, &typed, &semantics, &options)
			.expect("compile through typed path");
		assert!(!artifacts.is_empty());
		assert!(backend.typed_calls > 0);
		assert_eq!(backend.ir_calls, 0);
	}

	#[test]
	fn llvm_backend_can_compile_via_ir_path_flag() {
		let (typed, semantics, errors) = load_typed_program("testdata/ir_lowering_void");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let mut backend = LlvmBackend::new();
		let options = BackendOptions {
			emit_llvm: true,
			emit_obj: false,
			..BackendOptions::default()
		};
		let result = compile_modules(&mut backend, &typed, &semantics, &options);

		let artifacts = result.expect("llvm IR-path compile succeeds");
		assert!(!artifacts.is_empty());
		assert!(artifacts
			.iter()
			.any(|artifact| artifact.llvm_ir_text.as_ref().is_some()));
	}

	#[test]
	fn llvm_backend_prefers_ir_by_default() {
		let backend = LlvmBackend::new();
		assert!(backend.supports_ir());
	}

	#[test]
	fn llvm_ir_path_compiles_interface_fixture() {
		let (typed, semantics, errors) =
			load_typed_program("testdata/backend_interface_multi_methods");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let mut backend = LlvmBackend::new();
		let options = BackendOptions {
			emit_llvm: true,
			emit_obj: false,
			..BackendOptions::default()
		};
		let result = compile_modules(&mut backend, &typed, &semantics, &options);

		assert!(
			result.is_ok(),
			"llvm IR path should compile interface fixture"
		);
	}

	#[test]
	fn llvm_ir_path_emits_object_file() {
		let (typed, semantics, errors) = load_typed_program("testdata/ir_lowering_void");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let stamp = SystemTime::now()
			.duration_since(UNIX_EPOCH)
			.expect("time")
			.as_nanos();
		let emit_dir = std::env::temp_dir().join(format!("heron-ir-obj-{stamp}"));

		let mut backend = LlvmBackend::new();
		let options = BackendOptions {
			emit_llvm: false,
			emit_obj: true,
			emit_dir: emit_dir.clone(),
			..BackendOptions::default()
		};
		let result = compile_modules(&mut backend, &typed, &semantics, &options);

		let artifacts = result.expect("llvm IR path object emission succeeds");
		assert!(!artifacts.is_empty());
		assert!(artifacts.iter().any(|artifact| artifact
			.object_path
			.as_ref()
			.is_some_and(|p| p.exists())));
		let _ = std::fs::remove_dir_all(emit_dir);
	}

	#[test]
	fn llvm_ir_path_compiles_union_match_fixture() {
		let (typed, semantics, errors) = load_typed_program("testdata/backend_match");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let mut backend = LlvmBackend::new();
		let options = BackendOptions {
			emit_llvm: true,
			emit_obj: false,
			..BackendOptions::default()
		};
		let result = compile_modules(&mut backend, &typed, &semantics, &options);

		assert!(
			result.is_ok(),
			"llvm IR path should compile union match fixture: {result:?}"
		);
	}

	#[test]
	fn llvm_ir_path_compiles_ctor_fixture() {
		let (typed, semantics, errors) = load_typed_program("testdata/backend_ctor_return");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let mut backend = LlvmBackend::new();
		let options = BackendOptions {
			emit_llvm: true,
			emit_obj: false,
			..BackendOptions::default()
		};
		let result = compile_modules(&mut backend, &typed, &semantics, &options);

		assert!(
			result.is_ok(),
			"llvm IR path should compile ctor fixture: {result:?}"
		);
	}

	#[test]
	fn llvm_ir_path_compiles_backend_fixture_suite() {
		let fixtures = [
			"testdata/backend_smoke",
			"testdata/backend_expr_block",
			"testdata/backend_method",
			"testdata/backend_interface",
			"testdata/backend_interface_multi_methods",
			"testdata/backend_interface_receiver_mix",
			"testdata/backend_match",
			"testdata/backend_ctor_return",
			"testdata/backend_interface_cross_module",
		];

		for fixture in fixtures {
			let (typed, semantics, errors) = load_typed_program(fixture);
			assert!(
				errors.is_empty(),
				"frontend errors for {fixture}: {errors:?}"
			);

			let mut backend = LlvmBackend::new();
			let options = BackendOptions {
				emit_llvm: true,
				emit_obj: false,
				..BackendOptions::default()
			};
			let result = compile_modules(&mut backend, &typed, &semantics, &options);

			assert!(
				result.is_ok(),
				"llvm IR path should compile fixture `{fixture}`: {result:?}"
			);
		}
	}

	#[test]
	fn llvm_ir_path_emits_debug_metadata() {
		let (typed, semantics, errors) = load_typed_program("testdata/backend_smoke");
		assert!(errors.is_empty(), "frontend errors: {errors:?}");

		let mut backend = LlvmBackend::new();
		let options = BackendOptions {
			emit_llvm: true,
			emit_obj: false,
			debug_info: true,
			..BackendOptions::default()
		};
		let artifacts = compile_modules(&mut backend, &typed, &semantics, &options)
			.expect("llvm IR path with debug info should compile");
		assert!(!artifacts.is_empty());
		let ir_text = artifacts
			.iter()
			.find_map(|artifact| artifact.llvm_ir_text.as_ref())
			.expect("llvm text emitted");
		assert!(
			ir_text.contains("!dbg"),
			"expected debug metadata markers in LLVM IR output"
		);
	}

	#[test]
	fn llvm_ir_path_emits_phi_for_if_merge_fixture() {
		let ir_text = compile_fixture_to_llvm_text("testdata/ir_lowering_if_merge");
		assert!(
			ir_text.contains(" phi "),
			"expected phi nodes for block-parameter merge in LLVM IR"
		);
	}

	#[test]
	fn llvm_ir_path_emits_tag_and_payload_ops_for_match_fixture() {
		let ir_text = compile_fixture_to_llvm_text("testdata/backend_match");
		assert!(
			ir_text.contains("extractvalue") && ir_text.contains("payload.ptr"),
			"expected tag extraction and payload addressing for match lowering in LLVM IR"
		);
	}

	#[test]
	fn llvm_ir_path_emits_interface_dispatch_shape() {
		let ir_text =
			compile_fixture_to_llvm_text("testdata/backend_interface_multi_methods");
		assert!(
			ir_text.contains("ifc.vtbl") && ir_text.contains("ifc.fn.raw"),
			"expected interface vtable extraction and indirect method loading in LLVM IR"
		);
	}
}
