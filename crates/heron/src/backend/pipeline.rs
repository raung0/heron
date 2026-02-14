use crate::backend::{
	Backend, BackendError, BackendOptions, BinaryFormat, BuildCache, LinkMode, ModuleArtifact,
	resolve_binary_format,
};
use crate::frontend::{Semantics, TypedProgram};
use crate::ir::{lower_typed_program_to_ir, verify_program};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
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
	let needs_link = options.link_mode.is_some();
	let needs_objects = options.emit_obj || needs_link;
	let mut compile_options = options.clone();
	compile_options.emit_obj = needs_objects;

	let cache_load_start = Instant::now();
	let mut cache = BuildCache::load_or_create(options, backend.name());
	timings.cache_load = cache_load_start.elapsed();

	let cache_plan_start = Instant::now();
	cache.prune_modules(program);
	let plan = cache.compute_build_plan(program, semantics, backend.name(), &compile_options);
	timings.cache_plan = cache_plan_start.elapsed();

	let cache_trace = std::env::var("HERON_CACHE_TRACE").ok().as_deref() == Some("1");
	let ir_program = if backend.supports_ir() {
		let ir_lower_start = Instant::now();
		match lower_typed_program_to_ir(program, semantics) {
			Ok(ir) => {
				timings.ir_lower = ir_lower_start.elapsed();
				let ir_verify_start = Instant::now();
				if let Err(verify_errors) = verify_program(&ir) {
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
		let must_compile = options.no_cache
			|| (needs_objects && module.needs_compile)
			|| options.emit_llvm;
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
					&compile_options,
				)
			} else {
				backend.compile_module(
					&module.module_id,
					program,
					semantics,
					&compile_options,
				)
			};
			timings.module_compile += module_compile_start.elapsed();
			match result {
				Ok(artifact) => {
					if !options.no_cache
						&& needs_objects && let Some(object_path) =
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
			if needs_objects {
				artifact.object_path = Some(module.object_path.clone());
			}
			artifacts.push(artifact);
			timings.module_reuse += module_reuse_start.elapsed();
		}
	}

	if errors.is_empty() {
		let cache_save_start = Instant::now();
		if !options.no_cache
			&& needs_objects && let Err(err) = cache.save()
		{
			return Err(vec![BackendError {
				module_id: "<build-cache>".to_string(),
				message: err,
				location: None,
			}]);
		}
		timings.cache_save = cache_save_start.elapsed();
		if needs_link {
			match link_artifacts(&artifacts, options) {
				Ok(linked_path) => {
					artifacts.push(ModuleArtifact {
						module_id: "<link>".to_string(),
						linked_output_path: Some(linked_path),
						..ModuleArtifact::default()
					});
				}
				Err(link_error) => return Err(vec![link_error]),
			}
		}
		timings.total = total_start.elapsed();
		Ok((artifacts, timings))
	} else {
		Err(errors)
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LinkerFlavor {
	Mold,
	Lld,
	Ld,
}

struct LinkerTool {
	path: PathBuf,
	flavor: LinkerFlavor,
}

fn link_artifacts(
	artifacts: &[ModuleArtifact],
	options: &BackendOptions,
) -> Result<PathBuf, BackendError> {
	let Some(link_mode) = options.link_mode else {
		return Err(BackendError {
			module_id: "<link>".to_string(),
			message: "missing link mode".to_string(),
			location: None,
		});
	};
	let object_files: Vec<PathBuf> = artifacts
		.iter()
		.filter_map(|artifact| artifact.object_path.clone())
		.collect();
	if object_files.is_empty() {
		return Err(BackendError {
			module_id: "<link>".to_string(),
			message: "no object files available for final link".to_string(),
			location: None,
		});
	}

	let format = resolve_binary_format(options.binary_format, options.target_triple.as_deref());
	validate_binary_format(format, options.target_triple.as_deref())?;

	let out_path = options
		.out_path
		.clone()
		.unwrap_or_else(|| default_link_output_path(&options.emit_dir, link_mode, format));
	if let Some(parent) = out_path.parent()
		&& let Err(err) = fs::create_dir_all(parent)
	{
		return Err(BackendError {
			module_id: "<link>".to_string(),
			message: format!(
				"failed to create output dir `{}`: {err}",
				parent.display()
			),
			location: None,
		});
	}

	match link_mode {
		LinkMode::Executable | LinkMode::SharedLibrary => {
			let linker = discover_linker()?;
			run_linker(
				&linker,
				&object_files,
				&out_path,
				link_mode,
				format,
				options.debug_info,
				options.target_triple.as_deref(),
			)?;
			if options.debug_info {
				emit_debug_sidecar(&out_path, format)?;
			}
		}
		LinkMode::StaticLibrary => {
			create_static_archive(&object_files, &out_path, format)?;
		}
	}

	Ok(out_path)
}

fn validate_binary_format(
	format: BinaryFormat,
	target_triple: Option<&str>,
) -> Result<(), BackendError> {
	let inferred = resolve_binary_format(BinaryFormat::Auto, target_triple);
	if format != inferred {
		let target = target_triple.unwrap_or("<host-default>");
		return Err(BackendError {
			module_id: "<link>".to_string(),
			message: format!(
				"binary format `{}` does not match target `{}` (expected `{}`)",
				format.as_str(),
				target,
				inferred.as_str()
			),
			location: None,
		});
	}
	Ok(())
}

fn default_link_output_path(emit_dir: &Path, link_mode: LinkMode, format: BinaryFormat) -> PathBuf {
	let mut name = "a".to_string();
	match link_mode {
		LinkMode::Executable => {
			if format == BinaryFormat::Pe {
				name.push_str(".exe");
			}
		}
		LinkMode::SharedLibrary => {
			if format != BinaryFormat::Pe {
				name = format!("lib{name}");
			}
			name.push_str(match format {
				BinaryFormat::Pe => ".dll",
				BinaryFormat::Elf => ".so",
				BinaryFormat::MachO => ".dylib",
				BinaryFormat::Auto => unreachable!("auto format must be resolved"),
			});
		}
		LinkMode::StaticLibrary => {
			if format != BinaryFormat::Pe {
				name = format!("lib{name}");
			}
			name.push_str(match format {
				BinaryFormat::Pe => ".lib",
				BinaryFormat::Elf | BinaryFormat::MachO => ".a",
				BinaryFormat::Auto => unreachable!("auto format must be resolved"),
			});
		}
	}
	emit_dir.join("bin").join(name)
}

fn discover_linker() -> Result<LinkerTool, BackendError> {
	if let Some(path) = find_program_on_path("mold") {
		return Ok(LinkerTool {
			path,
			flavor: LinkerFlavor::Mold,
		});
	}
	if let Some(path) = find_program_on_path("ld.lld") {
		return Ok(LinkerTool {
			path,
			flavor: LinkerFlavor::Lld,
		});
	}
	if let Some(path) = find_program_on_path("ld64.lld") {
		return Ok(LinkerTool {
			path,
			flavor: LinkerFlavor::Lld,
		});
	}
	if let Some(path) = find_program_on_path("lld") {
		return Ok(LinkerTool {
			path,
			flavor: LinkerFlavor::Lld,
		});
	}
	if let Some(path) = find_program_on_path("ld") {
		return Ok(LinkerTool {
			path,
			flavor: LinkerFlavor::Ld,
		});
	}
	Err(BackendError {
		module_id: "<link>".to_string(),
		message: "no linker found on PATH (expected one of: mold, lld/ld.lld, ld)"
			.to_string(),
		location: None,
	})
}

fn find_program_on_path(name: &str) -> Option<PathBuf> {
	let path_var = env::var_os("PATH")?;
	for dir in env::split_paths(&path_var) {
		for candidate in executable_candidates(name) {
			let full = dir.join(candidate);
			if full.is_file() {
				return Some(full);
			}
		}
	}
	None
}

fn executable_candidates(name: &str) -> Vec<String> {
	let mut candidates = vec![name.to_string()];
	if cfg!(windows) && !name.contains('.') {
		candidates.push(format!("{name}.exe"));
		candidates.push(format!("{name}.cmd"));
		candidates.push(format!("{name}.bat"));
	}
	candidates
}

fn run_linker(
	linker: &LinkerTool,
	objects: &[PathBuf],
	out_path: &Path,
	link_mode: LinkMode,
	format: BinaryFormat,
	debug_info: bool,
	target_triple: Option<&str>,
) -> Result<(), BackendError> {
	if format == BinaryFormat::MachO {
		return run_macho_link_via_clang(
			objects,
			out_path,
			link_mode,
			debug_info,
			target_triple,
		);
	}

	let mut command = Command::new(&linker.path);
	if linker_is_generic_lld(&linker.path) {
		let flavor = match format {
			BinaryFormat::Pe => "link",
			BinaryFormat::MachO => "darwin",
			BinaryFormat::Elf | BinaryFormat::Auto => "gnu",
		};
		command.arg("-flavor").arg(flavor);
	}
	if debug_info && format == BinaryFormat::Pe && linker_supports_msvc_debug_flag(&linker.path)
	{
		command.arg("/DEBUG");
	}
	if matches!(link_mode, LinkMode::SharedLibrary) {
		if format == BinaryFormat::MachO {
			command.arg("-dylib");
		} else {
			command.arg("-shared");
		}
	}
	if let Some(triple) = target_triple
		&& matches!(linker.flavor, LinkerFlavor::Lld)
	{
		command.arg("-target").arg(triple);
	}
	if format == BinaryFormat::MachO {
		if let Some(arch) = target_arch(target_triple) {
			command.arg("-arch").arg(arch);
		}
		if let Some(sdk_root) = macos_sdk_root() {
			command.arg("-syslibroot").arg(sdk_root);
		}
		command.arg("-platform_version")
			.arg("macos")
			.arg("14.0")
			.arg("14.0");
		if matches!(link_mode, LinkMode::Executable) {
			let entry_symbol = if format == BinaryFormat::MachO {
				"_main::main"
			} else {
				"main::main"
			};
			command.arg("-e").arg(entry_symbol);
			command.arg("-lSystem");
		}
	}
	command.arg("-o").arg(out_path);
	for object in objects {
		command.arg(object);
	}

	let output = command.output().map_err(|err| BackendError {
		module_id: "<link>".to_string(),
		message: format!("failed to run linker `{}`: {err}", linker.path.display()),
		location: None,
	})?;
	if !output.status.success() {
		let stderr = String::from_utf8_lossy(&output.stderr);
		let stdout = String::from_utf8_lossy(&output.stdout);
		let details = if stderr.trim().is_empty() {
			stdout.trim().to_string()
		} else {
			stderr.trim().to_string()
		};
		return Err(BackendError {
			module_id: "<link>".to_string(),
			message: format!("linker failed: {details}"),
			location: None,
		});
	}
	Ok(())
}

fn run_macho_link_via_clang(
	objects: &[PathBuf],
	out_path: &Path,
	link_mode: LinkMode,
	debug_info: bool,
	target_triple: Option<&str>,
) -> Result<(), BackendError> {
	let driver = discover_macos_clang_driver()?;
	let mut command = Command::new(&driver);
	command.arg("-nostdlib");
	if debug_info {
		command.arg("-g");
	}
	if matches!(link_mode, LinkMode::SharedLibrary) {
		command.arg("-dynamiclib");
	}
	if let Some(triple) = target_triple {
		command.arg("-target").arg(triple);
	}
	if let Some(arch) = target_arch(target_triple) {
		command.arg("-arch").arg(arch);
	}
	if let Some(sdk_root) = macos_sdk_root() {
		command.arg("-isysroot").arg(sdk_root);
	}
	if matches!(link_mode, LinkMode::Executable) {
		command.arg("-Wl,-e,_main::main");
		command.arg("-Wl,-lSystem");
	}
	command.arg("-o").arg(out_path);
	for object in objects {
		command.arg(object);
	}

	let output = command.output().map_err(|err| BackendError {
		module_id: "<link>".to_string(),
		message: format!(
			"failed to run Mach-O linker driver `{}`: {err}",
			driver.display()
		),
		location: None,
	})?;
	if !output.status.success() {
		let stderr = String::from_utf8_lossy(&output.stderr);
		let stdout = String::from_utf8_lossy(&output.stdout);
		let details = if stderr.trim().is_empty() {
			stdout.trim().to_string()
		} else {
			stderr.trim().to_string()
		};
		return Err(BackendError {
			module_id: "<link>".to_string(),
			message: format!("linker failed: {details}"),
			location: None,
		});
	}
	Ok(())
}

fn discover_macos_clang_driver() -> Result<PathBuf, BackendError> {
	if let Some(path) = find_program_on_path("clang") {
		return Ok(path);
	}
	if let Some(path) = find_program_on_path("cc") {
		return Ok(path);
	}
	Err(BackendError {
		module_id: "<link>".to_string(),
		message: "Mach-O linking requires `clang` or `cc` on PATH".to_string(),
		location: None,
	})
}

fn emit_debug_sidecar(out_path: &Path, format: BinaryFormat) -> Result<(), BackendError> {
	if format != BinaryFormat::MachO {
		return Ok(());
	}
	let dsymutil = find_program_on_path("dsymutil").ok_or_else(|| BackendError {
		module_id: "<link>".to_string(),
		message: "debug-info on Mach-O requires `dsymutil` on PATH".to_string(),
		location: None,
	})?;
	let dsym_bundle = format!("{}.dSYM", out_path.display());
	let output = Command::new(&dsymutil)
		.arg(out_path)
		.arg("-o")
		.arg(&dsym_bundle)
		.output()
		.map_err(|err| BackendError {
			module_id: "<link>".to_string(),
			message: format!("failed to run `dsymutil`: {err}"),
			location: None,
		})?;
	if !output.status.success() {
		let stderr = String::from_utf8_lossy(&output.stderr);
		let stdout = String::from_utf8_lossy(&output.stdout);
		let details = if stderr.trim().is_empty() {
			stdout.trim().to_string()
		} else {
			stderr.trim().to_string()
		};
		return Err(BackendError {
			module_id: "<link>".to_string(),
			message: format!("`dsymutil` failed: {details}"),
			location: None,
		});
	}
	Ok(())
}

fn linker_supports_msvc_debug_flag(path: &Path) -> bool {
	let Some(name) = path.file_name().and_then(|v| v.to_str()) else {
		return false;
	};
	let name = name.to_ascii_lowercase();
	name == "link" || name == "link.exe" || name == "lld-link" || name == "lld-link.exe"
}

fn linker_is_generic_lld(path: &Path) -> bool {
	let Some(name) = path.file_name().and_then(|v| v.to_str()) else {
		return false;
	};
	let name = name.to_ascii_lowercase();
	name == "lld" || name == "lld.exe"
}

fn target_arch(target_triple: Option<&str>) -> Option<&'static str> {
	let triple = target_triple.unwrap_or("").to_ascii_lowercase();
	if triple.contains("aarch64") || triple.contains("arm64") {
		Some("arm64")
	} else if triple.contains("x86_64") || triple.contains("amd64") {
		Some("x86_64")
	} else if triple.is_empty() {
		match env::consts::ARCH {
			"aarch64" => Some("arm64"),
			"x86_64" => Some("x86_64"),
			_ => None,
		}
	} else {
		None
	}
}

fn macos_sdk_root() -> Option<String> {
	let output = Command::new("xcrun").arg("--show-sdk-path").output().ok()?;
	if !output.status.success() {
		return None;
	}
	let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
	if path.is_empty() { None } else { Some(path) }
}

fn create_static_archive(
	objects: &[PathBuf],
	out_path: &Path,
	format: BinaryFormat,
) -> Result<(), BackendError> {
	if format == BinaryFormat::Pe {
		let archive = find_program_on_path("llvm-lib")
			.or_else(|| find_program_on_path("lib"))
			.ok_or_else(|| {
				BackendError {
				module_id: "<link>".to_string(),
				message:
					"PE static-library linking requires `llvm-lib` or `lib` on PATH"
						.to_string(),
				location: None,
			}
			})?;
		let mut command = Command::new(&archive);
		command.arg(format!("/OUT:{}", out_path.display()));
		for object in objects {
			command.arg(object);
		}
		let output = command.output().map_err(|err| BackendError {
			module_id: "<link>".to_string(),
			message: format!("failed to run `{}`: {err}", archive.display()),
			location: None,
		})?;
		if !output.status.success() {
			let stderr = String::from_utf8_lossy(&output.stderr);
			return Err(BackendError {
				module_id: "<link>".to_string(),
				message: format!(
					"`{}` failed: {}",
					archive.display(),
					stderr.trim()
				),
				location: None,
			});
		}
		return Ok(());
	}

	let archive = find_program_on_path("ar").ok_or_else(|| BackendError {
		module_id: "<link>".to_string(),
		message: "static-library linking requires `ar` on PATH".to_string(),
		location: None,
	})?;
	let mut command = Command::new(&archive);
	command.arg("crs").arg(out_path);
	for object in objects {
		command.arg(object);
	}
	let output = command.output().map_err(|err| BackendError {
		module_id: "<link>".to_string(),
		message: format!("failed to run `{}`: {err}", archive.display()),
		location: None,
	})?;
	if !output.status.success() {
		let stderr = String::from_utf8_lossy(&output.stderr);
		return Err(BackendError {
			module_id: "<link>".to_string(),
			message: format!("`{}` failed: {}", archive.display(), stderr.trim()),
			location: None,
		});
	}
	Ok(())
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
		assert!(
			ir_text.contains("Dwarf Version"),
			"expected Dwarf module flag in LLVM IR output"
		);
		assert!(
			ir_text.contains("Debug Info Version"),
			"expected debug info module flag in LLVM IR output"
		);
	}

	#[test]
	fn llvm_ir_debug_locations_use_source_line_axis() {
		let (typed, semantics, errors) =
			load_typed_program("testdata/ir_lowering_deref_store");
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
		let ir_text = artifacts
			.iter()
			.find_map(|artifact| artifact.llvm_ir_text.as_ref())
			.expect("llvm text emitted");
		assert!(
			ir_text.contains("!DILocation(line: 6"),
			"expected deref store debug location to map to source line 6"
		);
	}

	#[test]
	fn llvm_ir_emits_local_variable_debug_metadata() {
		let (typed, semantics, errors) =
			load_typed_program("testdata/ir_lowering_deref_store");
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
		let ir_text = artifacts
			.iter()
			.find_map(|artifact| artifact.llvm_ir_text.as_ref())
			.expect("llvm text emitted");
		assert!(
			ir_text.contains("!DILocalVariable(name: \"funny_ptr\""),
			"expected local variable debug metadata for funny_ptr"
		);
		assert!(
			ir_text.contains("dbg_value") || ir_text.contains("dbg.value"),
			"expected debug value intrinsic/record in LLVM IR"
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
