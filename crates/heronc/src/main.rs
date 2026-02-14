#![allow(dead_code)]

use std::collections::{HashMap, HashSet, VecDeque};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use clap::{Arg, ArgAction, Command};
use heron::backend::{
	BackendOptions, BackendTimings, BinaryFormat, LinkMode, LlvmBackend, OptimizationLevel,
	compile_modules_timed, resolve_binary_format,
};
use heron::diagnostics::{Message, emit_message, pretty_print_error, pretty_print_multiple_errors};
use heron::frontend::{
	self, FrontendError, PassTimings, dot_module_graph, run_passes_with_modules_timed,
};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

fn main() {
	let matches = Command::new("heronc")
		.about("Heron compiler")
		.arg(Arg::new("file")
			.help("Path to the Heron source file to parse")
			.required(true)
			.value_name("FILE"))
		.arg(Arg::new("dump_ast")
			.short('A')
			.long("dump-ast")
			.help("Dump the parsed AST")
			.action(ArgAction::SetTrue))
		.arg(Arg::new("module_path")
			.short('M')
			.value_name("DIR")
			.help("Add a module search path")
			.action(ArgAction::Append))
		.arg(Arg::new("dump_module_graph")
			.long("dump-module-graph")
			.help("Emit Graphviz DOT module graph")
			.action(ArgAction::SetTrue))
		.arg(Arg::new("dump_ctfe_results")
			.long("dump-ctfe-results")
			.help("Dump evaluated top-level comptime results")
			.action(ArgAction::SetTrue))
		.arg(Arg::new("emit_llvm")
			.long("emit-llvm")
			.help("Emit LLVM IR files for debugging")
			.action(ArgAction::SetTrue))
		.arg(Arg::new("emit_dir")
			.long("emit-dir")
			.value_name("DIR")
			.help("Output directory for backend artifacts"))
		.arg(Arg::new("debug_info")
			.short('g')
			.long("debug-info")
			.help("Emit debug information into object files")
			.action(ArgAction::SetTrue))
		.arg(Arg::new("target")
			.long("target")
			.value_name("TRIPLE")
			.help("Override target triple for backend emission"))
		.arg(Arg::new("optimization_level")
			.short('o')
			.long("optimization-level")
			.value_name("LEVEL")
			.value_parser(["none", "size", "optimized", "aggressive"])
			.default_value("none")
			.help("Backend optimization level"))
		.arg(Arg::new("build_type")
			.long("build-type")
			.alias("linking")
			.value_name("MODE")
			.value_parser(["executable", "shared-library", "static-library"])
			.default_value("executable")
			.help("Final artifact kind"))
		.arg(Arg::new("binfmt")
			.long("binfmt")
			.value_name("FORMAT")
			.value_parser(["auto", "pe", "elf", "mach-o"])
			.default_value("auto")
			.help("Final binary format (auto infers from target)"))
		.arg(Arg::new("out")
			.long("out")
			.value_name("PATH")
			.help("Output path for final linked artifact"))
		.get_matches();

	let dump_ast = matches.get_flag("dump_ast");
	let dump_module_graph = matches.get_flag("dump_module_graph");
	let dump_ctfe_results = matches.get_flag("dump_ctfe_results");
	let emit_llvm = matches.get_flag("emit_llvm");
	let debug_info = matches.get_flag("debug_info");
	let emit_dir = matches
		.get_one::<String>("emit_dir")
		.map(PathBuf::from)
		.unwrap_or_else(|| PathBuf::from("target/heron-out"));
	let target_triple = matches.get_one::<String>("target").cloned();
	let link_mode = match matches
		.get_one::<String>("build_type")
		.map(String::as_str)
		.unwrap_or("executable")
	{
		"executable" => LinkMode::Executable,
		"shared-library" => LinkMode::SharedLibrary,
		"static-library" => LinkMode::StaticLibrary,
		_ => LinkMode::Executable,
	};
	let binary_format = match matches
		.get_one::<String>("binfmt")
		.map(String::as_str)
		.unwrap_or("auto")
	{
		"auto" => BinaryFormat::Auto,
		"pe" => BinaryFormat::Pe,
		"elf" => BinaryFormat::Elf,
		"mach-o" => BinaryFormat::MachO,
		_ => BinaryFormat::Auto,
	};
	let optimization_level = match matches
		.get_one::<String>("optimization_level")
		.map(String::as_str)
		.unwrap_or("none")
	{
		"none" => OptimizationLevel::None,
		"size" => OptimizationLevel::Size,
		"optimized" => OptimizationLevel::Optimized,
		"aggressive" => OptimizationLevel::Aggressive,
		_ => OptimizationLevel::None,
	};
	let module_paths: Vec<String> = matches
		.get_many::<String>("module_path")
		.map(|values| values.cloned().collect())
		.unwrap_or_default();

	let file = matches
		.get_one::<String>("file")
		.expect("clap enforces required argument")
		.to_owned();
	let out_path = matches
		.get_one::<String>("out")
		.map(PathBuf::from)
		.unwrap_or_else(|| {
			default_output_path(
				Path::new(&file),
				&emit_dir,
				link_mode,
				binary_format,
				target_triple.as_deref(),
			)
		});
	let file_for_lexer = file.clone();
	let compilation_start = Instant::now();
	let mut timings = CompilationTimings::default();
	let mut compilation_succeeded = false;

	let read_start = Instant::now();
	let input = match std::fs::read_to_string(&file) {
		Ok(input) => {
			timings.read = Some(read_start.elapsed());
			input
		}
		Err(e) => {
			timings.read = Some(read_start.elapsed());
			eprintln!("error: failed to read `{file}`: {e}");
			eprintln!();
			timings.total = compilation_start.elapsed();
			let _ = print_compilation_timings(&timings, false);
			std::process::exit(1);
		}
	};

	let parse_start = Instant::now();
	let mut lexer = frontend::Lexer::new(input, file_for_lexer);
	let input = lexer.get_input();
	let mut parser = match frontend::Parser::new(&mut lexer) {
		Ok(parser) => parser,
		Err(err) => {
			timings.parse = Some(parse_start.elapsed());
			pretty_print_error(
				heron::frontend::FrontendError::ParseError(err),
				input.as_str(),
			);
			eprintln!();
			timings.total = compilation_start.elapsed();
			let _ = print_compilation_timings(&timings, false);
			return;
		}
	};
	let ast = parser.parse();
	let errors = parser.take_errors();
	timings.parse = Some(parse_start.elapsed());
	if errors.is_empty() {
		match ast {
			Ok(ast) => {
				let ast_for_ctfe_dump = ast.clone();
				if dump_ast && !dump_module_graph {
					println!(
						"{}",
						frontend::AST::pretty_format(
							format!("{}", ast).as_str()
						)
					);
				}
				let (
					typed_program,
					semantics,
					program,
					errors,
					warnings,
					pass_timings,
				) = run_passes_with_modules_timed(ast, file.as_str(), &module_paths);
				timings.frontend = Some(pass_timings);
				if !warnings.is_empty() {
					let mut stderr = StandardStream::stderr(ColorChoice::Auto);
					for warning in warnings {
						let _ = emit_message(
							&mut stderr,
							Message::Warning(warning),
							input.as_str(),
						);
					}
				}
				if !errors.is_empty() {
					pretty_print_multiple_errors(input, errors);
					eprintln!();
					timings.total = compilation_start.elapsed();
					let _ = print_compilation_timings(&timings, false);
					return;
				}
				if dump_ctfe_results {
					dump_ctfe_results_for_ast(ast_for_ctfe_dump.as_ref());
				}
				if dump_module_graph {
					println!("{}", dot_module_graph(&program));
				}

				{
					let options = BackendOptions {
						emit_obj: true,
						emit_llvm,
						debug_info,
						emit_dir: emit_dir.clone(),
						target_triple,
						optimization_level,
						link_mode: Some(link_mode),
						binary_format,
						out_path: Some(out_path.clone()),
					};
					let mut backend = LlvmBackend::new();
					match compile_modules_timed(
						&mut backend,
						&typed_program,
						&semantics,
						&options,
					) {
						Ok((artifacts, backend_timings)) => {
							timings.backend = Some(backend_timings);
							if emit_llvm {
								for artifact in &artifacts {
									if let Some(path) =
										&artifact
											.llvm_ir_path
									{
										eprintln!("emitted llvm-ir {} -> {}", artifact.module_id, path.display());
									}
								}
							}
							for artifact in &artifacts {
								if let Some(path) =
									&artifact.linked_output_path
								{
									eprintln!(
										"emitted linked output -> {}",
										path.display()
									);
								}
							}
						}
						Err(backend_errors) => {
							for err in backend_errors {
								if let Some(location) = err.location
								{
									eprintln!(
										"backend error in {} at {}:{}:{}: {}",
										err.module_id,
										location.file,
										location.range
											.begin
											.0,
										location.range
											.begin
											.1,
										err.message
									);
								} else {
									eprintln!(
										"backend error in {}: {}",
										err.module_id,
										err.message
									);
								}
							}
							eprintln!();
							timings.total = compilation_start.elapsed();
							let _ = print_compilation_timings(
								&timings, false,
							);
							return;
						}
					}
				}
				compilation_succeeded = true;
			}
			Err(e) => pretty_print_error(
				heron::frontend::FrontendError::ParseError(e),
				input.as_str(),
			),
		}
	} else {
		let errors = errors
			.iter()
			.map(|x| FrontendError::ParseError(x.clone()))
			.collect();
		pretty_print_multiple_errors(input, errors);
	}
	if !compilation_succeeded {
		eprintln!();
	}
	timings.total = compilation_start.elapsed();
	let _ = print_compilation_timings(&timings, compilation_succeeded);
}

#[derive(Default)]
struct CompilationTimings {
	read: Option<Duration>,
	parse: Option<Duration>,
	frontend: Option<PassTimings>,
	backend: Option<BackendTimings>,
	total: Duration,
}

fn print_compilation_timings(
	timings: &CompilationTimings,
	compilation_succeeded: bool,
) -> io::Result<()> {
	let mut stderr = StandardStream::stderr(ColorChoice::Auto);
	writeln!(&mut stderr, "Compilation timings:\n")?;
	stderr.reset()?;
	if let Some(read) = timings.read {
		write_duration_line(&mut stderr, "Read Source", read)?;
	}
	if let Some(parse) = timings.parse {
		write_duration_line(&mut stderr, "Parse", parse)?;
	}
	if let Some(frontend) = timings.frontend {
		write_duration_line(&mut stderr, "Frontend", frontend.total)?;
		write_duration_line_indented(&mut stderr, "Pass 1", frontend.pass_1, 2)?;
		write_duration_line_indented(&mut stderr, "Pass 2", frontend.pass_2, 2)?;
		write_duration_line_indented(&mut stderr, "Pass 3", frontend.pass_3, 2)?;
		write_duration_line_indented(&mut stderr, "Pass 4", frontend.pass_4, 2)?;
		write_duration_line_indented(&mut stderr, "Pass 5", frontend.pass_5, 2)?;
	}
	if let Some(backend) = timings.backend.as_ref() {
		write_duration_line(&mut stderr, "Backend", backend.total)?;
		write_duration_line_indented(&mut stderr, "Cache Load", backend.cache_load, 2)?;
		write_duration_line_indented(&mut stderr, "Cache Plan", backend.cache_plan, 2)?;
		if backend.ir_lower > Duration::ZERO {
			write_duration_line_indented(&mut stderr, "IR Lower", backend.ir_lower, 2)?;
		}
		if backend.ir_verify > Duration::ZERO {
			write_duration_line_indented(
				&mut stderr,
				"IR Verify",
				backend.ir_verify,
				2,
			)?;
		}
		write_duration_line_indented(
			&mut stderr,
			"Module Compile",
			backend.module_compile,
			2,
		)?;
		write_duration_line_indented(&mut stderr, "Module Reuse", backend.module_reuse, 2)?;
		if backend.cache_save > Duration::ZERO {
			write_duration_line_indented(
				&mut stderr,
				"Cache Save",
				backend.cache_save,
				2,
			)?;
		}
		write_info_line_indented(
			&mut stderr,
			"Backend Modules",
			&format!(
				"{} compiled, {} reused",
				backend.modules_compiled, backend.modules_reused
			),
			2,
		)?;
	}
	writeln!(&mut stderr)?;

	let mut status_color = ColorSpec::new();
	if compilation_succeeded {
		status_color.set_fg(Some(Color::Green)).set_bold(true);
		stderr.set_color(&status_color)?;
		write!(&mut stderr, "Compilation finished")?;
	} else {
		status_color.set_fg(Some(Color::Red)).set_bold(true);
		stderr.set_color(&status_color)?;
		write!(&mut stderr, "Compilation failed")?;
	}
	stderr.reset()?;
	write!(&mut stderr, " in ")?;
	write_cyan_duration(&mut stderr, timings.total)?;
	writeln!(&mut stderr)?;
	Ok(())
}

fn write_duration_line<W: WriteColor>(
	writer: &mut W,
	label: &str,
	duration: Duration,
) -> io::Result<()> {
	write_duration_line_indented(writer, label, duration, 1)
}

fn write_duration_line_indented<W: WriteColor>(
	writer: &mut W,
	label: &str,
	duration: Duration,
	indent_level: usize,
) -> io::Result<()> {
	let mut label_spec = ColorSpec::new();
	label_spec.set_fg(Some(Color::Yellow));
	for _ in 0..indent_level {
		write!(writer, "\t")?;
	}
	writer.set_color(&label_spec)?;
	write!(writer, "{label}")?;
	writer.reset()?;
	write!(writer, ": ")?;
	write_cyan_duration(writer, duration)?;
	writeln!(writer)?;
	Ok(())
}

fn write_info_line_indented<W: WriteColor>(
	writer: &mut W,
	label: &str,
	value: &str,
	indent_level: usize,
) -> io::Result<()> {
	let mut label_spec = ColorSpec::new();
	label_spec.set_fg(Some(Color::Yellow));
	for _ in 0..indent_level {
		write!(writer, "\t")?;
	}
	writer.set_color(&label_spec)?;
	write!(writer, "{label}")?;
	writer.reset()?;
	writeln!(writer, ": {value}")?;
	Ok(())
}

fn write_cyan_duration<W: WriteColor>(writer: &mut W, duration: Duration) -> io::Result<()> {
	let mut spec = ColorSpec::new();
	spec.set_fg(Some(Color::Cyan));
	writer.set_color(&spec)?;
	write!(writer, "{}", format_duration(duration))?;
	writer.reset()?;
	Ok(())
}

fn format_duration(duration: Duration) -> String {
	if duration.as_secs() >= 60 {
		let total_seconds = duration.as_secs_f64();
		let minutes = (total_seconds / 60.0).floor() as u64;
		let seconds = total_seconds - (minutes as f64 * 60.0);
		format!("{minutes}m {seconds:.3}s")
	} else if duration.as_secs_f64() >= 1.0 {
		format!("{:.3}s", duration.as_secs_f64())
	} else if duration.as_secs_f64() >= 0.001 {
		format!("{:.3}ms", duration.as_secs_f64() * 1_000.0)
	} else if duration.as_secs_f64() >= 0.000_001 {
		format!("{:.3}us", duration.as_secs_f64() * 1_000_000.0)
	} else {
		format!("{}ns", duration.as_nanos())
	}
}

fn default_output_path(
	input_file: &Path,
	emit_dir: &Path,
	link_mode: LinkMode,
	binary_format: BinaryFormat,
	target_triple: Option<&str>,
) -> PathBuf {
	let stem = input_file
		.file_stem()
		.and_then(|s| s.to_str())
		.filter(|s| !s.is_empty())
		.unwrap_or("a");
	let resolved = resolve_binary_format(binary_format, target_triple);
	let mut name = stem.to_string();
	match link_mode {
		LinkMode::Executable => match resolved {
			BinaryFormat::Pe => name.push_str(".exe"),
			BinaryFormat::Elf | BinaryFormat::MachO => {}
			BinaryFormat::Auto => unreachable!("auto format must be resolved"),
		},
		LinkMode::SharedLibrary => {
			if resolved != BinaryFormat::Pe {
				name = format!("lib{name}");
			}
			match resolved {
				BinaryFormat::Pe => name.push_str(".dll"),
				BinaryFormat::Elf => name.push_str(".so"),
				BinaryFormat::MachO => name.push_str(".dylib"),
				BinaryFormat::Auto => unreachable!("auto format must be resolved"),
			}
		}
		LinkMode::StaticLibrary => {
			if resolved != BinaryFormat::Pe {
				name = format!("lib{name}");
			}
			match resolved {
				BinaryFormat::Pe => name.push_str(".lib"),
				BinaryFormat::Elf | BinaryFormat::MachO => name.push_str(".a"),
				BinaryFormat::Auto => unreachable!("auto format must be resolved"),
			}
		}
	}
	emit_dir.join("bin").join(name)
}

fn dump_ctfe_results_for_ast(ast: &frontend::AST) {
	let mut engine = frontend::CtfeEngine::new(frontend::CtfeLimits::default());
	if let frontend::ASTValue::ExprList { items, .. }
	| frontend::ASTValue::ExprListNoScope { items, .. } = &ast.v
	{
		let required_comptimes = collect_required_comptime_names(items);
		for item in items {
			engine.register_function_decls(unwrap_pub(item.as_ref()));
		}
		for item in items {
			let target = unwrap_pub(item.as_ref());
			match &target.v {
				frontend::ASTValue::DeclarationComptime(name, value) => {
					if !required_comptimes.contains(name) {
						continue;
					}
					dump_one_comptime_result(&mut engine, name, value.as_ref());
				}
				frontend::ASTValue::DeclarationMulti {
					names,
					values: Some(values),
					comptime: true,
					..
				} => {
					for (idx, name) in names.iter().enumerate() {
						if !required_comptimes.contains(name) {
							continue;
						}
						if let Some(value) =
							values.get(idx).or_else(|| values.first())
						{
							dump_one_comptime_result(
								&mut engine,
								name,
								value.as_ref(),
							);
						}
					}
				}
				_ => {}
			}
		}
		for item in items {
			let target = unwrap_pub(item.as_ref());
			dump_runtime_ctfe_candidates(&mut engine, target);
		}
	}
}

fn collect_required_comptime_names(items: &[Box<frontend::AST>]) -> HashSet<String> {
	let mut comptime_values: HashMap<String, Box<frontend::AST>> = HashMap::new();
	let mut queue: VecDeque<String> = VecDeque::new();

	for item in items {
		let node = unwrap_pub(item.as_ref());
		match &node.v {
			frontend::ASTValue::DeclarationComptime(name, value) => {
				comptime_values.insert(name.clone(), value.clone());
			}
			frontend::ASTValue::DeclarationMulti {
				names,
				values: Some(values),
				comptime: true,
				..
			} => {
				for (idx, name) in names.iter().enumerate() {
					if let Some(value) =
						values.get(idx).or_else(|| values.first())
					{
						comptime_values.insert(name.clone(), value.clone());
					}
				}
			}
			frontend::ASTValue::Declaration { value, .. } => {
				collect_ids_in_ast(value.as_ref(), &mut queue);
			}
			frontend::ASTValue::DeclarationMulti {
				values: Some(values),
				comptime: false,
				..
			} => {
				for value in values {
					collect_ids_in_ast(value.as_ref(), &mut queue);
				}
			}
			_ => {}
		}
	}

	let mut required = HashSet::new();
	while let Some(name) = queue.pop_front() {
		if !required.insert(name.clone()) {
			continue;
		}
		if let Some(value) = comptime_values.get(&name) {
			collect_ids_in_ast(value.as_ref(), &mut queue);
		}
	}
	required
}

fn collect_ids_in_ast(node: &frontend::AST, out: &mut VecDeque<String>) {
	match &node.v {
		frontend::ASTValue::Id(name) => out.push_back(name.clone()),
		frontend::ASTValue::Pub(inner)
		| frontend::ASTValue::UnaryPlus(inner)
		| frontend::ASTValue::UnaryMinus(inner)
		| frontend::ASTValue::Not(inner)
		| frontend::ASTValue::Deref(inner)
		| frontend::ASTValue::Mut(inner)
		| frontend::ASTValue::PtrOf(inner)
		| frontend::ASTValue::Defer(inner) => collect_ids_in_ast(inner.as_ref(), out),
		frontend::ASTValue::Ref { v, .. }
		| frontend::ASTValue::DeclarationComptime(_, v)
		| frontend::ASTValue::Set(_, v) => collect_ids_in_ast(v.as_ref(), out),
		frontend::ASTValue::Cast { value, .. }
		| frontend::ASTValue::Transmute { value, .. } => collect_ids_in_ast(value.as_ref(), out),
		frontend::ASTValue::BinExpr { lhs, rhs, .. } => {
			collect_ids_in_ast(lhs.as_ref(), out);
			collect_ids_in_ast(rhs.as_ref(), out);
		}
		frontend::ASTValue::Declaration { value, .. } => {
			collect_ids_in_ast(value.as_ref(), out)
		}
		frontend::ASTValue::DeclarationMulti {
			values: Some(values),
			..
		}
		| frontend::ASTValue::ExprList { items: values, .. }
		| frontend::ASTValue::ExprListNoScope { items: values, .. } => {
			for value in values {
				collect_ids_in_ast(value.as_ref(), out);
			}
		}
		frontend::ASTValue::SetMulti { values, .. } => {
			for value in values {
				collect_ids_in_ast(value.as_ref(), out);
			}
		}
		frontend::ASTValue::Call { callee, args } => {
			collect_ids_in_ast(callee.as_ref(), out);
			for arg in args {
				collect_ids_in_ast(arg.as_ref(), out);
			}
		}
		frontend::ASTValue::NamedArg { value, .. }
		| frontend::ASTValue::Return(Some(value)) => collect_ids_in_ast(value.as_ref(), out),
		frontend::ASTValue::If {
			cond,
			decl,
			body,
			else_,
		} => {
			collect_ids_in_ast(cond.as_ref(), out);
			if let Some(decl) = decl {
				collect_ids_in_ast(decl.as_ref(), out);
			}
			collect_ids_in_ast(body.as_ref(), out);
			if let Some(else_) = else_ {
				collect_ids_in_ast(else_.as_ref(), out);
			}
		}
		frontend::ASTValue::While { cond, decl, body } => {
			collect_ids_in_ast(cond.as_ref(), out);
			if let Some(decl) = decl {
				collect_ids_in_ast(decl.as_ref(), out);
			}
			collect_ids_in_ast(body.as_ref(), out);
		}
		frontend::ASTValue::ForLoop {
			init,
			cond,
			step,
			body,
		} => {
			if let Some(init) = init {
				collect_ids_in_ast(init.as_ref(), out);
			}
			if let Some(cond) = cond {
				collect_ids_in_ast(cond.as_ref(), out);
			}
			if let Some(step) = step {
				collect_ids_in_ast(step.as_ref(), out);
			}
			collect_ids_in_ast(body.as_ref(), out);
		}
		frontend::ASTValue::For { iter, body, .. } => {
			collect_ids_in_ast(iter.as_ref(), out);
			collect_ids_in_ast(body.as_ref(), out);
		}
		frontend::ASTValue::InitializerList(items)
		| frontend::ASTValue::TypedInitializerList { items, .. } => {
			for item in items {
				match item {
					frontend::InitializerItem::Positional(value)
					| frontend::InitializerItem::Named { value, .. } => collect_ids_in_ast(value.as_ref(), out),
				}
			}
		}
		frontend::ASTValue::Index { target, indices } => {
			collect_ids_in_ast(target.as_ref(), out);
			for idx in indices {
				collect_ids_in_ast(idx.as_ref(), out);
			}
		}
		frontend::ASTValue::Match {
			scrutinee, cases, ..
		} => {
			collect_ids_in_ast(scrutinee.as_ref(), out);
			for case in cases {
				if let frontend::MatchCasePattern::Exprs(exprs) = &case.pattern {
					for expr in exprs {
						collect_ids_in_ast(expr.as_ref(), out);
					}
				}
				if let Some(guard) = &case.guard {
					collect_ids_in_ast(guard.as_ref(), out);
				}
				collect_ids_in_ast(case.body.as_ref(), out);
			}
		}
		frontend::ASTValue::Fn {
			pre,
			where_clause,
			ensures,
			body,
			..
		} => {
			for pre in pre {
				collect_ids_in_ast(pre.as_ref(), out);
			}
			if let Some(where_clause) = where_clause {
				collect_ids_in_ast(where_clause.as_ref(), out);
			}
			for ensure in ensures {
				collect_ids_in_ast(ensure.condition.as_ref(), out);
			}
			match body {
				frontend::FnBody::Expr(expr) | frontend::FnBody::Block(expr) => {
					collect_ids_in_ast(expr.as_ref(), out)
				}
				frontend::FnBody::Uninitialized => {}
			}
		}
		_ => {}
	}
}

fn dump_one_comptime_result(engine: &mut frontend::CtfeEngine, name: &str, value: &frontend::AST) {
	if !should_eval_comptime_value(value) {
		return;
	}
	match engine.eval_expr(value) {
		Ok(result) => {
			println!("{name} = {:?}", result);
			engine.bind_const(name, result);
		}
		Err(err) => {
			println!("{name} = <ctfe error {:?} at {:?}>", err.kind, err.location);
		}
	}
}

fn should_eval_comptime_value(value: &frontend::AST) -> bool {
	!matches!(
		value.v,
		frontend::ASTValue::Fn { .. }
			| frontend::ASTValue::Struct { .. }
			| frontend::ASTValue::Enum { .. }
			| frontend::ASTValue::Union { .. }
			| frontend::ASTValue::RawUnion { .. }
			| frontend::ASTValue::Newtype { .. }
			| frontend::ASTValue::Alias { .. }
	)
}

fn should_eval_runtime_candidate(value: &frontend::AST) -> bool {
	!matches!(
		value.v,
		frontend::ASTValue::Fn { .. }
			| frontend::ASTValue::Struct { .. }
			| frontend::ASTValue::Enum { .. }
			| frontend::ASTValue::Union { .. }
			| frontend::ASTValue::RawUnion { .. }
			| frontend::ASTValue::Newtype { .. }
			| frontend::ASTValue::Alias { .. }
	)
}

fn dump_runtime_ctfe_candidates(engine: &mut frontend::CtfeEngine, node: &frontend::AST) {
	match &node.v {
		frontend::ASTValue::Declaration {
			name,
			value,
			mutable: _,
		} => {
			if let frontend::ASTValue::Fn { body, .. } = &value.v {
				let mut runtime_scopes = vec![HashSet::new()];
				dump_fn_body_ctfe_candidates(engine, body, &mut runtime_scopes);
			} else {
				let runtime_scopes = vec![HashSet::new()];
				dump_runtime_decl_value(
					engine,
					name,
					value.as_ref(),
					&runtime_scopes,
				);
			}
		}
		frontend::ASTValue::DeclarationMulti {
			names,
			values: Some(values),
			comptime: false,
			..
		} => {
			let runtime_scopes = vec![HashSet::new()];
			for (idx, name) in names.iter().enumerate() {
				if let Some(value) = values.get(idx).or_else(|| values.first()) {
					dump_runtime_decl_value(
						engine,
						name,
						value.as_ref(),
						&runtime_scopes,
					);
				}
			}
		}
		_ => {}
	}
}

fn dump_runtime_decl_value(
	engine: &mut frontend::CtfeEngine,
	name: &str,
	value: &frontend::AST,
	runtime_scopes: &[HashSet<String>],
) {
	if let frontend::ASTValue::Fn { body, .. } = &value.v {
		let mut nested_scopes = runtime_scopes.to_vec();
		nested_scopes.push(HashSet::new());
		dump_fn_body_ctfe_candidates(engine, body, &mut nested_scopes);
		return;
	}
	if expr_depends_on_runtime_locals(value, runtime_scopes) {
		return;
	}
	if !expr_contains_call(value) {
		return;
	}
	if should_eval_runtime_candidate(value)
		&& let Ok(result) = engine.eval_expr(value)
	{
		println!("{name} = {:?}", result);
	}
}

fn dump_fn_body_ctfe_candidates(
	engine: &mut frontend::CtfeEngine,
	body: &frontend::FnBody,
	runtime_scopes: &mut Vec<HashSet<String>>,
) {
	match body {
		frontend::FnBody::Block(ast) | frontend::FnBody::Expr(ast) => {
			dump_ast_ctfe_candidates(engine, ast.as_ref(), runtime_scopes)
		}
		frontend::FnBody::Uninitialized => {}
	}
}

fn dump_ast_ctfe_candidates(
	engine: &mut frontend::CtfeEngine,
	node: &frontend::AST,
	runtime_scopes: &mut Vec<HashSet<String>>,
) {
	match &node.v {
		frontend::ASTValue::ExprList { items, .. }
		| frontend::ASTValue::ExprListNoScope { items, .. } => {
			runtime_scopes.push(HashSet::new());
			for item in items {
				dump_ast_ctfe_candidates(engine, item.as_ref(), runtime_scopes);
			}
			let _ = runtime_scopes.pop();
		}
		frontend::ASTValue::Declaration { name, value, .. } => {
			dump_runtime_decl_value(engine, name, value.as_ref(), runtime_scopes);
			if let Some(scope) = runtime_scopes.last_mut() {
				scope.insert(name.clone());
			}
		}
		frontend::ASTValue::DeclarationMulti {
			names,
			values: Some(values),
			comptime: false,
			..
		} => {
			for (idx, name) in names.iter().enumerate() {
				if let Some(value) = values.get(idx).or_else(|| values.first()) {
					dump_runtime_decl_value(
						engine,
						name,
						value.as_ref(),
						runtime_scopes,
					);
				}
				if let Some(scope) = runtime_scopes.last_mut() {
					scope.insert(name.clone());
				}
			}
		}
		frontend::ASTValue::DeclarationMulti {
			names,
			comptime: false,
			..
		} => {
			if let Some(scope) = runtime_scopes.last_mut() {
				for name in names {
					scope.insert(name.clone());
				}
			}
		}
		frontend::ASTValue::If {
			decl, body, else_, ..
		} => {
			if let Some(decl) = decl {
				dump_ast_ctfe_candidates(engine, decl.as_ref(), runtime_scopes);
			}
			dump_ast_ctfe_candidates(engine, body.as_ref(), runtime_scopes);
			if let Some(else_) = else_ {
				dump_ast_ctfe_candidates(engine, else_.as_ref(), runtime_scopes);
			}
		}
		frontend::ASTValue::While { decl, body, .. } => {
			if let Some(decl) = decl {
				dump_ast_ctfe_candidates(engine, decl.as_ref(), runtime_scopes);
			}
			dump_ast_ctfe_candidates(engine, body.as_ref(), runtime_scopes);
		}
		frontend::ASTValue::ForLoop {
			init, step, body, ..
		} => {
			if let Some(init) = init {
				dump_ast_ctfe_candidates(engine, init.as_ref(), runtime_scopes);
			}
			if let Some(step) = step {
				dump_ast_ctfe_candidates(engine, step.as_ref(), runtime_scopes);
			}
			dump_ast_ctfe_candidates(engine, body.as_ref(), runtime_scopes);
		}
		frontend::ASTValue::For { body, .. } => {
			dump_ast_ctfe_candidates(engine, body.as_ref(), runtime_scopes)
		}
		frontend::ASTValue::Match { cases, .. } => {
			for case in cases {
				dump_ast_ctfe_candidates(
					engine,
					case.body.as_ref(),
					runtime_scopes,
				);
			}
		}
		_ => {}
	}
}

fn expr_depends_on_runtime_locals(
	node: &frontend::AST,
	runtime_scopes: &[HashSet<String>],
) -> bool {
	let mut ids = VecDeque::new();
	collect_ids_in_ast(node, &mut ids);
	while let Some(id) = ids.pop_front() {
		if runtime_scopes.iter().rev().any(|scope| scope.contains(&id)) {
			return true;
		}
	}
	false
}

fn expr_contains_call(node: &frontend::AST) -> bool {
	match &node.v {
		frontend::ASTValue::Call { .. } => true,
		frontend::ASTValue::Pub(inner)
		| frontend::ASTValue::UnaryPlus(inner)
		| frontend::ASTValue::UnaryMinus(inner)
		| frontend::ASTValue::Not(inner)
		| frontend::ASTValue::Deref(inner)
		| frontend::ASTValue::Mut(inner)
		| frontend::ASTValue::PtrOf(inner)
		| frontend::ASTValue::Defer(inner)
		| frontend::ASTValue::Ref { v: inner, .. }
		| frontend::ASTValue::DeclarationComptime(_, inner)
		| frontend::ASTValue::Set(_, inner)
		| frontend::ASTValue::Cast { value: inner, .. }
		| frontend::ASTValue::Transmute { value: inner, .. }
		| frontend::ASTValue::Declaration { value: inner, .. }
		| frontend::ASTValue::NamedArg { value: inner, .. }
		| frontend::ASTValue::Return(Some(inner)) => expr_contains_call(inner.as_ref()),
		frontend::ASTValue::BinExpr { lhs, rhs, .. } => {
			expr_contains_call(lhs.as_ref()) || expr_contains_call(rhs.as_ref())
		}
		frontend::ASTValue::DeclarationMulti {
			values: Some(values),
			..
		}
		| frontend::ASTValue::ExprList { items: values, .. }
		| frontend::ASTValue::ExprListNoScope { items: values, .. }
		| frontend::ASTValue::SetMulti { values, .. } => {
			values.iter().any(|v| expr_contains_call(v.as_ref()))
		}
		frontend::ASTValue::GenericApply { target, .. } => {
			expr_contains_call(target.as_ref())
		}
		frontend::ASTValue::If {
			cond,
			decl,
			body,
			else_,
		} => {
			expr_contains_call(cond.as_ref())
				|| decl.as_ref()
					.is_some_and(|d| expr_contains_call(d.as_ref()))
				|| expr_contains_call(body.as_ref())
				|| else_.as_ref()
					.is_some_and(|e| expr_contains_call(e.as_ref()))
		}
		frontend::ASTValue::While { cond, decl, body } => {
			expr_contains_call(cond.as_ref())
				|| decl.as_ref()
					.is_some_and(|d| expr_contains_call(d.as_ref()))
				|| expr_contains_call(body.as_ref())
		}
		frontend::ASTValue::ForLoop {
			init,
			cond,
			step,
			body,
		} => {
			init.as_ref()
				.is_some_and(|i| expr_contains_call(i.as_ref()))
				|| cond.as_ref()
					.is_some_and(|c| expr_contains_call(c.as_ref()))
				|| step.as_ref()
					.is_some_and(|s| expr_contains_call(s.as_ref()))
				|| expr_contains_call(body.as_ref())
		}
		frontend::ASTValue::For { iter, body, .. } => {
			expr_contains_call(iter.as_ref()) || expr_contains_call(body.as_ref())
		}
		frontend::ASTValue::InitializerList(items)
		| frontend::ASTValue::TypedInitializerList { items, .. } => items.iter().any(|item| match item {
			frontend::InitializerItem::Positional(value)
			| frontend::InitializerItem::Named { value, .. } => expr_contains_call(value.as_ref()),
		}),
		frontend::ASTValue::Index { target, indices } => {
			expr_contains_call(target.as_ref())
				|| indices.iter().any(|idx| expr_contains_call(idx.as_ref()))
		}
		frontend::ASTValue::Match {
			scrutinee, cases, ..
		} => {
			expr_contains_call(scrutinee.as_ref())
				|| cases.iter().any(|case| {
					(case.guard
						.as_ref()
						.is_some_and(|g| expr_contains_call(g.as_ref())))
						|| expr_contains_call(case.body.as_ref())
				})
		}
		frontend::ASTValue::Fn {
			pre,
			where_clause,
			ensures,
			body,
			..
		} => {
			pre.iter().any(|p| expr_contains_call(p.as_ref()))
				|| where_clause
					.as_ref()
					.is_some_and(|w| expr_contains_call(w.as_ref()))
				|| ensures
					.iter()
					.any(|e| expr_contains_call(e.condition.as_ref()))
				|| match body {
					frontend::FnBody::Expr(expr)
					| frontend::FnBody::Block(expr) => expr_contains_call(expr.as_ref()),
					frontend::FnBody::Uninitialized => false,
				}
		}
		_ => false,
	}
}

fn unwrap_pub(node: &frontend::AST) -> &frontend::AST {
	match &node.v {
		frontend::ASTValue::Pub(inner) => inner.as_ref(),
		_ => node,
	}
}
