use crate::frontend::{
	AST, FrontendError, FrontendWarning, ResolvedProgram, TypedProgram, pass_1::pass_1,
	pass_2::pass_2, pass_3::pass_3, pass_4::pass_4, pass_5::pass_5,
};
use std::time::{Duration, Instant};

#[derive(Clone, Copy, Debug)]
pub struct PassTimings {
	pub pass_1: Duration,
	pub pass_2: Duration,
	pub pass_3: Duration,
	pub pass_4: Duration,
	pub pass_5: Duration,
	pub total: Duration,
}

pub fn run_passes(ast: Box<AST>) -> (Box<AST>, Vec<FrontendError>) {
	let mut ast = pass_1(ast);
	let errors = pass_2(&mut ast);
	(ast, errors)
}

pub fn run_passes_with_modules(
	ast: Box<AST>,
	entry_file: &str,
	module_paths: &[String],
) -> (
	TypedProgram,
	ResolvedProgram,
	Vec<FrontendError>,
	Vec<FrontendWarning>,
) {
	let (typed, resolved, errors, warnings, _timings) =
		run_passes_with_modules_timed(ast, entry_file, module_paths);
	(typed, resolved, errors, warnings)
}

pub fn run_passes_with_modules_timed(
	ast: Box<AST>,
	entry_file: &str,
	module_paths: &[String],
) -> (
	TypedProgram,
	ResolvedProgram,
	Vec<FrontendError>,
	Vec<FrontendWarning>,
	PassTimings,
) {
	let total_start = Instant::now();

	let pass_1_start = Instant::now();
	let mut ast = pass_1(ast);
	let pass_1_duration = pass_1_start.elapsed();

	let pass_2_start = Instant::now();
	let mut errors = pass_2(&mut ast);
	let pass_2_duration = pass_2_start.elapsed();

	let pass_3_start = Instant::now();
	let pass_3 = pass_3(ast, entry_file, module_paths);
	let pass_3_duration = pass_3_start.elapsed();
	errors.extend(pass_3.errors);

	let pass_4_start = Instant::now();
	let mut pass_4 = pass_4(pass_3.program.clone());
	let pass_4_duration = pass_4_start.elapsed();
	errors.extend(pass_4.errors);

	let pass_5_start = Instant::now();
	pass_5(&mut pass_4.program, &mut pass_4.semantics);
	let pass_5_duration = pass_5_start.elapsed();

	(
		pass_4.program,
		pass_3.program,
		errors,
		pass_3.warnings,
		PassTimings {
			pass_1: pass_1_duration,
			pass_2: pass_2_duration,
			pass_3: pass_3_duration,
			pass_4: pass_4_duration,
			pass_5: pass_5_duration,
			total: total_start.elapsed(),
		},
	)
}
