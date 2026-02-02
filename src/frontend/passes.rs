use crate::frontend::{
	AST, FrontendError, FrontendWarning, ResolvedProgram, pass_1::pass_1, pass_2::pass_2,
	pass_3::pass_3,
};

pub fn run_passes(ast: Box<AST>) -> (Box<AST>, Vec<FrontendError>) {
	let ast = pass_1(ast);
	let errors = pass_2(&ast);
	(ast, errors)
}

pub fn run_passes_with_modules(
	ast: Box<AST>,
	entry_file: &str,
	module_paths: &[String],
) -> (ResolvedProgram, Vec<FrontendError>, Vec<FrontendWarning>) {
	let ast = pass_1(ast);
	let mut errors = pass_2(&ast);
	let pass_3 = pass_3(ast, entry_file, module_paths);
	errors.extend(pass_3.errors);
	(pass_3.program, errors, pass_3.warnings)
}
