use crate::frontend::{
	AST, FrontendError, FrontendWarning, ResolvedProgram, TypedProgram, pass_1::pass_1,
	pass_2::pass_2, pass_3::pass_3, pass_4::pass_4, pass_5::pass_5,
};

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
	let mut ast = pass_1(ast);
	let mut errors = pass_2(&mut ast);
	let pass_3 = pass_3(ast, entry_file, module_paths);
	errors.extend(pass_3.errors);
	let mut pass_4 = pass_4(pass_3.program.clone());
	errors.extend(pass_4.errors);
	pass_5(&mut pass_4.program, &mut pass_4.semantics);
	(pass_4.program, pass_3.program, errors, pass_3.warnings)
}
