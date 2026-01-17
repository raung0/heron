use crate::frontend::{pass_1::pass_1, pass_2::pass_2, FrontendError, AST};

pub fn run_passes(ast: Box<AST>) -> (Box<AST>, Vec<FrontendError>) {
	let ast = pass_1(ast);
	let errors = pass_2(&ast);
	(ast, errors)
}
