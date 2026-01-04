use crate::frontend::{AST, FrontendError, pass_1::pass_1};

pub fn run_passes(ast: Box<AST>) -> Vec<FrontendError> {
    pass_1(ast)
}
