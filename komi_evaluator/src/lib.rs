//! # Evaluator
//!
//! Reads *an abstract syntax tree (AST)* and returns a *value* as defined in the `komi_syntax` crate.
//! Note that *value* is a technical term referring to the result of evaluation.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod ast_reducer;
mod environment;
mod err;

use crate::environment::Environment;
use ast_reducer::reduce_ast;
pub use err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, Value};

type ResVal = Result<Value, EvalError>;

/// Produces a value from an AST.
struct Evaluator<'a> {
    ast: &'a Box<Ast>,
}

impl<'a> Evaluator<'a> {
    pub fn new(ast: &'a Box<Ast>) -> Self {
        Self { ast }
    }

    pub fn eval(&self) -> ResVal {
        let mut env = Environment::new();

        reduce_ast(self.ast, &mut env)
    }
}

/// Produces a value from an AST.
pub fn eval(ast: &Box<Ast>) -> ResVal {
    Evaluator::new(ast).eval()
}
