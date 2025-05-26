//! # Evaluator
//!
//! Reads *an abstract syntax tree (AST)* and returns a *value* as defined in the `komi_syntax` crate.
//! Note that *value* is a technical term referring to the result of evaluation.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod ast_reducer;
mod builtins;
mod environment;
mod err;

use crate::environment::Environment;
use ast_reducer::reduce_ast;
pub use err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, Value};

type ResVal = Result<Value, EvalError>;
type StdoutHandler = fn(&str) -> ();

/// Produces a value from an AST.
struct Evaluator<'a> {
    ast: &'a Box<Ast>,
    stdout_handler: Option<StdoutHandler>,
}

impl<'a> Evaluator<'a> {
    pub fn new(ast: &'a Box<Ast>) -> Self {
        Self { ast, stdout_handler: None }
    }

    pub fn add_stdout_handler(&mut self, stdout_handler: StdoutHandler) -> () {
        self.stdout_handler = Some(stdout_handler);
    }

    pub fn eval(&self) -> ResVal {
        let mut env = Environment::new();

        builtins::bind(&mut env);

        reduce_ast(self.ast, &mut env, self.stdout_handler)
    }
}

/// Produces a value from an AST.
pub fn eval(ast: &Box<Ast>) -> ResVal {
    Evaluator::new(ast).eval()
}
