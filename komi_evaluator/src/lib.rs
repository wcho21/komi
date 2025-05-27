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
use komi_syntax::{Ast, Stdout, Value};

type ResVal = Result<Value, EvalError>;

/// Produces a value from an AST.
pub struct Evaluator<'a> {
    ast: &'a Box<Ast>,
    stdouts: Stdout,
}

impl<'a> Evaluator<'a> {
    pub fn new(ast: &'a Box<Ast>) -> Self {
        Self { ast, stdouts: vec![] }
    }

    pub fn eval(&mut self) -> ResVal {
        let mut env = Environment::new();

        builtins::bind(&mut env);

        reduce_ast(self.ast, &mut env, &mut self.stdouts)
    }

    pub fn flush(&mut self) -> String {
        let stdout = self.stdouts.join(" ");

        self.stdouts.clear();

        stdout
    }
}

/// Produces a value from an AST.
#[deprecated]
pub fn eval(ast: &Box<Ast>) -> ResVal {
    Evaluator::new(ast).eval()
}

#[cfg(test)]
mod tests {
    use super::*;
    use komi_syntax::{Ast, AstKind, ValueKind, mkast};
    use komi_util::Range;

    #[test]
    fn test_stdout_num_without_decimal() {
        let ast = mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(call loc 0, 0, 0, 5,
                target mkast!(identifier "쓰기", loc 0, 0, 0, 2),
                args vec![
                    mkast!(num 1.0, loc 0, 3, 0, 4),
                ],
            ),
        ]);
        let mut evaluator = Evaluator::new(&ast);

        let repr = evaluator.eval();
        let stdout = evaluator.flush();

        assert_eq!(
            repr,
            Ok(Value::new(ValueKind::Number(1.0), Range::from_nums(0, 0, 0, 5))) // TODO: correct location
        );
        assert_eq!(stdout, "1");
    }

    #[test]
    fn test_stdout_num_with_decimal() {
        let ast = mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(call loc 0, 0, 0, 5,
                target mkast!(identifier "쓰기", loc 0, 0, 0, 2),
                args vec![
                    mkast!(num 12.25, loc 0, 3, 0, 4),
                ],
            ),
        ]);
        let mut evaluator = Evaluator::new(&ast);

        let repr = evaluator.eval();
        let stdout = evaluator.flush();

        assert_eq!(
            repr,
            Ok(Value::new(ValueKind::Number(5.0), Range::from_nums(0, 0, 0, 5))) // TODO: correct location
        );
        assert_eq!(stdout, "12.25");
    }

    #[test]
    fn test_stdout_id() {
        let ast = mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixEquals, loc 0, 0, 0, 0,
                left mkast!(identifier "사과", loc 0, 0, 0, 0),
                right mkast!(num 1.0, loc 0, 0, 0, 0),
            ),
            mkast!(call loc 0, 0, 0, 5,
                target mkast!(identifier "쓰기", loc 0, 0, 0, 2),
                args vec![
                    mkast!(identifier "사과", loc 0, 0, 0, 0),
                ],
            ),
        ]);
        let mut evaluator = Evaluator::new(&ast);

        let _ = evaluator.eval();

        let stdout = evaluator.flush();

        assert_eq!(stdout, "1");
    }
}
