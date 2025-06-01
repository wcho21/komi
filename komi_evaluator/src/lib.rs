//! # Evaluator
//!
//! Reads *an abstract syntax tree (AST)* and returns a *value* as defined in the `komi_syntax` crate.
//! Note that *value* is a technical term referring to the result of evaluation.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod ast_reducer;
mod builtins;
mod environment;

use crate::environment::Environment;
use ast_reducer::reduce_ast;
use komi_syntax::error::EvalError;
use komi_syntax::{Ast, Stdout, Value};

pub type ValRes = Result<Value, EvalError>;
pub type ValsRes = Result<Vec<Value>, EvalError>;

/// Produces a value from an AST.
pub struct Evaluator<'a> {
    ast: &'a Box<Ast>,
    stdouts: Stdout,
}

impl<'a> Evaluator<'a> {
    pub fn new(ast: &'a Box<Ast>) -> Self {
        Self { ast, stdouts: vec![] }
    }

    pub fn eval(&mut self) -> ValRes {
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
pub fn eval(ast: &Box<Ast>) -> ValRes {
    Evaluator::new(ast).eval()
}

#[cfg(test)]
mod tests {
    use super::*;
    use komi_syntax::{Ast, AstKind, ValueKind, mkast};
    use komi_util::{Range, str_loc};

    #[test]
    fn test_stdout_num_without_decimal() {
        let ast = mkast!(prog loc str_loc!("", "쓰기(42)"), vec![
            mkast!(call loc str_loc!("", "쓰기(42)"),
                target mkast!(identifier "쓰기", loc str_loc!("", "쓰기")),
                args vec![
                    mkast!(num 42.0, loc str_loc!("쓰기(", "42")),
                ],
            ),
        ]);
        let mut evaluator = Evaluator::new(&ast);

        let repr = evaluator.eval();
        let stdout = evaluator.flush();

        assert_eq!(repr, Ok(Value::new(ValueKind::Number(2.0), str_loc!("", "쓰기(42)")))); // `2` is the length of `42`
        assert_eq!(stdout, "42");
    }

    #[test]
    fn test_stdout_num_with_decimal() {
        let ast = mkast!(prog loc str_loc!("", "쓰기(12.25)"), vec![
            mkast!(call loc str_loc!("", "쓰기(12.25)"),
                target mkast!(identifier "쓰기", loc str_loc!("", "쓰기")),
                args vec![
                    mkast!(num 12.25, loc str_loc!("쓰기(", "12.25")),
                ],
            ),
        ]);
        let mut evaluator = Evaluator::new(&ast);

        let repr = evaluator.eval();
        let stdout = evaluator.flush();

        assert_eq!(
            repr,
            Ok(Value::new(ValueKind::Number(5.0), str_loc!("", "쓰기(12.25)"))), // `5` is the length of "12.25"
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
