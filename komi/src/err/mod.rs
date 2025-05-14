pub use komi_evaluator::EvalError;
pub use komi_lexer::LexError;
pub use komi_parser::ParseError;
use std::error::Error;
use std::fmt;

/// Errors that can occur during the execution process.
#[derive(Debug, PartialEq)]
pub enum ExecError {
    Lex(LexError),
    Parse(ParseError),
    Eval(EvalError),
}

impl<'a> fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecError::Lex(err) => err.fmt(f),
            ExecError::Parse(err) => err.fmt(f),
            ExecError::Eval(err) => err.fmt(f),
        }
    }
}

macro_rules! from_error {
    ($err:ty, $var:ident) => {
        impl From<$err> for ExecError {
            fn from(err: $err) -> Self {
                ExecError::$var(err)
            }
        }
    };
}

from_error!(LexError, Lex);
from_error!(ParseError, Parse);
from_error!(EvalError, Eval);

impl<'a> Error for ExecError {}
