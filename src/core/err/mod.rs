mod eval;
mod lex;
mod parse;

pub use eval::EvalError;
pub use lex::LexError;
pub use parse::ParseError;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum ExecError {
    LexError(LexError),
    ParseError(ParseError),
    EvalError(EvalError),
}

impl<'a> fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecError::LexError(err) => err.fmt(f),
            ExecError::ParseError(err) => err.fmt(f),
            ExecError::EvalError(err) => err.fmt(f),
        }
    }
}

impl From<LexError> for ExecError {
    fn from(err: LexError) -> Self {
        ExecError::LexError(err)
    }
}

impl From<ParseError> for ExecError {
    fn from(err: ParseError) -> Self {
        ExecError::ParseError(err)
    }
}

impl From<EvalError> for ExecError {
    fn from(err: EvalError) -> Self {
        ExecError::EvalError(err)
    }
}

impl<'a> Error for ExecError {}
