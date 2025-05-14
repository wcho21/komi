mod eval;

pub use eval::EvalError;
pub use komi_lexer::LexError;
pub use komi_parser::ParseError;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
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

impl From<LexError> for ExecError {
    fn from(err: LexError) -> Self {
        ExecError::Lex(err)
    }
}

impl From<ParseError> for ExecError {
    fn from(err: ParseError) -> Self {
        ExecError::Parse(err)
    }
}

impl From<EvalError> for ExecError {
    fn from(err: EvalError) -> Self {
        ExecError::Eval(err)
    }
}

impl<'a> Error for ExecError {}
