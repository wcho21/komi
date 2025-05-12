mod eval;
mod lex;
mod parse;

pub use eval::EvalErr;
pub use lex::LexErr;
pub use parse::ParseErr;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum ExecErr {
    LexErr(LexErr),
    ParseErr(ParseErr),
    EvalErr(EvalErr),
}

impl<'a> fmt::Display for ExecErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecErr::LexErr(err) => err.fmt(f),
            ExecErr::ParseErr(err) => err.fmt(f),
            ExecErr::EvalErr(err) => err.fmt(f),
        }
    }
}

impl From<LexErr> for ExecErr {
    fn from(err: LexErr) -> Self {
        ExecErr::LexErr(err)
    }
}

impl From<ParseErr> for ExecErr {
    fn from(err: ParseErr) -> Self {
        ExecErr::ParseErr(err)
    }
}

impl From<EvalErr> for ExecErr {
    fn from(err: EvalErr) -> Self {
        ExecErr::EvalErr(err)
    }
}

impl<'a> Error for ExecErr {}
