mod lex;
mod parse;

pub use lex::LexErr;
pub use parse::ParseErr;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum ExecErr {
    LexErr(LexErr),
    ParseErr(ParseErr),
}

impl<'a> fmt::Display for ExecErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecErr::LexErr(err) => write!(f, "{}", err),
            ExecErr::ParseErr(err) => write!(f, "{}", err),
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

impl<'a> Error for ExecErr {}
