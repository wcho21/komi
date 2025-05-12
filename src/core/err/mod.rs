mod lex;
use std::error::Error;
use std::fmt;

pub use lex::LexErr;

#[derive(Debug)]
pub enum ExecErr<'a> {
    Lex(LexErr<'a>),
}

impl<'a> fmt::Display for ExecErr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecErr::Lex(err) => write!(f, "{}", err),
        }
    }
}

impl<'a> From<LexErr<'a>> for ExecErr<'a> {
    fn from(err: LexErr<'a>) -> Self {
        ExecErr::Lex(err)
    }
}

impl<'a> Error for ExecErr<'a> {}
