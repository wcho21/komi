use crate::util::Range;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum LexErr<'a> {
    IllegalChar(&'a str, Range),
    BadNumLiteral(&'a str, Range),
}

impl<'a> fmt::Display for LexErr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexErr::IllegalChar(str, location) => write!(
                f,
                "Reason: LEX_ILLEGAL_CHAR, Cause: '{}', Location: {:?}",
                str, location
            ),
            LexErr::BadNumLiteral(str, location) => write!(
                f,
                "Reason: LEX_BAD_NUM_LITERAL, Cause: '{}', Location: {:?}",
                str, location
            ),
        }
    }
}

impl<'a> Error for LexErr<'a> {}
