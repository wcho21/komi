use crate::util::Range;
use std::error::Error;
use std::fmt;

/// Errors that can occur during the lexing process.
/// Serves as the interface between a lexer and its user.
#[derive(Debug)]
pub enum LexErr {
    IllegalChar(String, Range),
    BadNumLiteral(String, Range),
}

impl fmt::Display for LexErr {
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

impl<'a> Error for LexErr {}
