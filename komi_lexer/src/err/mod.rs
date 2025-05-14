use komi_util::Range;
use std::error::Error;
use std::fmt;

/// Errors that can occur during the lexing process.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq)]
pub enum LexError {
    /// An illegal char, not in the syntax.
    IllegalChar { char: String, location: Range },
    /// An internal error impossible to occur if lexed as expected.
    Unexpected { expected: String, received: String, location: Range },
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexError::IllegalChar { char, location } => write!(
                f,
                "Reason: LEX_ILLEGAL_CHAR, Cause: '{}', Location: {:?}",
                char, location
            ),
            LexError::Unexpected { expected, received, location } => write!(
                f,
                "Reason: LEX_UNEXPECTED, Cause: '{}', Expected: '{}', Location: {:?}",
                received, expected, location
            ),
        }
    }
}

impl<'a> Error for LexError {}
