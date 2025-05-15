use komi_util::Range;
use std::error::Error;
use std::fmt;

/// Errors that can occur during the lexing process.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq)]
pub enum LexError {
    /// An illegal char, not in the syntax.
    IllegalChar { cause: String, location: Range },
    /// An illegal number literal, such as `12.`.
    IllegalNumLiteral { cause: String, location: Range },
    /// An internal error impossible to occur if lexed as expected.
    Unexpected { cause: String, location: Range },
}

macro_rules! write_lex_err {
    ($fmt:ident, $reason:literal, $cause:ident, $loc: ident) => {
        write!($fmt, "Reason: {}, Cause: '{}', Location: {:?}", $reason, $cause, $loc)
    };
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexError::IllegalChar { cause: c, location: l } => write_lex_err!(f, "LEX_ILLEGAL_CHAR", c, l),
            LexError::IllegalNumLiteral { cause: c, location: l } => write_lex_err!(f, "LEX_ILLEGAL_NUM_LITERAL", c, l),
            LexError::Unexpected { cause: c, location: l } => write_lex_err!(f, "LEX_UNEXPECTED", c, l),
        }
    }
}

impl<'a> Error for LexError {}
