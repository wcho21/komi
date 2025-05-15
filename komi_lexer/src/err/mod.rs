use komi_util::EngineError;
use std::fmt;

/// Errors that may occurs during the lexing process.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq)]
pub enum LexErrorKind {
    /// An illegal char, not in the syntax.
    IllegalChar,
    /// An illegal number literal, such as `12.`.
    IllegalNumLiteral,
    /// An internal error impossible to occur if lexed as expected.
    Unexpected,
}

pub type LexError = EngineError<LexErrorKind>;

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            LexErrorKind::IllegalChar => "IllegalChar",
            LexErrorKind::IllegalNumLiteral => "IllegalNumLiteral",
            LexErrorKind::Unexpected => "Unexpected",
        };
        write!(f, "{}", s)
    }
}
