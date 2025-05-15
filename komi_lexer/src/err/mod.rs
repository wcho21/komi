use komi_util::Range;
use std::error::Error;
use std::fmt;

/// An Error that occurs during the lexing process.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub reason: LexErrorReason,
}

/// Kinds of errors due to the lexing.
#[derive(Debug, PartialEq)]
pub enum LexErrorKind {
    /// An illegal char, not in the syntax.
    IllegalChar,
    /// An illegal number literal, such as `12.`.
    IllegalNumLiteral,
    /// An internal error impossible to occur if lexed as expected.
    Unexpected,
}

/// Reason of the error, with the string `cause` and its location `location` in the source.
#[derive(Debug, PartialEq)]
pub struct LexErrorReason {
    pub cause: String,
    pub location: Range,
}

impl LexError {
    pub fn new(kind: LexErrorKind, cause: String, location: Range) -> Self {
        Self { kind, reason: LexErrorReason::new(cause, location) }
    }
}

impl LexErrorReason {
    pub fn new(cause: String, location: Range) -> Self {
        Self { cause, location }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Reason: '{}', Cause: {}, Location: {:?}",
            self.kind, self.reason.cause, self.reason.location
        )
    }
}

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

impl<'a> Error for LexError {}
