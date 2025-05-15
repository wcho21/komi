use komi_util::{ErrorReason, Range};
use std::error::Error;
use std::fmt;

/// Errors that can occur during the parsing process.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub reason: ErrorReason,
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    Unexpected,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, location: Range) -> Self {
        Self { kind, reason: ErrorReason::new(location) }
    }
}

impl<'a> fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Reason: '{}', Location: {:?}", self.kind, self.reason.location)
    }
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            ParseErrorKind::Unexpected => "Unexpected",
        };
        write!(f, "{}", s)
    }
}

impl<'a> Error for ParseError {}
