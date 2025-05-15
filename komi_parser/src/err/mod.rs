use komi_util::Range;
use std::error::Error;
use std::fmt;

/// Errors that can occur during the parsing process.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub reason: ParseErrorReason,
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    Unexpected,
}

/// Reason of the error, with the string `cause` and its location `location` in the source.
#[derive(Debug, PartialEq)]
pub struct ParseErrorReason {
    pub cause: String,
    pub location: Range,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, cause: String, location: Range) -> Self {
        Self { kind, reason: ParseErrorReason::new(cause, location) }
    }
}

impl ParseErrorReason {
    pub fn new(cause: String, location: Range) -> Self {
        Self { cause, location }
    }
}

impl<'a> fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Reason: '{}', Cause: '{}', Location: {:?}",
            self.kind, self.reason.cause, self.reason.location
        )
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
