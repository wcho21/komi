use super::Range;
use std::error::Error;
use std::fmt;

/// An error that occurs in the execution engine.
#[derive(Debug, PartialEq)]
pub struct EngineError<T> {
    /// An error kind.
    pub kind: T,
    /// The location of the cause in the source.
    pub location: Range,
}

impl<T> EngineError<T> {
    pub fn new(kind: T, location: Range) -> Self {
        Self { kind, location }
    }
}

impl<T: fmt::Display> fmt::Display for EngineError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Reason: '{}', Location: {:?}", self.kind, self.location)
    }
}

impl<T: fmt::Display + fmt::Debug> Error for EngineError<T> {}
