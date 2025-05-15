use super::Range;

/// Reason of the error, with the string `cause` and its location `location` in the source.
#[derive(Debug, PartialEq)]
pub struct ErrorReason {
    pub cause: String,
    pub location: Range,
}

impl ErrorReason {
    pub fn new(cause: String, location: Range) -> Self {
        Self { cause, location }
    }
}
