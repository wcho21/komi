use super::Range;

/// Reason of the error, with its location `location` in the source.
#[derive(Debug, PartialEq)]
pub struct ErrorReason {
    pub location: Range,
}

impl ErrorReason {
    pub fn new(location: Range) -> Self {
        Self { location }
    }
}
