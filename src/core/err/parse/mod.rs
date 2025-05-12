use crate::util::Range;
use std::error::Error;
use std::fmt;

/// Errors that can occur during the parsing process.
/// Serves as the interface between a parser and its user.
#[derive(Debug)]
pub enum ParseErr {
    Unexpected(String, Range),
}

impl<'a> fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseErr::Unexpected(str, location) => write!(
                f,
                "Reason: PARSE_UNEXPECTED, Cause: '{}', Location: {:?}",
                str, location
            ),
        }
    }
}

impl<'a> Error for ParseErr {}
