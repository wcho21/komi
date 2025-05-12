use crate::util::Range;
use std::error::Error;
use std::fmt;

/// Errors that can occur during the evaluating process.
/// Serves as the interface between a evaluator and its user.
#[derive(Debug)]
pub enum EvalErr {
    Unexpected(String, Range),
}

impl<'a> fmt::Display for EvalErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalErr::Unexpected(str, location) => write!(
                f,
                "Reason: PARSE_UNEXPECTED, Cause: '{}', Location: {:?}",
                str, location
            ),
        }
    }
}

impl<'a> Error for EvalErr {}
