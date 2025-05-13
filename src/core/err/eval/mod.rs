use crate::util::Range;
use std::error::Error;
use std::fmt;

/// Errors that can occur during the evaluating process.
/// Serves as the interface between a evaluator and its user.
#[derive(Debug)]
pub enum EvalError {
    BadAdditionOperand(String, Range),
    Unexpected(String, Range),
}

impl<'a> fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::BadAdditionOperand(str, location) => write!(
                f,
                "Reason: EVAL_BAD_ADDITION_OPERAND, Cause: '{}', Location: {:?}",
                str, location
            ),
            EvalError::Unexpected(str, location) => write!(
                f,
                "Reason: EVAL_UNEXPECTED, Cause: '{}', Location: {:?}",
                str, location
            ),
        }
    }
}

impl<'a> Error for EvalError {}
