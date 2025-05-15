use komi_util::EngineError;
use std::fmt;

/// Errors that may occur during the evaluating process.
/// Serves as the interface between a evaluator and its user.
#[derive(Debug, PartialEq)]
pub enum EvalErrorKind {
    InvalidAdditionOperand,
    InvalidPlusPrefixOperand,
    Unexpected,
}

pub type EvalError = EngineError<EvalErrorKind>;

impl fmt::Display for EvalErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            EvalErrorKind::InvalidAdditionOperand => "InvalidAdditionOperand",
            EvalErrorKind::InvalidPlusPrefixOperand => "InvalidPrefixOperand",
            EvalErrorKind::Unexpected => "Unexpected",
        };
        write!(f, "{}", s)
    }
}
