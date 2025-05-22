use komi_util::EngineError;
use std::fmt;

/// Errors that may occur during the evaluating process.
/// Serves as the interface between a evaluator and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum EvalErrorKind {
    UndefinedIdentifier,
    InvalidNumInfixOperand,
    InvalidBoolInfixOperand,
    InvalidNumPrefixOperand,
    InvalidBoolPrefixOperand,
    Unexpected,
}

pub type EvalError = EngineError<EvalErrorKind>;

impl fmt::Display for EvalErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            EvalErrorKind::UndefinedIdentifier => "UndefinedIdentifier",
            EvalErrorKind::InvalidNumInfixOperand => "InvalidNumInfixOperand",
            EvalErrorKind::InvalidBoolInfixOperand => "InvalidBoolInfixOperand",
            EvalErrorKind::InvalidNumPrefixOperand => "InvalidNumPrefixOperand",
            EvalErrorKind::InvalidBoolPrefixOperand => "InvalidBoolPrefixOperand",
            EvalErrorKind::Unexpected => "Unexpected",
        };
        write!(f, "{}", s)
    }
}
