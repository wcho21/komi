use komi_util::error::EngineError;
use std::fmt;

/// Errors that may occur during the evaluating process.
/// Serves as the interface between a evaluator and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum EvalErrorKind {
    /// No expressions to evaluate.
    NoExpressions,
    /// An identifier is undefined, such as using it before assignment.
    UndefinedIdentifier,
    /// A left-hand side value of an assignment expression is not identifier, such as `1 = 2`.
    NonIdLeftValInAssign,
    /// Expected a numeric value as an operand of an infix, but it isn't, such as `참` in `1 + 참`.
    NonNumInfixOperand,
    /// Expected a boolean value as an operand of an infix, but it isn't, such as `1` in `참 또는 1`.
    NonBoolInfixOperand,
    /// Expected a numeric value as an operand of a prefix, but it isn't, such as `참` in `+참`.
    NonNumPrefixOperand,
    /// Expected a boolean value as an operand of a prefix, but it isn't, such as `1` in `!1`.
    NonBoolPrefixOperand,
    /// Expected a callble value as a call target, but it isn't, such as `1()`.
    InvalidCallTarget,
}

pub type EvalError = EngineError<EvalErrorKind>;

impl fmt::Display for EvalErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            EvalErrorKind::NoExpressions => "NoExpressions",
            EvalErrorKind::UndefinedIdentifier => "UndefinedIdentifier",
            EvalErrorKind::NonIdLeftValInAssign => "NonIdLeftValInAssign",
            EvalErrorKind::NonNumInfixOperand => "NonNumInfixOperand",
            EvalErrorKind::NonBoolInfixOperand => "NonBoolInfixOperand",
            EvalErrorKind::NonNumPrefixOperand => "NonNumPrefixOperand",
            EvalErrorKind::NonBoolPrefixOperand => "NonBoolPrefixOperand",
            EvalErrorKind::InvalidCallTarget => "InvalidCallTarget",
        };
        write!(f, "{}", s)
    }
}
