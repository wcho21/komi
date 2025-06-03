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
    #[deprecated]
    NonNumInfixOperand,
    /// Expected a boolean value as an operand of an infix, but it isn't, such as `1` in `참 또는 1`.
    #[deprecated]
    NonBoolInfixOperand,
    /// Expected a numeric value as a left-hand side operand of an index, but it isn't, such as `참` in `참 - 1`.
    NonNumInfixLeftOperand,
    /// Expected a numeric or string value as a left-hand side operand of an index, but it isn't, such as `참` in `참 + 1`.
    NonNumOrStrInfixLeftOperand,
    /// Expected a numeric value as a right-hand side operand of an index, but it isn't, such as `참` in `1 + 참`.
    NonNumInfixRightOperand,
    /// Expected a string value as a right-hand side operand of an index, but it isn't, such as `1` in `"사과" + 1`.
    NonStrInfixRightOperand,
    /// Expected a numeric value as an operand of a prefix, but it isn't, such as `참` in `+참`.
    NonNumPrefixOperand,
    /// Expected a boolean value as an operand of a prefix, but it isn't, such as `1` in `!1`.
    NonBoolPrefixOperand,
    /// Expected a callble value as a call target, but it isn't, such as `1()`.
    InvalidCallTarget,
    /// Expected a boolean value as a predicate, but it isn't, such as `1` in `만약 1 { 2 } 아니면 { 3 }`.
    NonBoolPred,
}

pub type EvalError = EngineError<EvalErrorKind>;

impl fmt::Display for EvalErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            EvalErrorKind::NoExpressions => "NoExpressions",
            EvalErrorKind::UndefinedIdentifier => "UndefinedIdentifier",
            EvalErrorKind::NonIdLeftValInAssign => "NonIdLeftValInAssign",
            #[allow(deprecated)]
            EvalErrorKind::NonNumInfixOperand => "NonNumInfixOperand",
            #[allow(deprecated)]
            EvalErrorKind::NonBoolInfixOperand => "NonBoolInfixOperand",
            EvalErrorKind::NonNumInfixLeftOperand => "NonNumInfixLeftOperand",
            EvalErrorKind::NonNumOrStrInfixLeftOperand => "NonNumOrStrInfixLeftOperand",
            EvalErrorKind::NonNumInfixRightOperand => "NonNumInfixRightOperand",
            EvalErrorKind::NonStrInfixRightOperand => "NonStrInfixRightOperand",
            EvalErrorKind::NonNumPrefixOperand => "NonNumPrefixOperand",
            EvalErrorKind::NonBoolPrefixOperand => "NonBoolPrefixOperand",
            EvalErrorKind::InvalidCallTarget => "InvalidCallTarget",
            EvalErrorKind::NonBoolPred => "NonBoolPred",
        };
        write!(f, "{}", s)
    }
}
