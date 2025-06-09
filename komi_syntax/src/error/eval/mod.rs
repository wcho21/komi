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
    /// Expected a boolean value as an operand of an infix, but it isn't, such as `1` in `참 또는 1`.
    NonBoolInfixOperand,
    /// Expected a numeric value as a left-hand side operand of an index, but it isn't, such as `참` in `참 - 1`.
    NonNumInfixLeftOperand,
    /// Expected a numeric or string value as a left-hand side operand of an index, but it isn't, such as `참` in `참 + 1`.
    NonNumOrStrInfixLeftOperand,
    /// Expected a numeric value as a right-hand side operand of an index, but it isn't, such as `참` in `1 + 참`.
    NonNumInfixRightOperand,
    /// Expected a string value as a right-hand side operand of an index, but it isn't, such as `1` in `"사과" + 1`.
    NonStrInfixRightOperand,
    // TODO: correct typo 'indix' to 'infix'
    /// Expected a non-negative integer value as a right-hand side operand of an index, but it isn't, such as `-1.5` in `"사과" * -1.5`.
    NonNonnegIntInfixRightOperand,
    /// Expected the same type operands for an infix, but it isn't, such as `1 == 참`.
    NotSameTypeInfixOperands,
    /// The type of the left operand is not comparable under equality, such as `함수 { 1 }` in `함수 { 1 } == 1`.
    BadTypeEqLeftOperand,
    /// The type of the right operand is not comparable under equality, such as `함수 { 1 }` in `1 == 함수 { 1 }`.
    BadTypeEqRightOperand,
    /// The type of the left operand is invalid for ordering relation, such as `참` in `참 == 1`.
    BadTypeOrdLeftOperand,
    /// The type of the right operand is invalid for ordering relation, such as `참` in `1 == 참`.
    BadTypeOrdRightOperand,
    /// Expected a numeric value as an operand of a prefix, but it isn't, such as `참` in `+참`.
    NonNumPrefixOperand,
    /// Expected a boolean value as an operand of a prefix, but it isn't, such as `1` in `!1`.
    NonBoolPrefixOperand,
    /// Expected a callble value as a call target, but it isn't, such as `1()`.
    InvalidCallTarget,
    /// Expected a boolean value as a predicate, but it isn't, such as `1` in `만약 1 { 2 } 아니면 { 3 }`.
    NonBoolPred,
    /// The number of arguments is not the same with the one of parameters.
    BadNumArgs,
}

pub type EvalError = EngineError<EvalErrorKind>;

impl fmt::Display for EvalErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            EvalErrorKind::NoExpressions => "NoExpressions",
            EvalErrorKind::UndefinedIdentifier => "UndefinedIdentifier",
            EvalErrorKind::NonIdLeftValInAssign => "NonIdLeftValInAssign",
            EvalErrorKind::NonBoolInfixOperand => "NonBoolInfixOperand",
            EvalErrorKind::NonNumInfixLeftOperand => "NonNumInfixLeftOperand",
            EvalErrorKind::NonNumOrStrInfixLeftOperand => "NonNumOrStrInfixLeftOperand",
            EvalErrorKind::NonNumInfixRightOperand => "NonNumInfixRightOperand",
            EvalErrorKind::NonStrInfixRightOperand => "NonStrInfixRightOperand",
            // TODO: better name `NotNonneg...`?
            EvalErrorKind::NonNonnegIntInfixRightOperand => "NonNonnegIntInfixRightOperadn",
            EvalErrorKind::NotSameTypeInfixOperands => "NotSameTypeInfixOperands",
            EvalErrorKind::BadTypeEqLeftOperand => "BadTypeEqLeftOperand",
            EvalErrorKind::BadTypeEqRightOperand => "BadTypeEqRightOperand",
            EvalErrorKind::BadTypeOrdLeftOperand => "BadTypeOrdLeftOperand",
            EvalErrorKind::BadTypeOrdRightOperand => "BadTypeOrdRightOperand",
            EvalErrorKind::NonNumPrefixOperand => "NonNumPrefixOperand",
            EvalErrorKind::NonBoolPrefixOperand => "NonBoolPrefixOperand",
            EvalErrorKind::InvalidCallTarget => "InvalidCallTarget",
            EvalErrorKind::NonBoolPred => "NonBoolPred",
            EvalErrorKind::BadNumArgs => "BadNumArgs",
        };
        write!(f, "{}", s)
    }
}
