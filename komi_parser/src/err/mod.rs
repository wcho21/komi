use komi_util::EngineError;
use std::fmt;

/// Errors that may occur during the parsing process.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    // An invalid token at the start of an expression, such as `*2` and `/3.
    InvalidExprStart,
    // A left parenthesis `(` not closed, such as `(1+2`.
    LParenNotClosed,
    // No infix right operand, such as `1+`.
    NoInfixRightOperand,
    // No prefix operand, such as `+`.
    NoPrefixOperand,
    // Invalid tokens in function parameters, such as `함수 +`.
    InvalidFuncParam,
    // A function body beginning with `{` is not closed, such as `함수 {`
    FuncBodyNotClosed,
    // Invalid tokens in call arguments, such as `사과(`.
    InvalidCallArgs,
    /// An internal error impossible to occur if parsed as expected.
    Unexpected,
}
// TODO: rename func to closure

pub type ParseError = EngineError<ParseErrorKind>;

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            ParseErrorKind::InvalidExprStart => "InvalidExprStart",
            ParseErrorKind::LParenNotClosed => "LParenNotClosed",
            ParseErrorKind::NoInfixRightOperand => "NoInfixRightOperand",
            ParseErrorKind::NoPrefixOperand => "NoPrefixOperand",
            ParseErrorKind::InvalidFuncParam => "InvalidFuncParam",
            ParseErrorKind::FuncBodyNotClosed => "FuncBodyNotClosed",
            ParseErrorKind::InvalidCallArgs => "InvalidCallArgs",
            ParseErrorKind::Unexpected => "Unexpected",
        };
        write!(f, "{}", s)
    }
}
