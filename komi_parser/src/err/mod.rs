use komi_util::EngineError;
use std::fmt;

/// Errors that may occur during the parsing process.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    // An invalid token at the start of an expression, such as `*2` and `/3.
    InvalidExprStart,
    // No prefix operand, such as `+`.
    NoPrefixOperand,
    // No infix right operand, such as `1+`.
    NoInfixRightOperand,
    // A left parenthesis `(` not closed, such as `(1+2`.
    LParenNotClosed,
    // Invalid tokens in closure parameters, such as `함수 +`.
    InvalidClosureParam,
    // A closure body beginning with `{` is not closed, such as `함수 {`
    ClosureBodyNotClosed,
    // Invalid tokens in call arguments, such as `사과(`.
    InvalidCallArgs,
    /// An internal error impossible to occur if parsed as expected.
    Unexpected,
}

pub type ParseError = EngineError<ParseErrorKind>;

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            ParseErrorKind::InvalidExprStart => "InvalidExprStart",
            ParseErrorKind::LParenNotClosed => "LParenNotClosed",
            ParseErrorKind::NoInfixRightOperand => "NoInfixRightOperand",
            ParseErrorKind::NoPrefixOperand => "NoPrefixOperand",
            ParseErrorKind::InvalidClosureParam => "InvalidClosureParam",
            ParseErrorKind::ClosureBodyNotClosed => "ClosureBodyNotClosed",
            ParseErrorKind::InvalidCallArgs => "InvalidCallArgs",
            ParseErrorKind::Unexpected => "Unexpected",
        };
        write!(f, "{}", s)
    }
}
