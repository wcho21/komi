use komi_util::error::EngineError;
use std::fmt;

/// Errors that may occur during the parsing process.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    /// An invalid token at the start of an expression, such as `*2` and `/3.
    InvalidExprStart,
    /// No prefix operand, such as `+`.
    NoPrefixOperand,
    /// No infix right operand, such as `1+`.
    NoInfixRightOperand,
    /// A left parenthesis `(` not closed for grouping, such as `(1+2`.
    NoClosingParenInGroup,
    /// No closure parameters but the source ends with the closure keyword, such as `함수`.
    NoClosureParams,
    /// No closure body but the source ends with closure parameters, such as `함수 사과` or `함수 사과, 오렌지`.
    NoClosureBody,
    /// Non-identifier closure parameters, such as `함수 1`.
    NonIdClosureParams,
    /// Something else appears where the comma would be in the closure parameters, such as `함수 사과 바나나`.
    NoCommaInClosureParams,
    /// A closure body beginning with `{` is not closed, such as `함수 {`
    NoClosingBraceInClosureBody,
    /// A left parenthesis `(` not closed for call arguments, such as `사과(` or `사과(1,`.
    NoClosingParenInCallArgs,
    /// Something else appears where the comma would be in the call arguments, such as `2` in `사과(1 2)`.
    NoCommaInCallArgs,
    /// A closure body is empty, which should not, such as `함수 {}`.
    NoExpressionInClosureBody,
    /// An unexpected error due to incorrect expression parsing. Should not occur.
    UnexpectedExprInfix,
}

pub type ParseError = EngineError<ParseErrorKind>;

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            ParseErrorKind::InvalidExprStart => "InvalidExprStart",
            ParseErrorKind::NoClosingParenInGroup => "NoClosingParenInGroup",
            ParseErrorKind::NoInfixRightOperand => "NoInfixRightOperand",
            ParseErrorKind::NoPrefixOperand => "NoPrefixOperand",
            ParseErrorKind::NoClosureParams => "NoClosureParams",
            ParseErrorKind::NoClosureBody => "NoClosureBody",
            ParseErrorKind::NonIdClosureParams => "NonIdClosureParams",
            ParseErrorKind::NoCommaInClosureParams => "NoCommaInClosureParams",
            ParseErrorKind::NoClosingBraceInClosureBody => "NoClosingBraceInClosureBody",
            ParseErrorKind::NoClosingParenInCallArgs => "NoClosingParenInCallArgs",
            ParseErrorKind::NoCommaInCallArgs => "NoCommaInCallArgs",
            ParseErrorKind::NoExpressionInClosureBody => "NoExpressionInClosureBody",
            ParseErrorKind::UnexpectedExprInfix => "UnexpectedExprInfix",
        };
        write!(f, "{}", s)
    }
}
