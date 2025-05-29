use komi_util::EngineError;
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
    GroupNotClosed,
    /// The source ends with closure keyword, such as `함수`.
    EndWithClosureKeyword,
    /// The source ends with closure parameters, such as `함수 사과` or `함수 사과, 오렌지.
    EndWithClosureParams,
    /// Non-identifier closure parameters, such as `함수 1`.
    NonIdClosureParams,
    /// Something else appears where the comma would be in the closure parameters, such as `함수 사과 바나나`.
    MissingCommaClosureParams,
    /// Invalid tokens in closure parameters, such as `함수 +`.
    // TODO: remove this error (replaced by NonIdentifierClosureParams)
    InvalidClosureParams,
    /// A closure body beginning with `{` is not closed, such as `함수 {`
    ClosureBodyNotClosed,
    /// A left parenthesis `(` not closed for call arguments, such as `사과(` or `사과(1,`.
    CallArgsNotClosed,
    /// Something else appears where the comma would be in the call arguments, such as `2` in `사과(1 2)`.
    MissingCommaCallArgs,
    /// A closure body is empty, which should not, such as `함수 {}`.
    EmptyClosureBody,
    /// An unexpected error due to incorrect expression parsing. Should not occur.
    UnexpectedExprInfix,
}

pub type ParseError = EngineError<ParseErrorKind>;

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            ParseErrorKind::InvalidExprStart => "InvalidExprStart",
            ParseErrorKind::GroupNotClosed => "GroupNotClosed",
            ParseErrorKind::NoInfixRightOperand => "NoInfixRightOperand",
            ParseErrorKind::NoPrefixOperand => "NoPrefixOperand",
            ParseErrorKind::EndWithClosureKeyword => "EndWithClosureKeyword",
            ParseErrorKind::EndWithClosureParams => "EndWithClosureParams",
            ParseErrorKind::NonIdClosureParams => "NonIdClosureParams",
            ParseErrorKind::MissingCommaClosureParams => "MissingCommaClosureParams",
            ParseErrorKind::InvalidClosureParams => "InvalidClosureParams",
            ParseErrorKind::ClosureBodyNotClosed => "ClosureBodyNotClosed",
            ParseErrorKind::CallArgsNotClosed => "CallArgsNotClosed",
            ParseErrorKind::MissingCommaCallArgs => "MissingCommaCallArgs",
            ParseErrorKind::EmptyClosureBody => "EmptyClosureBody",
            ParseErrorKind::UnexpectedExprInfix => "UnexpectedExprInfix",
        };
        write!(f, "{}", s)
    }
}
