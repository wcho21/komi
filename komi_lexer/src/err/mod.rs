use komi_util::EngineError;
use std::fmt;

/// Errors that may occurs during the lexing process.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq)]
pub enum LexErrorKind {
    /// No source to lex.
    NoSource,
    /// An illegal char, not in the syntax.
    IllegalChar,
    /// An illegal number literal, such as `12.`.
    IllegalNumLiteral,
    /// Not closed quote, such as `"...`
    StrQuoteNotClosed,
    /// String interpolation has begun with a left brace but not closed with a right brace, such as `"{"`.
    InterpolationNotClosed,
    /// String interpolation has no identifier, such as `"{}"`.
    NoInterpolatedIdentifier,
    /// An illegal character for an identifier in string interpolation, such as `+` in `"{사+}"`.
    IllegalInterpolationChar,
    /// An illegal right brace, such as `}` in `"}"`.
    IllegalRBraceInStr,
    /// An internal error impossible to occur if lexed as expected.
    Unexpected,
}

pub type LexError = EngineError<LexErrorKind>;

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            LexErrorKind::NoSource => "NoSource",
            LexErrorKind::IllegalChar => "IllegalChar",
            LexErrorKind::IllegalNumLiteral => "IllegalNumLiteral",
            LexErrorKind::StrQuoteNotClosed => "StrQuoteNotClosed",
            LexErrorKind::InterpolationNotClosed => "InterpolationNotClosed",
            LexErrorKind::NoInterpolatedIdentifier => "NoInterpolatedIdentifier",
            LexErrorKind::IllegalInterpolationChar => "IllegalInterpolationChar",
            LexErrorKind::IllegalRBraceInStr => "IllegalRBraceInStr",
            LexErrorKind::Unexpected => "Unexpected",
        };
        write!(f, "{}", s)
    }
}
