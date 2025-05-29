//! A segment of a string for syntax modules.
//!
//! A module to express string interpolations.
//! See [`token`] and [`ast`] modules.
//!
//! [`token`]: ../../komi_syntax/token/struct.StrSegment.html
//! [`ast`]: ../../komi_syntax/ast/struct.StrSegment.html

use crate::Range;

/// A string segment in a string token.
#[derive(Debug, PartialEq, Clone)]
pub struct StrSegment {
    pub kind: StrSegmentKind,
    pub location: Range,
}

impl StrSegment {
    pub fn new(kind: StrSegmentKind, location: Range) -> Self {
        Self { kind, location }
    }
}

/// A kind of string segment in a string token.
#[derive(Debug, PartialEq, Clone)]
pub enum StrSegmentKind {
    /// A string segment, such as `사과` in `"사과{오렌지}"`.
    Str(String),
    /// An interpolated identifier, such as `오렌지` in `"사과{오렌지}"`.
    Identifier(String),
}

impl StrSegmentKind {
    pub fn str(s: impl Into<String>) -> Self {
        Self::Str(s.into())
    }

    pub fn identifier(s: impl Into<String>) -> Self {
        Self::Identifier(s.into())
    }
}

#[macro_export]
macro_rules! mkstrseg {
    ($kind:ident, $val:expr, $range:expr) => {
        StrSegment::new(StrSegmentKind::$kind($val.to_owned()), $range)
    };
}
