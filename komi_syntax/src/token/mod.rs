use komi_util::location::Range;
use komi_util::str_segment::StrSegment;

/// A token produced during lexing.
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Range,
}

impl Token {
    pub const fn new(kind: TokenKind, location: Range) -> Self {
        Token { kind, location }
    }
}

/// Kinds of tokens produced during lexing.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    /// A number with or without decimal, such as `12` or `12.25`.
    Number(f64),
    /// A boolean `참` or `거짓`.
    Bool(bool),
    /// A string segment with interpolations, such as `사과` in `"사과"`, or `오렌지` in `"{사과}오렌지"`.
    Str(Vec<StrSegment>),
    /// An identifier, such as `사과` or `오렌지`.
    Identifier(String),
    /// A plus `+`.
    Plus,
    /// A minus `-`.
    Minus,
    /// An asterisk `*`.
    Asterisk,
    /// A slash `/`.
    Slash,
    /// A percent `%`.
    Percent,
    /// A left parenthesis `(`.
    LParen,
    /// A right parenthesis `)`.
    RParen,
    /// A left brace `{`.
    LBrace,
    /// A right brace `}`.
    RBrace,
    /// A left bracket `<`.
    LBracket,
    /// A right bracket `>`.
    RBracket,
    /// A colon `:`.
    Colon,
    /// A comma `,`.
    Comma,
    /// A bang `!`.
    Bang,
    /// An equals `=`.
    Equals,
    /// A plus-equals `+=`,
    PlusEquals,
    /// A minus-equals `-=`.
    MinusEquals,
    /// A asterisk-equals `*=`.
    AsteriskEquals,
    /// A slash-equals `/=`.
    SlashEquals,
    /// A percent-equals `%=`.
    PercentEquals,
    /// A double-equals `==`.
    DoubleEquals,
    /// A bang-equals `!=`.
    BangEquals,
    /// A left-bracket-equals `<=`.
    LBracketEquals,
    /// A right-bracket-equals `>=`.
    RBracketEquals,
    /// A conjunction `그리고`.
    Conjunct,
    /// A disjunction `또는`.
    Disjunct,
    /// A closure keyword `함수`.
    Closure,
    /// An if-branch keyword `만약`.
    IfBranch,
    /// An else-branch keyword `아니면`.
    ElseBranch,
    /// An iteration keyword `반복`.
    Iteration,
}

/// Makes a token with the kind and the location.
/// Helps write a token declaratively.
#[macro_export]
macro_rules! mktoken {
    ($range:expr, $kind:expr $(,)?) => {
        Token::new($kind, $range)
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::*;
    use komi_util::str_segment::StrSegmentKind;

    mod token {
        use super::*;

        #[test]
        fn new() {
            let token = Token::new(TokenKind::Number(1.0), range());

            assert_eq!(token, Token { kind: TokenKind::Number(1.0), location: range() })
        }
    }

    mod str_segment {
        use super::*;

        #[test]
        fn new() {
            let seg = StrSegment::new(StrSegmentKind::Str(String::from("사과")), range());

            assert_eq!(
                seg,
                StrSegment {
                    kind: StrSegmentKind::Str(String::from("사과")),
                    location: range()
                }
            )
        }
    }

    mod str_segment_kind {
        use super::*;

        #[test]
        fn str() {
            let kind = StrSegmentKind::str("사과");

            assert_eq!(kind, StrSegmentKind::Str(String::from("사과")))
        }

        #[test]
        fn identifier() {
            let kind = StrSegmentKind::identifier("사과");

            assert_eq!(kind, StrSegmentKind::Identifier(String::from("사과")))
        }
    }

    mod fixtures {
        use super::*;

        pub fn range() -> Range {
            Range::from_nums(0, 0, 1, 1)
        }
    }
}
