use komi_util::Range;

/// Kinds of tokens produced during lexing.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    /// A number with or without decimal, such as `12` or `12.25`.
    Number(f64),
    /// A boolean `참` or `거짓`.
    Bool(bool),
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
    /// A bang `!`.
    Bang,
    /// A conjunction `그리고`.
    Conjunct,
    /// A disjunction `또는`.
    Disjunct,
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
}

/// A token produced during lexing.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Range,
}

impl Token {
    pub const fn new(kind: TokenKind, location: Range) -> Self {
        Token { kind, location }
    }
}

/// Makes a token with the kind and the location specified by four numbers.
/// Helps write a token declaratively.
#[macro_export]
macro_rules! mktoken {
    ($kind:expr, loc $br:expr, $bc:expr, $er:expr, $ec:expr) => {
        Token::new($kind, Range::from_nums($br as u32, $bc as u32, $er as u32, $ec as u32))
    };
}

/// Test code as a specification.
/// Each test case shows which token the function returns for a given location.
#[cfg(test)]
mod tests {
    use super::*;
    use komi_util::Spot;

    const RANGE_MOCK: Range = Range::new(Spot::new(1, 2), Spot::new(3, 4));

    #[test]
    fn test_new() {
        let token = Token::new(TokenKind::Number(1.0), RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::Number(1.0), location: RANGE_MOCK })
    }
}
