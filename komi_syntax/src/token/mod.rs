use komi_util::Range;

/// Kinds of tokens produced during lexing.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    /// A number with or without decimal, such as `12` or `12.25`.
    Number(f64),
    /// A plus `+`
    Plus,
    /// A minus `-`
    Minus,
    /// An asterisk `*`
    Asterisk,
    /// A slash `/`
    Slash,
    /// A percent `%`
    Percent,
    /// A left parenthesis `(`
    LParen,
    /// A right parenthesis `)`
    RParen,
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

    pub const fn from_num(num: f64, location: Range) -> Self {
        Token::new(TokenKind::Number(num), location)
    }

    pub const fn from_plus(location: Range) -> Self {
        Token::new(TokenKind::Plus, location)
    }

    pub const fn from_minus(location: Range) -> Self {
        Token::new(TokenKind::Minus, location)
    }

    pub const fn from_asterisk(location: Range) -> Self {
        Token::new(TokenKind::Asterisk, location)
    }

    pub const fn from_slash(location: Range) -> Self {
        Token::new(TokenKind::Slash, location)
    }

    pub const fn from_percent(location: Range) -> Self {
        Token::new(TokenKind::Percent, location)
    }

    pub const fn from_lparen(location: Range) -> Self {
        Token::new(TokenKind::LParen, location)
    }

    pub const fn from_rparen(location: Range) -> Self {
        Token::new(TokenKind::RParen, location)
    }
}

/// Makes a token with the kind and the location specified by four numbers.
#[macro_export]
macro_rules! mktoken {
    ($kind:expr, loc $br:expr, $bc:expr, $er:expr, $ec:expr) => {
        Token::new($kind, Range::from_nums($br as u64, $bc as u64, $er as u64, $ec as u64))
    };
}

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

    #[test]
    fn test_from_num() {
        let token = Token::from_num(1.0, RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::Number(1.0), location: RANGE_MOCK })
    }

    #[test]
    fn test_from_plus() {
        let token = Token::from_plus(RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::Plus, location: RANGE_MOCK })
    }

    #[test]
    fn test_from_minus() {
        let token = Token::from_minus(RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::Minus, location: RANGE_MOCK })
    }

    #[test]
    fn test_from_asterisk() {
        let token = Token::from_asterisk(RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::Asterisk, location: RANGE_MOCK })
    }

    #[test]
    fn test_from_slash() {
        let token = Token::from_slash(RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::Slash, location: RANGE_MOCK })
    }

    #[test]
    fn test_from_percent() {
        let token = Token::from_percent(RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::Percent, location: RANGE_MOCK })
    }

    #[test]
    fn test_from_lparen() {
        let token = Token::from_lparen(RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::LParen, location: RANGE_MOCK })
    }

    #[test]
    fn test_from_rparen() {
        let token = Token::from_rparen(RANGE_MOCK);

        assert_eq!(token, Token { kind: TokenKind::RParen, location: RANGE_MOCK })
    }
}
