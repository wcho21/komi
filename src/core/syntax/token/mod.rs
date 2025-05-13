use crate::util::Range;

/// Kinds of tokens produced during lexing.
/// Serves as the interface between a lexer and its user.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Number(f64),
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::Spot;

    const RANGE_MOCK: Range = Range::new(Spot::new(1, 2), Spot::new(3, 4));

    #[test]
    fn test_new() {
        let token = Token::new(TokenKind::Number(1.0), RANGE_MOCK);

        assert_eq!(
            token,
            Token {
                kind: TokenKind::Number(1.0),
                location: RANGE_MOCK,
            }
        )
    }

    #[test]
    fn test_from_num() {
        let token = Token::from_num(1.0, RANGE_MOCK);

        assert_eq!(
            token,
            Token {
                kind: TokenKind::Number(1.0),
                location: RANGE_MOCK,
            }
        )
    }
}
