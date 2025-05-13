use crate::core::syntax::Token;
use crate::util::{Range, Scanner, range};

pub struct TokenScanner<'a> {
    tokens: &'a Vec<Token>,
    base_index: usize,
    last_location: Range,
}

impl<'a> TokenScanner<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens,
            base_index: 0,
            last_location: range::ORIGIN,
        }
    }

    fn is_end(&self) -> bool {
        self.base_index == self.tokens.len()
    }

    fn make_location_from_last_location_end(&self) -> Range {
        let end = self.last_location.end;
        Range::new(end, end)
    }
}

impl<'a> Scanner for TokenScanner<'a> {
    type Item = &'a Token;

    fn read(&self) -> Option<Self::Item> {
        self.tokens.get(self.base_index)
    }

    fn advance(&mut self) -> () {
        if self.is_end() {
            return;
        }

        self.last_location = self.read().unwrap().location;
        self.base_index += 1;
    }

    fn locate(&self) -> Range {
        if !self.is_end() {
            self.read().unwrap().location
        } else {
            self.make_location_from_last_location_end()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::syntax::TokenKind;
    use crate::util::{Range, Spot};

    const RANGE_MOCKS: &[Range] = &[
        Range::new(Spot::new(0, 0), Spot::new(1, 0)),
        Range::new(Spot::new(1, 0), Spot::new(3, 0)),
        Range::new(Spot::new(3, 0), Spot::new(3, 0)),
    ];

    const TOKEN_MOCKS: &[Token] = &[
        Token::new(TokenKind::Number(1.0), RANGE_MOCKS[0]),
        Token::new(TokenKind::Number(2.0), RANGE_MOCKS[1]),
    ];

    #[test]
    fn test_read_for_empty() {
        let tokens: Vec<Token> = vec![];

        let scanner = TokenScanner::new(&tokens);

        assert_eq!(scanner.read(), None);
    }

    #[test]
    fn test_read_twice() {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let scanner = TokenScanner::new(&tokens);

        assert_eq!(scanner.read(), Some(&TOKEN_MOCKS[0]));
        assert_eq!(scanner.read(), Some(&TOKEN_MOCKS[0]));
    }

    #[test]
    fn test_advance() {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let mut scanner = TokenScanner::new(&tokens);

        scanner.advance();
        assert_eq!(scanner.read(), Some(&TOKEN_MOCKS[1]));
    }

    #[test]
    fn test_advance_for_empty() {
        let tokens: Vec<Token> = vec![];

        let mut scanner = TokenScanner::new(&tokens);

        scanner.advance();
        assert_eq!(scanner.read(), None);
    }

    #[test]
    fn test_locate() {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let mut scanner = TokenScanner::new(&tokens);

        assert_eq!(scanner.locate(), RANGE_MOCKS[0]);
        scanner.advance();
        assert_eq!(scanner.locate(), RANGE_MOCKS[1]);
        scanner.advance();
        assert_eq!(scanner.locate(), RANGE_MOCKS[2]);
    }

    #[test]
    fn test_locate_for_empty() {
        let tokens = vec![];

        let mut scanner = TokenScanner::new(&tokens);

        assert_eq!(scanner.locate(), range::ORIGIN);
        scanner.advance();
        assert_eq!(scanner.locate(), range::ORIGIN);
    }
}
