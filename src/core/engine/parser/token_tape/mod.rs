use crate::core::syntax::Token;
use crate::util::Tape;

pub struct TokenTape<'a> {
    tokens: &'a Vec<Token>,
    base_index: usize,
}

impl<'a> TokenTape<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens,
            base_index: 0,
        }
    }
}

impl<'a> Tape for TokenTape<'a> {
    type Item = &'a Token;

    fn get_current(&self) -> Option<Self::Item> {
        self.tokens.get(self.base_index)
    }

    fn peek_next(&self) -> Option<Self::Item> {
        self.tokens.get(self.base_index + 1)
    }

    fn advance(&mut self) -> () {
        if self.base_index == self.tokens.len() {
            return ();
        }

        self.base_index += 1;
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
    ];

    const TOKEN_MOCKS: &[Token] = &[
        Token::new(TokenKind::Number(1.0), RANGE_MOCKS[0]),
        Token::new(TokenKind::Number(2.0), RANGE_MOCKS[1]),
    ];

    #[test]
    fn test_get_current_for_empty() {
        let tokens: Vec<Token> = vec![];

        let tape = TokenTape::new(&tokens);

        assert_eq!(tape.get_current(), None);
    }

    #[test]
    fn test_get_current_twice() {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let tape = TokenTape::new(&tokens);

        assert_eq!(tape.get_current(), Some(&TOKEN_MOCKS[0]));
        assert_eq!(tape.get_current(), Some(&TOKEN_MOCKS[0]));
    }

    #[test]
    fn test_peek_next() {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let tape = TokenTape::new(&tokens);

        assert_eq!(tape.peek_next(), Some(&TOKEN_MOCKS[1]));
    }

    #[test]
    fn test_peek_next_twice() {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let tape = TokenTape::new(&tokens);

        assert_eq!(tape.peek_next(), Some(&TOKEN_MOCKS[1]));
        assert_eq!(tape.peek_next(), Some(&TOKEN_MOCKS[1]));
    }

    #[test]
    fn test_peek_next_for_empty() {
        let tokens: Vec<Token> = vec![];

        let tape = TokenTape::new(&tokens);

        assert_eq!(tape.get_current(), None);
    }

    #[test]
    fn test_advance() {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let mut tape = TokenTape::new(&tokens);

        tape.advance();
        assert_eq!(tape.get_current(), Some(&TOKEN_MOCKS[1]));
        assert_eq!(tape.peek_next(), None);
    }

    #[test]
    fn test_advance_for_empty() {
        let tokens: Vec<Token> = vec![];

        let mut tape = TokenTape::new(&tokens);

        tape.advance();
        assert_eq!(tape.get_current(), None);
        assert_eq!(tape.peek_next(), None);
    }
}
