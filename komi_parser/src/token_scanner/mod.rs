use komi_syntax::token::Token;
use komi_util::location::Range;
use komi_util::scanner::Scanner;

pub struct TokenScanner<'a> {
    tokens: &'a Vec<Token>,
    base_index: usize,
    last_location: Range,
}

impl<'a> TokenScanner<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self { tokens, base_index: 0, last_location: Range::ORIGIN }
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
    type Item = Option<&'a Token>;

    fn read(&self) -> Self::Item {
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
    use fixtures::*;
    use komi_syntax::token::TokenKind;

    #[test]
    fn test_read_for_empty() {
        let tokens: Vec<Token> = vec![];

        let scanner = TokenScanner::new(&tokens);

        assert_eq!(scanner.read(), None);
    }

    #[test]
    fn test_read_twice() {
        let tokens = vec![token1(), token2()];

        let scanner = TokenScanner::new(&tokens);

        assert_eq!(scanner.read(), Some(&token1()));
        assert_eq!(scanner.read(), Some(&token1()));
    }

    #[test]
    fn test_advance() {
        let tokens = vec![token1(), token2()];

        let mut scanner = TokenScanner::new(&tokens);

        scanner.advance();
        assert_eq!(scanner.read(), Some(&token2()));
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
        let tokens = vec![
            Token::new(token_kind1(), Range::from_nums(0, 0, 0, 2)),
            Token::new(token_kind2(), Range::from_nums(0, 2, 0, 5)),
        ];

        let mut scanner = TokenScanner::new(&tokens);

        assert_eq!(scanner.locate(), Range::from_nums(0, 0, 0, 2));
        scanner.advance();
        assert_eq!(scanner.locate(), Range::from_nums(0, 2, 0, 5));
        scanner.advance();
        assert_eq!(scanner.locate(), Range::from_nums(0, 5, 0, 5));
    }

    #[test]
    fn test_locate_for_empty() {
        let tokens = vec![];

        let mut scanner = TokenScanner::new(&tokens);

        assert_eq!(scanner.locate(), Range::ORIGIN);
        scanner.advance();
        assert_eq!(scanner.locate(), Range::ORIGIN);
    }

    mod fixtures {
        use super::*;

        pub fn range1() -> Range {
            Range::from_nums(0, 0, 0, 1)
        }
        pub fn range2() -> Range {
            Range::from_nums(0, 1, 0, 2)
        }
        pub fn token_kind1() -> TokenKind {
            TokenKind::Number(1.0)
        }
        pub fn token_kind2() -> TokenKind {
            TokenKind::Number(2.0)
        }
        pub fn token1() -> Token {
            Token::new(token_kind1(), range1())
        }
        pub fn token2() -> Token {
            Token::new(token_kind2(), range2())
        }
    }
}
