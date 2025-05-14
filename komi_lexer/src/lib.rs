//! # Lexer
//!
//! Reads a source code and returns tokens as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod err;
mod source_scanner;
mod utf8_tape;

pub use err::LexError;
use komi_syntax::Token;
use komi_util::string;
use komi_util::{Range, Scanner};
use source_scanner::SourceScanner;

type ResTokens = Result<Vec<Token>, LexError>;
type ResToken = Result<Token, LexError>;

/// A lexer to produce tokens from a source.
pub struct Lexer<'a> {
    scanner: SourceScanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { scanner: SourceScanner::new(source) }
    }

    pub fn lex(&mut self) -> ResTokens {
        let mut tokens: Vec<Token> = vec![];

        loop {
            match self.scanner.read() {
                Some(s) if string::is_digit(s) => {
                    let token = self.advance_and_lex_with_first_char(Self::lex_num, s)?;
                    tokens.push(token);
                }
                Some("+") => {
                    let token = self.advance_and_lex(Self::lex_plus)?;
                    tokens.push(token);
                }
                Some("-") => {
                    let token = self.advance_and_lex(Self::lex_minus)?;
                    tokens.push(token);
                }
                Some("*") => {
                    let token = self.advance_and_lex(Self::lex_asterisk)?;
                    tokens.push(token);
                }
                Some("/") => {
                    let token = self.advance_and_lex(Self::lex_slash)?;
                    tokens.push(token);
                }
                Some("%") => {
                    let token = self.advance_and_lex(Self::lex_percent)?;
                    tokens.push(token);
                }
                Some("#") => {
                    self.scanner.advance();
                    self.skip_comment();
                }
                Some(s) if string::is_whitespace(s) => self.scanner.advance(),
                Some(x) => {
                    return Err(LexError::IllegalChar { char: x.to_string(), location: self.scanner.locate() });
                }
                None => {
                    break;
                }
            }
        }

        Ok(tokens)
    }

    fn skip_comment(&mut self) -> () {
        loop {
            match self.scanner.read() {
                Some("\n") | Some("\r") | Some("\r\n") | None => {
                    self.scanner.advance();
                    break;
                }
                _ => {
                    self.scanner.advance();
                }
            }
        }
    }

    fn advance_and_lex<F>(&mut self, lex: F) -> ResToken
    where
        F: FnOnce(&mut Self, Range) -> ResToken,
    {
        let first_location = self.scanner.locate();
        self.scanner.advance();
        lex(self, first_location)
    }

    fn advance_and_lex_with_first_char<F>(&mut self, lex: F, first_char: &'a str) -> ResToken
    where
        F: FnOnce(&mut Self, Range, &'a str) -> ResToken,
    {
        let first_location = self.scanner.locate();
        self.scanner.advance();
        lex(self, first_location, first_char)
    }

    fn lex_plus(&mut self, first_location: Range) -> ResToken {
        Ok(Token::from_plus(first_location))
    }

    fn lex_minus(&mut self, first_location: Range) -> ResToken {
        Ok(Token::from_minus(first_location))
    }

    fn lex_asterisk(&mut self, first_location: Range) -> ResToken {
        Ok(Token::from_asterisk(first_location))
    }

    fn lex_slash(&mut self, first_location: Range) -> ResToken {
        Ok(Token::from_slash(first_location))
    }

    fn lex_percent(&mut self, first_location: Range) -> ResToken {
        Ok(Token::from_percent(first_location))
    }

    fn lex_num(&mut self, first_location: Range, first_char: &'a str) -> ResToken {
        let mut lexeme = first_char.to_string();
        let begin = first_location.begin;

        // read whole number part
        loop {
            match self.scanner.read() {
                Some(s) if string::is_digit(s) => {
                    lexeme.push_str(s);
                    self.scanner.advance();
                }
                _ => {
                    break;
                }
            }
        }

        if self.scanner.read() != Some(".") {
            let num = lexeme.parse::<f64>().unwrap();
            let end = self.scanner.locate().begin;

            let token = Token::from_num(num, Range::new(begin, end));
            return Ok(token);
        }

        lexeme.push_str(".");
        self.scanner.advance();

        // read decimal part
        loop {
            match self.scanner.read() {
                Some(s) if string::is_digit(s) => {
                    lexeme.push_str(s);
                    self.scanner.advance();
                }
                _ => {
                    break;
                }
            }
        }

        let num = lexeme.parse::<f64>().unwrap();
        let end = self.scanner.locate().begin;

        let token = Token::from_num(num, Range::new(begin, end));
        return Ok(token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use komi_syntax::TokenKind;

    type Res = Result<(), LexError>;

    mod empty {
        use super::*;

        #[test]
        fn test_lex_empty() -> Res {
            let source = "";

            let token = Lexer::new(source).lex()?;

            let expected = vec![];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_whitespaces() -> Res {
            let source = "   ";

            let token = Lexer::new(source).lex()?;

            let expected = vec![];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_tabs() -> Res {
            let source = "\t\t";

            let token = Lexer::new(source).lex()?;

            let expected = vec![];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_new_lines() -> Res {
            let source = "\n\n\r\r\r\n\r\n";

            let token = Lexer::new(source).lex()?;

            let expected = vec![];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_comment() -> Res {
            let source = "# comment";

            let token = Lexer::new(source).lex()?;

            let expected = vec![];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_multi_line_comment() -> Res {
            let source = "# comment line 1\r\n# comment line 2";

            let token = Lexer::new(source).lex()?;

            let expected = vec![];
            assert_eq!(token, expected);
            Ok(())
        }
    }

    mod num {
        use super::*;

        #[test]
        fn test_lex_without_decimal() -> Res {
            let source = "123";

            let token = Lexer::new(source).lex()?;

            let expected = vec![Token::new(
                TokenKind::Number(123.0),
                Range::from_nums(0, 0, 0, "123".len() as u64),
            )];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_with_decimal() -> Res {
            let source = "12.25"; // chosen to be equal on float comparison

            let token = Lexer::new(source).lex()?;

            let expected = vec![Token::new(
                TokenKind::Number(12.25),
                Range::from_nums(0, 0, 0, "12.25".len() as u64),
            )];
            assert_eq!(token, expected);
            Ok(())
        }
    }

    mod single_chars {
        use super::*;

        #[test]
        fn test_lex_plus() -> Res {
            let source = "+";

            let token = Lexer::new(source).lex()?;

            let expected = vec![Token::new(TokenKind::Plus, Range::from_nums(0, 0, 0, 1))];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_minus() -> Res {
            let source = "-";

            let token = Lexer::new(source).lex()?;

            let expected = vec![Token::new(TokenKind::Minus, Range::from_nums(0, 0, 0, 1))];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_asterisk() -> Res {
            let source = "*";

            let token = Lexer::new(source).lex()?;

            let expected = vec![Token::new(TokenKind::Asterisk, Range::from_nums(0, 0, 0, 1))];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_slash() -> Res {
            let source = "/";

            let token = Lexer::new(source).lex()?;

            let expected = vec![Token::new(TokenKind::Slash, Range::from_nums(0, 0, 0, 1))];
            assert_eq!(token, expected);
            Ok(())
        }

        #[test]
        fn test_lex_percent() -> Res {
            let source = "%";

            let token = Lexer::new(source).lex()?;

            let expected = vec![Token::new(TokenKind::Percent, Range::from_nums(0, 0, 0, 1))];
            assert_eq!(token, expected);
            Ok(())
        }
    }

    mod expression {
        use super::*;

        #[test]
        fn test_lex() -> Res {
            let source = "12 + 34.675";

            let token = Lexer::new(source).lex()?;

            let expected = vec![
                Token::new(TokenKind::Number(12.0), Range::from_nums(0, 0, 0, "12".len() as u64)),
                Token::new(
                    TokenKind::Plus,
                    Range::from_nums(0, "12 ".len() as u64, 0, "12 +".len() as u64),
                ),
                Token::new(
                    TokenKind::Number(34.675),
                    Range::from_nums(0, "12 + ".len() as u64, 0, "12 + 34.675".len() as u64),
                ),
            ];
            assert_eq!(token, expected);
            Ok(())
        }
    }

    mod fail {
        use super::*;

        #[test]
        fn test_illegal_char() -> Res {
            let source = "^";

            let token = Lexer::new(source).lex();

            let _expected = LexError::IllegalChar {
                char: "^".to_string(),
                location: Range::from_nums(0, 0, 0, 1),
            };
            assert!(matches!(token, Err(_expected)));
            Ok(())
        }
    }
}
