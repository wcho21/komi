mod source_scanner;
mod utf8_tape;

use crate::core::err::LexError;
use crate::core::syntax::Token;
use crate::util::string;
use crate::util::{Range, Scanner};
use source_scanner::SourceScanner;

type ResTokens = Result<Vec<Token>, LexError>;
type ResToken = Result<Token, LexError>;

/// A lexer to produce tokens from a source.
struct Lexer<'a> {
    scanner: SourceScanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            scanner: SourceScanner::new(source),
        }
    }

    pub fn lex(&mut self) -> ResTokens {
        let mut tokens: Vec<Token> = vec![];

        loop {
            match self.scanner.read() {
                Some(s) if string::is_ascii_single_digit(s) => tokens.push(self.lex_num()?),
                Some("+") => tokens.push(self.lex_plus()?),
                Some(s) if string::is_ascii_single_whitespace(s) || s =="\r\n" => {
                    self.scanner.advance();
                    continue;
                }
                Some(x) => {
                    return Err(LexError::IllegalChar {
                        char: x.to_string(),
                        location: self.scanner.locate(),
                    });
                }
                None => {
                    break;
                }
            }
        }

        Ok(tokens)
    }

    fn lex_plus(&mut self) -> ResToken {
        let location = self.scanner.locate();

        match self.scanner.read() {
            Some("+") => {
                self.scanner.advance();
                return Ok(Token::from_plus(location));
            }
            other => Err(LexError::Unexpected {
                expected: "+".to_string(),
                received: other.unwrap_or("").to_string(),
                location,
            }),
        }
    }

    fn lex_num(&mut self) -> ResToken {
        let mut lexeme = String::new();
        let begin = self.scanner.locate().begin;

        // read whole number part
        loop {
            match self.scanner.read() {
                Some(s) if string::is_ascii_single_digit(s) => {
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
                Some(s) if string::is_ascii_single_digit(s) => {
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

pub fn lex(source: &str) -> ResTokens {
    let tokens = Lexer::new(source).lex()?;
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::syntax::TokenKind;

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

    mod plus {
        use super::*;

        #[test]
        fn test_lex() -> Res {
            let source = "+";

            let token = Lexer::new(source).lex()?;

            let expected = vec![Token::new(
                TokenKind::Plus,
                Range::from_nums(0, 0, 0, "+".len() as u64),
            )];
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
                Token::new(
                    TokenKind::Number(12.0),
                    Range::from_nums(0, 0, 0, "12".len() as u64),
                ),
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

            let _expected = LexError::IllegalChar{char: "^".to_string(), location: Range::from_nums(0,0,0,1)};
            assert!(matches!(token, Err(_expected)));
            Ok(())
        }
    }
}
