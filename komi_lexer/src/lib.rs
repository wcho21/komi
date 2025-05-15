//! # Lexer
//!
//! Reads a source code and returns *tokens* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod err;
mod source_scanner;
mod utf8_tape;

pub use err::{LexError, LexErrorKind, LexErrorReason};
use komi_syntax::Token;
use komi_util::string;
use komi_util::{Range, Scanner};
use source_scanner::SourceScanner;

type ResTokens = Result<Vec<Token>, LexError>;
type ResToken = Result<Token, LexError>;

/// Produces tokens from source codes.
struct Lexer<'a> {
    scanner: SourceScanner<'a>,
}

macro_rules! advance_and_lex {
    ($self:ident, $lex_fn:expr) => {{
        let first_location = $self.scanner.locate();
        $self.scanner.advance();
        $lex_fn($self, first_location)
    }};
    ($self:ident, $lex_fn:expr, $first_char:expr) => {{
        let first_location = $self.scanner.locate();
        $self.scanner.advance();
        $lex_fn($self, first_location, $first_char)
    }};
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
                    let token = advance_and_lex!(self, Self::lex_num, s)?;
                    tokens.push(token);
                }
                Some("+") => {
                    let token = advance_and_lex!(self, Self::lex_plus)?;
                    tokens.push(token);
                }
                Some("-") => {
                    let token = advance_and_lex!(self, Self::lex_minus)?;
                    tokens.push(token);
                }
                Some("*") => {
                    let token = advance_and_lex!(self, Self::lex_asterisk)?;
                    tokens.push(token);
                }
                Some("/") => {
                    let token = advance_and_lex!(self, Self::lex_slash)?;
                    tokens.push(token);
                }
                Some("%") => {
                    let token = advance_and_lex!(self, Self::lex_percent)?;
                    tokens.push(token);
                }
                Some("#") => {
                    self.scanner.advance();
                    self.skip_comment();
                }
                Some(s) if string::is_whitespace(s) => self.scanner.advance(),
                Some(x) => {
                    return Err(LexError::new(
                        LexErrorKind::IllegalChar,
                        x.to_string(),
                        self.scanner.locate(),
                    ));
                }
                None => {
                    break;
                }
            }
        }

        Ok(tokens)
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

        // return if not dot
        let Some(".") = self.scanner.read() else {
            let num = lexeme.parse::<f64>().unwrap();
            let end = self.scanner.locate().begin;

            let token = Token::from_num(num, Range::new(begin, end));
            return Ok(token);
        };

        // read dot
        self.scanner.advance();
        lexeme.push_str(".");

        // return if not digit
        match self.scanner.read() {
            Some(s) if string::is_digit(s) => (),
            other => {
                let cause = format!("{}{}", lexeme, other.unwrap_or(""));
                let location = Range::new(begin, self.scanner.locate().end);
                return Err(LexError::new(LexErrorKind::IllegalNumLiteral, cause, location));
            }
        }

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

        // parse into number and return token
        let num = lexeme.parse::<f64>().unwrap();
        let end = self.scanner.locate().begin;

        let token = Token::from_num(num, Range::new(begin, end));
        return Ok(token);
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
}

/// Produces tokens from source codes.
pub fn lex(source: &str) -> ResTokens {
    Lexer::new(source).lex()
}

#[cfg(test)]
mod tests {
    use super::err::LexErrorKind;
    use super::{LexError, Range, Token, lex};
    use komi_syntax::{TokenKind, mktoken};

    type Res = Result<(), LexError>;

    /// Asserts a given literal to be lexed into the expected tokens.
    /// Helps write a test more declaratively.
    macro_rules! assert_lex {
        ($source:literal, $expected:expr) => {
            assert_eq!(
                lex($source)?,
                $expected,
                "received tokens (left) from the source '{}', but expected the different tokens (right)",
                $source,
            );
            return Ok(())
        };
    }

    /// Asserts lexing a given literal will fail.
    /// Helps write a test more declaratively.
    macro_rules! assert_lex_fail {
        ($source:literal, $expected:expr) => {
            assert_eq!(
                lex($source),
                Err($expected),
                "received a result (left), but expected lexing the source '{}' to fail (right)",
                $source,
            );
            return Ok(())
        };
    }

    mod empty {
        use super::*;

        #[test]
        fn test_empty() -> Res {
            assert_lex!("", vec![]);
        }

        #[test]
        fn test_whitespaces() -> Res {
            assert_lex!("   ", vec![]);
        }

        #[test]
        fn test_tabs() -> Res {
            assert_lex!("\t\t", vec![]);
        }

        #[test]
        fn test_new_lines() -> Res {
            assert_lex!("\n\n\r\r\r\n\r\n", vec![]);
        }

        #[test]
        fn test_comment() -> Res {
            assert_lex!("# comment", vec![]);
        }

        #[test]
        fn test_multi_line_comment() -> Res {
            assert_lex!("# comment line 1\r\n# comment line 2", vec![]);
        }
    }

    mod num_literals {
        use super::*;

        mod success {
            use super::*;

            #[test]
            fn test_without_decimal() -> Res {
                assert_lex!(
                    "123",
                    vec![mktoken!(TokenKind::Number(123.0), loc 0, 0, 0, "123".len())]
                );
            }

            #[test]
            fn test_with_decimal() -> Res {
                assert_lex!(
                    "12.25",
                    vec![mktoken!(TokenKind::Number(12.25), loc 0, 0, 0, "12.25".len())]
                );
            }
        }

        mod fail {
            use super::*;

            #[test]
            fn test_illegal() -> Res {
                assert_lex_fail!(
                    "12^",
                    LexError::new(LexErrorKind::IllegalChar, "^".to_string(), Range::from_nums(0, 2, 0, 3),)
                );
            }

            #[test]
            fn test_beginning_with_dot() -> Res {
                assert_lex_fail!(
                    ".25",
                    LexError::new(LexErrorKind::IllegalChar, ".".to_string(), Range::from_nums(0, 0, 0, 1),)
                );
            }

            #[test]
            fn test_ending_with_dot() -> Res {
                assert_lex_fail!(
                    "12.",
                    LexError::new(
                        LexErrorKind::IllegalNumLiteral,
                        "12.".to_string(),
                        Range::from_nums(0, 0, 0, 3),
                    )
                );
            }

            #[test]
            fn test_two_dots_decimal() -> Res {
                assert_lex_fail!(
                    "12..",
                    LexError::new(
                        LexErrorKind::IllegalNumLiteral,
                        "12..".to_string(),
                        Range::from_nums(0, 0, 0, 4),
                    )
                );
            }

            #[test]
            fn test_illegal_decimal() -> Res {
                assert_lex_fail!(
                    "12.^",
                    LexError::new(
                        LexErrorKind::IllegalNumLiteral,
                        "12.^".to_string(),
                        Range::from_nums(0, 0, 0, 4),
                    )
                );
            }
        }
    }

    mod single_chars {
        use super::*;

        #[test]
        fn test_plus() -> Res {
            assert_lex!("+", vec![mktoken!(TokenKind::Plus, loc 0, 0, 0, 1)]);
        }

        #[test]
        fn test_minus() -> Res {
            assert_lex!("-", vec![mktoken!(TokenKind::Minus, loc 0, 0, 0, 1)]);
        }

        #[test]
        fn test_asterisk() -> Res {
            assert_lex!("*", vec![mktoken!(TokenKind::Asterisk, loc 0, 0, 0, 1)]);
        }

        #[test]
        fn test_slash() -> Res {
            assert_lex!("/", vec![mktoken!(TokenKind::Slash, loc 0, 0, 0, 1)]);
        }

        #[test]
        fn test_percent() -> Res {
            assert_lex!("%", vec![mktoken!(TokenKind::Percent, loc 0, 0, 0, 1)]);
        }
    }

    mod multi_chars {
        use super::*;

        #[test]
        fn test_addition_expression() -> Res {
            assert_lex!(
                "12 + 34.675",
                vec![
                    mktoken!(TokenKind::Number(12.0), loc 0, 0, 0, "12".len()),
                    mktoken!(TokenKind::Plus, loc 0, "12 ".len(), 0, "12 +".len()),
                    mktoken!(TokenKind::Number(34.675), loc 0, "12 + ".len(), 0, "12 + 34.675".len()),
                ]
            );
        }

        /// Should not fail, since the lexer does not know the syntax.
        #[test]
        fn test_two_pluses() -> Res {
            assert_lex!(
                "+ +",
                vec![
                    mktoken!(TokenKind::Plus, loc 0, 0, 0, "+".len()),
                    mktoken!(TokenKind::Plus, loc 0, "+ ".len(), 0, "+ +".len()),
                ]
            );
        }
    }

    mod illegal {
        use super::*;

        #[test]
        fn test_unrecognizable() -> Res {
            assert_lex_fail!(
                "^",
                LexError::new(LexErrorKind::IllegalChar, "^".to_string(), Range::from_nums(0, 0, 0, 1),)
            );
        }
    }
}
