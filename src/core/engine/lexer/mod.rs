mod source_scanner;
mod utf8_tape;

use crate::core::err::LexError;
use crate::core::syntax::{Token, TokenKind};
use crate::util::string;
use crate::util::{Range, Scanner};
use source_scanner::SourceScanner;

type ResTokens = Result<Vec<Token>, LexError>;
type ResToken = Result<Token, LexError>;

/// A lexer to produce tokens from a source.
struct Lexer<'a> {
    reader: SourceScanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            reader: SourceScanner::new(source),
        }
    }

    pub fn lex(&mut self) -> ResTokens {
        let mut tokens: Vec<Token> = vec![];

        loop {
            match self.reader.read() {
                Some(s) if string::is_ascii_single_digit(s) => tokens.push(self.lex_num()?),
                Some(x) => {
                    return Err(LexError::IllegalChar(x.to_string(), self.reader.locate()));
                }
                None => {
                    break;
                }
            }
        }

        Ok(tokens)
    }

    fn lex_num(&mut self) -> ResToken {
        let mut lexeme = String::new();
        let begin = self.reader.locate().begin;

        // read whole number part
        loop {
            match self.reader.read() {
                Some(s) if string::is_ascii_single_digit(s) => {
                    lexeme.push_str(s);
                    self.reader.advance();
                }
                Some(s) if !string::is_ascii_single_whitespace(s) && s != "." => {
                    let end = self.reader.locate().begin;
                    return Err(LexError::BadNumLiteral(
                        s.to_string(),
                        Range::new(begin, end),
                    ));
                }
                _ => {
                    break;
                }
            }
        }

        if self.reader.read() != Some(".") {
            let num = lexeme.parse::<f64>().unwrap();
            let end = self.reader.locate().begin;

            let token = Token::new(TokenKind::Number(num), Range::new(begin, end));
            return Ok(token);
        }

        lexeme.push_str(".");
        self.reader.advance();

        // read decimal part
        loop {
            match self.reader.read() {
                Some(s) if string::is_ascii_single_digit(s) => {
                    lexeme.push_str(s);
                    self.reader.advance();
                }
                Some(s) if !string::is_ascii_single_whitespace(s) => {
                    let end = self.reader.locate().begin;
                    return Err(LexError::BadNumLiteral(
                        s.to_string(),
                        Range::new(begin, end),
                    ));
                }
                _ => {
                    break;
                }
            }
        }

        let num = lexeme.parse::<f64>().unwrap();
        let end = self.reader.locate().begin;

        let token = Token::new(TokenKind::Number(num), Range::new(begin, end));
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
    use crate::util::Spot;

    type Res = Result<(), LexError>;

    #[test]
    fn lex_num_int() -> Res {
        let source = "123";

        let token = Lexer::new(source).lex()?;

        let expected = vec![Token::new(
            TokenKind::Number(123.0),
            Range::new(Spot::new(0, 0), Spot::new(0, 3)),
        )];
        assert_eq!(token, expected);
        Ok(())
    }

    #[test]
    fn lex_num_float() -> Res {
        let source = "12.25"; // chosen to be equal on float comparison

        let token = Lexer::new(source).lex()?;

        let expected = vec![Token::new(
            TokenKind::Number(12.25),
            Range::new(Spot::new(0, 0), Spot::new(0, 5)),
        )];
        assert_eq!(token, expected);
        Ok(())
    }

    #[test]
    fn lex_num_fail() -> Res {
        let source = "12a";

        let token = Lexer::new(source).lex();

        assert!(matches!(token, Err(LexError::BadNumLiteral(_, _))));
        Ok(())
    }

    #[test]
    fn lex_fail() -> Res {
        let source = " ";

        let token = Lexer::new(source).lex();

        assert!(matches!(token, Err(LexError::IllegalChar(_, _))));
        Ok(())
    }
}
