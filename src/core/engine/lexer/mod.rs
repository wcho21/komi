mod source_reader;
mod utf8_tape;

use crate::core::err::LexErr;
use crate::core::syntax::{Token, TokenKind};
use crate::util::string;
use crate::util::{Range, Spot, range};
use source_reader::SourceReader;

/// A lexer to produce tokens from a source.
struct Lexer<'a> {
    reader: SourceReader<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            reader: SourceReader::new(source),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexErr> {
        let mut tokens: Vec<Token> = vec![];

        loop {
            match self.reader.read() {
                Some(s) if string::is_ascii_single_digit(s) => tokens.push(self.lex_num()?),
                Some(x) => {
                    return Err(LexErr::IllegalChar(
                        x.to_string(),
                        range::from_spot(&self.reader.spot()),
                    ));
                }
                None => {
                    break;
                }
            }
        }

        Ok(tokens)
    }

    fn lex_num(&mut self) -> Result<Token, LexErr> {
        let mut lexeme = String::new();
        let begin = self.reader.spot();

        // read whole number part
        loop {
            match self.reader.read() {
                Some(s) if string::is_ascii_single_digit(s) => {
                    lexeme.push_str(s);
                    self.reader.advance();
                }
                Some(s) if !string::is_ascii_single_whitespace(s) && s != "." => {
                    let end = self.reader.spot();
                    return Err(LexErr::BadNumLiteral(s.to_string(), Range::new(begin, end)));
                }
                _ => {
                    break;
                }
            }
        }

        if self.reader.read() != Some(".") {
            let num = lexeme.parse::<f64>().unwrap();
            let end = self.reader.spot();

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
                    let end = self.reader.spot();
                    return Err(LexErr::BadNumLiteral(s.to_string(), Range::new(begin, end)));
                }
                _ => {
                    break;
                }
            }
        }

        let num = lexeme.parse::<f64>().unwrap();
        let end = self.reader.spot();

        let token = Token::new(TokenKind::Number(num), Range::new(begin, end));
        return Ok(token);
    }
}

pub fn lex<'a>(source: &'a str) -> Result<Vec<Token>, LexErr> {
    let tokens = Lexer::new(source).lex()?;
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;

    #[test]
    fn lex_num_int() -> Result<(), Box<dyn Error>> {
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
    fn lex_num_float() -> Result<(), Box<dyn Error>> {
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
    fn lex_num_fail() -> Result<(), Box<dyn Error>> {
        let source = "12a";

        let token = Lexer::new(source).lex();

        assert!(matches!(token, Err(LexErr::BadNumLiteral(_, _))));
        Ok(())
    }

    #[test]
    fn lex_fail() -> Result<(), Box<dyn Error>> {
        let source = " ";

        let token = Lexer::new(source).lex();

        assert!(matches!(token, Err(LexErr::IllegalChar(_, _))));
        Ok(())
    }
}
