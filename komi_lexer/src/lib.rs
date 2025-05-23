//! # Lexer
//!
//! Reads a source code and returns *tokens* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod err;
mod source_scanner;
mod utf8_tape;

pub use err::{LexError, LexErrorKind};
use komi_syntax::{Token, TokenKind};
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
    ($self:ident, $lex_fn:expr $(,)?) => {{
        let first_location = $self.scanner.locate();
        $self.scanner.advance();
        $lex_fn($self, &first_location)
    }};
    ($self:ident, $lex_fn:expr, $first_char:expr $(,)?) => {{
        let first_location = $self.scanner.locate();
        $self.scanner.advance();
        $lex_fn($self, &first_location, $first_char)
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { scanner: SourceScanner::new(source) }
    }

    pub fn lex(&mut self) -> ResTokens {
        let mut tokens: Vec<Token> = vec![];

        while let Some(x) = self.scanner.read() {
            match x {
                x if string::is_digit(x) => {
                    let token = advance_and_lex!(self, Self::lex_num, x)?;
                    tokens.push(token);
                }
                "참" => {
                    let token = advance_and_lex!(self, Self::lex_true)?;
                    tokens.push(token);
                }
                "거" => {
                    let token = advance_and_lex!(self, Self::lex_false)?;
                    tokens.push(token);
                }
                "+" => {
                    let token = advance_and_lex!(self, Self::lex_plus)?;
                    tokens.push(token);
                }
                "-" => {
                    let token = advance_and_lex!(self, Self::lex_minus)?;
                    tokens.push(token);
                }
                "*" => {
                    let token = advance_and_lex!(self, Self::lex_asterisk)?;
                    tokens.push(token);
                }
                "/" => {
                    let token = advance_and_lex!(self, Self::lex_slash)?;
                    tokens.push(token);
                }
                "%" => {
                    let token = advance_and_lex!(self, Self::lex_percent)?;
                    tokens.push(token);
                }
                "(" => {
                    let token = advance_and_lex!(self, Self::lex_lparen)?;
                    tokens.push(token);
                }
                ")" => {
                    let token = advance_and_lex!(self, Self::lex_rparen)?;
                    tokens.push(token);
                }
                "!" => {
                    let token = advance_and_lex!(self, Self::lex_bang)?;
                    tokens.push(token);
                }
                "그" => {
                    let token = advance_and_lex!(self, Self::lex_conjunct)?;
                    tokens.push(token);
                }
                "또" => {
                    let token = advance_and_lex!(self, Self::lex_disjunct)?;
                    tokens.push(token);
                }
                "#" => {
                    self.scanner.advance();
                    self.skip_comment();
                }
                s if string::is_whitespace(s) => self.scanner.advance(),
                _ => {
                    return Err(LexError::new(LexErrorKind::IllegalChar, self.scanner.locate()));
                }
            }
        }

        Ok(tokens)
    }

    /// Returns a number literal token if successfully lexed, or error otherwise.
    ///
    /// Call after advancing the scanner `self.scanner` past the initial character, with its location passed as `first_location`.
    fn lex_num(&mut self, first_location: &Range, first_char: &'a str) -> ResToken {
        let mut lexeme = String::new();
        let begin = first_location.begin;

        // Read the whole number part.
        lexeme.push_str(&self.read_digits(first_char));

        // Return a token if not a dot.
        let Some(".") = self.scanner.read() else {
            let end = self.scanner.locate().begin;
            let token = Self::parse_num_lexeme(&lexeme, Range::new(begin, end));

            return Ok(token);
        };

        // Read a dot.
        self.scanner.advance();
        lexeme.push_str(".");

        // Return an error if end or not a digit.
        let Some(x) = self.scanner.read() else {
            let location = Range::new(begin, self.scanner.locate().end);
            return Err(LexError::new(LexErrorKind::IllegalNumLiteral, location));
        };
        if !string::is_digit(x) {
            let location = Range::new(begin, self.scanner.locate().end);
            return Err(LexError::new(LexErrorKind::IllegalNumLiteral, location));
        }
        self.scanner.advance();

        // Read the decimal part.
        lexeme.push_str(&self.read_digits(x));

        // Parse into a number and return a token.
        let end = self.scanner.locate().begin;
        let token = Self::parse_num_lexeme(&lexeme, Range::new(begin, end));

        Ok(token)
    }

    fn lex_true(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_boolean(true, *first_location))
    }

    /// Returns a false literal token `거짓` if successfully lexed, or error otherwise.
    ///
    /// Call after advancing the scanner `self.scanner` past the initial character `거`, with its location passed as `first_location`.
    fn lex_false(&mut self, first_location: &Range) -> ResToken {
        let Some("짓") = self.scanner.read() else {
            // TODO: return an identifier token, when the identifier token is implemented.
            return Err(LexError::new(LexErrorKind::IllegalChar, self.scanner.locate()));
        };

        let location = Range::new(first_location.begin, self.scanner.locate().end);
        self.scanner.advance();

        Ok(Token::from_boolean(false, location))
    }

    fn lex_plus(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_plus(*first_location))
    }

    fn lex_minus(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_minus(*first_location))
    }

    fn lex_asterisk(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_asterisk(*first_location))
    }

    fn lex_slash(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_slash(*first_location))
    }

    fn lex_percent(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_percent(*first_location))
    }

    fn lex_lparen(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_lparen(*first_location))
    }

    fn lex_rparen(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_rparen(*first_location))
    }

    fn lex_bang(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::from_bang(*first_location))
    }

    /// Returns a conjunction token `그리고` if successfully lexed, or error otherwise.
    ///
    /// Call after advancing the scanner `self.scanner` past the initial character `그`, with its location passed as `first_location`.
    fn lex_conjunct(&mut self, first_location: &Range) -> ResToken {
        let Some("리") = self.scanner.read() else {
            // TODO: return an identifier token, when the identifier token is implemented.
            return Err(LexError::new(LexErrorKind::IllegalChar, self.scanner.locate()));
        };

        self.scanner.advance();
        let Some("고") = self.scanner.read() else {
            // TODO: return an identifier token, when the identifier token is implemented.
            return Err(LexError::new(LexErrorKind::IllegalChar, self.scanner.locate()));
        };

        let location = Range::new(first_location.begin, self.scanner.locate().end);
        self.scanner.advance();

        // TODO: enough to make a token, is helper function `from_*()` in syntax token library really necessary, or just increasing bundled output size?
        Ok(Token::new(TokenKind::Conjunct, location))
    }

    /// Returns a conjunction token `또는` if successfully lexed, or error otherwise.
    ///
    /// Call after advancing the scanner `self.scanner` past the initial character `또`, with its location passed as `first_location`.
    fn lex_disjunct(&mut self, first_location: &Range) -> ResToken {
        let Some("는") = self.scanner.read() else {
            return Err(LexError::new(LexErrorKind::IllegalChar, self.scanner.locate()));
        };

        let location = Range::new(first_location.begin, self.scanner.locate().end);
        self.scanner.advance();

        Ok(Token::new(TokenKind::Disjunct, location))
    }

    fn skip_comment(&mut self) -> () {
        while let Some(x) = self.scanner.read() {
            self.scanner.advance();

            if let "\n" | "\r" | "\r\n" = x {
                break;
            }
        }
    }

    fn parse_num_lexeme(lexeme: &String, location: Range) -> Token {
        let num = lexeme.parse::<f64>().unwrap();

        Token::from_num(num, location)
    }

    fn read_digits(&mut self, first_char: &'a str) -> String {
        let mut lexeme = first_char.to_string();

        while let Some(x) = self.scanner.read() {
            if !string::is_digit(x) {
                break;
            }

            lexeme.push_str(x);
            self.scanner.advance();
        }

        lexeme
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
    use rstest::rstest;

    /// Asserts a given literal to be lexed into the expected tokens.
    /// Helps write a test declaratively.
    macro_rules! assert_lex {
        ($source:expr, $expected:expr $(,)?) => {
            assert_eq!(
                lex($source),
                Ok($expected),
                "received tokens (left) from the source '{}', but expected the different tokens (right)",
                $source,
            );
        };
    }

    /// Asserts lexing a given literal will fail.
    /// Helps write a test declaratively.
    macro_rules! assert_lex_fail {
        ($source:expr, $expected:expr $(,)?) => {
            assert_eq!(
                lex($source),
                Err($expected),
                "received a result (left), but expected an error from the source '{}' (right)",
                $source,
            );
        };
    }

    #[rstest]
    #[case::empty("")]
    #[case::whitespaces("  ")]
    #[case::tabs("\t\t")]
    #[case::new_lines("\n\n\r\r\r\n\r\n")]
    #[case::comment("# foo")]
    #[case::multi_line_comment("# foo\r\n# bar")]
    fn empty(#[case] source: &str) {
        assert_lex!(source, vec![]);
    }

    #[rstest]
    #[case::without_decimal("12", vec![mktoken!(TokenKind::Number(12.0), loc 0, 0, 0, "12".len())])]
    #[case::with_decimal("12.25", vec![mktoken!(TokenKind::Number(12.25), loc 0, 0, 0, "12.25".len())])]
    fn num_literal(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    #[rstest]
    #[case::illegal_char("12^", LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 2, 0, 3)))]
    #[case::beginning_with_dot(".25", LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))]
    #[case::ending_with_dot("12.", LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 3)))]
    #[case::ending_with_two_dots("12..", LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 4)))]
    #[case::ending_with_two_dots("12..", LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 4)))]
    #[case::illegal_decimal("12..", LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 4)))]
    fn illegal_num(#[case] source: &str, #[case] expected: LexError) {
        assert_lex_fail!(source, expected);
    }

    #[rstest]
    #[case::the_true("참", vec![mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1)])]
    #[case::the_false("거짓", vec![mktoken!(TokenKind::Bool(false), loc 0, 0, 0, 2)])]
    fn boolean(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    #[rstest]
    #[case::plus("+", vec![mktoken!(TokenKind::Plus, loc 0, 0, 0, 1)])]
    #[case::minus("-", vec![mktoken!(TokenKind::Minus, loc 0, 0, 0, 1)])]
    #[case::asterisk("*", vec![mktoken!(TokenKind::Asterisk, loc 0, 0, 0, 1)])]
    #[case::slash("/", vec![mktoken!(TokenKind::Slash, loc 0, 0, 0, 1)])]
    #[case::percent("%", vec![mktoken!(TokenKind::Percent, loc 0, 0, 0, 1)])]
    #[case::lparen("(", vec![mktoken!(TokenKind::LParen, loc 0, 0, 0, 1)])]
    #[case::rparen(")", vec![mktoken!(TokenKind::RParen, loc 0, 0, 0, 1)])]
    #[case::bang("!", vec![mktoken!(TokenKind::Bang, loc 0, 0, 0, 1)])]
    #[case::bang("그리고", vec![mktoken!(TokenKind::Conjunct, loc 0, 0, 0, 3)])]
    #[case::bang("또는", vec![mktoken!(TokenKind::Disjunct, loc 0, 0, 0, 2)])]
    fn single_token(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    #[rstest]
    /// Should not fail, since the lexer does not know the syntax.
    #[case::two_pluses("+ +", vec![
        mktoken!(TokenKind::Plus, loc 0, 0, 0, "+".len()),
        mktoken!(TokenKind::Plus, loc 0, "+ ".len(), 0, "+ +".len()),
    ])]
    #[case::addition_expression("12 + 34.675", vec![
        mktoken!(TokenKind::Number(12.0), loc 0, 0, 0, "12".len()),
        mktoken!(TokenKind::Plus, loc 0, "12 ".len(), 0, "12 +".len()),
        mktoken!(TokenKind::Number(34.675), loc 0, "12 + ".len(), 0, "12 + 34.675".len()),
    ])]
    fn multiple_tokens(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    #[rstest]
    #[case::caret("^", LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))]
    #[case::dollar("$", LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))]
    fn illegal_char(#[case] source: &str, #[case] expected: LexError) {
        assert_lex_fail!(source, expected);
    }
}
