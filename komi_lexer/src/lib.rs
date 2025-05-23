//! # Lexer
//!
//! Reads a source code and returns *tokens* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod err;
mod source_scanner;
mod utf8_tape;

pub use err::{LexError, LexErrorKind};
use komi_syntax::{Token, TokenKind};
use komi_util::char_validator;
use komi_util::{Range, Scanner, Spot};
use source_scanner::SourceScanner;

type ResTokens = Result<Vec<Token>, LexError>;
type ResToken = Result<Token, LexError>;

/// Produces tokens from source codes.
struct Lexer<'a> {
    scanner: SourceScanner<'a>,
}

fn is_not_id_domain(char: &str) -> bool {
    !char_validator::is_id_domain(char)
}

fn is_id_domain_other_than(char: &str, filter: &str) -> bool {
    char_validator::is_id_domain(char) && char != filter
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { scanner: SourceScanner::new(source) }
    }

    // TODO: replace locate and advance logic with this, to force marking the previous location.
    fn locate_and_advance(&mut self) -> Range {
        let location = self.scanner.locate();
        self.scanner.advance();

        location
    }

    fn expect_or(char: &'a str) -> Token {
        todo!()
    }

    pub fn lex(&mut self) -> ResTokens {
        let mut tokens: Vec<Token> = vec![];

        while let Some(first_char) = self.scanner.read() {
            let first_location = self.locate_and_advance();

            match first_char {
                first_char if char_validator::is_digit(first_char) => {
                    let token = self.lex_num(&first_location, first_char)?;
                    tokens.push(token);
                }
                "참" => {
                    let token = self.lex_true(&first_location)?;
                    tokens.push(token);
                }
                "거" => {
                    let second_char = self.scanner.read();
                    if second_char.is_none_or(is_not_id_domain) {
                        let first_char_end = first_location.end;
                        let lexeme = String::from(first_char);
                        let location = Range::new(first_location.begin, first_char_end);
                        let token = Token::new(TokenKind::Identifier(lexeme), location);
                        tokens.push(token);

                        continue;
                    } else if second_char.is_some_and(|c| is_id_domain_other_than(c, "짓")) {
                        let second_char_end = self.scanner.locate().end;
                        self.scanner.advance();

                        let init_seg = String::from(first_char) + second_char.unwrap();
                        let token =
                            self.lex_identifier_with_init_seg(&first_location.begin, &init_seg, &second_char_end)?;
                        tokens.push(token);

                        continue;
                    }

                    let second_location = self.locate_and_advance();

                    let third_char = self.scanner.read();
                    if third_char.is_some_and(char_validator::is_id_domain) {
                        let third_char_end = self.scanner.locate().end;
                        self.scanner.advance();

                        let init_seg = String::from(first_char) + second_char.unwrap() + third_char.unwrap();
                        let token =
                            self.lex_identifier_with_init_seg(&first_location.begin, &init_seg, &third_char_end)?;
                        tokens.push(token);

                        continue;
                    }

                    let lexeme_location = Range::new(first_location.begin, second_location.end);
                    let token = Token::new(TokenKind::Bool(false), lexeme_location);
                    tokens.push(token);
                }
                "+" => {
                    let token = self.lex_plus(&first_location)?;
                    tokens.push(token);
                }
                "-" => {
                    let token = self.lex_minus(&first_location)?;
                    tokens.push(token);
                }
                "*" => {
                    let token = self.lex_asterisk(&first_location)?;
                    tokens.push(token);
                }
                "/" => {
                    let token = self.lex_slash(&first_location)?;
                    tokens.push(token);
                }
                "%" => {
                    let token = self.lex_percent(&first_location)?;
                    tokens.push(token);
                }
                "(" => {
                    let token = self.lex_lparen(&first_location)?;
                    tokens.push(token);
                }
                ")" => {
                    let token = self.lex_rparen(&first_location)?;
                    tokens.push(token);
                }
                "!" => {
                    let token = self.lex_bang(&first_location)?;
                    tokens.push(token);
                }
                "그" => {
                    let second_char = self.scanner.read();
                    if second_char.is_none_or(is_not_id_domain) {
                        // if end or non id, stop reading and make a first char to token
                        // part *1
                        let first_char_end = first_location.end;
                        let lexeme = String::from(first_char);
                        let location = Range::new(first_location.begin, first_char_end);
                        let token = Token::new(TokenKind::Identifier(lexeme), location);
                        tokens.push(token);

                        continue;
                    } else if second_char.is_some_and(|c| is_id_domain_other_than(c, "리")) {
                        // if id other than expected, consume the char and pass it to id lexer
                        // part *2
                        let second_char_end = self.scanner.locate().end;
                        self.scanner.advance();

                        let init_seg = String::from(first_char) + second_char.unwrap();
                        let token =
                            self.lex_identifier_with_init_seg(&first_location.begin, &init_seg, &second_char_end)?;
                        tokens.push(token);

                        continue;
                    }

                    let second_location = self.locate_and_advance();

                    let third_char = self.scanner.read();
                    if third_char.is_none_or(is_not_id_domain) {
                        // if end or non id, stop reading and make first and second char to token
                        // part *1
                        let second_char_end = second_location.end;
                        let lexeme = String::from(first_char) + second_char.unwrap();
                        let location = Range::new(first_location.begin, second_char_end);
                        let token = Token::new(TokenKind::Identifier(lexeme), location);
                        tokens.push(token);

                        continue;
                    } else if third_char.is_some_and(|c| is_id_domain_other_than(c, "고")) {
                        // if id other than expected, consume the char and pass it to id lexer
                        // part *2
                        let third_char_end = self.scanner.locate().end;
                        self.scanner.advance();

                        let init_seg = String::from(first_char) + second_char.unwrap() + third_char.unwrap();
                        let token =
                            self.lex_identifier_with_init_seg(&first_location.begin, &init_seg, &third_char_end)?;
                        tokens.push(token);

                        continue;
                    }

                    let third_location = self.locate_and_advance();

                    let fourth_char = self.scanner.read();
                    if fourth_char.is_some_and(char_validator::is_id_domain) {
                        // if id still, consume the char and pass it to id lexer
                        // part *2
                        let fourth_char_end = self.scanner.locate().end;
                        self.scanner.advance();

                        let init_seg = String::from(first_char)
                            + second_char.unwrap()
                            + third_char.unwrap()
                            + fourth_char.unwrap();
                        let token =
                            self.lex_identifier_with_init_seg(&first_location.begin, &init_seg, &fourth_char_end)?;
                        tokens.push(token);

                        continue;
                    }

                    let lexeme_location = Range::new(first_location.begin, third_location.end);
                    let token = Token::new(TokenKind::Conjunct, lexeme_location);
                    tokens.push(token);
                }
                "또" => {
                    let token = self.lex_disjunct_or_identifier(&first_location)?;
                    tokens.push(token);
                }
                "#" => {
                    self.skip_comment();
                }
                s if char_validator::is_whitespace(s) => {
                    continue;
                }
                s if char_validator::is_id_domain(s) => {
                    let token = self.lex_identifier_with_init_seg(&first_location.begin, s, &first_location.end)?;
                    tokens.push(token);
                }
                _ => {
                    return Err(LexError::new(LexErrorKind::IllegalChar, first_location));
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
        if !char_validator::is_digit(x) {
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
        Ok(Token::new(TokenKind::Bool(true), *first_location))
    }

    /// Returns a false literal token `거짓` if successfully lexed, or error otherwise.
    ///
    /// Call after advancing the scanner `self.scanner` past the initial character `거`, with its location passed as `first_location`.
    fn lex_false_or_identifier(&mut self, first_location: &Range) -> ResToken {
        match self.scanner.read() {
            Some("짓") => {
                let location = Range::new(first_location.begin, self.scanner.locate().end);
                self.scanner.advance();

                Ok(Token::new(TokenKind::Bool(false), location))
            }
            Some(x) if char_validator::is_id_domain(x) => {
                let begin_chars = String::from("거") + x;
                self.scanner.advance();
                let token = self.lex_identifier(first_location, &begin_chars)?;

                Ok(token)
            }
            Some(x) if !char_validator::is_whitespace(x) => {
                Err(LexError::new(LexErrorKind::IllegalChar, self.scanner.locate()))
            }
            _ => {
                // For whitespace or None
                let token = Token::new(TokenKind::Identifier(String::from("거")), *first_location);

                Ok(token)
            }
        }
    }

    fn lex_identifier_with_init_seg(&mut self, lexeme_begin: &Spot, init_seg: &str, init_seg_end: &Spot) -> ResToken {
        let mut lexeme = String::from(init_seg);
        let mut lexeme_end = *init_seg_end;

        while let Some(x) = self.scanner.read() {
            if !char_validator::is_id_domain(x) {
                break;
            }

            lexeme.push_str(x);
            lexeme_end = self.scanner.locate().end;

            self.scanner.advance();
        }

        let location = Range::new(*lexeme_begin, lexeme_end);
        Ok(Token::new(TokenKind::Identifier(lexeme), location))
    }

    fn lex_identifier(&mut self, first_location: &Range, begin_chars: &str) -> ResToken {
        let mut lexeme = String::from(begin_chars);

        while let Some(x) = self.scanner.read() {
            if !char_validator::is_id_domain(x) {
                break;
            }

            lexeme.push_str(x);

            self.scanner.advance();
        }

        let location = Range::new(first_location.begin, self.scanner.locate().end);
        Ok(Token::new(TokenKind::Identifier(lexeme), location))
    }

    fn lex_plus(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::new(TokenKind::Plus, *first_location))
    }

    fn lex_minus(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::new(TokenKind::Minus, *first_location))
    }

    fn lex_asterisk(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::new(TokenKind::Asterisk, *first_location))
    }

    fn lex_slash(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::new(TokenKind::Slash, *first_location))
    }

    fn lex_percent(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::new(TokenKind::Percent, *first_location))
    }

    fn lex_lparen(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::new(TokenKind::LParen, *first_location))
    }

    fn lex_rparen(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::new(TokenKind::RParen, *first_location))
    }

    fn lex_bang(&mut self, first_location: &Range) -> ResToken {
        Ok(Token::new(TokenKind::Bang, *first_location))
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
    fn lex_disjunct_or_identifier(&mut self, first_location: &Range) -> ResToken {
        match self.scanner.read() {
            Some("는") => {
                let location = Range::new(first_location.begin, self.scanner.locate().end);
                self.scanner.advance();

                Ok(Token::new(TokenKind::Disjunct, location))
            }
            Some(x) if komi_util::char_validator::is_id_domain(x) => {
                let begin_chars = String::from("또") + x;
                self.scanner.advance();
                let token = self.lex_identifier(first_location, &begin_chars)?;

                Ok(token)
            }
            Some(x) if !char_validator::is_whitespace(x) => {
                Err(LexError::new(LexErrorKind::IllegalChar, self.scanner.locate()))
            }
            _ => {
                // For whitespace or None
                let token = Token::new(TokenKind::Identifier(String::from("또")), *first_location);

                Ok(token)
            }
        }
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

        Token::new(TokenKind::Number(num), location)
    }

    fn read_digits(&mut self, first_char: &'a str) -> String {
        let mut lexeme = first_char.to_string();

        while let Some(x) = self.scanner.read() {
            if !char_validator::is_digit(x) {
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
    #[case::conjunct("그리고", vec![mktoken!(TokenKind::Conjunct, loc 0, 0, 0, 3)])]
    #[case::disjunct("또는", vec![mktoken!(TokenKind::Disjunct, loc 0, 0, 0, 2)])]
    fn single_token(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    #[rstest]
    #[case::single_alphabat_char("a", vec![mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1)])]
    #[case::single_hangul_char("가", vec![mktoken!(TokenKind::Identifier(String::from("가")), loc 0, 0, 0, 1)])]
    #[case::first_char_false_but_end("거", vec![mktoken!(TokenKind::Identifier(String::from("거")), loc 0, 0, 0, 1)])]
    #[case::first_char_false_but_second_non_id("거 ", vec![mktoken!(TokenKind::Identifier(String::from("거")), loc 0, 0, 0, 1)])]
    #[case::first_char_false_but_second_other_id("거a", vec![mktoken!(TokenKind::Identifier(String::from("거a")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_false_but_third_other_id("거짓a", vec![mktoken!(TokenKind::Identifier(String::from("거짓a")), loc 0, 0, 0, 3)])]
    #[case::first_char_conjunct_but_end("그", vec![mktoken!(TokenKind::Identifier(String::from("그")), loc 0, 0, 0, 1)])]
    #[case::first_char_conjunct_but_second_non_id("그 ", vec![mktoken!(TokenKind::Identifier(String::from("그")), loc 0, 0, 0, 1)])]
    #[case::first_char_conjunct_but_second_other_id("그a", vec![mktoken!(TokenKind::Identifier(String::from("그a")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_conjunct_but_end("그리", vec![mktoken!(TokenKind::Identifier(String::from("그리")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_conjunct_but_third_non_id("그리 ", vec![mktoken!(TokenKind::Identifier(String::from("그리")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_conjunct_but_third_other_id("그리a", vec![mktoken!(TokenKind::Identifier(String::from("그리a")), loc 0, 0, 0, 3)])]
    #[case::first_three_chars_conjunct_but_fourth_other_id("그리고a", vec![mktoken!(TokenKind::Identifier(String::from("그리고a")), loc 0, 0, 0, 4)])]
    #[case::first_two_chars_false_but_not_third("거짓a", vec![mktoken!(TokenKind::Identifier(String::from("거짓a")), loc 0, 0, 0, 3)])]
    #[case::first_char_disjunct_but_end("또", vec![mktoken!(TokenKind::Identifier(String::from("또")), loc 0, 0, 0, 1)])]
    #[case::first_char_disjunct_but_not_second("또늗", vec![mktoken!(TokenKind::Identifier(String::from("또늗")), loc 0, 0, 0, 2)])]
    // TODO: add first_two_chars_disjuct_but_not_third
    fn single_identifier(#[case] source: &str, #[case] expected: Vec<Token>) {
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
    #[case::false_false("거짓 거짓", vec![
        mktoken!(TokenKind::Bool(false), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Bool(false), loc 0, 3, 0, 5),
    ])]
    #[case::id1_false("거 거짓", vec![
        mktoken!(TokenKind::Identifier(String::from("거")), loc 0, 0, 0, 1),
        mktoken!(TokenKind::Bool(false), loc 0, 2, 0, 4),
    ])]
    #[case::id2_false("거짔 거짓", vec![
        mktoken!(TokenKind::Identifier(String::from("거짔")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Bool(false), loc 0, 3, 0, 5),
    ])]
    #[case::id3_false("거짓a 거짓", vec![
        mktoken!(TokenKind::Identifier(String::from("거짓a")), loc 0, 0, 0, 3),
        mktoken!(TokenKind::Bool(false), loc 0, 4, 0, 6),
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
