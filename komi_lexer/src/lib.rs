//! # Lexer
//!
//! Reads a source code and returns *tokens* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod err;
mod source_scanner;
mod utf8_tape;

pub use err::{LexError, LexErrorKind};
use komi_syntax::{StrSegment, StrSegmentKind, Token, TokenKind};
use komi_util::char_validator;
use komi_util::{Range, Scanner, Spot};
use source_scanner::SourceScanner;

type ResTokens = Result<Vec<Token>, LexError>;
type ResToken = Result<Token, LexError>;

/// Produces tokens from source codes.
struct Lexer<'a> {
    scanner: SourceScanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { scanner: SourceScanner::new(source) }
    }

    pub fn lex(&mut self) -> ResTokens {
        let mut tokens: Vec<Token> = vec![];

        // Lex characters into tokens one by one
        while let Some(char) = self.scanner.read() {
            let location = self.locate_and_advance();
            let token = match char {
                char if char_validator::is_digit(char) => self.lex_num(location, char)?,
                "참" => Token::new(TokenKind::Bool(true), location),
                "거" => self.expect_or_lex_identifier("짓", TokenKind::Bool(false), char, location)?,
                "+" => self.expect_next_or_token("=", TokenKind::PlusEquals, TokenKind::Plus, location)?,
                "-" => self.expect_next_or_token("=", TokenKind::MinusEquals, TokenKind::Minus, location)?,
                "*" => self.expect_next_or_token("=", TokenKind::AsteriskEquals, TokenKind::Asterisk, location)?,
                "/" => self.expect_next_or_token("=", TokenKind::SlashEquals, TokenKind::Slash, location)?,
                "%" => self.expect_next_or_token("=", TokenKind::PercentEquals, TokenKind::Percent, location)?,
                "(" => Token::new(TokenKind::LParen, location),
                ")" => Token::new(TokenKind::RParen, location),
                "{" => Token::new(TokenKind::LBrace, location),
                "}" => Token::new(TokenKind::RBrace, location),
                "<" => self.expect_next_or_token("=", TokenKind::LBracketEquals, TokenKind::LBracket, location)?,
                ">" => self.expect_next_or_token("=", TokenKind::RBracketEquals, TokenKind::RBracket, location)?,
                "\"" => self.lex_str(location)?,
                ":" => Token::new(TokenKind::Colon, location),
                "," => Token::new(TokenKind::Comma, location),
                "!" => self.expect_next_or_token("=", TokenKind::BangEquals, TokenKind::Bang, location)?,
                "=" => self.expect_next_or_token("=", TokenKind::DoubleEquals, TokenKind::Equals, location)?,
                "그" => self.expect_or_lex_identifier("리고", TokenKind::Conjunct, char, location)?,
                "또" => self.expect_or_lex_identifier("는", TokenKind::Disjunct, char, location)?,
                "함" => self.expect_or_lex_identifier("수", TokenKind::Closure, char, location)?,
                "만" => self.expect_or_lex_identifier("약", TokenKind::IfBranch, char, location)?,
                "아" => self.expect_or_lex_identifier("니면", TokenKind::ElseBranch, char, location)?,
                "반" => self.expect_or_lex_identifier("복", TokenKind::Iteration, char, location)?,
                "#" => {
                    self.skip_comment();
                    continue;
                }
                s if char_validator::is_in_identifier_domain(s) => {
                    self.lex_identifier_with_init_seg(&String::from(s), location)?
                }
                s if char_validator::is_whitespace(s) => {
                    continue;
                }
                _ => {
                    return Err(LexError::new(LexErrorKind::IllegalChar, location));
                }
            };
            tokens.push(token);
        }

        Ok(tokens)
    }

    /// Returns a number literal token if successfully lexed, or error otherwise.
    ///
    /// Call after advancing the scanner `self.scanner` past the initial character, with its location passed as `first_location`.
    fn lex_num(&mut self, first_location: Range, first_char: &'a str) -> ResToken {
        let mut lexeme = String::new();
        let begin = first_location.begin;

        // Read the whole number part
        lexeme.push_str(&self.read_digits(first_char));

        // Return a token if not a dot
        let Some(".") = self.scanner.read() else {
            let end = self.scanner.locate().begin;
            let token = Self::parse_num_lexeme(&lexeme, Range::new(begin, end));

            return Ok(token);
        };

        // Read a dot
        self.scanner.advance();
        lexeme.push_str(".");

        // Return an error if end or not a digit
        let Some(x) = self.scanner.read() else {
            let location = Range::new(begin, self.scanner.locate().end);
            return Err(LexError::new(LexErrorKind::IllegalNumLiteral, location));
        };
        if !char_validator::is_digit(x) {
            let location = Range::new(begin, self.scanner.locate().end);
            return Err(LexError::new(LexErrorKind::IllegalNumLiteral, location));
        }
        self.scanner.advance();

        // Read the decimal part
        lexeme.push_str(&self.read_digits(x));

        // Parse into a number and return a token
        let end = self.scanner.locate().begin;
        let token = Self::parse_num_lexeme(&lexeme, Range::new(begin, end));

        Ok(token)
    }

    /// Returns a sequence of tokens in a string literal if successfully lexed, or error otherwise.
    ///
    /// Call after advancing the scanner `self.scanner` past the left beginning quote, with its location passed as `first_location`.
    fn lex_str(&mut self, first_location: Range) -> ResToken {
        let mut segments: Vec<StrSegment> = vec![];
        let mut segments_location = first_location;

        // Read each segment into `seg` and push it to `segments`.
        let mut seg = String::new();
        let mut seg_location = self.scanner.locate();
        loop {
            // Return error if end of source
            let char_location = self.scanner.locate();
            let Some(char) = self.scanner.read_and_advance() else {
                return Err(LexError::new(LexErrorKind::StrQuoteNotClosed, seg_location));
            };

            // Break if end of string literal
            if char == "\"" {
                segments_location.end = char_location.end;

                self.push_segment_str_if_non_empty(&seg, &seg_location, &mut segments);

                break;
            }

            // Expect an escaped right brace "{{" or return error
            if char == "}" {
                let second_char_location = self.scanner.locate();
                seg_location.end = second_char_location.end;
                let Some("}") = self.scanner.read_and_advance() else {
                    return Err(LexError::new(LexErrorKind::IllegalRBraceInStr, seg_location));
                };

                seg.push_str(char);
                continue;
            }

            if char == "{" {
                let second_char_location = self.scanner.locate();
                let Some(second_char) = self.scanner.read_and_advance() else {
                    seg_location.end = second_char_location.end;
                    return Err(LexError::new(LexErrorKind::InterpolationNotClosed, seg_location));
                };
                // Push a single left brace "{" if an escaped left brace "{{" encountered
                if second_char == "{" {
                    seg_location.end = second_char_location.end;
                    seg.push_str(char);
                    continue;
                }
                if second_char == "}" {
                    seg_location.end = second_char_location.end;
                    return Err(LexError::new(LexErrorKind::NoInterpolatedIdentifier, seg_location));
                }
                if !char_validator::is_in_identifier_domain(second_char) {
                    return Err(LexError::new(
                        LexErrorKind::IllegalInterpolationChar,
                        second_char_location,
                    ));
                }

                // Push and reinitialize a segment, if a segment read previously
                self.push_segment_str_if_non_empty(&seg, &seg_location, &mut segments);
                seg = String::from(second_char);

                seg_location.begin = second_char_location.begin;
                // TODO: factor out into a function (as a reading function, not lexing) with common codes in `lex_identifier_with_init_seg`
                while let Some(x) = self.scanner.read() {
                    if !char_validator::is_in_identifier_domain(x) {
                        break;
                    }

                    // Extend a segment
                    seg.push_str(x);
                    seg_location.end = self.scanner.locate().end;

                    self.scanner.advance();
                }

                let last_char_location = self.scanner.locate();
                let Some(last_char) = self.scanner.read_and_advance() else {
                    seg_location.begin = char_location.begin;
                    seg_location.end = last_char_location.end;
                    return Err(LexError::new(LexErrorKind::InterpolationNotClosed, seg_location));
                };
                if last_char != "}" {
                    return Err(LexError::new(
                        LexErrorKind::IllegalInterpolationChar,
                        last_char_location,
                    ));
                }

                // Push and reinitialize a segment
                segments.push(StrSegment::new(StrSegmentKind::identifier(seg.clone()), seg_location));
                seg_location.begin = self.scanner.locate().begin;
                seg = String::new();

                continue;
            }

            // Push the character if not the cases above
            seg.push_str(char);
            seg_location.end = char_location.end;
        }

        let token = Token::new(TokenKind::Str(segments), segments_location);
        Ok(token)
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
        let mut digits = first_char.to_string();

        while let Some(x) = self.scanner.read() {
            if !char_validator::is_digit(x) {
                break;
            }

            digits.push_str(x);
            self.scanner.advance();
        }

        digits
    }

    /// Advances the scanner and returns a location before advancing.
    fn locate_and_advance(&mut self) -> Range {
        let location = self.scanner.locate();
        self.scanner.advance();

        location
    }

    fn expect_next_or_token(
        &mut self,
        expected: &str,
        expected_kind: TokenKind,
        alt_kind: TokenKind,
        first_location: Range,
    ) -> ResToken {
        match self.scanner.read() {
            Some(char) if char == expected => {
                let char_location = self.locate_and_advance();
                let lexeme_location = Range::new(first_location.begin, char_location.end);

                Ok(Token::new(expected_kind, lexeme_location))
            }
            _ => Ok(Token::new(alt_kind, first_location)),
        }
    }

    /// Returns a token with the kind `expected_kind` if the scanner reads the expected characters `expected`; otherwise, returns an identifier token.
    fn expect_or_lex_identifier(
        &mut self,
        expected: &str,
        expected_kind: TokenKind,
        first_char: &'a str,
        first_location: Range,
    ) -> ResToken {
        // Stores characters to lex an identifier token if an unexpected character encountered.
        let mut init_seg = String::from(first_char);
        let mut init_seg_location = first_location;

        // Read subsequent characters and match them against the expected characters one by one.
        // Return an identifier token if unexpected character encountered.
        for expected_char in expected.chars().map(|c| String::from(c)) {
            let char = self.scanner.read();

            if !Self::is_equal_str(char, &expected_char) {
                let token = self.lex_identifier_with_init_seg_or(char, &init_seg, &init_seg_location.begin, || {
                    // An identifier with characters read so far.
                    Ok(Token::new(TokenKind::Identifier(init_seg.clone()), init_seg_location))
                })?;
                return Ok(token);
            }

            init_seg.push_str(char.unwrap());
            init_seg_location.end = self.locate_and_advance().end;
        }

        // All expected characters matched; return the token with the expected kind.
        let char = self.scanner.read();
        let token = self.lex_identifier_with_init_seg_or(char, &init_seg, &init_seg_location.begin, || {
            Ok(Token::new(expected_kind.clone(), init_seg_location))
        })?;
        return Ok(token);
    }

    /// Returns an identifier token if `char_read` is a valid identifier character; otherwise, returns a token produced by `alt_op`.
    ///
    /// - `char_read`: A character just read by the scanner.
    /// - `init_seg`: The initial segment of characters already read by the scanner.
    /// - `init_seg_begin`: The beginning spot of the `init_seg`.
    /// - `alt_op`: A closure to invoke if an identifier cannot be lexed.
    fn lex_identifier_with_init_seg_or<F>(
        &mut self,
        char_read: Option<&'a str>,
        init_seg: &String,
        init_seg_begin: &Spot,
        alt_op: F,
    ) -> ResToken
    where
        F: Fn() -> ResToken,
    {
        match char_read {
            Some(c) if char_validator::is_in_identifier_domain(c) => {
                // Pass what the scanner just read to the identifier-lexing function below.
                let init_seg = init_seg.to_owned() + c;
                let char_end = self.scanner.locate().end;
                self.scanner.advance();

                let token = self.lex_identifier_with_init_seg(&init_seg, Range::new(*init_seg_begin, char_end))?;
                Ok(token)
            }
            _ => alt_op(),
        }
    }

    /// Returns an identifier token with the characters `init_seg` and subsequent characters the scanner read.
    fn lex_identifier_with_init_seg(&mut self, init_seg: &String, init_seg_location: Range) -> ResToken {
        let mut lexeme = init_seg.clone();
        let mut lexeme_location = init_seg_location.clone();

        while let Some(x) = self.scanner.read() {
            if !char_validator::is_in_identifier_domain(x) {
                break;
            }

            lexeme.push_str(x);
            lexeme_location.end = self.scanner.locate().end;

            self.scanner.advance();
        }

        Ok(Token::new(TokenKind::Identifier(lexeme), lexeme_location))
    }

    /// Returns true if `source` is `Some` and the value is equal to `target`.
    fn is_equal_str(source: Option<&str>, target: &str) -> bool {
        source.is_some_and(|c| c == target)
    }

    fn push_segment_str_if_non_empty(
        &self,
        segment: &String,
        segment_location: &Range,
        segments: &mut Vec<StrSegment>,
    ) -> () {
        if segment.len() == 0 {
            return;
        }

        segments.push(StrSegment::new(StrSegmentKind::str(segment), *segment_location));
    }
}

/// Produces tokens from source codes.
pub fn lex(source: &str) -> ResTokens {
    Lexer::new(source).lex()
}

#[cfg(test)]
mod tests {
    use super::*;
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

    // Should lex empty sources.
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

    // Should lex number literals.
    #[rstest]
    #[case::without_decimal("12", vec![mktoken!(TokenKind::Number(12.0), loc 0, 0, 0, "12".len())])]
    #[case::with_decimal("12.25", vec![mktoken!(TokenKind::Number(12.25), loc 0, 0, 0, "12.25".len())])]
    fn num_literal(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // Should fail to lex illegal number literals.
    #[rstest]
    #[case::illegal_char("12^", LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 2, 0, 3)))]
    #[case::beginning_with_dot(".25", LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))]
    #[case::ending_with_dot("12.", LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 3)))]
    #[case::ending_with_two_dots("12..", LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 4)))]
    #[case::ending_with_two_dots("12..", LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 4)))]
    #[case::illegal_decimal("12..", LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 4)))]
    fn illegal_num_literal(#[case] source: &str, #[case] error: LexError) {
        assert_lex_fail!(source, error);
    }

    // Should lex boolean literals
    #[rstest]
    #[case::the_true("참", vec![mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1)])]
    #[case::the_false("거짓", vec![mktoken!(TokenKind::Bool(false), loc 0, 0, 0, 2)])]
    fn bool_literal(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // TODO: Should lex str literals.
    #[rstest]
    #[case::empty_string("\"\"", vec![
        mktoken!(TokenKind::Str(vec![]), loc 0, 0, 0, 2),
    ])]
    #[case::str("\"사과\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("사과"), Range::from_nums(0,1,0,3))]), loc 0, 0, 0, 4),
    ])]
    #[case::str_with_otherwise_illegal_chars("\"!@# \"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("!@# "), Range::from_nums(0,1,0,5))]), loc 0, 0, 0, 6),
    ])]
    #[case::str_with_new_line("\"\r\n\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("\r\n"), Range::from_nums(0,1,1,0))]), loc 0, 0, 1, 1),
    ])]
    #[case::str_with_new_lines("\"\r\n\r\n\r\n\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("\r\n\r\n\r\n"), Range::from_nums(0,1,3,0))]), loc 0, 0, 3, 1),
    ])]
    #[case::lbrace_escape("\"{{\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("{"), Range::from_nums(0,1,0,3))]), loc 0, 0, 0, 4),
    ])]
    #[case::rbrace_escape("\"}}\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("}"), Range::from_nums(0,1,0,3))]), loc 0, 0, 0, 4),
    ])]
    #[case::two_lbrace_escape("\"{{{{\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("{{"), Range::from_nums(0,1,0,5))]), loc 0, 0, 0, 6),
    ])]
    #[case::two_rbrace_escapes("\"}}}}\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("}}"), Range::from_nums(0,1,0,5))]), loc 0, 0, 0, 6),
    ])]
    #[case::lbrace_and_rbrace_escapes("\"{{}}\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("{}"), Range::from_nums(0,1,0,5))]), loc 0, 0, 0, 6),
    ])]
    #[case::rbrace_and_lbrace_escapes("\"}}{{\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::str("}{"), Range::from_nums(0,1,0,5))]), loc 0, 0, 0, 6),
    ])]
    #[case::id("\"{사과}\"", vec![
        mktoken!(TokenKind::Str(vec![StrSegment::new(StrSegmentKind::identifier("사과"), Range::from_nums(0,2,0,4))]), loc 0, 0, 0, 6),
    ])]
    #[case::str_and_id("\"사과{오렌지}\"", vec![
        mktoken!(TokenKind::Str(vec![
            StrSegment::new(StrSegmentKind::str("사과"), Range::from_nums(0,1,0,3)),
            StrSegment::new(StrSegmentKind::identifier("오렌지"), Range::from_nums(0,4,0,7)),
        ]), loc 0, 0, 0, 9),
    ])]
    #[case::id_and_str("\"{사과}오렌지\"", vec![
        mktoken!(TokenKind::Str(vec![
            StrSegment::new(StrSegmentKind::identifier("사과"), Range::from_nums(0,2,0,4)),
            StrSegment::new(StrSegmentKind::str("오렌지"), Range::from_nums(0,5,0,8)),
        ]), loc 0, 0, 0, 9),
    ])]
    #[case::id_and_id("\"{사과}{오렌지}\"", vec![
        mktoken!(TokenKind::Str(vec![
            StrSegment::new(StrSegmentKind::identifier("사과"), Range::from_nums(0,2,0,4)),
            StrSegment::new(StrSegmentKind::identifier("오렌지"), Range::from_nums(0,6,0,9)),
        ]), loc 0, 0, 0, 11),
    ])]
    #[case::str_id_str("\"사과{오렌지}바나나\"", vec![
        mktoken!(TokenKind::Str(vec![
            StrSegment::new(StrSegmentKind::str("사과"), Range::from_nums(0,1,0,3)),
            StrSegment::new(StrSegmentKind::identifier("오렌지"), Range::from_nums(0,4,0,7)),
            StrSegment::new(StrSegmentKind::str("바나나"), Range::from_nums(0,8,0,11)),
        ]), loc 0, 0, 0, 12),
    ])]
    #[case::id_str_id("\"{사과}오렌지{바나나}\"", vec![
        mktoken!(TokenKind::Str(vec![
            StrSegment::new(StrSegmentKind::identifier("사과"), Range::from_nums(0,2,0,4)),
            StrSegment::new(StrSegmentKind::str("오렌지"), Range::from_nums(0,5,0,8)),
            StrSegment::new(StrSegmentKind::identifier("바나나"), Range::from_nums(0,9,0,12)),
        ]), loc 0, 0, 0, 14),
    ])]
    #[case::id_id_id("\"{사과}{오렌지}{바나나}\"", vec![
        mktoken!(TokenKind::Str(vec![
            StrSegment::new(StrSegmentKind::identifier("사과"), Range::from_nums(0,2,0,4)),
            StrSegment::new(StrSegmentKind::identifier("오렌지"), Range::from_nums(0,6,0,9)),
            StrSegment::new(StrSegmentKind::identifier("바나나"), Range::from_nums(0,11,0,14)),
        ]), loc 0, 0, 0, 16),
    ])]
    fn string_segment(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // Should fail to lex illegal str literals.
    #[rstest]
    #[case::lbrace_not_closed_with_immediate_end(
        "\"{",
        LexError::new(LexErrorKind::InterpolationNotClosed, Range::from_nums(0, 1, 0, 2))
    )]
    #[case::lbrace_not_closed_with_not_immediate_end(
        "\"{사과",
        LexError::new(LexErrorKind::InterpolationNotClosed, Range::from_nums(0, 1, 0, 4))
    )]
    #[case::rbrace_in_str("\"}", LexError::new(LexErrorKind::IllegalRBraceInStr, Range::from_nums(0, 1, 0, 2)))]
    #[case::empty_identifier(
        "\"{}\"",
        LexError::new(LexErrorKind::NoInterpolatedIdentifier, Range::from_nums(0, 1, 0, 3))
    )]
    #[case::illegal_identifier_char_at_first(
        "\"{+}\"",
        LexError::new(LexErrorKind::IllegalInterpolationChar, Range::from_nums(0, 2, 0, 3))
    )]
    #[case::illegal_identifier_char_in_middle(
        "\"{사+}\"",
        LexError::new(LexErrorKind::IllegalInterpolationChar, Range::from_nums(0, 3, 0, 4))
    )]
    #[case::lbrace_not_closed_with_some_chars(
        "\"{사과",
        LexError::new(LexErrorKind::InterpolationNotClosed, Range::from_nums(0, 1, 0, 4))
    )]
    fn illegal_string_segment(#[case] source: &str, #[case] error: LexError) {
        assert_lex_fail!(source, error);
    }

    // Should lex non-value literals.
    #[rstest]
    #[case::plus("+", vec![mktoken!(TokenKind::Plus, loc 0, 0, 0, 1)])]
    #[case::minus("-", vec![mktoken!(TokenKind::Minus, loc 0, 0, 0, 1)])]
    #[case::asterisk("*", vec![mktoken!(TokenKind::Asterisk, loc 0, 0, 0, 1)])]
    #[case::slash("/", vec![mktoken!(TokenKind::Slash, loc 0, 0, 0, 1)])]
    #[case::percent("%", vec![mktoken!(TokenKind::Percent, loc 0, 0, 0, 1)])]
    #[case::lparen("(", vec![mktoken!(TokenKind::LParen, loc 0, 0, 0, 1)])]
    #[case::rparen(")", vec![mktoken!(TokenKind::RParen, loc 0, 0, 0, 1)])]
    #[case::lbrace("{", vec![mktoken!(TokenKind::LBrace, loc 0, 0, 0, 1)])]
    #[case::lbrace("}", vec![mktoken!(TokenKind::RBrace, loc 0, 0, 0, 1)])]
    #[case::lbracket("<", vec![mktoken!(TokenKind::LBracket, loc 0, 0, 0, 1)])]
    #[case::rbracket(">", vec![mktoken!(TokenKind::RBracket, loc 0, 0, 0, 1)])]
    #[case::colon(":", vec![mktoken!(TokenKind::Colon, loc 0, 0, 0, 1)])]
    #[case::comma(",", vec![mktoken!(TokenKind::Comma, loc 0, 0, 0, 1)])]
    #[case::bang("!", vec![mktoken!(TokenKind::Bang, loc 0, 0, 0, 1)])]
    #[case::equals("=", vec![mktoken!(TokenKind::Equals, loc 0, 0, 0, 1)])]
    #[case::plus_equals("+=", vec![mktoken!(TokenKind::PlusEquals, loc 0, 0, 0, 2)])]
    #[case::minus_equals("-=", vec![mktoken!(TokenKind::MinusEquals, loc 0, 0, 0, 2)])]
    #[case::asterisk_equals("*=", vec![mktoken!(TokenKind::AsteriskEquals, loc 0, 0, 0, 2)])]
    #[case::slash_equals("/=", vec![mktoken!(TokenKind::SlashEquals, loc 0, 0, 0, 2)])]
    #[case::percent_equals("%=", vec![mktoken!(TokenKind::PercentEquals, loc 0, 0, 0, 2)])]
    #[case::double_equals("==", vec![mktoken!(TokenKind::DoubleEquals, loc 0, 0, 0, 2)])]
    #[case::bang_equals("!=", vec![mktoken!(TokenKind::BangEquals, loc 0, 0, 0, 2)])]
    #[case::lbracket_equals("<=", vec![mktoken!(TokenKind::LBracketEquals, loc 0, 0, 0, 2)])]
    #[case::rbracket_equals(">=", vec![mktoken!(TokenKind::RBracketEquals, loc 0, 0, 0, 2)])]
    #[case::conjunct("그리고", vec![mktoken!(TokenKind::Conjunct, loc 0, 0, 0, 3)])]
    #[case::disjunct("또는", vec![mktoken!(TokenKind::Disjunct, loc 0, 0, 0, 2)])]
    #[case::closure("함수", vec![mktoken!(TokenKind::Closure, loc 0, 0, 0, 2)])]
    #[case::if_branch("만약", vec![mktoken!(TokenKind::IfBranch, loc 0, 0, 0, 2)])]
    #[case::else_branch("아니면", vec![mktoken!(TokenKind::ElseBranch, loc 0, 0, 0, 3)])]
    #[case::iteration("반복", vec![mktoken!(TokenKind::Iteration, loc 0, 0, 0, 2)])]
    fn non_value_literal(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // Should lex identifiers.
    #[rstest]
    #[case::single_alphabat_char("a", vec![mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1)])]
    #[case::single_hangul_char("가", vec![mktoken!(TokenKind::Identifier(String::from("가")), loc 0, 0, 0, 1)])]
    #[case::mixed_multiple_chars("a가a가", vec![mktoken!(TokenKind::Identifier(String::from("a가a가")), loc 0, 0, 0, 4)])]
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
    #[case::first_char_disjunct_but_end("또", vec![mktoken!(TokenKind::Identifier(String::from("또")), loc 0, 0, 0, 1)])]
    #[case::first_char_disjunct_but_second_non_id("또" , vec![mktoken!(TokenKind::Identifier(String::from("또")), loc 0, 0, 0, 1)])]
    #[case::first_char_disjunct_but_second_other_id("또a", vec![mktoken!(TokenKind::Identifier(String::from("또a")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_disjunct_but_third_other_id("또는a", vec![mktoken!(TokenKind::Identifier(String::from("또는a")), loc 0, 0, 0, 3)])]
    #[case::first_char_if_branch_but_end("만", vec![mktoken!(TokenKind::Identifier(String::from("만")), loc 0, 0, 0, 1)])]
    #[case::first_char_if_branch_but_second_non_id("만" , vec![mktoken!(TokenKind::Identifier(String::from("만")), loc 0, 0, 0, 1)])]
    #[case::first_char_if_branch_but_second_other_id("만a", vec![mktoken!(TokenKind::Identifier(String::from("만a")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_if_branch_but_third_other_id("만약a", vec![mktoken!(TokenKind::Identifier(String::from("만약a")), loc 0, 0, 0, 3)])]
    #[case::first_char_else_branch_but_end("아", vec![mktoken!(TokenKind::Identifier(String::from("아")), loc 0, 0, 0, 1)])]
    #[case::first_char_else_branch_but_second_non_id("아 ", vec![mktoken!(TokenKind::Identifier(String::from("아")), loc 0, 0, 0, 1)])]
    #[case::first_char_else_branch_but_second_other_id("아a", vec![mktoken!(TokenKind::Identifier(String::from("아a")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_else_branch_but_end("아니", vec![mktoken!(TokenKind::Identifier(String::from("아니")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_else_branch_but_third_non_id("아니 ", vec![mktoken!(TokenKind::Identifier(String::from("아니")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_else_branch_but_third_other_id("아니a", vec![mktoken!(TokenKind::Identifier(String::from("아니a")), loc 0, 0, 0, 3)])]
    #[case::first_three_chars_else_branch_but_fourth_other_id("아니면a", vec![mktoken!(TokenKind::Identifier(String::from("아니면a")), loc 0, 0, 0, 4)])]
    #[case::first_char_iteration_but_end("반", vec![mktoken!(TokenKind::Identifier(String::from("반")), loc 0, 0, 0, 1)])]
    #[case::first_char_iteration_but_second_non_id("반" , vec![mktoken!(TokenKind::Identifier(String::from("반")), loc 0, 0, 0, 1)])]
    #[case::first_char_iteration_but_second_other_id("반a", vec![mktoken!(TokenKind::Identifier(String::from("반a")), loc 0, 0, 0, 2)])]
    #[case::first_two_chars_iteration_but_third_other_id("반복a", vec![mktoken!(TokenKind::Identifier(String::from("반복a")), loc 0, 0, 0, 3)])]
    fn single_identifier(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // Should lex two same literals.
    // Should not fail in all below cases, since the lexer does not know the syntax.
    // To test when tokens cannot be early determined by the first character.
    #[rstest]
    #[case::two_pluses("++", vec![
        mktoken!(TokenKind::Plus, loc 0, 0, 0, 1),
        mktoken!(TokenKind::Plus, loc 0, 1, 0, 2),
    ])]
    #[case::two_minuses("--", vec![
        mktoken!(TokenKind::Minus, loc 0, 0, 0, 1),
        mktoken!(TokenKind::Minus, loc 0, 1, 0, 2),
    ])]
    #[case::two_asterisks("**", vec![
        mktoken!(TokenKind::Asterisk, loc 0, 0, 0, 1),
        mktoken!(TokenKind::Asterisk, loc 0, 1, 0, 2),
    ])]
    #[case::two_slashes("//", vec![
        mktoken!(TokenKind::Slash, loc 0, 0, 0, 1),
        mktoken!(TokenKind::Slash, loc 0, 1, 0, 2),
    ])]
    #[case::two_slashes("%%", vec![
        mktoken!(TokenKind::Percent, loc 0, 0, 0, 1),
        mktoken!(TokenKind::Percent, loc 0, 1, 0, 2),
    ])]
    #[case::two_lbrackets("<<", vec![
        mktoken!(TokenKind::LBracket, loc 0, 0, 0, 1),
        mktoken!(TokenKind::LBracket, loc 0, 1, 0, 2),
    ])]
    #[case::two_rbrackets(">>", vec![
        mktoken!(TokenKind::RBracket, loc 0, 0, 0, 1),
        mktoken!(TokenKind::RBracket, loc 0, 1, 0, 2),
    ])]
    #[case::two_bangs("!!", vec![
        mktoken!(TokenKind::Bang, loc 0, 0, 0, 1),
        mktoken!(TokenKind::Bang, loc 0, 1, 0, 2),
    ])]
    #[case::double_equals_and_equals("===", vec![
        mktoken!(TokenKind::DoubleEquals, loc 0, 0, 0, 2),
        mktoken!(TokenKind::Equals, loc 0, 2, 0, 3),
    ])]
    #[case::false_false("거짓 거짓", vec![
        mktoken!(TokenKind::Bool(false), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Bool(false), loc 0, 3, 0, 5),
    ])]
    #[case::conjunct_conjunct("그리고 그리고", vec![
        mktoken!(TokenKind::Conjunct, loc 0, 0, 0, 3),
        mktoken!(TokenKind::Conjunct, loc 0, 4, 0, 7),
    ])]
    #[case::disjunct_disjunct("또는 또는", vec![
        mktoken!(TokenKind::Disjunct, loc 0, 0, 0, 2),
        mktoken!(TokenKind::Disjunct, loc 0, 3, 0, 5),
    ])]
    #[case::closure_closure("함수 함수", vec![
        mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
        mktoken!(TokenKind::Closure, loc 0, 3, 0, 5),
    ])]
    #[case::if_branch_if_branch("만약 만약", vec![
        mktoken!(TokenKind::IfBranch, loc 0, 0, 0, 2),
        mktoken!(TokenKind::IfBranch, loc 0, 3, 0, 5),
    ])]
    #[case::else_branch_else_branch("아니면 아니면", vec![
        mktoken!(TokenKind::ElseBranch, loc 0, 0, 0, 3),
        mktoken!(TokenKind::ElseBranch, loc 0, 4, 0, 7),
    ])]
    #[case::iteration_iteration("반복 반복", vec![
        mktoken!(TokenKind::Iteration, loc 0, 0, 0, 2),
        mktoken!(TokenKind::Iteration, loc 0, 3, 0, 5),
    ])]
    fn two_same_tokens(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // Should lex an identifier and a similar keyword.
    // To test locations of tokens, when the first identifier is lexed from the same lexing function with the second token.
    #[rstest]
    #[case::id1_false("거 거짓", vec![
        mktoken!(TokenKind::Identifier(String::from("거")), loc 0, 0, 0, 1),
        mktoken!(TokenKind::Bool(false), loc 0, 2, 0, 4),
    ])]
    #[case::id2_false("거a 거짓", vec![
        mktoken!(TokenKind::Identifier(String::from("거a")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Bool(false), loc 0, 3, 0, 5),
    ])]
    #[case::id3_false("거짓a 거짓", vec![
        mktoken!(TokenKind::Identifier(String::from("거짓a")), loc 0, 0, 0, 3),
        mktoken!(TokenKind::Bool(false), loc 0, 4, 0, 6),
    ])]
    #[case::id1_conjuct("그 그리고", vec![
        mktoken!(TokenKind::Identifier(String::from("그")), loc 0, 0, 0, 1),
        mktoken!(TokenKind::Conjunct, loc 0, 2, 0, 5),
    ])]
    #[case::id2_conjunct("그a 그리고", vec![
        mktoken!(TokenKind::Identifier(String::from("그a")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Conjunct, loc 0, 3, 0, 6),
    ])]
    #[case::id3_conjunct("그리 그리고", vec![
        mktoken!(TokenKind::Identifier(String::from("그리")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Conjunct, loc 0, 3, 0, 6),
    ])]
    #[case::id4_conjunct("그리a 그리고", vec![
        mktoken!(TokenKind::Identifier(String::from("그리a")), loc 0, 0, 0, 3),
        mktoken!(TokenKind::Conjunct, loc 0, 4, 0, 7),
    ])]
    #[case::id4_conjunct("그리고a 그리고", vec![
        mktoken!(TokenKind::Identifier(String::from("그리고a")), loc 0, 0, 0, 4),
        mktoken!(TokenKind::Conjunct, loc 0, 5, 0, 8),
    ])]
    #[case::id1_disjunct("또 또는", vec![
        mktoken!(TokenKind::Identifier(String::from("또")), loc 0, 0, 0, 1),
        mktoken!(TokenKind::Disjunct, loc 0, 2, 0, 4),
    ])]
    #[case::id2_disjunct("또a 또는", vec![
        mktoken!(TokenKind::Identifier(String::from("또a")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Disjunct, loc 0, 3, 0, 5),
    ])]
    #[case::id3_disjunct("또는a 또는", vec![
        mktoken!(TokenKind::Identifier(String::from("또는a")), loc 0, 0, 0, 3),
        mktoken!(TokenKind::Disjunct, loc 0, 4, 0, 6),
    ])]
    #[case::id1_closure("함 함수", vec![
        mktoken!(TokenKind::Identifier(String::from("함")), loc 0, 0, 0, 1),
        mktoken!(TokenKind::Closure, loc 0, 2, 0, 4),
    ])]
    #[case::id2_closure("함a 함수", vec![
        mktoken!(TokenKind::Identifier(String::from("함a")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Closure, loc 0, 3, 0, 5),
    ])]
    #[case::id3_closure("함수a 함수", vec![
        mktoken!(TokenKind::Identifier(String::from("함수a")), loc 0, 0, 0, 3),
        mktoken!(TokenKind::Closure, loc 0, 4, 0, 6),
    ])]
    #[case::id1_if_branch("만 만약", vec![
        mktoken!(TokenKind::Identifier(String::from("만")), loc 0, 0, 0, 1),
        mktoken!(TokenKind::IfBranch, loc 0, 2, 0, 4),
    ])]
    #[case::id2_if_branch("만a 만약", vec![
        mktoken!(TokenKind::Identifier(String::from("만a")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::IfBranch, loc 0, 3, 0, 5),
    ])]
    #[case::id3_if_branch("만약a 만약", vec![
        mktoken!(TokenKind::Identifier(String::from("만약a")), loc 0, 0, 0, 3),
        mktoken!(TokenKind::IfBranch, loc 0, 4, 0, 6),
    ])]
    #[case::id1_else_branch("아 아니면", vec![
        mktoken!(TokenKind::Identifier(String::from("아")), loc 0, 0, 0, 1),
        mktoken!(TokenKind::ElseBranch, loc 0, 2, 0, 5),
    ])]
    #[case::id2_else_branch("아a 아니면", vec![
        mktoken!(TokenKind::Identifier(String::from("아a")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::ElseBranch, loc 0, 3, 0, 6),
    ])]
    #[case::id3_else_branch("아니 아니면", vec![
        mktoken!(TokenKind::Identifier(String::from("아니")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::ElseBranch, loc 0, 3, 0, 6),
    ])]
    #[case::id4_else_branch("아니a 아니면", vec![
        mktoken!(TokenKind::Identifier(String::from("아니a")), loc 0, 0, 0, 3),
        mktoken!(TokenKind::ElseBranch, loc 0, 4, 0, 7),
    ])]
    #[case::id4_else_branch("아니면a 아니면", vec![
        mktoken!(TokenKind::Identifier(String::from("아니면a")), loc 0, 0, 0, 4),
        mktoken!(TokenKind::ElseBranch, loc 0, 5, 0, 8),
    ])]
    #[case::id1_iteration("반 반복", vec![
        mktoken!(TokenKind::Identifier(String::from("반")), loc 0, 0, 0, 1),
        mktoken!(TokenKind::Iteration, loc 0, 2, 0, 4),
    ])]
    #[case::id2_iteration("반a 반복", vec![
        mktoken!(TokenKind::Identifier(String::from("반a")), loc 0, 0, 0, 2),
        mktoken!(TokenKind::Iteration, loc 0, 3, 0, 5),
    ])]
    #[case::id3_iteration("반복a 반복", vec![
        mktoken!(TokenKind::Identifier(String::from("반복a")), loc 0, 0, 0, 3),
        mktoken!(TokenKind::Iteration, loc 0, 4, 0, 6),
    ])]
    fn two_similar_tokens(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // Should lex sequences of characters, that may appear in source codes.
    #[rstest]
    #[case::addition_expression("12 + 34.675", vec![
        mktoken!(TokenKind::Number(12.0), loc 0, 0, 0, "12".len()),
        mktoken!(TokenKind::Plus, loc 0, "12 ".len(), 0, "12 +".len()),
        mktoken!(TokenKind::Number(34.675), loc 0, "12 + ".len(), 0, "12 + 34.675".len()),
    ])]
    #[case::conjunction_expression("참 그리고 거짓", vec![
        mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
        mktoken!(TokenKind::Conjunct, loc 0, 2, 0, 5),
        mktoken!(TokenKind::Bool(false), loc 0, 6, 0, 8),
    ])]
    #[case::closure_expression_with_no_parameters("함수 { 1 }", vec![
        mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
        mktoken!(TokenKind::LBrace, loc 0, 3, 0, 4),
        mktoken!(TokenKind::Number(1.0), loc 0, 5, 0, 6),
        mktoken!(TokenKind::RBrace, loc 0, 7, 0, 8),
    ])]
    #[case::closure_expression_with_parameters("함수 사과, 바나나 { 1 }", vec![
        mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
        mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 3, 0, 5),
        mktoken!(TokenKind::Comma, loc 0, 5, 0, 6),
        mktoken!(TokenKind::Identifier(String::from("바나나")), loc 0, 7, 0, 10),
        mktoken!(TokenKind::LBrace, loc 0, 11, 0, 12),
        mktoken!(TokenKind::Number(1.0), loc 0, 13, 0, 14),
        mktoken!(TokenKind::RBrace, loc 0, 15, 0, 16),
    ])]
    fn multiple_tokens(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // Should fail to lex illegal characters.
    #[rstest]
    #[case::caret("^", LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))]
    #[case::dollar("$", LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))]
    fn illegal_char(#[case] source: &str, #[case] error: LexError) {
        assert_lex_fail!(source, error);
    }
}
