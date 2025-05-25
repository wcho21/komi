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

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { scanner: SourceScanner::new(source) }
    }

    pub fn lex(&mut self) -> ResTokens {
        let mut tokens: Vec<Token> = vec![];

        while let Some(char) = self.scanner.read() {
            let char_location = self.locate_and_advance();

            match char {
                char if char_validator::is_digit(char) => {
                    let token = self.lex_num(&char_location, char)?;
                    tokens.push(token);
                }
                "참" => {
                    let token = Token::new(TokenKind::Bool(true), char_location);
                    tokens.push(token);
                }
                "거" => {
                    let token = self.expect_or_lex_identifier("짓", TokenKind::Bool(false), char, &char_location)?;
                    tokens.push(token);
                }
                "+" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::PlusEquals, TokenKind::Plus, &char_location)?;
                    tokens.push(token);
                }
                "-" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::MinusEquals, TokenKind::Minus, &char_location)?;
                    tokens.push(token);
                }
                "*" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::AsteriskEquals, TokenKind::Asterisk, &char_location)?;
                    tokens.push(token);
                }
                "/" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::SlashEquals, TokenKind::Slash, &char_location)?;
                    tokens.push(token);
                }
                "%" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::PercentEquals, TokenKind::Percent, &char_location)?;
                    tokens.push(token);
                }
                "(" => {
                    let token = Token::new(TokenKind::LParen, char_location);
                    tokens.push(token);
                }
                ")" => {
                    let token = Token::new(TokenKind::RParen, char_location);
                    tokens.push(token);
                }
                "{" => {
                    let token = Token::new(TokenKind::LBrace, char_location);
                    tokens.push(token);
                }
                "}" => {
                    let token = Token::new(TokenKind::RBrace, char_location);
                    tokens.push(token);
                }
                "<" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::LBracketEquals, TokenKind::LBracket, &char_location)?;
                    tokens.push(token);
                }
                ">" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::RBracketEquals, TokenKind::RBracket, &char_location)?;
                    tokens.push(token);
                }
                "\"" => {
                    let mut string_tokens = self.lex_string(&char_location)?;
                    tokens.append(&mut string_tokens);
                }
                ":" => {
                    let token = Token::new(TokenKind::Colon, char_location);
                    tokens.push(token);
                }
                "," => {
                    let token = Token::new(TokenKind::Comma, char_location);
                    tokens.push(token);
                }
                "!" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::BangEquals, TokenKind::Bang, &char_location)?;
                    tokens.push(token);
                }
                "=" => {
                    let token =
                        self.expect_next_or_token("=", TokenKind::DoubleEquals, TokenKind::Equals, &char_location)?;
                    tokens.push(token);
                }
                "그" => {
                    let token = self.expect_or_lex_identifier("리고", TokenKind::Conjunct, char, &char_location)?;
                    tokens.push(token);
                }
                "또" => {
                    let token = self.expect_or_lex_identifier("는", TokenKind::Disjunct, char, &char_location)?;
                    tokens.push(token);
                }
                "함" => {
                    let token = self.expect_or_lex_identifier("수", TokenKind::Closure, char, &char_location)?;
                    tokens.push(token);
                }
                "만" => {
                    let token = self.expect_or_lex_identifier("약", TokenKind::IfBranch, char, &char_location)?;
                    tokens.push(token);
                }
                "아" => {
                    let token = self.expect_or_lex_identifier("니면", TokenKind::ElseBranch, char, &char_location)?;
                    tokens.push(token);
                }
                "반" => {
                    let token = self.expect_or_lex_identifier("복", TokenKind::Iteration, char, &char_location)?;
                    tokens.push(token);
                }
                "#" => {
                    self.skip_comment();
                }
                s if char_validator::is_in_identifier_domain(s) => {
                    let token = self.lex_identifier_with_init_seg(&String::from(s), &char_location)?;
                    tokens.push(token);
                }
                s if char_validator::is_whitespace(s) => {
                    continue;
                }
                _ => {
                    return Err(LexError::new(LexErrorKind::IllegalChar, char_location));
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

    fn lex_string(&mut self, first_location: &Range) -> ResTokens {
        let mut tokens: Vec<Token> = vec![];
        tokens.push(Token::new(TokenKind::Quote, *first_location));

        let mut segment = String::new();
        let mut segment_location = self.scanner.locate();

        loop {
            let Some(char) = self.scanner.read() else {
                return Err(LexError::new(LexErrorKind::QuoteNotClosed, segment_location));
            };

            if char == "\"" {
                tokens.push(Token::new(TokenKind::StringSegment(segment), segment_location));
                tokens.push(Token::new(TokenKind::Quote, self.scanner.locate()));
                self.scanner.advance();

                break;
            }

            if char == "{" {
                self.scanner.advance();

                match self.scanner.read() {
                    // `{{` is an escape for `{`.
                    Some("{") => {
                        segment.push_str("{");
                        self.scanner.advance();
                        continue;
                    }
                    _ => {
                        todo!()
                    }
                }
            }

            if char == "}" {
                self.scanner.advance();

                match self.scanner.read() {
                    Some("}") => {
                        segment.push_str("}");
                        self.scanner.advance();
                        continue;
                    }
                    _ => {
                        todo!()
                    }
                }
            }

            segment.push_str(char);
            segment_location.end = self.scanner.locate().end;

            self.scanner.advance();
        }

        Ok(tokens)
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
        first_location: &Range,
    ) -> ResToken {
        match self.scanner.read() {
            Some(char) if char == expected => {
                let char_location = self.locate_and_advance();
                let lexeme_location = Range::new(first_location.begin, char_location.end);

                Ok(Token::new(expected_kind, lexeme_location))
            }
            _ => Ok(Token::new(alt_kind, *first_location)),
        }
    }

    /// Returns a token with the kind `expected_kind` if the scanner reads the expected characters `expected`; otherwise, returns an identifier token.
    fn expect_or_lex_identifier(
        &mut self,
        expected: &str,
        expected_kind: TokenKind,
        first_char: &'a str,
        first_location: &Range,
    ) -> ResToken {
        // Stores characters to lex an identifier token if an unexpected character encountered.
        let mut init_seg = String::from(first_char);
        let mut init_seg_location = *first_location;

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

                let token = self.lex_identifier_with_init_seg(&init_seg, &Range::new(*init_seg_begin, char_end))?;
                Ok(token)
            }
            _ => alt_op(),
        }
    }

    /// Returns an identifier token with the characters `init_seg` and subsequent characters the scanner read.
    fn lex_identifier_with_init_seg(&mut self, init_seg: &String, init_seg_location: &Range) -> ResToken {
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
    fn illegal_num_literal(#[case] source: &str, #[case] expected: LexError) {
        assert_lex_fail!(source, expected);
    }

    // Should lex boolean literals
    #[rstest]
    #[case::the_true("참", vec![mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1)])]
    #[case::the_false("거짓", vec![mktoken!(TokenKind::Bool(false), loc 0, 0, 0, 2)])]
    fn bool_literal(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
    }

    // TODO: Should lex str literals.
    // TODO(?): Should fail to lex illegal str literals.
    #[rstest]
    #[case::simple_string("\"사과\"", vec![
        mktoken!(TokenKind::Quote, loc 0, 0, 0, 1),
        mktoken!(TokenKind::StringSegment(String::from("사과")), loc 0, 1, 0, 3),
        mktoken!(TokenKind::Quote, loc 0, 3, 0, 4),
    ])]
    #[case::lbrace_escape("\"사{{과\"", vec![
        mktoken!(TokenKind::Quote, loc 0, 0, 0, 1),
        mktoken!(TokenKind::StringSegment(String::from("사{과")), loc 0, 1, 0, 5),
        mktoken!(TokenKind::Quote, loc 0, 5, 0, 6),
    ])]
    #[case::rbrace_escape("\"사}}과\"", vec![
        mktoken!(TokenKind::Quote, loc 0, 0, 0, 1),
        mktoken!(TokenKind::StringSegment(String::from("사}과")), loc 0, 1, 0, 5),
        mktoken!(TokenKind::Quote, loc 0, 5, 0, 6),
    ])]
    /* TODO
    #[case::num_literal_interpolation("\"사{1}과\"", vec![
        mktoken!(TokenKind::Quote, loc 0, 0, 0, 1),
        mktoken!(TokenKind::LBrace, loc 0, 1, 0, 2),
        mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
        mktoken!(TokenKind::RBrace, loc 0, 3, 0, 4),
        mktoken!(TokenKind::Quote, loc 0, 4, 0, 5),
    ])]
    #[case::bool_literal_interpolation("\"사{참}과\"", vec![
        mktoken!(TokenKind::Quote, loc 0, 0, 0, 1),
        mktoken!(TokenKind::LBrace, loc 0, 1, 0, 2),
        mktoken!(TokenKind::Bool(true), loc 0, 2, 0, 3),
        mktoken!(TokenKind::RBrace, loc 0, 3, 0, 4),
        mktoken!(TokenKind::Quote, loc 0, 4, 0, 5),
    ])]
    #[case::string_literal_interpolation("\"사{\"오렌지\"}과\"", vec![
        mktoken!(TokenKind::Quote, loc 0, 0, 0, 1),
        mktoken!(TokenKind::LBrace, loc 0, 1, 0, 2),
        mktoken!(TokenKind::Quote, loc 0, 2, 0, 3),
        mktoken!(TokenKind::StringSegment(String::from("오렌지")), loc 0, 3, 0, 6),
        mktoken!(TokenKind::Quote, loc 0, 6, 0, 7),
        mktoken!(TokenKind::RBrace, loc 0, 7, 0, 8),
        mktoken!(TokenKind::Quote, loc 0, 8, 0, 9),
    ])]
    */
    // TODO: test string literal nested interpolation
    // TODO: test other interpolations
    fn string_segment(#[case] source: &str, #[case] expected: Vec<Token>) {
        assert_lex!(source, expected);
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
    fn illegal_char(#[case] source: &str, #[case] expected: LexError) {
        assert_lex_fail!(source, expected);
    }
}
