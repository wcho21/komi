//! # Lexer
//!
//! Reads a source code and returns *tokens* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod err;
mod lexer_tool;
mod source_scanner;
mod utf8_tape;

pub use err::{LexError, LexErrorKind};
use komi_syntax::{Token, TokenKind as Kind};
use komi_util::{Range, Scanner, Spot, char_validator};
use lexer_tool::{expect_or, expect_or_lex_identifier, lex_identifier_with_init_seg, lex_str};
use source_scanner::SourceScanner;

type Tokens = Vec<Token>;
pub type TokenRes = Result<Token, LexError>;
pub type TokensRes = Result<Tokens, LexError>;

/// Produces tokens from source codes.
struct Lexer<'a> {
    scanner: SourceScanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { scanner: SourceScanner::new(source) }
    }

    pub fn lex(&mut self) -> TokensRes {
        let tokens = self.lex_tokens()?;
        self.post_validate(&tokens)?;

        Ok(tokens)
    }

    fn lex_tokens(&mut self) -> TokensRes {
        let mut tokens: Tokens = vec![];

        // Lex characters into tokens one by one
        while let Some(char) = self.scanner.read() {
            let location = self.scanner.locate();
            self.scanner.advance();

            let token = match char {
                "(" => Token::new(Kind::LParen, location),
                ")" => Token::new(Kind::RParen, location),
                "{" => Token::new(Kind::LBrace, location),
                "}" => Token::new(Kind::RBrace, location),
                ":" => Token::new(Kind::Colon, location),
                "," => Token::new(Kind::Comma, location),
                "참" => Token::new(Kind::Bool(true), location),
                "거" => expect_or_lex_identifier(&mut self.scanner, "짓", Kind::Bool(false), char, location)?,
                "그" => expect_or_lex_identifier(&mut self.scanner, "리고", Kind::Conjunct, char, location)?,
                "또" => expect_or_lex_identifier(&mut self.scanner, "는", Kind::Disjunct, char, location)?,
                "함" => expect_or_lex_identifier(&mut self.scanner, "수", Kind::Closure, char, location)?,
                "만" => expect_or_lex_identifier(&mut self.scanner, "약", Kind::IfBranch, char, location)?,
                "아" => expect_or_lex_identifier(&mut self.scanner, "니면", Kind::ElseBranch, char, location)?,
                "반" => expect_or_lex_identifier(&mut self.scanner, "복", Kind::Iteration, char, location)?,
                "+" => expect_or(&mut self.scanner, "=", Kind::PlusEquals, Kind::Plus, location)?,
                "-" => expect_or(&mut self.scanner, "=", Kind::MinusEquals, Kind::Minus, location)?,
                "*" => expect_or(&mut self.scanner, "=", Kind::AsteriskEquals, Kind::Asterisk, location)?,
                "/" => expect_or(&mut self.scanner, "=", Kind::SlashEquals, Kind::Slash, location)?,
                "%" => expect_or(&mut self.scanner, "=", Kind::PercentEquals, Kind::Percent, location)?,
                "<" => expect_or(&mut self.scanner, "=", Kind::LBracketEquals, Kind::LBracket, location)?,
                ">" => expect_or(&mut self.scanner, "=", Kind::RBracketEquals, Kind::RBracket, location)?,
                "!" => expect_or(&mut self.scanner, "=", Kind::BangEquals, Kind::Bang, location)?,
                "=" => expect_or(&mut self.scanner, "=", Kind::DoubleEquals, Kind::Equals, location)?,
                "\"" => lex_str(&mut self.scanner, location)?,
                "#" => {
                    lexer_tool::skip_comment(&mut self.scanner);
                    continue;
                }
                s if char_validator::is_digit_char(s) => lexer_tool::lex_num(&mut self.scanner, location, char)?,
                // Lexing an identifier must come after attempting to lex a number
                s if char_validator::is_in_identifier_domain(s) => {
                    lex_identifier_with_init_seg(&mut self.scanner, String::from(s), location)?
                }
                s if char_validator::is_whitespace_char(s) => {
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

    fn post_validate(&self, tokens: &Tokens) -> Result<(), LexError> {
        if tokens.len() == 0 {
            let location = Range::new(Spot::new(0, 0), self.scanner.locate().end);
            return Err(LexError::new(LexErrorKind::NoSource, location));
        }

        Ok(())
    }
}

/// Produces tokens from source codes.
pub fn lex(source: &str) -> TokensRes {
    Lexer::new(source).lex()
}

#[cfg(test)]
mod tests {
    use super::*;
    use komi_syntax::{StrSegment, StrSegmentKind, mktoken};
    use komi_util::{Range, str_loc};
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

    /// Makes a `LexError`.
    /// The first argument is the error kind `LexErrorKind`.
    /// The second argument is the error location `Range`.
    macro_rules! mkerr {
        ($kind:ident, $range:expr) => {
            LexError::new(LexErrorKind::$kind, $range)
        };
    }

    // Should fail to lex empty sources.
    #[rstest]
    #[case::empty(
        "",
        mkerr!(NoSource, str_loc!("", ""))
    )]
    #[case::whitespaces(
        "  ",
        mkerr!(NoSource, str_loc!("", "  "))
    )]
    #[case::tabs(
        "\t\t",
        mkerr!(NoSource, str_loc!("", "\t\t"))
    )]
    #[case::new_lines(
        "\n\n\r\r\r\n\r\n",
        mkerr!(NoSource, str_loc!("", "", 6))
    )]
    #[case::comment(
        "# foo",
        mkerr!(NoSource, str_loc!("", "# foo"))
    )]
    #[case::multi_line_comment(
        "# foo\r\n# bar",
        mkerr!(NoSource, str_loc!("", "# bar", 1))
    )]
    fn empty(#[case] source: &str, #[case] error: LexError) {
        assert_lex_fail!(source, error);
    }

    // Should lex number literals.
    #[rstest]
    #[case::without_decimal(
        "12",
        vec![
            mktoken!(str_loc!("", "12"),
                Kind::Number(12.0)
            )
        ]
    )]
    #[case::with_decimal(
        "12.25",
        vec![
            mktoken!(str_loc!("", "12.25"),
                Kind::Number(12.25),
            )
        ]
    )]
    fn num_literal(#[case] source: &str, #[case] expected: Tokens) {
        assert_lex!(source, expected);
    }

    // Should fail to lex illegal number literals.
    #[rstest]
    #[case::illegal_char(
        "12^",
        mkerr!(IllegalChar, str_loc!("12", "^"))
    )]
    #[case::beginning_with_dot(
        ".25",
        mkerr!(IllegalChar, str_loc!("", "."))
    )]
    #[case::ending_with_dot(
        "12.",
        mkerr!(IllegalNumLiteral, str_loc!("", "12."))
    )]
    #[case::ending_with_two_dots(
        "12..",
        mkerr!(IllegalNumLiteral, str_loc!("", "12.."))
    )]
    #[case::illegal_decimal(
        "12.+",
        mkerr!(IllegalNumLiteral, str_loc!("", "12.+"))
    )]
    fn illegal_num_literal(#[case] source: &str, #[case] error: LexError) {
        assert_lex_fail!(source, error);
    }

    // Should lex boolean literals
    #[rstest]
    #[case::the_true(
        "참",
        vec![
            mktoken!(str_loc!("", "참"),
                Kind::Bool(true),
            )
        ]
    )]
    #[case::the_false(
        "거짓",
        vec![
            mktoken!(str_loc!("", "거짓"),
                Kind::Bool(false)
            )
        ]
    )]
    fn bool_literal(#[case] source: &str, #[case] expected: Tokens) {
        assert_lex!(source, expected);
    }

    // Should lex string literals.
    #[rstest]
    #[case::empty(
        "\"\"",
        vec![
            mktoken!(str_loc!("", "\"\""),
                Kind::Str(vec![]),
            )
        ]
    )]
    #[case::str(
        "\"사과\"",
        vec![
            mktoken!(str_loc!("", "\"사과\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("사과"), str_loc!("\"", "사과"))
            ])),
        ]
    )]
    #[case::str_with_otherwise_illegal_chars(
        "\"!@# \"",
        vec![
            mktoken!(str_loc!("", "\"!@# \""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("!@# "), str_loc!("\"", "!@# "))
            ])),
        ]
    )]
    #[case::str_with_new_line(
        "\"\r\n\"",
        vec![
            mktoken!(str_loc!("", "\"", 1), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("\r\n"), str_loc!("\"", "", 1))
            ])),
        ]
    )]
    #[case::str_with_new_lines(
        "\"\r\n\r\n\r\n\"",
        vec![
            mktoken!(str_loc!("", "\"", 3), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("\r\n\r\n\r\n"), str_loc!("\"", "", 3))
            ])),
        ]
    )]
    #[case::lbrace_escape(
        "\"{{\"",
        vec![
            mktoken!(str_loc!("", "\"{{\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("{"), str_loc!("\"", "{{"))
            ])),
        ]
    )]
    #[case::rbrace_escape(
        "\"}}\"",
        vec![
            mktoken!(str_loc!("", "\"}}\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("}"), str_loc!("\"", "}}"))
            ])),
        ]
    )]
    #[case::two_lbrace_escape(
        "\"{{{{\"",
        vec![
            mktoken!(str_loc!("", "\"{{{{\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("{{"), str_loc!("\"", "{{{{"))
            ])),
        ]
    )]
    #[case::two_rbrace_escapes(
        "\"}}}}\"",
        vec![
            mktoken!(str_loc!("", "\"}}}}\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("}}"), str_loc!("\"", "}}}}"))
            ])),
        ]
    )]
    #[case::lbrace_and_rbrace_escapes(
        "\"{{}}\"",
        vec![
            mktoken!(str_loc!("", "\"{{}}\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("{}"), str_loc!("\"", "{{}}"))
            ])),
        ]
    )]
    #[case::rbrace_and_lbrace_escapes(
        "\"}}{{\"",
        vec![
            mktoken!(str_loc!("", "\"}}{{\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("}{"), str_loc!("\"", "}}{{"))
            ])),
        ]
    )]
    #[case::id(
        "\"{사과}\"",
        vec![
            mktoken!(str_loc!("", "\"{사과}\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::identifier("사과"), str_loc!("\"{", "사과"))
            ])),
        ]
    )]
    #[case::str_and_id(
        "\"사과{오렌지}\"",
        vec![
            mktoken!(str_loc!("", "\"사과{오렌지}\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("사과"), str_loc!("\"", "사과")),
                StrSegment::new(StrSegmentKind::identifier("오렌지"), str_loc!("\"사과{", "오렌지"))
            ])),
        ]
    )]
    #[case::id_and_str(
        "\"{사과}오렌지\"",
        vec![
            mktoken!(str_loc!("", "\"{사과}오렌지\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::identifier("사과"), str_loc!("\"{", "사과")),
                StrSegment::new(StrSegmentKind::str("오렌지"), str_loc!("\"{사과}", "오렌지"))
            ])),
        ]
    )]
    #[case::id_and_id(
        "\"{사과}{오렌지}\"",
        vec![
            mktoken!(str_loc!("", "\"{사과}{오렌지}\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::identifier("사과"), str_loc!("\"{", "사과")),
                StrSegment::new(StrSegmentKind::identifier("오렌지"), str_loc!("\"{사과}{", "오렌지"))
            ])),
        ]
    )]
    #[case::str_id_str(
        "\"사과{오렌지}바나나\"",
        vec![
            mktoken!(str_loc!("", "\"사과{오렌지}바나나\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("사과"), str_loc!("\"", "사과")),
                StrSegment::new(StrSegmentKind::identifier("오렌지"), str_loc!("\"사과{", "오렌지")),
                StrSegment::new(StrSegmentKind::str("바나나"), str_loc!("\"사과{오렌지}", "바나나")),
            ])),
        ]
    )]
    #[case::id_str_id(
        "\"{사과}오렌지{바나나}\"",
        vec![
            mktoken!(str_loc!("", "\"{사과}오렌지{바나나}\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::identifier("사과"), str_loc!("\"{", "사과")),
                StrSegment::new(StrSegmentKind::str("오렌지"), str_loc!("\"{사과}", "오렌지")),
                StrSegment::new(StrSegmentKind::identifier("바나나"), str_loc!("\"{사과}오렌지{", "바나나")),
            ])),
        ]
    )]
    #[case::id_id_id(
        "\"{사과}{오렌지}{바나나}\"",
        vec![
            mktoken!(str_loc!("", "\"{사과}{오렌지}{바나나}\""), Kind::Str(vec![
                StrSegment::new(StrSegmentKind::identifier("사과"), str_loc!("\"{", "사과")),
                StrSegment::new(StrSegmentKind::identifier("오렌지"), str_loc!("\"{사과}{", "오렌지")),
                StrSegment::new(StrSegmentKind::identifier("바나나"), str_loc!("\"{사과}{오렌지}{", "바나나")),
            ])),
        ]
    )]
    fn string_segment(#[case] source: &str, #[case] expected: Tokens) {
        assert_lex!(source, expected);
    }

    // Should fail to lex illegal str literals.
    #[rstest]
    #[case::lbrace_not_closed_with_immediate_end(
        "\"{",
        mkerr!(InterpolationNotClosed, str_loc!("\"", "{")))
    ]
    #[case::lbrace_not_closed_with_not_immediate_end(
        "\"{사과",
        mkerr!(InterpolationNotClosed, str_loc!("\"", "{사과")))
    ]
    #[case::rbrace_in_str(
        "\"}",
        mkerr!(IllegalRBraceInStr, str_loc!("\"", "}")))
    ]
    #[case::empty_identifier(
        "\"{}\"",
        mkerr!(NoInterpolatedIdentifier, str_loc!("\"", "{}")))
    ]
    #[case::illegal_identifier_char_at_first(
        "\"{+}\"",
        mkerr!(IllegalInterpolationChar, str_loc!("\"{", "+")))
    ]
    #[case::illegal_identifier_char_in_middle(
        "\"{사+}\"",
        mkerr!(IllegalInterpolationChar, str_loc!("\"{사", "+")))
    ]
    #[case::lbrace_not_closed_with_some_chars(
        "\"{사과",
        mkerr!(InterpolationNotClosed, str_loc!("\"", "{사과")))
    ]
    fn illegal_string_segment(#[case] source: &str, #[case] error: LexError) {
        assert_lex_fail!(source, error);
    }

    // Should lex non-value literals.
    #[rstest]
    #[case::plus(
        "+",
        vec![
            mktoken!(str_loc!("", "+"),
                Kind::Plus,
            )
        ]
    )]
    #[case::minus(
        "-",
        vec![
            mktoken!(str_loc!("", "-"),
                Kind::Minus,
            )
        ]
    )]
    #[case::asterisk(
        "*",
        vec![
            mktoken!(str_loc!("", "*"),
                Kind::Asterisk,
            )
        ]
    )]
    #[case::slash(
        "/",
        vec![
            mktoken!(str_loc!("", "/"),
                Kind::Slash
            )
        ]
    )]
    #[case::percent(
        "%",
        vec![
            mktoken!(str_loc!("", "%"),
                Kind::Percent,
            )
        ]
    )]
    #[case::lparen(
        "(",
        vec![
            mktoken!(str_loc!("", "("),
                Kind::LParen,
            )
        ]
    )]
    #[case::rparen(
        ")",
        vec![
            mktoken!(str_loc!("", ")"),
                Kind::RParen,
            )
        ]
    )]
    #[case::lbrace(
        "{",
        vec![
            mktoken!(str_loc!("", "{"),
                Kind::LBrace,
            )
        ]
    )]
    #[case::lbrace(
        "}",
        vec![
            mktoken!(str_loc!("", "}"),
                Kind::RBrace,
            )
        ]
    )]
    #[case::lbracket(
        "<",
        vec![
            mktoken!(str_loc!("", "<"),
                Kind::LBracket,
            )
        ]
    )]
    #[case::rbracket(
        ">",
        vec![
            mktoken!(str_loc!("", ">"),
                Kind::RBracket,
            )
        ]
    )]
    #[case::colon(
        ":",
        vec![
            mktoken!(str_loc!("", ":"),
                Kind::Colon,
            )
        ]
    )]
    #[case::comma(
        ",",
        vec![
            mktoken!(str_loc!("", ","),
                Kind::Comma,
            )
        ]
    )]
    #[case::bang(
        "!",
        vec![
            mktoken!(str_loc!("", "!"),
                Kind::Bang,
            )
        ]
    )]
    #[case::equals(
        "=",
        vec![
            mktoken!(str_loc!("", "="),
                Kind::Equals
            )
        ]
    )]
    #[case::plus_equals(
        "+=",
        vec![
            mktoken!(str_loc!("", "+="),
                Kind::PlusEquals
            )
        ]
    )]
    #[case::minus_equals(
        "-=",
        vec![
            mktoken!(str_loc!("", "-="),
                Kind::MinusEquals,
            )
        ]
    )]
    #[case::asterisk_equals(
        "*=",
        vec![
            mktoken!(str_loc!("", "*="),
                Kind::AsteriskEquals,
            )
        ]
    )]
    #[case::slash_equals(
        "/=",
        vec![
            mktoken!(str_loc!("", "/="),
                Kind::SlashEquals,
            )
        ]
    )]
    #[case::percent_equals(
        "%=",
        vec![
            mktoken!(str_loc!("", "%="),
                Kind::PercentEquals,
            )
        ]
    )]
    #[case::double_equals(
        "==",
        vec![
            mktoken!(str_loc!("", "=="),
                Kind::DoubleEquals,
            )
        ]
    )]
    #[case::bang_equals(
        "!=",
        vec![
            mktoken!(str_loc!("", "!="),
                Kind::BangEquals,
            )
        ]
    )]
    #[case::lbracket_equals(
        "<=",
        vec![
            mktoken!(str_loc!("", "<="),
                Kind::LBracketEquals,
            )
        ]
    )]
    #[case::rbracket_equals(
        ">=",
        vec![
            mktoken!(str_loc!("", ">="),
                Kind::RBracketEquals,
            )
        ]
    )]
    #[case::conjunct(
        "그리고",
        vec![
            mktoken!(str_loc!("", "그리고"),
                Kind::Conjunct,
            )
        ]
    )]
    #[case::disjunct(
        "또는",
        vec![
            mktoken!(str_loc!("", "또는"),
                Kind::Disjunct,
            )
        ]
    )]
    #[case::closure(
        "함수",
        vec![
            mktoken!(str_loc!("", "함수"),
                Kind::Closure,
            )
        ]
    )]
    #[case::if_branch(
        "만약",
        vec![
            mktoken!(str_loc!("", "만약"),
                Kind::IfBranch,
            )
        ]
    )]
    #[case::else_branch(
        "아니면",
        vec![
            mktoken!(str_loc!("", "아니면"),
                Kind::ElseBranch,
            )
        ]
    )]
    #[case::iteration(
        "반복",
        vec![
            mktoken!(str_loc!("", "반복"),
                Kind::Iteration,
            )
        ]
    )]
    fn non_value_literal(#[case] source: &str, #[case] expected: Tokens) {
        assert_lex!(source, expected);
    }

    // Should lex identifiers.
    #[rstest]
    #[case::single_alphabat_char(
        "a",
        vec![
            mktoken!(str_loc!("", "a"),
                Kind::Identifier(String::from("a")),
            )
        ]
    )]
    #[case::single_hangul_char(
        "가",
        vec![
            mktoken!(str_loc!("", "가"),
                Kind::Identifier(String::from("가")),
            )
        ]
    )]
    #[case::mixed_multiple_chars(
        "a가a가",
        vec![
            mktoken!(str_loc!("", "a가a가"),
                Kind::Identifier(String::from("a가a가")),
            )
        ]
    )]
    #[case::first_char_false_but_end(
        "거",
        vec![
            mktoken!(str_loc!("", "거"),
                Kind::Identifier(String::from("거")),
            )
        ]
    )]
    #[case::first_char_false_but_second_non_id(
        "거#",
        vec![
            mktoken!(str_loc!("", "거"),
                Kind::Identifier(String::from("거")),
            )
        ]
    )]
    #[case::first_char_false_but_second_other_id(
        "거a",
        vec![
            mktoken!(str_loc!("", "거a"),
                Kind::Identifier(String::from("거a")),
            )
        ]
    )]
    #[case::first_two_chars_false_but_third_other_id(
        "거짓a",
        vec![
            mktoken!(str_loc!("", "거짓a"),
                Kind::Identifier(String::from("거짓a")),
            )
        ]
    )]
    #[case::first_char_conjunct_but_end(
        "그",
        vec![
            mktoken!(str_loc!("", "그"),
                Kind::Identifier(String::from("그")),
            )
        ]
    )]
    #[case::first_char_conjunct_but_second_non_id(
        "그#",
        vec![
            mktoken!(str_loc!("", "그"),
                Kind::Identifier(String::from("그")),
            )
        ]
    )]
    #[case::first_char_conjunct_but_second_other_id(
        "그a",
        vec![
            mktoken!(str_loc!("", "그a"),
                Kind::Identifier(String::from("그a")),
            )
        ]
    )]
    #[case::first_two_chars_conjunct_but_end(
        "그리",
        vec![
            mktoken!(str_loc!("", "그리"),
                Kind::Identifier(String::from("그리")),
            )
        ]
    )]
    #[case::first_two_chars_conjunct_but_third_non_id(
        "그리#",
        vec![
            mktoken!(str_loc!("", "그리"),
                Kind::Identifier(String::from("그리")),
            )
        ]
    )]
    #[case::first_two_chars_conjunct_but_third_other_id(
        "그리a",
        vec![
            mktoken!(str_loc!("", "그리a"),
                Kind::Identifier(String::from("그리a")),
            )
        ]
    )]
    #[case::first_three_chars_conjunct_but_fourth_other_id(
        "그리고a",
        vec![
            mktoken!(str_loc!("", "그리고a"),
                Kind::Identifier(String::from("그리고a")),
            )
        ]
    )]
    #[case::first_char_disjunct_but_end(
        "또",
        vec![
            mktoken!(str_loc!("", "또"),
                Kind::Identifier(String::from("또")),
            )
        ]
    )]
    #[case::first_char_disjunct_but_second_non_id(
        "또#",
        vec![
            mktoken!(str_loc!("", "또"),
                Kind::Identifier(String::from("또")),
            )
        ]
    )]
    #[case::first_char_disjunct_but_second_other_id(
        "또a",
        vec![
            mktoken!(str_loc!("", "또a"),
                Kind::Identifier(String::from("또a")),
            )
        ]
    )]
    #[case::first_two_chars_disjunct_but_third_other_id(
        "또는a",
        vec![
            mktoken!(str_loc!("", "또는a"),
                Kind::Identifier(String::from("또는a")),
            )
        ]
    )]
    #[case::first_char_if_branch_but_end(
        "만",
        vec![
            mktoken!(str_loc!("", "만"),
                Kind::Identifier(String::from("만")),
            )
        ]
    )]
    #[case::first_char_if_branch_but_second_non_id(
        "만#",
        vec![
            mktoken!(str_loc!("", "만"),
                Kind::Identifier(String::from("만")),
            )
        ]
    )]
    #[case::first_char_if_branch_but_second_other_id(
        "만a",
        vec![
            mktoken!(str_loc!("", "만a"),
                Kind::Identifier(String::from("만a")),
            )
        ]
    )]
    #[case::first_two_chars_if_branch_but_third_other_id(
        "만약a",
        vec![
            mktoken!(str_loc!("", "만약a"),
                Kind::Identifier(String::from("만약a")),
            )
        ]
    )]
    #[case::first_char_else_branch_but_end(
        "아",
        vec![
            mktoken!(str_loc!("", "아"),
                Kind::Identifier(String::from("아")),
            )
        ]
    )]
    #[case::first_char_else_branch_but_second_non_id(
        "아#",
        vec![
            mktoken!(str_loc!("", "아"),
                Kind::Identifier(String::from("아")),
            )
        ]
    )]
    #[case::first_char_else_branch_but_second_other_id(
        "아a",
        vec![
            mktoken!(str_loc!("", "아a"),
                Kind::Identifier(String::from("아a")),
            )
        ]
    )]
    #[case::first_two_chars_else_branch_but_end(
        "아니",
        vec![
            mktoken!(str_loc!("", "아니"),
                Kind::Identifier(String::from("아니")),
            )
        ]
    )]
    #[case::first_two_chars_else_branch_but_third_non_id(
        "아니#",
        vec![
            mktoken!(str_loc!("", "아니"),
                Kind::Identifier(String::from("아니")),
            )
        ]
    )]
    #[case::first_two_chars_else_branch_but_third_other_id(
        "아니a",
        vec![
            mktoken!(str_loc!("", "아니a"),
                Kind::Identifier(String::from("아니a")),
            )
        ]
    )]
    #[case::first_three_chars_else_branch_but_fourth_other_id(
        "아니면a",
        vec![
            mktoken!(str_loc!("", "아니면a"),
                Kind::Identifier(String::from("아니면a")),
            )
        ]
    )]
    #[case::first_char_iteration_but_end(
        "반",
        vec![
            mktoken!(str_loc!("", "반"),
                Kind::Identifier(String::from("반")),
            )
        ]
    )]
    #[case::first_char_iteration_but_second_non_id(
        "반#",
        vec![
            mktoken!(str_loc!("", "반"),
                Kind::Identifier(String::from("반")),
            )
        ]
    )]
    #[case::first_char_iteration_but_second_other_id(
        "반a",
        vec![
            mktoken!(str_loc!("", "반a"),
                Kind::Identifier(String::from("반a")),
            )
        ]
    )]
    #[case::first_two_chars_iteration_but_third_other_id(
        "반복a",
        vec![
            mktoken!(str_loc!("", "반복a"),
                Kind::Identifier(String::from("반복a")),
            )
        ]
    )]
    fn single_identifier(#[case] source: &str, #[case] expected: Tokens) {
        assert_lex!(source, expected);
    }

    // Should lex two same literals.
    // Should not fail in all below cases, since the lexer does not know the syntax.
    // To test when tokens cannot be early determined by the first character.
    #[rstest]
    #[case::two_pluses(
        "++",
        vec![
            mktoken!(str_loc!("", "+"),
                Kind::Plus,
            ),
            mktoken!(str_loc!("+", "+"),
                Kind::Plus,
            ),
        ]
    )]
    #[case::two_minuses(
        "--",
        vec![
            mktoken!(str_loc!("", "-"),
                Kind::Minus,
            ),
            mktoken!(str_loc!("-", "-"),
                Kind::Minus,
            ),
        ]
    )]
    #[case::two_asterisks(
        "**",
        vec![
            mktoken!(str_loc!("", "*"),
                Kind::Asterisk,
            ),
            mktoken!(str_loc!("*", "*"),
                Kind::Asterisk,
            ),
        ]
    )]
    #[case::two_slashes(
        "//",
        vec![
            mktoken!(str_loc!("", "/"),
                Kind::Slash,
            ),
            mktoken!(str_loc!("/", "/"),
                Kind::Slash,
            ),
        ]
    )]
    #[case::two_slashes(
        "%%",
        vec![
            mktoken!(str_loc!("", "%"),
                Kind::Percent,
            ),
            mktoken!(str_loc!("%", "%"),
                Kind::Percent,
            ),
        ]
    )]
    #[case::two_lbrackets(
        "<<",
        vec![
            mktoken!(str_loc!("", "<"),
                Kind::LBracket,
            ),
            mktoken!(str_loc!("<", "<"),
                Kind::LBracket,
            ),
        ]
    )]
    #[case::two_rbrackets(
        ">>",
        vec![
            mktoken!(str_loc!("", ">"),
                Kind::RBracket,
            ),
            mktoken!(str_loc!(">", ">"),
                Kind::RBracket,
            ),
        ]
    )]
    #[case::two_bangs(
        "!!",
        vec![
            mktoken!(str_loc!("", "!"),
                Kind::Bang,
            ),
            mktoken!(str_loc!("!", "!"),
                Kind::Bang,
            ),
        ]
    )]
    #[case::double_equals_and_equals(
        "===",
        vec![
            mktoken!(str_loc!("", "=="),
                Kind::DoubleEquals,
            ),
            mktoken!(str_loc!("==", "="),
                Kind::Equals,
            ),
        ]
    )]
    #[case::false_false(
        "거짓 거짓",
        vec![
            mktoken!(str_loc!("", "거짓"),
                Kind::Bool(false),
            ),
            mktoken!(str_loc!("거짓 ", "거짓"),
                Kind::Bool(false),
            ),
        ]
    )]
    #[case::conjunct_conjunct(
        "그리고 그리고",
        vec![
            mktoken!(str_loc!("", "그리고"),
                Kind::Conjunct,
            ),
            mktoken!(str_loc!("그리고 ", "그리고"),
                Kind::Conjunct,
            ),
        ]
    )]
    #[case::disjunct_disjunct(
        "또는 또는",
        vec![
            mktoken!(str_loc!("", "또는"),
                Kind::Disjunct,
            ),
            mktoken!(str_loc!("또는 ", "또는"),
                Kind::Disjunct,
            ),
        ]
    )]
    #[case::closure_closure(
        "함수 함수",
        vec![
            mktoken!(str_loc!("", "함수"),
                Kind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "함수"),
                Kind::Closure,
            ),
        ]
    )]
    #[case::if_branch_if_branch(
        "만약 만약",
        vec![
            mktoken!(str_loc!("", "만약"),
                Kind::IfBranch,
            ),
            mktoken!(str_loc!("만약 ", "만약"),
                Kind::IfBranch,
            ),
        ]
    )]
    #[case::else_branch_else_branch(
        "아니면 아니면",
        vec![
            mktoken!(str_loc!("", "아니면"),
                Kind::ElseBranch,
            ),
            mktoken!(str_loc!("아니면 ", "아니면"),
                Kind::ElseBranch,
            ),
        ]
    )]
    #[case::iteration_iteration(
        "반복 반복",
        vec![
            mktoken!(str_loc!("", "반복"),
                Kind::Iteration,
            ),
            mktoken!(str_loc!("반복 ", "반복"),
                Kind::Iteration,
            ),
        ]
    )]
    fn two_same_tokens(#[case] source: &str, #[case] expected: Tokens) {
        assert_lex!(source, expected);
    }

    // Should lex an identifier and a similar keyword.
    // To test locations of tokens, when the first identifier is lexed from the same lexing function with the second token.
    #[rstest]
    #[case::id1_false(
        "거 거짓",
        vec![
            mktoken!(str_loc!("", "거"),
                Kind::Identifier(String::from("거")),
            ),
            mktoken!(str_loc!("거 ", "거짓"),
                Kind::Bool(false),
            ),
        ]
    )]
    #[case::id2_false(
        "거a 거짓",
        vec![
            mktoken!(str_loc!("", "거a"),
                Kind::Identifier(String::from("거a")),
            ),
            mktoken!(str_loc!("거a ", "거짓"),
                Kind::Bool(false),
            ),
        ]
    )]
    #[case::id3_false(
        "거짓a 거짓",
        vec![
            mktoken!(str_loc!("", "거짓a"),
                Kind::Identifier(String::from("거짓a")),
            ),
            mktoken!(str_loc!("거짓a ", "거짓"),
                Kind::Bool(false),
            ),
        ]
    )]
    #[case::id1_conjuct(
        "그 그리고",
        vec![
            mktoken!(str_loc!("", "그"),
                Kind::Identifier(String::from("그")),
            ),
            mktoken!(str_loc!("그 ", "그리고"),
                Kind::Conjunct,
            ),
        ]
    )]
    #[case::id2_conjunct(
        "그a 그리고",
        vec![
            mktoken!(str_loc!("", "그a"),
                Kind::Identifier(String::from("그a")),
            ),
            mktoken!(str_loc!("그a ", "그리고"),
                Kind::Conjunct,
            ),
        ]
    )]
    #[case::id3_conjunct(
        "그리 그리고",
        vec![
            mktoken!(str_loc!("", "그리"),
                Kind::Identifier(String::from("그리")),
            ),
            mktoken!(str_loc!("그리 ", "그리고"),
                Kind::Conjunct,
            ),
        ]
    )]
    #[case::id4_conjunct(
        "그리a 그리고",
        vec![
            mktoken!(str_loc!("", "그리a"),
                Kind::Identifier(String::from("그리a")),
            ),
            mktoken!(str_loc!("그리a ", "그리고"),
                Kind::Conjunct,
            ),
        ]
    )]
    #[case::id4_conjunct(
        "그리고a 그리고",
        vec![
            mktoken!(str_loc!("", "그리고a"),
                Kind::Identifier(String::from("그리고a")),
            ),
            mktoken!(str_loc!("그리고a ", "그리고"),
                Kind::Conjunct,
            ),
        ]
    )]
    #[case::id1_disjunct(
        "또 또는",
        vec![
            mktoken!(str_loc!("", "또"),
                Kind::Identifier(String::from("또")),
            ),
            mktoken!(str_loc!("또 ", "또는"),
                Kind::Disjunct,
            ),
        ]
    )]
    #[case::id2_disjunct(
        "또a 또는",
        vec![
            mktoken!(str_loc!("", "또a"),
                Kind::Identifier(String::from("또a")),
            ),
            mktoken!(str_loc!("또a ", "또는"),
                Kind::Disjunct,
            ),
        ]
    )]
    #[case::id3_disjunct(
        "또는a 또는",
        vec![
            mktoken!(str_loc!("", "또는a"),
                Kind::Identifier(String::from("또는a")),
            ),
            mktoken!(str_loc!("또는a ", "또는"),
                Kind::Disjunct,
            ),
        ]
    )]
    #[case::id1_closure(
        "함 함수",
        vec![
            mktoken!(str_loc!("", "함"),
                Kind::Identifier(String::from("함")),
            ),
            mktoken!(str_loc!("함 ", "함수"),
                Kind::Closure,
            ),
        ]
    )]
    #[case::id2_closure(
        "함a 함수",
        vec![
            mktoken!(str_loc!("", "함a"),
                Kind::Identifier(String::from("함a")),
            ),
            mktoken!(str_loc!("함a ", "함수"),
                Kind::Closure,
            ),
        ]
    )]
    #[case::id3_closure(
        "함수a 함수",
        vec![
            mktoken!(str_loc!("", "함수a"),
                Kind::Identifier(String::from("함수a")),
            ),
            mktoken!(str_loc!("함수a ", "함수"),
                Kind::Closure,
            ),
        ]
    )]
    #[case::id1_if_branch(
        "만 만약",
        vec![
            mktoken!(str_loc!("", "만"),
                Kind::Identifier(String::from("만")),
            ),
            mktoken!(str_loc!("만 ", "만약"),
                Kind::IfBranch,
            ),
        ]
    )]
    #[case::id2_if_branch(
        "만a 만약",
        vec![
            mktoken!(str_loc!("", "만a"),
                Kind::Identifier(String::from("만a")),
            ),
            mktoken!(str_loc!("만a ", "만약"),
                Kind::IfBranch,
            ),
        ]
    )]
    #[case::id3_if_branch(
        "만약a 만약",
        vec![
            mktoken!(str_loc!("", "만약a"),
                Kind::Identifier(String::from("만약a")),
            ),
            mktoken!(str_loc!("만약a ", "만약"),
                Kind::IfBranch,
            ),
        ]
    )]
    #[case::id1_else_branch(
        "아 아니면",
        vec![
            mktoken!(str_loc!("", "아"),
                Kind::Identifier(String::from("아")),
            ),
            mktoken!(str_loc!("아 ", "아니면"),
                Kind::ElseBranch,
            ),
        ]
    )]
    #[case::id2_else_branch(
        "아a 아니면",
        vec![
            mktoken!(str_loc!("", "아a"),
                Kind::Identifier(String::from("아a")),
            ),
            mktoken!(str_loc!("아a ", "아니면"),
                Kind::ElseBranch,
            ),
        ]
    )]
    #[case::id3_else_branch(
        "아니 아니면",
        vec![
            mktoken!(str_loc!("", "아니"),
                Kind::Identifier(String::from("아니")),
            ),
            mktoken!(str_loc!("아니 ", "아니면"),
                Kind::ElseBranch,
            ),
        ]
    )]
    #[case::id4_else_branch(
        "아니a 아니면",
        vec![
            mktoken!(str_loc!("", "아니a"),
                Kind::Identifier(String::from("아니a")),
            ),
            mktoken!(str_loc!("아니a ", "아니면"),
                Kind::ElseBranch,
            ),
        ]
    )]
    #[case::id5_else_branch(
        "아니면a 아니면",
        vec![
            mktoken!(str_loc!("", "아니면a"),
                Kind::Identifier(String::from("아니면a")),
            ),
            mktoken!(str_loc!("아니면a ", "아니면"),
                Kind::ElseBranch,
            ),
        ]
    )]
    #[case::id1_iteration(
        "반 반복",
        vec![
            mktoken!(str_loc!("", "반"),
                Kind::Identifier(String::from("반")),
            ),
            mktoken!(str_loc!("반 ", "반복"),
                Kind::Iteration,
            ),
        ]
    )]
    #[case::id2_iteration(
        "반a 반복",
        vec![
            mktoken!(str_loc!("", "반a"),
                Kind::Identifier(String::from("반a")),
            ),
            mktoken!(str_loc!("반a ", "반복"),
                Kind::Iteration,
            ),
        ]
    )]
    #[case::id3_iteration(
        "반복a 반복",
        vec![
            mktoken!(str_loc!("", "반복a"),
                Kind::Identifier(String::from("반복a")),
            ),
            mktoken!(str_loc!("반복a ", "반복"),
                Kind::Iteration,
            )
        ]
    )]
    fn two_similar_tokens(#[case] source: &str, #[case] expected: Tokens) {
        assert_lex!(source, expected);
    }

    // Should lex sequences of characters, that may appear in source codes.
    #[rstest]
    #[case::addition_expression(
        "12 + 34.675",
        vec![
            mktoken!(str_loc!("", "12"),
                Kind::Number(12.0),
            ),
            mktoken!(str_loc!("12 ", "+"),
                Kind::Plus,
            ),
            mktoken!(str_loc!("12 + ", "34.675"),
                Kind::Number(34.675),
            ),
        ]
    )]
    #[case::conjunction_expression(
        "참 그리고 거짓",
        vec![
            mktoken!(str_loc!("", "참"),
                Kind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "그리고"),
                Kind::Conjunct,
            ),
            mktoken!(str_loc!("참 그리고 ", "거짓"),
                Kind::Bool(false),
            ),
        ]
    )]
    #[case::closure_expression_with_no_parameters(
        "함수 { 1 }",
        vec![
            mktoken!(str_loc!("", "함수"),
                Kind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "{"),
                Kind::LBrace,
            ),
            mktoken!(str_loc!("함수 { ", "1"),
                Kind::Number(1.0),
            ),
            mktoken!(str_loc!("함수 { 1 ", "}"),
                Kind::RBrace,
            ),
        ]
    )]
    #[case::closure_expression_with_parameters(
        "함수 사과, 바나나 { 1 }",
        vec![
            mktoken!(str_loc!("", "함수"),
                Kind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "사과"),
                Kind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("함수 사과", ","),
                Kind::Comma,
            ),
            mktoken!(str_loc!("함수 사과, ", "바나나"),
                Kind::Identifier(String::from("바나나")),
            ),
            mktoken!(str_loc!("함수 사과, 바나나 ", "{"),
                Kind::LBrace,
            ),
            mktoken!(str_loc!("함수 사과, 바나나 { ", "1"),
                Kind::Number(1.0),
            ),
            mktoken!(str_loc!("함수 사과, 바나나 { 1 ", "}"),
                Kind::RBrace,
            ),
        ]
    )]
    fn multiple_tokens(#[case] source: &str, #[case] expected: Tokens) {
        assert_lex!(source, expected);
    }

    // Should fail to lex illegal characters.
    #[rstest]
    #[case::caret("^", mkerr!(IllegalChar, str_loc!("", "^")))]
    #[case::dollar("$", mkerr!(IllegalChar, str_loc!("", "$")))]
    fn illegal_char(#[case] source: &str, #[case] error: LexError) {
        assert_lex_fail!(source, error);
    }
}
