use crate::parse;
use komi_syntax::ast::{Ast, AstKind};
use komi_syntax::error::{ParseError, ParseErrorKind};
use komi_syntax::token::{Token, TokenKind};
use komi_syntax::{mkast, mktoken};
use komi_util::location::Range;
use komi_util::str_loc;
use rstest::rstest;

/// Success cases: simple infix expressions.
#[rstest]
#[case::dot(
    // Represents `a . b`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "."),
            TokenKind::Dot,
        ),
        mktoken!(str_loc!("a . ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a . b"), vec![
        mkast!(infix InfixDot, loc str_loc!("", "a . b"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(identifier "b", loc str_loc!("a . ", "b")),
        ),
    ])
)]
fn success(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Success cases: left associativity.
#[rstest]
#[case::two_dots(
    // Represents `a . b . c`, and expects to be parsed into `(a . b) . c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "."),
            TokenKind::Dot,
        ),
        mktoken!(str_loc!("a . ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a . b ", "."),
            TokenKind::Dot,
        ),
        mktoken!(str_loc!("a . b . ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a . b . c"), vec![
        mkast!(infix InfixDot, loc str_loc!("", "a . b . c"),
            left mkast!(infix InfixDot, loc str_loc!("", "a . b"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(identifier "b", loc str_loc!("a . ", "b")),
            ),
            right mkast!(identifier "c", loc str_loc!("a . b . ", "c")),
        ),
    ])
)]
fn left_assoc(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Success cases: priority.
#[rstest]
#[case::dot_prioritized_over_call(
    // Represents `a . b()`, and expects to be parsed into `(a . b)()`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "."),
            TokenKind::Dot,
        ),
        mktoken!(str_loc!("a . ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a . b", "("),
            TokenKind::LParen,
        ),
        mktoken!(str_loc!("a . b(", ")"),
            TokenKind::RParen,
        ),
    ],
    mkast!(prog loc str_loc!("", "a . b()"), vec![
        mkast!(call loc str_loc!("", "a . b()"),
            target mkast!(infix InfixDot, loc str_loc!("", "a . b"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(identifier "b", loc str_loc!("a . ", "b")),
            ),
            args vec![],
        ),
    ])
)]
fn priority(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Failure cases: an infix without the left operand.
#[rstest]
#[case::dot(
    // Represents `. a`.
    vec![
        mktoken!(str_loc!("", "."),
            TokenKind::Dot,
        ),
        mktoken!(str_loc!(". ", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", ".")),
)]
fn fail_without_left(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}

/// Failure cases: an infix without the right operand.
#[rstest]
#[case::dot(
    // Represents `a .`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "."),
            TokenKind::Dot,
        ),
    ],
    mkerr!(NoInfixRightOperand, str_loc!("", "a +")),
)]
fn fail_without_right(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}

/// Failure cases: a single infix, with no operands.
#[rstest]
#[case::plus(
    // Represents `.`.
    vec![
        mktoken!(str_loc!("", "."),
            TokenKind::Dot,
        )
    ],
    mkerr!(InvalidExprStart, str_loc!("", "."))
)]
fn fail_single(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}
