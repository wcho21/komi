use crate::{assert_parse, assert_parse_fail, mkerr, parse};
use komi_syntax::ast::{Ast, AstKind};
use komi_syntax::error::{ParseError, ParseErrorKind};
use komi_syntax::token::{Token, TokenKind};
use komi_syntax::{mkast, mktoken};
use komi_util::location::Range;
use komi_util::str_loc;
use rstest::rstest;

mod ok {
    use super::*;

    #[rstest]
    #[case::id_with_no_args(
        // Represents `사과()`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "사과()"), vec![
            mkast!(call loc str_loc!("", "사과()"),
                target mkast!(identifier "사과", loc str_loc!("", "사과")),
                args vec![],
            ),
        ])
    )]
    #[case::id_with_single_arg(
        // Represents `사과(1)`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("사과(1", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "사과(1)"), vec![
            mkast!(call loc str_loc!("", "사과(1)"),
                target mkast!(identifier "사과", loc str_loc!("", "사과")),
                args vec![
                    mkast!(num 1.0, loc str_loc!("사과(", "1")),
                ],
            ),
        ])
    )]
    #[case::id_with_multiple_args(
        // Represents `사과(1, 2, 3)`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("사과(1 ", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(1, ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("사과(1, 2", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(1, 2, ", "3"),
                TokenKind::Number(3.0),
            ),
            mktoken!(str_loc!("사과(1, 2, 3", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "사과(1, 2, 3)"), vec![
            mkast!(call loc str_loc!("", "사과(1, 2, 3)"),
                target mkast!(identifier "사과", loc str_loc!("", "사과")),
                args vec![
                    mkast!(num 1.0, loc str_loc!("사과(", "1")),
                    mkast!(num 2.0, loc str_loc!("사과(1, ", "2")),
                    mkast!(num 3.0, loc str_loc!("사과(1, 2, ", "3")),
                ],
            ),
        ])
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}

mod incomplete {
    use super::*;

    #[rstest]
    #[case::rparen_and_end(
        // Represents `사과(`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
        ],
        mkerr!(NoClosingParenInCallArgs, str_loc!("", "사과("))
    )]
    #[case::args_and_no_rparen(
        // Represents `사과(1, 2`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("사과(1", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(1, ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkerr!(NoClosingParenInCallArgs, str_loc!("", "사과(1, 2"))
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }
}

mod invalid_arguments {
    use super::*;

    #[rstest]
    #[case::comma_after_lparen(
        // Represents `사과(,)`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(,", ")"),
                TokenKind::RParen,
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("사과(", ","))
    )]
    #[case::two_comma_after_arg(
        // Represents `사과(1,,)`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("사과(1", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(1,", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(1,,", ")"),
                TokenKind::RParen,
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("사과(1,", ","))
    )]
    #[case::two_args_without_no_comma(
        // Represents `사과(1 2)`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("사과(1 ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("사과(1 2", ")"),
                TokenKind::RParen,
            ),
        ],
        mkerr!(NoCommaInCallArgs, str_loc!("사과(1 ", "2"))
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }
}
