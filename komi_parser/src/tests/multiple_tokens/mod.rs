use crate::{assert_parse, parse};
use komi_syntax::ast::{Ast, AstKind};
use komi_syntax::token::{Token, TokenKind};
use komi_syntax::{mkast, mktoken};
use komi_util::location::Range;
use komi_util::str_loc;
use rstest::rstest;

mod ok {
    use super::*;

    // Test if the location is correct after parsing an expression.
    #[rstest]
    #[case::two_numbers(
        // Represents `1 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 2"), vec![
            mkast!(num 1.0, loc str_loc!("", "1")),
            mkast!(num 2.0, loc str_loc!("1 ", "2")),
        ])
    )]
    #[case::two_bools(
        // Represents `참 거짓`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "거짓"),
                TokenKind::Bool(false),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 거짓"), vec![
            mkast!(boolean true, loc str_loc!("", "참")),
            mkast!(boolean false, loc str_loc!("참 ", "거짓")),
        ])
    )]
    #[case::two_identifiers(
        // Represents `사과 오렌지`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과 ", "오렌지"),
                TokenKind::Identifier(String::from("오렌지")),
            ),
        ],
        mkast!(prog loc str_loc!("", "사과 오렌지"), vec![
            mkast!(identifier "사과", loc str_loc!("", "사과")),
            mkast!(identifier "오렌지", loc str_loc!("사과 ", "오렌지")),
        ])
    )]
    #[case::two_calls_with_empty_args(
        // Represents `사과() 오렌지()`.
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
            mktoken!(str_loc!("사과() ", "오렌지"),
                TokenKind::Identifier(String::from("오렌지")),
            ),
            mktoken!(str_loc!("사과() 오렌지", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과() 오렌지(", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "사과() 오렌지()"), vec![
            mkast!(call loc str_loc!("", "사과()"),
                target mkast!(identifier "사과", loc str_loc!("", "사과")),
                args vec![],
            ),
            mkast!(call loc str_loc!("사과() ", "오렌지()"),
                target mkast!(identifier "오렌지", loc str_loc!("사과() ", "오렌지")),
                args vec![],
            ),
        ])
    )]
    #[case::two_calls_with_single_args(
        // Represents `사과(1) 오렌지(2)`.
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
            mktoken!(str_loc!("사과(1) ", "오렌지"),
                TokenKind::Identifier(String::from("오렌지")),
            ),
            mktoken!(str_loc!("사과(1) 오렌지", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(1) 오렌지(", "("),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("사과(1) 오렌지(2", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "사과(1) 오렌지(1)"), vec![
            mkast!(call loc str_loc!("", "사과(1)"),
                target mkast!(identifier "사과", loc str_loc!("", "사과")),
                args vec![
                    mkast!(num 1.0, loc str_loc!("사과(", "1")),
                ],
            ),
            mkast!(call loc str_loc!("사과(1) ", "오렌지(2)"),
                target mkast!(identifier "오렌지", loc str_loc!("사과(1) ", "오렌지")),
                args vec![
                    mkast!(num 2.0, loc str_loc!("사과(1) 오렌지(", "2")),
                ],
            ),
        ])
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}
