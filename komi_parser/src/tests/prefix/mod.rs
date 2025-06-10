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
    #[case::plus_num(
        // Represents `+1`.
        vec![
            mktoken!(str_loc!("", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("+", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "+1"), vec![
            mkast!(prefix PrefixPlus, loc str_loc!("", "+1"),
                operand mkast!(num 1.0, loc str_loc!("+", "1")),
            ),
        ])
    )]
    #[case::minus_num(
        // Represents `-1`.
        vec![
            mktoken!(str_loc!("", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("-", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "-1"), vec![
            mkast!(prefix PrefixMinus, loc str_loc!("", "-1"),
                operand mkast!(num 1.0, loc str_loc!("-", "1")),
            ),
        ])
    )]
    #[case::two_pluses_num(
        // Represents `++1`.
        vec![
            mktoken!(str_loc!("", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("+", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("++", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "++1"), vec![
            mkast!(prefix PrefixPlus, loc str_loc!("", "++1"),
                operand mkast!(prefix PrefixPlus, loc str_loc!("+", "+1"),
                    operand mkast!(num 1.0, loc str_loc!("++", "1")),
                ),
            ),
        ])
    )]
    #[case::two_minuses_num(
        // Represents `--1`.
        vec![
            mktoken!(str_loc!("", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("-", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("--", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "--1"), vec![
            mkast!(prefix PrefixMinus, loc str_loc!("", "--1"),
                operand mkast!(prefix PrefixMinus, loc str_loc!("-", "-1"),
                    operand mkast!(num 1.0, loc str_loc!("--", "1")),
                ),
            ),
        ])
    )]
    #[case::bang_bool(
        // Represents `!참`.
        vec![
            mktoken!(str_loc!("", "!"),
                TokenKind::Bang,
            ),
            mktoken!(str_loc!("!", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkast!(prog loc str_loc!("", "!참"), vec![
            mkast!(prefix PrefixBang, loc str_loc!("", "!참"),
                operand mkast!(boolean true, loc str_loc!("!", "참")),
            ),
        ])
    )]
    #[case::two_bang_bool(
        // Represents `!!참`.
        vec![
            mktoken!(str_loc!("", "!"),
                TokenKind::Bang,
            ),
            mktoken!(str_loc!("!", "!"),
                TokenKind::Bang,
            ),
            mktoken!(str_loc!("!!", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkast!(prog loc str_loc!("", "!!참"), vec![
            mkast!(prefix PrefixBang, loc str_loc!("", "!!참"),
                operand mkast!(prefix PrefixBang, loc str_loc!("!", "!참"),
                    operand mkast!(boolean true, loc str_loc!("!!", "참")),
                ),
            ),
        ])
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}

mod single {
    use super::*;

    // See the `infix` module for other prefixes, which are covered in there.
    #[rstest]
    #[case::bang(
        // Represents `!`.
        vec![
            mktoken!(str_loc!("", "!"),
                TokenKind::Bang,
            )
        ],
        mkerr!(NoPrefixOperand, str_loc!("", "!"))
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }
}
