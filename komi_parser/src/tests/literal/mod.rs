use crate::{assert_parse, parse};
use komi_syntax::ast::{Ast, AstKind};
use komi_syntax::token::{Token, TokenKind};
use komi_syntax::{mkast, mktoken};
use komi_util::location::Range;
use komi_util::str_segment::{StrSegment, StrSegmentKind};
use komi_util::{mkstrseg, str_loc};
use rstest::rstest;

mod ok {
    use super::*;

    #[rstest]
    #[case::num(
        // Represents `1`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0)
            )
        ],
        mkast!(prog loc str_loc!("", "1"), vec![
            mkast!(num 1.0, loc str_loc!("", "1")),
        ])
    )]
    #[case::bool(
        // Represents `참`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true)
            )
        ],
        mkast!(prog loc str_loc!("", "참"), vec![
            mkast!(boolean true, loc str_loc!("", "참")),
        ])
    )]
    #[case::str(
        // Represents `"사과{오렌지}"`.
        vec![
            mktoken!(str_loc!("", "\"사과{오렌지}\""),
                TokenKind::Str(vec![
                    mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    mkstrseg!(Identifier, "오렌지", str_loc!("\"사과{", "오렌지")),
                ]),
            )
        ],
        mkast!(prog loc str_loc!("", "\"사과{오렌지}\""), vec![
            mkast!(string loc str_loc!("", "\"사과{오렌지}\""), vec![
                mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                mkstrseg!(Identifier, "오렌지", str_loc!("\"사과{", "오렌지")),
            ]),
        ])
    )]
    #[case::identifier(
        // Represents `a`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a"))
            )
        ],
        mkast!(prog loc str_loc!("", "a"), vec![
            mkast!(identifier "a", loc str_loc!("", "a")),
        ])
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}
