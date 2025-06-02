#[cfg(test)]
mod tests {
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
        #[case::no_params_and_single_expression(
            // Represents `함수 { 1 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkast!(prog loc str_loc!("", "함수 { 1 }"), vec![
                mkast!(closure loc str_loc!("", "함수 { 1 }"),
                    params vec![],
                    body vec![
                        mkast!(num 1.0, loc str_loc!("함수 { ", "1")),
                    ],
                ),
            ])
        )]
        #[case::no_params_and_multiple_expression(
            // Represents `함수 { 1 2 3 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 { 1 ", "2"),
                    TokenKind::Number(2.0),
                ),
                mktoken!(str_loc!("함수 { 1 2 ", "3"),
                    TokenKind::Number(3.0),
                ),
                mktoken!(str_loc!("함수 { 1 2 3 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkast!(prog loc str_loc!("", "함수 { 1 2 3 }"), vec![
                mkast!(closure loc str_loc!("", "함수 { 1 2 3 }"),
                    params vec![],
                    body vec![
                        mkast!(num 1.0, loc str_loc!("함수 { ", "1")),
                        mkast!(num 2.0, loc str_loc!("함수 { 1 ", "2")),
                        mkast!(num 3.0, loc str_loc!("함수 { 1 2 ", "3")),
                    ],
                ),
            ])
        )]
        #[case::multiple_params_and_multiple_expression(
            // Represents `함수 사과, 오렌지, 바나나 { 1 2 3 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과, ", "오렌지"),
                    TokenKind::Identifier(String::from("오렌지")),
                ),
                mktoken!(str_loc!("함수 사과, 오렌지", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, ", "바나나"),
                    TokenKind::Identifier(String::from("바나나")),
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, 바나나 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, 바나나 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, 바나나 { 1 ", "2"),
                    TokenKind::Number(2.0),
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, 바나나 { 1 2 ", "3"),
                    TokenKind::Number(3.0),
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, 바나나 { 1 2 3 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkast!(prog loc str_loc!("", "함수 사과, 오렌지, 바나나 { 1 2 3 }"), vec![
                mkast!(closure loc str_loc!("", "함수 사과, 오렌지, 바나나 { 1 2 3 }"),
                    params vec![
                        String::from("사과"),
                        String::from("오렌지"),
                        String::from("바나나"),
                    ],
                    body vec![
                        mkast!(num 1.0, loc str_loc!("함수 사과, 오렌지, 바나나 { ", "1")),
                        mkast!(num 2.0, loc str_loc!("함수 사과, 오렌지, 바나나 { 1 ", "2")),
                        mkast!(num 3.0, loc str_loc!("함수 사과, 오렌지, 바나나 { 1 2 ", "3")),
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
        #[case::end_with_keyword(
            // Represents `함수`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
            ],
            mkerr!(NoClosureParams, str_loc!("", "함수"))
        )]
        #[case::end_with_single_parameter(
            // Represents `함수 사과`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
            ],
            mkerr!(NoClosureBody, str_loc!("", "함수 사과"))
        )]
        #[case::end_with_comma_after_single_parameter(
            // Represents `함수 사과,`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과", ","),
                    TokenKind::Comma,
                ),
            ],
            mkerr!(NoClosureBody, str_loc!("", "함수 사과,"))
        )]
        #[case::end_with_multiple_parameter(
            // Represents `함수 사과, 오렌지`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과, ", "오렌지"),
                    TokenKind::Identifier(String::from("오렌지")),
                ),
            ],
            mkerr!(NoClosureBody, str_loc!("", "함수 사과, 오렌지"))
        )]
        #[case::empty_body_not_closed(
            // Represents `함수 {`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "{"),
                    TokenKind::LBrace,
                ),
            ],
            mkerr!(NoClosingBraceInClosureBody, str_loc!("함수 {", ""))
        )]
        #[case::nonempty_body_not_closed(
            // Represents `함수 { 1`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 { ", "1"),
                    TokenKind::Number(1.0),
                ),
            ],
            mkerr!(NoClosingBraceInClosureBody, str_loc!("함수 { 1", ""))
        )]
        fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
            assert_parse_fail!(&tokens, error);
        }
    }

    mod invalid_param {
        use super::*;

        #[rstest]
        #[case::single_non_id_param(
            // Represents `함수 1 { 1 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 1 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 1 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 1 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NonIdClosureParams, str_loc!("함수 ", "1"))
        )]
        #[case::non_id_last_param(
            // Represents `함수 사과, 1 { 1 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과, ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 사과, 1 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 사과, 1 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 사과, 1 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NonIdClosureParams, str_loc!("함수 사과, ", "1"))
        )]
        #[case::no_params_but_comma(
            // Represents `함수 , { 1 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 , ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 , { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 , { 1 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NonIdClosureParams, str_loc!("함수 ", ","))
        )]
        #[case::params_end_with_comma(
            // Represents `함수 사과, { 1 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과, ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 사과, { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 사과, { 1 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NonIdClosureParams, str_loc!("함수 사과, ", "{"))
        )]
        #[case::two_commas_between_params(
            // Represents `함수 사과,,바나나 { 1 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "1"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과,", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과,,", "바나나"),
                    TokenKind::Identifier(String::from("바나나")),
                ),
                mktoken!(str_loc!("함수 사과,,바나나 ", "{"),
                    TokenKind::LBrace
                ),
                mktoken!(str_loc!("함수 사과,,바나나 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 사과,,바나나 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NonIdClosureParams, str_loc!("함수 사과,", ","))
        )]
        #[case::params_without_comma(
            // Represents `함수 사과 오렌지 { 1 }`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과 ", "오렌지"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과 오렌지 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 사과 오렌지 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("함수 사과 오렌지 { 1  ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NoCommaInClosureParams, str_loc!("함수 사과 ", "오렌지"))
        )]
        fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
            assert_parse_fail!(&tokens, error);
        }
    }

    mod empty_body {
        use super::*;

        #[rstest]
        #[case::no_params_and_empty_body(
            // Represents `함수 {}`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 {", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NoExprInClosureBody, str_loc!("", "함수 {}"))
        )]
        #[case::single_parameter_and_empty_body(
            // Represents `함수 사과 {}`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 사과 {", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NoExprInClosureBody, str_loc!("", "함수 사과 {}"))
        )]
        #[case::multiple_params_and_empty_body(
            // Represents `함수 사과, 오렌지, 바나나 {}`.
            vec![
                mktoken!(str_loc!("", "함수"),
                    TokenKind::Closure,
                ),
                mktoken!(str_loc!("함수 ", "사과"),
                    TokenKind::Identifier(String::from("사과")),
                ),
                mktoken!(str_loc!("함수 사과", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과, ", "오렌지"),
                    TokenKind::Identifier(String::from("오렌지")),
                ),
                mktoken!(str_loc!("함수 사과, 오렌지", ","),
                    TokenKind::Comma,
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, ", "바나나"),
                    TokenKind::Identifier(String::from("바나나")),
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, 바나나 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("함수 사과, 오렌지, 바나나 {", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NoExprInClosureBody, str_loc!("", "함수 사과, 오렌지, 바나나 {}"))
        )]
        fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
            assert_parse_fail!(&tokens, error);
        }
    }
}
