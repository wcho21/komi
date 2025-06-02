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
        #[case::conseq_and_altern(
            // Represents `만약 참 { 1 } 아니면 { 2 }`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("만약 참 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 1 } ", "아니면"),
                    TokenKind::ElseBranch,
                ),
                mktoken!(str_loc!("만약 참 { 1 } 아니면 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { 1 } 아니면 { ", "2"),
                    TokenKind::Number(2.0),
                ),
                mktoken!(str_loc!("만약 참 { 1 } 아니면 { 2 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkast!(prog loc str_loc!("", "만약 참 { 1 } 아니면 { 2 }"), vec![
                mkast!(branch loc str_loc!("", "만약 참 { 1 } 아니면 { 2 }"),
                    pred mkast!(boolean true, loc str_loc!("만약 ", "참")),
                    conseq vec![
                        mkast!(num 1.0, loc str_loc!("만약 참 { ", "1")),
                    ],
                    altern vec![
                        mkast!(num 2.0, loc str_loc!("만약 참 { 1 } 아니면 { ", "2")),
                    ],
                )
            ])
        )]
        #[case::nested_conseq(
            // Represents `만약 참 { 만약 참 { 1 } 아니면 { 2 } } 아니면 { 3 }`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 참 { 만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } ", "아니면"),
                    TokenKind::ElseBranch,
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } 아니면 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } 아니면 { ", "2"),
                    TokenKind::Number(2.0),
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } 아니면 { 2 ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } 아니면 { 2 } ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } 아니면 { 2 } } ", "아니면"),
                    TokenKind::ElseBranch,
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } 아니면 { 2 } } 아니면 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } 아니면 { 2 } } 아니면 { ", "3"),
                    TokenKind::Number(3.0),
                ),
                mktoken!(str_loc!("만약 참 { 만약 참 { 1 } 아니면 { 2 } } 아니면 { 3 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkast!(prog loc str_loc!("", "만약 참 { 만약 참 { 1 } 아니면 { 2 } } 아니면 { 3 }"), vec![
                mkast!(branch loc str_loc!("", "만약 참 { 만약 참 { 1 } 아니면 { 2 } } 아니면 { 3 }"),
                    pred mkast!(boolean true, loc str_loc!("만약 ", "참")),
                    conseq vec![
                        mkast!(branch loc str_loc!("만약 참 { ", "만약 참 { 1 } 아니면 { 2 }"),
                            pred mkast!(boolean true, loc str_loc!("만약 참 { 만약 ", "참")),
                            conseq vec![
                                mkast!(num 1.0, loc str_loc!("만약 참 { 만약 참 { ", "1")),
                            ],
                            altern vec![
                                mkast!(num 2.0, loc str_loc!("만약 참 { 만약 참 { 1 } 아니면 { ", "2")),
                            ],
                        ),
                    ],
                    altern vec![
                        mkast!(num 3.0, loc str_loc!("만약 참 { 만약 참 { 1 } 아니면 { 2 } } 아니면 { ", "3")),
                    ],
                )
            ])
        )]
        fn branch(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
            assert_parse!(&tokens, expected);
        }
    }

    mod incomplete {
        use super::*;

        #[rstest]
        #[case::no_predicate(
            // Represents `만약`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
            ],
            mkerr!(NoPredicate, str_loc!("", "만약"))
        )]
        #[case::no_conseq_block(
            // Represents `만약 참`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
            ],
            mkerr!(NoConseqBlock, str_loc!("", "만약 참"))
        )]
        #[case::no_conseq_opening_brace(
            // Represents `만약 참 1`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "1"),
                    TokenKind::Number(1.0),
                ),
            ],
            mkerr!(NoOpeningBraceInConseq, str_loc!("만약 참 ", "1"))
        )]
        #[case::no_conseq_closing_brace(
            // Represents `만약 참 {`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
            ],
            mkerr!(NoClosingBraceInConseq, str_loc!("만약 참 ", "{"))
        )]
        #[case::empty_conseq(
            // Represents `만약 참 { }`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NoExprConseq, str_loc!("만약 참 ", "{ }"))
        )]
        #[case::no_altern_block(
            // Represents `만약 참 { 1 }`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("만약 참 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NoAlternBlock, str_loc!("", "만약 참 { 1 }"))
        )]
        #[case::no_altern_keyword(
            // Represents `만약 참 { 1 } 1`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("만약 참 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 1 } ", "1"),
                    TokenKind::Number(1.0),
                ),
            ],
            mkerr!(NoAlternKeyword, str_loc!("만약 참 { 1 } ", "1"))
        )]
        #[case::no_altern_block(
            // Represents `만약 참 { 1 } 아니면 `.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("만약 참 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 1 } ", "아니면"),
                    TokenKind::ElseBranch,
                ),
            ],
            mkerr!(NoAlternBlock, str_loc!("만약 참 { 1 } ", "아니면"))
        )]
        #[case::no_altern_opening_brace(
            // Represents `만약 참 { 1 } 아니면 2`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("만약 참 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 1 } ", "아니면"),
                    TokenKind::ElseBranch,
                ),
                mktoken!(str_loc!("만약 참 { 1 } 아니면 ", "2"),
                    TokenKind::Number(2.0),
                ),
            ],
            mkerr!(NoOpeningBraceInAltern, str_loc!("만약 참 { 1 } 아니면 ", "2"))
        )]
        #[case::no_altern_closing_brace(
            // Represents `만약 참 { 1 } 아니면 {`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("만약 참 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 1 } ", "아니면"),
                    TokenKind::ElseBranch,
                ),
                mktoken!(str_loc!("만약 참 { 1 } 아니면 ", "{"),
                    TokenKind::LBrace,
                ),
            ],
            mkerr!(NoClosingBraceInAltern, str_loc!("만약 참 { 1 } 아니면 ", "{"))
        )]
        #[case::empty_altern(
            // Represents `만약 참 { 1 } 아니면 { }`.
            vec![
                mktoken!(str_loc!("", "만약"),
                    TokenKind::IfBranch,
                ),
                mktoken!(str_loc!("만약 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("만약 참 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { ", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("만약 참 { 1 ", "}"),
                    TokenKind::RBrace,
                ),
                mktoken!(str_loc!("만약 참 { 1 } ", "아니면"),
                    TokenKind::ElseBranch,
                ),
                mktoken!(str_loc!("만약 참 { 1 } 아니면 ", "{"),
                    TokenKind::LBrace,
                ),
                mktoken!(str_loc!("만약 참 { 1 } 아니면 { ", "}"),
                    TokenKind::RBrace,
                ),
            ],
            mkerr!(NoExprInAltern, str_loc!("만약 참 { 1 } 아니면 ", "{ }"))
        )]
        fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
            assert_parse_fail!(&tokens, error);
        }
    }
}
