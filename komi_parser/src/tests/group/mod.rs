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
        #[case::arithmetic_grouping(
            // Represents `(1 - 2) * 3`
            vec![
                mktoken!(str_loc!("", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("(", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("(1 ", "-"),
                    TokenKind::Minus,
                ),
                mktoken!(str_loc!("(1 - ", "2"),
                    TokenKind::Number(2.0),
                ),
                mktoken!(str_loc!("(1 - 2", ")"),
                    TokenKind::RParen,
                ),
                mktoken!(str_loc!("(1 - 2) ", "*"),
                    TokenKind::Asterisk,
                ),
                mktoken!(str_loc!("(1 - 2) * ", "3"),
                    TokenKind::Number(3.0),
                ),
            ],
            mkast!(prog loc str_loc!("", "(1 - 2) * 3"), vec![
                mkast!(infix InfixAsterisk, loc str_loc!("", "(1 - 2) * 3"),
                    left mkast!(infix InfixMinus, loc str_loc!("", "(1 - 2)"),
                        left mkast!(num 1.0, loc str_loc!("(", "1")),
                        right mkast!(num 2.0, loc str_loc!("(1 - ", "2")),
                    ),
                    right mkast!(num 3.0, loc str_loc!("(1 - 2) * ", "3")),
                ),
            ])
        )]
        #[case::arithmetic_nested_grouping(
            // Represents `1 - (2 * (3 - 4))`
            vec![
                mktoken!(str_loc!("", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("1 ", "-"),
                    TokenKind::Minus,
                ),
                mktoken!(str_loc!("1 - ", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("1 - (", "2"),
                    TokenKind::Number(2.0),
                ),
                mktoken!(str_loc!("1 - (2 ", "*"),
                    TokenKind::Asterisk,
                ),
                mktoken!(str_loc!("1 - (2 * ", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("1 - (2 * (", "3"),
                    TokenKind::Number(3.0),
                ),
                mktoken!(str_loc!("1 - (2 * (3 ", "-"),
                    TokenKind::Minus,
                ),
                mktoken!(str_loc!("1 - (2 * (3 - ", "4"),
                    TokenKind::Number(4.0),
                ),
                mktoken!(str_loc!("1 - (2 * (3 - 4", ")"),
                    TokenKind::RParen,
                ),
                mktoken!(str_loc!("1 - (2 * (3 - 4)", ")"),
                    TokenKind::RParen,
                ),
            ],
            mkast!(prog loc str_loc!("", "1 - (2 * (3 - 4))"), vec![
                mkast!(infix InfixMinus, loc str_loc!("", "1 - (2 * (3 - 4))"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(infix InfixAsterisk, loc str_loc!("1 - ", "(2 * (3 - 4))"),
                        left mkast!(num 2.0, loc str_loc!("1 - (", "2")),
                        right mkast!(infix InfixMinus, loc str_loc!("1 - (2 * ", "(3 - 4)"),
                            left mkast!(num 3.0, loc str_loc!("1 - (2 * (", "3")),
                            right mkast!(num 4.0, loc str_loc!("1 - (2 * (3 - ", "4")),
                        ),
                    ),
                ),
            ])
        )]
        #[case::connective_grouping(
            // Represents `참 또는 (참 그리고 참)`
            vec![
                mktoken!(str_loc!("", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("참 ", "또는"),
                    TokenKind::Disjunct,
                ),
                mktoken!(str_loc!("참 또는 ", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("참 또는 (", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("참 또는 (참 ", "그리고"),
                    TokenKind::Conjunct,
                ),
                mktoken!(str_loc!("참 또는 (참 그리고 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("참 또는 (참 그리고 참", ")"),
                    TokenKind::RParen,
                ),
            ],
            mkast!(prog loc str_loc!("", "참 또는 (참 그리고 참)"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 (참 그리고 참)"), 
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(infix InfixConjunct, loc str_loc!("참 또는 ", "(참 그리고 참)"),
                        left mkast!(boolean true, loc str_loc!("참 또는 (", "참")),
                        right mkast!(boolean true, loc str_loc!("참 또는 (참 그리고 ", "참")),
                    ),
                ),
            ])
        )]
        #[case::connective_nested_grouping(
            // Represents `참 또는 (거짓 그리고 (참 또는 참))`
            vec![
                mktoken!(str_loc!("", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("참 ", "또는"),
                    TokenKind::Disjunct,
                ),
                mktoken!(str_loc!("참 또는 ", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("참 또는 (", "거짓"),
                    TokenKind::Bool(false),
                ),
                mktoken!(str_loc!("참 또는 (거짓 ", "그리고"),
                    TokenKind::Conjunct,
                ),
                mktoken!(str_loc!("참 또는 (거짓 그리고 ", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("참 또는 (거짓 그리고 (", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("참 또는 (거짓 그리고 (참 ", "또는"),
                    TokenKind::Disjunct,
                ),
                mktoken!(str_loc!("참 또는 (거짓 그리고 (참 또는 ", "참"),
                    TokenKind::Bool(true),
                ),
                mktoken!(str_loc!("참 또는 (거짓 그리고 (참 또는 참", ")"),
                    TokenKind::RParen,
                ),
                mktoken!(str_loc!("참 또는 (거짓 그리고 (참 또는 참)", ")"),
                    TokenKind::RParen,
                ),
            ],
            mkast!(prog loc str_loc!("", "참 또는 (거짓 그리고 (참 또는 참))"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 (거짓 그리고 (참 또는 참))"), 
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(infix InfixConjunct, loc str_loc!("참 또는 ", "(거짓 그리고 (참 또는 참))"),
                        left mkast!(boolean false, loc str_loc!("참 또는 (", "거짓")),
                        right mkast!(infix InfixDisjunct, loc str_loc!("참 또는 (거짓 그리고 ", "(참 또는 참)"),
                            left mkast!(boolean true, loc str_loc!("참 또는 (거짓 그리고 (", "참")),
                            right mkast!(boolean true, loc str_loc!("참 또는 (거짓 그리고 (참 또는 ", "참")),
                        ),
                    ),
                ),
            ])
        )]
        fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
            assert_parse!(&tokens, expected);
        }
    }

    mod parenthesis {
        use super::*;

        #[rstest]
        #[case::lparen(
            // Represents `(`.
            vec![
                mktoken!(str_loc!("", "("),
                    TokenKind::LParen,
                )
            ],
            mkerr!(NoClosingParenInGroup, str_loc!("", "("))
        )]
        #[case::rparen(
            // Represents `)`.
            vec![
                mktoken!(str_loc!("", ")"),
                    TokenKind::RParen,
                )
            ],
            mkerr!(InvalidExprStart, str_loc!("", ")"))
        )]
        fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
            assert_parse_fail!(&tokens, error);
        }
    }

    mod unmatched_parenthesis {
        use super::*;

        #[rstest]
        #[case::lparen_not_closed_and_end(
            // Represents `(1 + 2`.
            vec![
                mktoken!(str_loc!("", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("(", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("(1 ", "+"),
                    TokenKind::Plus,
                ),
                mktoken!(str_loc!("(1 + ", "2"),
                    TokenKind::Number(2.0),
                ),
            ],
            mkerr!(NoClosingParenInGroup, str_loc!("", "(1 + 2"))
        )]
        #[case::lparen_not_closed_and_something(
            // Represents `(1 + 2 3`.
            vec![
                mktoken!(str_loc!("", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("(", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("(1 ", "+"),
                    TokenKind::Plus,
                ),
                mktoken!(str_loc!("(1 + ", "2"),
                    TokenKind::Number(2.0),
                ),
                mktoken!(str_loc!("(1 + 2 ", "3"),
                    TokenKind::Number(2.0),
                ),
            ],
            mkerr!(NoClosingParenInGroup, str_loc!("", "(1 + 2 3"))
        )]
        #[case::closed_twice(
            // Represents `(1))`.
            vec![
                mktoken!(str_loc!("", "("),
                    TokenKind::LParen,
                ),
                mktoken!(str_loc!("(", "1"),
                    TokenKind::Number(1.0),
                ),
                mktoken!(str_loc!("(1", ")"),
                    TokenKind::RParen,
                ),
                mktoken!(str_loc!("(1)", ")"),
                    TokenKind::RParen,
                ),
            ],
            mkerr!(InvalidExprStart, str_loc!("(1)", ")"))
        )]
        fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
            assert_parse_fail!(&tokens, error);
        }
    }
}
