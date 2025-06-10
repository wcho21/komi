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
    #[case::plus(
        // Represents `1 + 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 + 2"), vec![
            mkast!(infix InfixPlus, loc str_loc!("", "1 + 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 + ", "2")),
            ),
        ])
    )]
    #[case::minus(
        // Represents `1 - 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 - 2"), vec![
            mkast!(infix InfixMinus, loc str_loc!("", "1 - 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 - ", "2")),
            ),
        ])
    )]
    #[case::asterisk(
        // Represents `1 * 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 * ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 * 2"), vec![
            mkast!(infix InfixAsterisk, loc str_loc!("", "1 * 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 * ", "2")),
            ),
        ])
    )]
    #[case::slash(
        // Represents `1 / 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("1 / ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 / 2"), vec![
            mkast!(infix InfixSlash, loc str_loc!("", "1 / 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 / ", "2")),
            ),
        ])
    )]
    #[case::percent(
        // Represents `1 % 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("1 % ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 % 2"), vec![
            mkast!(infix InfixPercent, loc str_loc!("", "1 % 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 % ", "2")),
            ),
        ])
    )]
    #[case::conjunct(
        // Represents `참 그리고 거짓`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("참 그리고 ", "거짓"),
                TokenKind::Bool(false),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 그리고 거짓"), vec![
            mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 거짓"),
                left mkast!(boolean true, loc str_loc!("", "참")),
                right mkast!(boolean false, loc str_loc!("참 그리고 ", "거짓")),
            ),
        ])
    )]
    #[case::disjunct(
        // Represents `참 또는 거짓`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 ", "거짓"),
                TokenKind::Bool(false),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 또는 거짓"), vec![
            mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 거짓"),
                left mkast!(boolean true, loc str_loc!("", "참")),
                right mkast!(boolean false, loc str_loc!("참 또는 ", "거짓")),
            ),
        ])
    )]
    #[case::equals(
        // Represents `a = 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("a = ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a = 1"), vec![
            mkast!(infix InfixEquals, loc str_loc!("", "a = 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a = ", "1")),
            ),
        ])
    )]
    #[case::plus_equals(
        // Represents `a += 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "+="),
                TokenKind::PlusEquals,
            ),
            mktoken!(str_loc!("a += ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a += 1"), vec![
            mkast!(infix InfixPlusEquals, loc str_loc!("", "a += 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a += ", "1")),
            ),
        ])
    )]
    #[case::minus_equals(
        // Represents `a -= 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "-="),
                TokenKind::MinusEquals,
            ),
            mktoken!(str_loc!("a -= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a -= 1"), vec![
            mkast!(infix InfixMinusEquals, loc str_loc!("", "a -= 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a -= ", "1")),
            ),
        ])
    )]
    #[case::asterisk_equals(
        // Represents `a *= 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "*="),
                TokenKind::AsteriskEquals,
            ),
            mktoken!(str_loc!("a *= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a *= 1"), vec![
            mkast!(infix InfixAsteriskEquals, loc str_loc!("", "a *= 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a *= ", "1")),
            ),
        ])
    )]
    #[case::slash_equals(
        // Represents `a /= 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "/="),
                TokenKind::SlashEquals,
            ),
            mktoken!(str_loc!("a /= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a /= 1"), vec![
            mkast!(infix InfixSlashEquals, loc str_loc!("", "a /= 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a /= ", "1")),
            ),
        ])
    )]
    #[case::percent_equals(
        // Represents `a %= 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "%="),
                TokenKind::PercentEquals,
            ),
            mktoken!(str_loc!("a %= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a %= 1"), vec![
            mkast!(infix InfixPercentEquals, loc str_loc!("", "a %= 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a %= ", "1")),
            ),
        ])
    )]
    // TODO(?): split tests into combinator infix, assignment infix, and comparison infix files
    #[case::double_equals(
        // Represents `1 == 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "=="),
                TokenKind::DoubleEquals,
            ),
            mktoken!(str_loc!("1 == ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 == 2"), vec![
            mkast!(infix InfixDoubleEquals, loc str_loc!("", "1 == 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 == ", "2")),
            ),
        ])
    )]
    #[case::bang_equals(
        // Represents `1 != 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "!="),
                TokenKind::BangEquals,
            ),
            mktoken!(str_loc!("1 != ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 != 2"), vec![
            mkast!(infix InfixBangEquals, loc str_loc!("", "1 != 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 != ", "2")),
            ),
        ])
    )]
    #[case::lbracket_equals(
        // Represents `1 >= 1`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", ">="),
                TokenKind::LBracketEquals,
            ),
            mktoken!(str_loc!("1 >= ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 >= 2"), vec![
            mkast!(infix InfixLBracketEquals, loc str_loc!("", "1 >= 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 >= ", "2")),
            ),
        ])
    )]
    #[case::rbracket_equals(
        // Represents `1 <= 1`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "<="),
                TokenKind::RBracketEquals,
            ),
            mktoken!(str_loc!("1 <= ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 <= 2"), vec![
            mkast!(infix InfixRBracketEquals, loc str_loc!("", "1 <= 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 <= ", "2")),
            ),
        ])
    )]
    #[case::lbracket(
        // Represents `1 > 1`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", ">"),
                TokenKind::LBracket,
            ),
            mktoken!(str_loc!("1 > ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 > 2"), vec![
            mkast!(infix InfixLBracket, loc str_loc!("", "1 > 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 > ", "2")),
            ),
        ])
    )]
    #[case::rbracket(
        // Represents `1 < 1`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "<"),
                TokenKind::RBracket,
            ),
            mktoken!(str_loc!("1 < ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 < 2"), vec![
            mkast!(infix InfixRBracket, loc str_loc!("", "1 < 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 < ", "2")),
            ),
        ])
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}

mod without_left {
    use super::*;

    #[rstest]
    #[case::asterisk(
        // Represents `* 1`.
        vec![
            mktoken!(str_loc!("", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("* ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "*")),
    )]
    #[case::slash(
        // Represents `/ 1`.
        vec![
            mktoken!(str_loc!("", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("/ ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "/")),
    )]
    #[case::percent(
        // Represents `% 1`.
        vec![
            mktoken!(str_loc!("", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("% ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "%")),
    )]
    #[case::conjunct(
        // Represents `그리고 참`.
        vec![
            mktoken!(str_loc!("", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("그리고 ", "그리고 참"),
                TokenKind::Bool(true),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "그리고")),
    )]
    #[case::disjunct(
        // Represents `또는 참`.
        vec![
            mktoken!(str_loc!("", "또는"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("또는 ", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "또는")),
    )]
    #[case::equals(
        // Represents `= 1`.
        vec![
            mktoken!(str_loc!("", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "=")),
    )]
    #[case::plus_equals(
        // Represents `+= 1`.
        vec![
            mktoken!(str_loc!("", "+="),
                TokenKind::PlusEquals,
            ),
            mktoken!(str_loc!("+= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "+=")),
    )]
    #[case::minus_equals(
        // Represents `-= 1`.
        vec![
            mktoken!(str_loc!("", "-="),
                TokenKind::MinusEquals,
            ),
            mktoken!(str_loc!("-= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "-=")),
    )]
    #[case::asterisk_equals(
        // Represents `*= 1`.
        vec![
            mktoken!(str_loc!("", "*="),
                TokenKind::AsteriskEquals,
            ),
            mktoken!(str_loc!("*= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "*=")),
    )]
    #[case::slash_equals(
        // Represents `/= 1`.
        vec![
            mktoken!(str_loc!("", "/="),
                TokenKind::SlashEquals,
            ),
            mktoken!(str_loc!("/= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "/=")),
    )]
    #[case::percent_equals(
        // Represents `%= 1`.
        vec![
            mktoken!(str_loc!("", "%="),
                TokenKind::PercentEquals,
            ),
            mktoken!(str_loc!("%= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "%=")),
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }
}

mod without_right {
    use super::*;

    #[rstest]
    #[case::plus(
        // Represents `1 +`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 +")),
    )]
    #[case::minus(
        // Represents `1 -`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "-"),
                TokenKind::Minus,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 -")),
    )]
    #[case::asterisk(
        // Represents `1 *`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "*"),
                TokenKind::Asterisk,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 *")),
    )]
    #[case::slash(
        // Represents `1 /`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "/"),
                TokenKind::Slash,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 /")),
    )]
    #[case::percent(
        // Represents `1 %`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "%"),
                TokenKind::Percent,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 %")),
    )]
    #[case::conjunct(
        // Represents `참 그리고`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "그리고"),
                TokenKind::Conjunct,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "참 그리고")),
    )]
    #[case::disjunct(
        // Represents `참 또는`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "또는"),
                TokenKind::Disjunct,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "참 또는")),
    )]
    #[case::equals(
        // Represents `a =`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "="),
                TokenKind::Equals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a =")),
    )]
    #[case::plus_equals(
        // Represents `a +=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "+="),
                TokenKind::PlusEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a +=")),
    )]
    #[case::minus_equals(
        // Represents `a -=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "-="),
                TokenKind::MinusEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a -=")),
    )]
    #[case::asterisk_equals(
        // Represents `a *=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "*="),
                TokenKind::AsteriskEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a *=")),
    )]
    #[case::slash_equals(
        // Represents `a /=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "/="),
                TokenKind::SlashEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a /=")),
    )]
    #[case::percent_equals(
        // Represents `a %=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "%="),
                TokenKind::PercentEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a %=")),
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }
}

mod single {
    use super::*;

    #[rstest]
    #[case::plus(
        // Represents `+`.
        vec![
            mktoken!(str_loc!("", "+"),
                TokenKind::Plus,
            )
        ],
        mkerr!(NoPrefixOperand, str_loc!("", "+"))
    )]
    #[case::minus(
        // Represents `-`.
        vec![
            mktoken!(str_loc!("", "-"),
                TokenKind::Minus,
            )
        ],
        mkerr!(NoPrefixOperand, str_loc!("", "-"))
    )]
    #[case::asterisk(
        // Represents `*`.
        vec![
            mktoken!(str_loc!("", "*"),
                TokenKind::Asterisk,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "*"))
    )]
    #[case::slash(
        // Represents `/`.
        vec![
            mktoken!(str_loc!("", "/"),
                TokenKind::Slash,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "/"))
    )]
    #[case::percent(
        // Represents `%`.
        vec![
            mktoken!(str_loc!("", "%"),
                TokenKind::Percent,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "%"))
    )]
    #[case::conjunct(
        // Represents `그리고`.
        vec![
            mktoken!(str_loc!("", "그리고"),
                TokenKind::Conjunct,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "그리고"))
    )]
    #[case::disjunct(
        // Represents `또는`.
        vec![
            mktoken!(str_loc!("", "또는"),
                TokenKind::Disjunct,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "또는"))
    )]
    #[case::equals(
        // Represents `=`.
        vec![
            mktoken!(str_loc!("", "="),
                TokenKind::Equals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "="))
    )]
    #[case::plus_equals(
        // Represents `+=`.
        vec![
            mktoken!(str_loc!("", "+="),
                TokenKind::PlusEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "+="))
    )]
    #[case::minus_equals(
        // Represents `-=`.
        vec![
            mktoken!(str_loc!("", "-="),
                TokenKind::MinusEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "-="))
    )]
    #[case::asterisk_equals(
        // Represents `*=`.
        vec![
            mktoken!(str_loc!("", "*="),
                TokenKind::AsteriskEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "*="))
    )]
    #[case::slash_equals(
        // Represents `/=`.
        vec![
            mktoken!(str_loc!("", "/="),
                TokenKind::SlashEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "/="))
    )]
    #[case::percent_equals(
        // Represents `%=`.
        vec![
            mktoken!(str_loc!("", "%="),
                TokenKind::PercentEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "%="))
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }
}

mod left_associativity {
    use super::*;

    #[rstest]
    #[case::two_pluses(
        // Represents `1 + 2 + 3`, and expects to be parsed into `(1 + 2) + 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 + 2 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + 2 + ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 + 2 + 3"), vec![
            mkast!(infix InfixPlus, loc str_loc!("", "1 + 2 + 3"),
                left mkast!(infix InfixPlus, loc str_loc!("", "1 + 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 + ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 + 2 + ", "3")),
            ),
        ])
    )]
    #[case::two_minuses(
        // Represents `1 - 2 - 3`, and expects to be parsed into `(1 - 2) - 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 - 2 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - 2 - ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 - 2 - 3"), vec![
            mkast!(infix InfixMinus, loc str_loc!("", "1 - 2 - 3"),
                left mkast!(infix InfixMinus, loc str_loc!("", "1 - 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 - ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 - 2 - ", "3")),
            ),
        ])
    )]
    #[case::two_asterisks(
        // Represents `1 * 2 * 3`, and expects to be parsed into `(1 * 2) * 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 * ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 * 2 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 * 2 * ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 * 2 * 3"), vec![
            mkast!(infix InfixAsterisk, loc str_loc!("", "1 * 2 * 3"),
                left mkast!(infix InfixAsterisk, loc str_loc!("", "1 * 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 * ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 * 2 * ", "3")),
            ),
        ])
    )]
    #[case::two_slashes(
        // Represents `1 / 2 / 3`, and expects to be parsed into `(1 / 2) / 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("1 / ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 / 2 ", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("1 / 2 / ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 / 2 / 3"), vec![
            mkast!(infix InfixSlash, loc str_loc!("", "1 / 2 / 3"),
                left mkast!(infix InfixSlash, loc str_loc!("", "1 / 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 / ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 / 2 / ", "3")),
            ),
        ])
    )]
    #[case::two_percents(
        // Represents `1 % 2 % 3`, and expects to be parsed into `(1 % 2) % 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("1 % ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 % 2 ", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("1 % 2 % ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 % 2 % 3"), vec![
            mkast!(infix InfixPercent, loc str_loc!("", "1 % 2 % 3"),
                left mkast!(infix InfixPercent, loc str_loc!("", "1 % 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 % ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 % 2 % ", "3")),
            ),
        ])
    )]
    #[case::two_conjuncts(
        // Represents `참 그리고 참 그리고 참`, and expects to be parsed into `(참 그리고 참) 그리고 참`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("참 그리고 ", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 그리고 참 ", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("참 그리고 참 그리고 ", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 그리고 참 그리고 참"), vec![
            mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 참 그리고 참"),
                left mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 참"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean true, loc str_loc!("참 그리고 ", "참")),
                ),
                right mkast!(boolean true, loc str_loc!("참 그리고 참 그리고 ", "참")),
            ),
        ])
    )]
    #[case::two_disjuncts(
        // Represents `참 또는 참 또는 참`, and expects to be parsed into `(참 또는 참) 또는 참`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 ", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 또는 참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 참 또는 ", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 또는 참 또는 참"), vec![
            mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 참 또는 참"),
                left mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 참"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean true, loc str_loc!("참 또는 ", "참")),
                ),
                right mkast!(boolean true, loc str_loc!("참 또는 참 또는 ", "참")),
            ),
        ])
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}

mod right_associativity {
    use super::*;

    #[rstest]
    #[case::two_equals(
        // Represents `a = b = c`, and expects to be parsed into `a = (b = c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("a = ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a = b ", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("a = b = ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a = b = c"), vec![
            mkast!(infix InfixEquals, loc str_loc!("", "a = b = c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixEquals, loc str_loc!("a = ", "b = c"),
                    left mkast!(identifier "b", loc str_loc!("a = ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a = b = ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_plus_equals(
        // Represents `a += b += c`, and expects to be parsed into `a += (b += c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "+="),
                TokenKind::PlusEquals,
            ),
            mktoken!(str_loc!("a += ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a += b ", "+="),
                TokenKind::PlusEquals,
            ),
            mktoken!(str_loc!("a += b += ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a += b += c"), vec![
            mkast!(infix InfixPlusEquals, loc str_loc!("", "a += b += c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixPlusEquals, loc str_loc!("a += ", "b += c"),
                    left mkast!(identifier "b", loc str_loc!("a += ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a += b += ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_minus_equals(
        // Represents `a -= b -= c`, and expects to be parsed into `a -= (b -= c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "-="),
                TokenKind::MinusEquals,
            ),
            mktoken!(str_loc!("a -= ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a -= b ", "-="),
                TokenKind::MinusEquals,
            ),
            mktoken!(str_loc!("a -= b -= ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a -= b -= c"), vec![
            mkast!(infix InfixMinusEquals, loc str_loc!("", "a -= b -= c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixMinusEquals, loc str_loc!("a -= ", "b -= c"),
                    left mkast!(identifier "b", loc str_loc!("a -= ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a -= b -= ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_asterisk_equals(
        // Represents `a *= b *= c`, and expects to be parsed into `a *= (b *= c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "*="),
                TokenKind::AsteriskEquals,
            ),
            mktoken!(str_loc!("a *= ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a *= b ", "*="),
                TokenKind::AsteriskEquals,
            ),
            mktoken!(str_loc!("a *= b *= ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a *= b *= c"), vec![
            mkast!(infix InfixAsteriskEquals, loc str_loc!("", "a *= b *= c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixAsteriskEquals, loc str_loc!("a *= ", "b *= c"),
                    left mkast!(identifier "b", loc str_loc!("a *= ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a *= b *= ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_slash_equals(
        // Represents `a /= b /= c`, and expects to be parsed into `a /= (b /= c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "/="),
                TokenKind::SlashEquals,
            ),
            mktoken!(str_loc!("a /= ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a /= b ", "/="),
                TokenKind::SlashEquals,
            ),
            mktoken!(str_loc!("a /= b /= ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a /= b /= c"), vec![
            mkast!(infix InfixSlashEquals, loc str_loc!("", "a /= b /= c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixSlashEquals, loc str_loc!("a /= ", "b /= c"),
                    left mkast!(identifier "b", loc str_loc!("a /= ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a /= b /= ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_percent_equals(
        // Represents `a %= b %= c`, and expects to be parsed into `a %= (b %= c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "%="),
                TokenKind::PercentEquals,
            ),
            mktoken!(str_loc!("a %= ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a %= b ", "%="),
                TokenKind::PercentEquals,
            ),
            mktoken!(str_loc!("a %= b %= ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a %= b %= c"), vec![
            mkast!(infix InfixPercentEquals, loc str_loc!("", "a %= b %= c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixPercentEquals, loc str_loc!("a %= ", "b %= c"),
                    left mkast!(identifier "b", loc str_loc!("a %= ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a %= b %= ", "c")),
                ),
            ),
        ])
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}

mod priority {
    use super::*;

    #[rstest]
    #[case::asterisk_prioritized_over_plus(
        // Represents `1 + 2 * 3`, and expects to be parsed into `1 + (2 * 3)`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 + 2 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 + 2 * ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 + 2 * 3"), vec![
            mkast!(infix InfixPlus, loc str_loc!("", "1 + 2 * 3"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(infix InfixAsterisk, loc str_loc!("1 + ", "2 * 3"),
                    left mkast!(num 2.0, loc str_loc!("1 + ", "2")),
                    right mkast!(num 3.0, loc str_loc!("1 + 2 * ", "3")),
                ),
            ),
        ])
    )]
    #[case::slash_prioritized_over_minus(
        // Represents `1 - 2 / 3`, and expects to be parsed into `1 - (2 / 3)`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 - 2 ", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("1 - 2 / ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 - 2 / 3"), vec![
            mkast!(infix InfixMinus, loc str_loc!("", "1 - 2 / 3"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(infix InfixSlash, loc str_loc!("1 - ", "2 / 3"),
                    left mkast!(num 2.0, loc str_loc!("1 - ", "2")),
                    right mkast!(num 3.0, loc str_loc!("1 - 2 / ", "3")),
                ),
            ),
        ])
    )]
    #[case::percent_prioritized_over_plus(
        // Represents `1 + 2 % 3`, and expects to be parsed into `1 + (2 % 3)`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 + 2 ", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("1 + 2 % ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 + 2 % 3"), vec![
            mkast!(infix InfixPlus, loc str_loc!("", "1 + 2 % 3"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(infix InfixPercent, loc str_loc!("1 + ", "2 % 3"),
                    left mkast!(num 2.0, loc str_loc!("1 + ", "2")),
                    right mkast!(num 3.0, loc str_loc!("1 + 2 % ", "3")),
                ),
            ),
        ])
    )]
    #[case::plus_prioritized_over_equals(
        // Represents `a = 1 + 2`, and expects to be parsed into `a = (1 + 2)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("a = ", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("a = 1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("a = 1 + ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a = 1 + 2"), vec![
            mkast!(infix InfixEquals, loc str_loc!("", "a = 1 + 2"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixPlus, loc str_loc!("a = ", "1 + 2"),
                    left mkast!(num 1.0, loc str_loc!("a = ", "1")),
                    right mkast!(num 2.0, loc str_loc!("a = 1 + ", "2")),
                ),
            ),
        ])
    )]
    fn test(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}
