#[cfg(test)]
mod tests {
    use crate::{assert_parse, assert_parse_fail, mkerr, parse};
    use komi_syntax::ast::{Ast, AstKind};
    use komi_syntax::error::{ParseError, ParseErrorKind};
    use komi_syntax::token::{Token, TokenKind};
    use komi_syntax::{mkast, mktoken};
    use komi_util::location::Range;
    use komi_util::str_segment::{StrSegment, StrSegmentKind};
    use komi_util::{mkstrseg, str_loc};
    use rstest::rstest;

    #[test]
    fn empty() {
        // Represents ``.
        assert_parse!(&vec![], mkast!(prog loc str_loc!("", ""), vec![]));
    }

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
    fn single_literal(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

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
    fn prefix(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

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
    fn infix(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::asterisk_without_left(
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
    #[case::slash_without_left(
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
    #[case::percent_without_left(
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
    #[case::conjunct_without_left(
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
    #[case::disjunct_without_left(
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
    #[case::equals_without_left(
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
    #[case::plus_equals_without_left(
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
    #[case::minus_equals_without_left(
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
    #[case::asterisk_equals_without_left(
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
    #[case::slash_equals_without_left(
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
    #[case::percent_equals_without_left(
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
    fn infix_no_left_operand(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::plus_without_right(
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
    #[case::minus_without_right(
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
    #[case::asterisk_without_right(
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
    #[case::slash_without_right(
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
    #[case::percent_without_right(
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
    #[case::conjunct_without_right(
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
    #[case::disjunct_without_right(
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
    #[case::equals_without_right(
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
    #[case::plus_equals_without_right(
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
    #[case::minus_equals_without_right(
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
    #[case::asterisk_equals_without_right(
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
    #[case::slash_equals_without_right(
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
    #[case::percent_equals_without_right(
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
    fn infix_no_right_operand(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    #[case::bang(
        // Represents `!`.
        vec![
            mktoken!(str_loc!("", "!"),
                TokenKind::Bang,
            )
        ],
        mkerr!(NoPrefixOperand, str_loc!("", "!"))
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
    fn single_token(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    fn parenthesis(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    fn left_associativity(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

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
    fn right_associativity(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

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
    fn priority(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

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
    fn grouping(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

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
    fn unmatched_parenthesis(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    fn closure(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

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
    fn incomplete_closure(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    #[case::last_non_id_param(
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
    fn invalid_closure_params(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
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
    fn wrong_comma_in_closure_params(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
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
    fn incomplete_body_closure(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    fn empty_body_closure(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    fn call(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

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
    fn incomplete_call(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    fn wrong_comma_call(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

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
    fn invalid_branch(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    // Basically to test if the location is correct after parsing an expression.
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
    fn multiple_tokens(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}
