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
fn success(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Success cases: right associativity.
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
fn right_assoc(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Success cases: priority.
#[rstest]
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
#[case::plus_prioritized_over_plus_equals(
    // Represents `a += 1 + 2`, and expects to be parsed into `a += (1 + 2)`.
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
        mktoken!(str_loc!("a += 1 ", "+"),
            TokenKind::Plus,
        ),
        mktoken!(str_loc!("a += 1 + ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "a += 1 + 2"), vec![
        mkast!(infix InfixPlusEquals, loc str_loc!("", "a += 1 + 2"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixPlus, loc str_loc!("a += ", "1 + 2"),
                left mkast!(num 1.0, loc str_loc!("a += ", "1")),
                right mkast!(num 2.0, loc str_loc!("a += 1 + ", "2")),
            ),
        ),
    ])
)]
#[case::plus_prioritized_over_minus_equals(
    // Represents `a -= 1 + 2`, and expects to be parsed into `a -= (1 + 2)`.
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
        mktoken!(str_loc!("a -= 1 ", "+"),
            TokenKind::Plus,
        ),
        mktoken!(str_loc!("a -= 1 + ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "a -= 1 + 2"), vec![
        mkast!(infix InfixMinusEquals, loc str_loc!("", "a -= 1 + 2"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixPlus, loc str_loc!("a -= ", "1 + 2"),
                left mkast!(num 1.0, loc str_loc!("a -= ", "1")),
                right mkast!(num 2.0, loc str_loc!("a -= 1 + ", "2")),
            ),
        ),
    ])
)]
#[case::plus_prioritized_over_asterisk_equals(
    // Represents `a *= 1 + 2`, and expects to be parsed into `a *= (1 + 2)`.
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
        mktoken!(str_loc!("a *= 1 ", "+"),
            TokenKind::Plus,
        ),
        mktoken!(str_loc!("a *= 1 + ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "a *= 1 + 2"), vec![
        mkast!(infix InfixAsteriskEquals, loc str_loc!("", "a *= 1 + 2"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixPlus, loc str_loc!("a *= ", "1 + 2"),
                left mkast!(num 1.0, loc str_loc!("a *= ", "1")),
                right mkast!(num 2.0, loc str_loc!("a *= 1 + ", "2")),
            ),
        ),
    ])
)]
#[case::plus_prioritized_over_slash_equals(
    // Represents `a /= 1 + 2`, and expects to be parsed into `a /= (1 + 2)`.
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
        mktoken!(str_loc!("a /= 1 ", "+"),
            TokenKind::Plus,
        ),
        mktoken!(str_loc!("a /= 1 + ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "a /= 1 + 2"), vec![
        mkast!(infix InfixSlashEquals, loc str_loc!("", "a /= 1 + 2"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixPlus, loc str_loc!("a /= ", "1 + 2"),
                left mkast!(num 1.0, loc str_loc!("a /= ", "1")),
                right mkast!(num 2.0, loc str_loc!("a /= 1 + ", "2")),
            ),
        ),
    ])
)]
#[case::plus_prioritized_over_percent_equals(
    // Represents `a %= 1 + 2`, and expects to be parsed into `a %= (1 + 2)`.
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
        mktoken!(str_loc!("a %= 1 ", "+"),
            TokenKind::Plus,
        ),
        mktoken!(str_loc!("a %= 1 + ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "a %= 1 + 2"), vec![
        mkast!(infix InfixPercentEquals, loc str_loc!("", "a %= 1 + 2"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixPlus, loc str_loc!("a %= ", "1 + 2"),
                left mkast!(num 1.0, loc str_loc!("a %= ", "1")),
                right mkast!(num 2.0, loc str_loc!("a %= 1 + ", "2")),
            ),
        ),
    ])
)]
fn priority(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Failure cases: an infix without the left operand.
#[rstest]
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
fn fail_without_left(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}

/// Failure cases: an infix without the right operand.
#[rstest]
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
fn fail_without_right(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}

/// Failure cases: a single infix, with no operands.
#[rstest]
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
fn fail_single(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}
