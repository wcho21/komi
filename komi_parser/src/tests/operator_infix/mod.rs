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
fn success(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Success cases: left associativity.
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
fn left_assoc(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Success cases: priority.
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
#[case::slash_prioritized_over_plus(
    // Represents `1 + 2 / 3`, and expects to be parsed into `1 + (2 / 3)`.
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
        mktoken!(str_loc!("1 + 2 ", "/"),
            TokenKind::Slash,
        ),
        mktoken!(str_loc!("1 + 2 / ", "3"),
            TokenKind::Number(3.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "1 + 2 / 3"), vec![
        mkast!(infix InfixPlus, loc str_loc!("", "1 + 2 / 3"),
            left mkast!(num 1.0, loc str_loc!("", "1")),
            right mkast!(infix InfixSlash, loc str_loc!("1 + ", "2 / 3"),
                left mkast!(num 2.0, loc str_loc!("1 + ", "2")),
                right mkast!(num 3.0, loc str_loc!("1 + 2 / ", "3")),
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
fn priority(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Failure cases: an infix without the left operand.
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
fn fail_without_left(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}

/// Failure cases: an infix without the right operand.
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
fn fail_without_right(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}

/// Failure cases: a single infix, with no operands.
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
fn fail_single(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}
