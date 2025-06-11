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
    // Represents `1 <= 1`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", "<="),
            TokenKind::LBracketEquals,
        ),
        mktoken!(str_loc!("1 <= ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "1 <= 2"), vec![
        mkast!(infix InfixLBracketEquals, loc str_loc!("", "1 <= 2"),
            left mkast!(num 1.0, loc str_loc!("", "1")),
            right mkast!(num 2.0, loc str_loc!("1 <= ", "2")),
        ),
    ])
)]
#[case::rbracket_equals(
    // Represents `1 >= 1`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", ">="),
            TokenKind::RBracketEquals,
        ),
        mktoken!(str_loc!("1 >= ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "1 >= 2"), vec![
        mkast!(infix InfixRBracketEquals, loc str_loc!("", "1 >= 2"),
            left mkast!(num 1.0, loc str_loc!("", "1")),
            right mkast!(num 2.0, loc str_loc!("1 >= ", "2")),
        ),
    ])
)]
#[case::lbracket(
    // Represents `1 < 1`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", "<"),
            TokenKind::LBracket,
        ),
        mktoken!(str_loc!("1 < ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "1 < 2"), vec![
        mkast!(infix InfixLBracket, loc str_loc!("", "1 < 2"),
            left mkast!(num 1.0, loc str_loc!("", "1")),
            right mkast!(num 2.0, loc str_loc!("1 < ", "2")),
        ),
    ])
)]
#[case::rbracket(
    // Represents `1 > 1`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", ">"),
            TokenKind::RBracket,
        ),
        mktoken!(str_loc!("1 > ", "2"),
            TokenKind::Number(2.0),
        ),
    ],
    mkast!(prog loc str_loc!("", "1 > 2"), vec![
        mkast!(infix InfixRBracket, loc str_loc!("", "1 > 2"),
            left mkast!(num 1.0, loc str_loc!("", "1")),
            right mkast!(num 2.0, loc str_loc!("1 > ", "2")),
        ),
    ])
)]
fn success(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Success cases: left associativity.
#[rstest]
#[case::two_double_equals(
    // Represents `a == b == c`, and expects to be parsed into `(a == b) == c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "=="),
            TokenKind::DoubleEquals,
        ),
        mktoken!(str_loc!("a == ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a == b ", "=="),
            TokenKind::DoubleEquals,
        ),
        mktoken!(str_loc!("a == b == ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a == b == c"), vec![
        mkast!(infix InfixDoubleEquals, loc str_loc!("", "a == b == c"),
            left mkast!(infix InfixDoubleEquals, loc str_loc!("", "a == b"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(identifier "b", loc str_loc!("a == ", "b")),
            ),
            right mkast!(identifier "c", loc str_loc!("a == b == ", "c")),
        ),
    ])
)]
#[case::two_bang_equals(
    // Represents `a != b != c`, and expects to be parsed into `(a != b) != c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "!="),
            TokenKind::BangEquals,
        ),
        mktoken!(str_loc!("a != ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a != b ", "!="),
            TokenKind::BangEquals,
        ),
        mktoken!(str_loc!("a != b != ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a != b != c"), vec![
        mkast!(infix InfixBangEquals, loc str_loc!("", "a != b != c"),
            left mkast!(infix InfixBangEquals, loc str_loc!("", "a != b"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(identifier "b", loc str_loc!("a != ", "b")),
            ),
            right mkast!(identifier "c", loc str_loc!("a != b != ", "c")),
        ),
    ])
)]
#[case::two_lbracket_equals(
    // Represents `a <= b <= c`, and expects to be parsed into `(a <= b) <= c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "<="),
            TokenKind::LBracketEquals,
        ),
        mktoken!(str_loc!("a <= ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a <= b ", "<="),
            TokenKind::LBracketEquals,
        ),
        mktoken!(str_loc!("a <= b <= ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a <= b <= c"), vec![
        mkast!(infix InfixLBracketEquals, loc str_loc!("", "a <= b <= c"),
            left mkast!(infix InfixLBracketEquals, loc str_loc!("", "a <= b"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(identifier "b", loc str_loc!("a <= ", "b")),
            ),
            right mkast!(identifier "c", loc str_loc!("a <= b <= ", "c")),
        ),
    ])
)]
#[case::two_rbracket_equals(
    // Represents `a >= b >= c`, and expects to be parsed into `(a >= b) >= c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", ">="),
            TokenKind::RBracketEquals,
        ),
        mktoken!(str_loc!("a >= ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a >= b ", ">="),
            TokenKind::RBracketEquals,
        ),
        mktoken!(str_loc!("a >= b >= ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a >= b >= c"), vec![
        mkast!(infix InfixRBracketEquals, loc str_loc!("", "a >= b >= c"),
            left mkast!(infix InfixRBracketEquals, loc str_loc!("", "a >= b"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(identifier "b", loc str_loc!("a >= ", "b")),
            ),
            right mkast!(identifier "c", loc str_loc!("a >= b >= ", "c")),
        ),
    ])
)]
#[case::two_lbrackets(
    // Represents `a < b < c`, and expects to be parsed into `(a < b) < c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "<"),
            TokenKind::LBracket,
        ),
        mktoken!(str_loc!("a < ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a < b ", "<"),
            TokenKind::LBracket,
        ),
        mktoken!(str_loc!("a < b < ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a < b < c"), vec![
        mkast!(infix InfixLBracket, loc str_loc!("", "a < b < c"),
            left mkast!(infix InfixLBracket, loc str_loc!("", "a < b"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(identifier "b", loc str_loc!("a < ", "b")),
            ),
            right mkast!(identifier "c", loc str_loc!("a < b < ", "c")),
        ),
    ])
)]
#[case::two_rbrackets(
    // Represents `a > b > c`, and expects to be parsed into `(a > b) > c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", ">"),
            TokenKind::RBracket,
        ),
        mktoken!(str_loc!("a > ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a > b ", ">"),
            TokenKind::RBracket,
        ),
        mktoken!(str_loc!("a > b > ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a > b > c"), vec![
        mkast!(infix InfixRBracket, loc str_loc!("", "a > b > c"),
            left mkast!(infix InfixRBracket, loc str_loc!("", "a > b"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(identifier "b", loc str_loc!("a > ", "b")),
            ),
            right mkast!(identifier "c", loc str_loc!("a > b > ", "c")),
        ),
    ])
)]
fn left_assoc(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Success cases: priority.
#[rstest]
#[case::double_equals_prioritized_over_conjunct(
    // Represents `a 그리고 b == c`, and expects to be parsed into `a 그리고 b == c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "그리고"),
            TokenKind::Conjunct,
        ),
        mktoken!(str_loc!("a 그리고 ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a 그리고 b ", "=="),
            TokenKind::DoubleEquals,
        ),
        mktoken!(str_loc!("a 그리고 b == ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a 그리고 b == c"), vec![
        mkast!(infix InfixConjunct, loc str_loc!("", "a 그리고 b == c"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixDoubleEquals, loc str_loc!("a 그리고 ", "b == c"),
                left mkast!(identifier "b", loc str_loc!("a 그리고 ", "b")),
                right mkast!(identifier "c", loc str_loc!("a 그리고 b == ", "c")),
            ),
        ),
    ])
)]
#[case::bang_equals_prioritized_over_conjunct(
    // Represents `a 그리고 b != c`, and expects to be parsed into `a 그리고 b != c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "그리고"),
            TokenKind::Conjunct,
        ),
        mktoken!(str_loc!("a 그리고 ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a 그리고 b ", "!="),
            TokenKind::BangEquals,
        ),
        mktoken!(str_loc!("a 그리고 b != ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a 그리고 b != c"), vec![
        mkast!(infix InfixConjunct, loc str_loc!("", "a 그리고 b != c"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixBangEquals, loc str_loc!("a 그리고 ", "b != c"),
                left mkast!(identifier "b", loc str_loc!("a 그리고 ", "b")),
                right mkast!(identifier "c", loc str_loc!("a 그리고 b != ", "c")),
            ),
        ),
    ])
)]
#[case::lbracket_equals_prioritized_over_conjunct(
    // Represents `a 그리고 b <= c`, and expects to be parsed into `a 그리고 b <= c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "그리고"),
            TokenKind::Conjunct,
        ),
        mktoken!(str_loc!("a 그리고 ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a 그리고 b ", "<="),
            TokenKind::LBracketEquals,
        ),
        mktoken!(str_loc!("a 그리고 b <= ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a 그리고 b <= c"), vec![
        mkast!(infix InfixConjunct, loc str_loc!("", "a 그리고 b <= c"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixLBracketEquals, loc str_loc!("a 그리고 ", "b <= c"),
                left mkast!(identifier "b", loc str_loc!("a 그리고 ", "b")),
                right mkast!(identifier "c", loc str_loc!("a 그리고 b <= ", "c")),
            ),
        ),
    ])
)]
#[case::rbracket_equals_prioritized_over_conjunct(
    // Represents `a 그리고 b >= c`, and expects to be parsed into `a 그리고 b >= c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "그리고"),
            TokenKind::Conjunct,
        ),
        mktoken!(str_loc!("a 그리고 ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a 그리고 b ", ">="),
            TokenKind::RBracketEquals,
        ),
        mktoken!(str_loc!("a 그리고 b >= ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a 그리고 b >= c"), vec![
        mkast!(infix InfixConjunct, loc str_loc!("", "a 그리고 b >= c"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixRBracketEquals, loc str_loc!("a 그리고 ", "b >= c"),
                left mkast!(identifier "b", loc str_loc!("a 그리고 ", "b")),
                right mkast!(identifier "c", loc str_loc!("a 그리고 b >= ", "c")),
            ),
        ),
    ])
)]
#[case::lbracket_prioritized_over_conjunct(
    // Represents `a 그리고 b < c`, and expects to be parsed into `a 그리고 b < c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "그리고"),
            TokenKind::Conjunct,
        ),
        mktoken!(str_loc!("a 그리고 ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a 그리고 b ", "<"),
            TokenKind::LBracket,
        ),
        mktoken!(str_loc!("a 그리고 b < ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a 그리고 b < c"), vec![
        mkast!(infix InfixConjunct, loc str_loc!("", "a 그리고 b < c"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixLBracket, loc str_loc!("a 그리고 ", "b < c"),
                left mkast!(identifier "b", loc str_loc!("a 그리고 ", "b")),
                right mkast!(identifier "c", loc str_loc!("a 그리고 b < ", "c")),
            ),
        ),
    ])
)]
#[case::rbracket_prioritized_over_conjunct(
    // Represents `a 그리고 b > c`, and expects to be parsed into `a 그리고 b > c`.
    vec![
        mktoken!(str_loc!("", "a"),
            TokenKind::Identifier(String::from("a")),
        ),
        mktoken!(str_loc!("a ", "그리고"),
            TokenKind::Conjunct,
        ),
        mktoken!(str_loc!("a 그리고 ", "b"),
            TokenKind::Identifier(String::from("b")),
        ),
        mktoken!(str_loc!("a 그리고 b ", ">"),
            TokenKind::RBracket,
        ),
        mktoken!(str_loc!("a 그리고 b > ", "c"),
            TokenKind::Identifier(String::from("c")),
        ),
    ],
    mkast!(prog loc str_loc!("", "a 그리고 b > c"), vec![
        mkast!(infix InfixConjunct, loc str_loc!("", "a 그리고 b > c"),
            left mkast!(identifier "a", loc str_loc!("", "a")),
            right mkast!(infix InfixRBracket, loc str_loc!("a 그리고 ", "b > c"),
                left mkast!(identifier "b", loc str_loc!("a 그리고 ", "b")),
                right mkast!(identifier "c", loc str_loc!("a 그리고 b > ", "c")),
            ),
        ),
    ])
)]
fn priority(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
    assert_parse!(&tokens, expected);
}

/// Failure cases: an infix without the left operand.
#[rstest]
#[case::double_equals(
    // Represents `== 1`.
    vec![
        mktoken!(str_loc!("", "=="),
            TokenKind::DoubleEquals,
        ),
        mktoken!(str_loc!("== ", "1"),
            TokenKind::Number(1.0),
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", "==")),
)]
#[case::bang_equals(
    // Represents `!= 1`.
    vec![
        mktoken!(str_loc!("", "!="),
            TokenKind::BangEquals,
        ),
        mktoken!(str_loc!("!= ", "1"),
            TokenKind::Number(1.0),
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", "!=")),
)]
#[case::lbracket_equals(
    // Represents `<= 1`.
    vec![
        mktoken!(str_loc!("", "<="),
            TokenKind::LBracketEquals,
        ),
        mktoken!(str_loc!("<= ", "1"),
            TokenKind::Number(1.0),
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", "<=")),
)]
#[case::rbracket_equals(
    // Represents `>= 1`.
    vec![
        mktoken!(str_loc!("", ">="),
            TokenKind::RBracketEquals,
        ),
        mktoken!(str_loc!(">= ", "1"),
            TokenKind::Number(1.0),
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", ">=")),
)]
#[case::lbracket(
    // Represents `< 1`.
    vec![
        mktoken!(str_loc!("", "<"),
            TokenKind::LBracket,
        ),
        mktoken!(str_loc!("< ", "1"),
            TokenKind::Number(1.0),
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", "<")),
)]
#[case::rbracket(
    // Represents `> 1`.
    vec![
        mktoken!(str_loc!("", ">"),
            TokenKind::RBracket,
        ),
        mktoken!(str_loc!("> ", "1"),
            TokenKind::Number(1.0),
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", ">")),
)]
fn fail_without_left(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}

/// Failure cases: an infix without the right operand.
#[rstest]
#[case::double_equals(
    // Represents `1 ==`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", "=="),
            TokenKind::DoubleEquals,
        ),
    ],
    mkerr!(NoInfixRightOperand, str_loc!("", "1 ==")),
)]
#[case::bang_equals(
    // Represents `1 !=`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", "!="),
            TokenKind::BangEquals,
        ),
    ],
    mkerr!(NoInfixRightOperand, str_loc!("", "1 !=")),
)]
#[case::lbracket_equals(
    // Represents `1 <=`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", "<="),
            TokenKind::LBracketEquals,
        ),
    ],
    mkerr!(NoInfixRightOperand, str_loc!("", "1 <=")),
)]
#[case::rbracket_equals(
    // Represents `1 >=`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", ">="),
            TokenKind::RBracketEquals,
        ),
    ],
    mkerr!(NoInfixRightOperand, str_loc!("", "1 >=")),
)]
#[case::lbracket(
    // Represents `1 <`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", "<"),
            TokenKind::LBracket,
        ),
    ],
    mkerr!(NoInfixRightOperand, str_loc!("", "1 <")),
)]
#[case::rbracket(
    // Represents `1 >`.
    vec![
        mktoken!(str_loc!("", "1"),
            TokenKind::Number(1.0),
        ),
        mktoken!(str_loc!("1 ", ">"),
            TokenKind::RBracket,
        ),
    ],
    mkerr!(NoInfixRightOperand, str_loc!("", "1 >")),
)]
fn fail_without_right(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}

/// Failure cases: an infix without the right operand.
#[rstest]
#[case::double_equals(
    // Represents `==`.
    vec![
        mktoken!(str_loc!("", "=="),
            TokenKind::DoubleEquals,
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", "==")),
)]
#[case::bang_equals(
    // Represents `!=`.
    vec![
        mktoken!(str_loc!("", "!="),
            TokenKind::BangEquals,
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", "!=")),
)]
#[case::lbracket_equals(
    // Represents `<=`.
    vec![
        mktoken!(str_loc!("", "<="),
            TokenKind::LBracketEquals,
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", "<=")),
)]
#[case::rbracket_equals(
    // Represents `>=`.
    vec![
        mktoken!(str_loc!("", ">="),
            TokenKind::RBracketEquals,
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", ">=")),
)]
#[case::lbracket(
    // Represents `<`.
    vec![
        mktoken!(str_loc!("", "<"),
            TokenKind::LBracket,
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", "<")),
)]
#[case::rbracket(
    // Represents `>`.
    vec![
        mktoken!(str_loc!("", ">"),
            TokenKind::RBracket,
        ),
    ],
    mkerr!(InvalidExprStart, str_loc!("", ">")),
)]
fn fail_single(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
    assert_parse_fail!(&tokens, error);
}
