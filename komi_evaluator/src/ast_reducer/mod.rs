mod assignment_infix;
mod combinator_infix;
mod expressions;
mod leaf;
mod prefix;
mod util;

use crate::environment::Environment;
use crate::err::EvalError;
use assignment_infix as assign_infix;
use combinator_infix as comb_infix;
use komi_syntax::{Ast, AstKind, Value};

type ResVal = Result<Value, EvalError>;

pub fn reduce_ast(ast: &Box<Ast>, env: &mut Environment) -> ResVal {
    // Design principle: once you read something, pass it as an argument.
    // This avoids unnecessary repeated reading in subfunctions.
    // Moreover, if you delay determining the kind of what you read, the decision is only postponed to subfunctions.
    // You'll have to handle exceptional cases that could have been avoided.

    let loc = ast.location;
    match &ast.kind {
        AstKind::Program { expressions: e } => expressions::reduce(&e, &loc, env),
        AstKind::Identifier(id) => leaf::evaluate_identifier(id, &loc, env),
        AstKind::Number(n) => leaf::evaluate_num(*n, &loc),
        AstKind::Bool(b) => leaf::evaluate_bool(*b, &loc),
        AstKind::Closure { parameters: p, body: b } => leaf::evaluate_closure(p, b, &loc, env),
        AstKind::PrefixPlus { operand: op } => prefix::reduce_plus(&op, &loc, env),
        AstKind::PrefixMinus { operand: op } => prefix::reduce_minus(&op, &loc, env),
        AstKind::PrefixBang { operand: op } => prefix::reduce_bang(&op, &loc, env),
        AstKind::InfixPlus { left: l, right: r } => comb_infix::reduce_plus(&l, &r, &loc, env),
        AstKind::InfixMinus { left: l, right: r } => comb_infix::reduce_minus(&l, &r, &loc, env),
        AstKind::InfixAsterisk { left: l, right: r } => comb_infix::reduce_asterisk(&l, &r, &loc, env),
        AstKind::InfixSlash { left: l, right: r } => comb_infix::reduce_slash(&l, &r, &loc, env),
        AstKind::InfixPercent { left: l, right: r } => comb_infix::reduce_percent(&l, &r, &loc, env),
        AstKind::InfixConjunct { left: l, right: r } => comb_infix::reduce_conjunct(&l, &r, &loc, env),
        AstKind::InfixDisjunct { left: l, right: r } => comb_infix::reduce_disjunct(&l, &r, &loc, env),
        AstKind::InfixEquals { left: l, right: r } => assign_infix::reduce_equals(&l, &r, &loc, env),
        AstKind::InfixPlusEquals { left: l, right: r } => assign_infix::reduce_plus_equals(&l, &r, &loc, env),
        AstKind::InfixMinusEquals { left: l, right: r } => assign_infix::reduce_minus_equals(&l, &r, &loc, env),
        AstKind::InfixAsteriskEquals { left: l, right: r } => assign_infix::reduce_asterisk_equals(&l, &r, &loc, env),
        AstKind::InfixSlashEquals { left: l, right: r } => assign_infix::reduce_slash_equals(&l, &r, &loc, env),
        AstKind::InfixPercentEquals { left: l, right: r } => assign_infix::reduce_percent_equals(&l, &r, &loc, env),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::{Ast, Environment, EvalError, Value, reduce_ast};
    use crate::EvalErrorKind;
    use fixtures::*;
    use komi_syntax::{AstKind, ValueKind, mkast};
    use komi_util::Range;
    use rstest::rstest;

    /// Asserts a given AST to be evaluated into the expected value.
    /// Helps write a test declaratively.
    macro_rules! assert_eval {
        ($ast:expr, $expected:expr $(,)?) => {
            let mut env = Environment::new();
            assert_eq!(
                reduce_ast($ast, &mut env),
                Ok($expected),
                "received a value (left) evaluated from the ast, but expected the different value (right)",
            );
        };
        ($ast:expr, $env:expr, $expected:expr $(,)?) => {
            assert_eq!(
                reduce_ast($ast, $env),
                Ok($expected),
                "received a value (left) evaluated from the ast, but expected the different value (right)",
            );
        };
    }

    /// Asserts evaluating a given AST will fail.
    /// Helps write a test declaratively.
    macro_rules! assert_eval_fail {
        ($ast:expr, $expected:expr $(,)?) => {
            let mut env = Environment::new();
            assert_eq!(
                reduce_ast($ast, &mut env),
                Err($expected),
                "received a result (left), but expected an error (right)",
            );
        };
        ($ast:expr, $env:expr, $expected:expr $(,)?) => {
            assert_eq!(
                reduce_ast($ast, $env),
                Err($expected),
                "received a result (left), but expected an error (right)",
            );
        };
    }

    #[test]
    fn empty() {
        // Represents ``.
        assert_eval!(
            &mkast!(prog loc 0, 0, 0, 0, vec![]),
            Value::new(ValueKind::Empty, Range::from_nums(0, 0, 0, 0))
        );
    }

    mod identifier {
        use super::*;

        #[rstest]
        #[case::num(
            // Represents `사과`.
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(identifier "사과", loc 0, 0, 0, 2),
            ]),
            // Represents a binding for `사과` to `1.0`.
            root_env("사과", &Value::from_num(1.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(1.0, Range::from_nums(0, 0, 0, 2)),
        )]
        #[case::bool(
            // Represents `사과`.
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(identifier "사과", loc 0, 0, 0, 2),
            ]),
            // Represents a binding for `사과` to `참`.
            root_env("사과", &Value::from_bool(true, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_bool(true, Range::from_nums(0, 0, 0, 2)),
        )]
        fn single_identifier(#[case] ast: Box<Ast>, #[case] mut env: Environment, #[case] expected: Value) {
            assert_eval!(&ast, &mut env, expected);
        }

        #[rstest]
        #[case::undefined(
            // Represents `사과`.
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(identifier "사과", loc 0, 0, 0, 2),
            ]),
            EvalError::new(EvalErrorKind::UndefinedIdentifier, Range::from_nums(0, 0, 0, 2)),
        )]
        fn single_undefined_identifier(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }
    }

    mod leaf {
        use super::*;

        #[rstest]
        #[case::num(
            // Represents `1`.
            mkast!(prog loc 0, 0, 0, 1, vec![
                mkast!(num 1.0, loc 0, 0, 0, 1),
            ]),
            Value::from_num(1.0, Range::from_nums(0, 0, 0, 1))
        )]
        #[case::bool(
            // Represents `참`.
            mkast!(prog loc 0, 0, 0, 1, vec![
                mkast!(boolean true, loc 0, 0, 0, 1),
            ]),
            Value::from_bool(true, Range::from_nums(0, 0, 0, 1))
        )]
        #[case::closure(
            // Represents `함수 사과, 오렌지, 바나나 { 1 2 3 }`.
            mkast!(prog loc 0, 0, 0, 25, vec![
                mkast!(closure loc 0, 0, 0, 25,
                    param vec![String::from("사과"), String::from("오렌지"), String::from("바나나")],
                    body vec![
                        mkast!(num 1.0, loc 0, 18, 0, 19),
                        mkast!(num 2.0, loc 0, 20, 0, 21),
                        mkast!(num 3.0, loc 0, 22, 0, 23),
                    ],
                ),
            ]),
            Value::new(ValueKind::Closure {
                parameters: vec![String::from("사과"), String::from("오렌지"), String::from("바나나")],
                body: vec![
                    mkast!(num 1.0, loc 0, 18, 0, 19),
                    mkast!(num 2.0, loc 0, 20, 0, 21),
                    mkast!(num 3.0, loc 0, 22, 0, 23),
                ],
                env: Environment::new()
            }, Range::from_nums(0, 0, 0, 25))
        )]
        fn single(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }
    }

    mod prefix {
        use super::*;

        #[rstest]
        #[case::plus_prefix(
            // Represents `+1`
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(prefix PrefixPlus, loc 0, 0, 0, 2,
                    operand mkast!(num 1.0, loc 0, 1, 0, 2),
                ),
            ]),
            Value::from_num(1.0, Range::from_nums(0, 0, 0, 2))
        )]
        #[case::minus_prefix(
            // Represents `-1`
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(prefix PrefixMinus, loc 0, 0, 0, 2,
                    operand mkast!(num 1.0, loc 0, 1, 0, 2),
                ),
            ]),
            Value::from_num(-1.0, Range::from_nums(0, 0, 0, 2))
        )]
        #[case::two_plus_prefixes(
            // Represents `++1`
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(prefix PrefixPlus, loc 0, 0, 0, 3,
                    operand mkast!(prefix PrefixPlus, loc 0, 1, 0, 3,
                        operand mkast!(num 1.0, loc 0, 2, 0, 3),
                    ),
                ),
            ]),
            Value::from_num(1.0, Range::from_nums(0, 0, 0, 3))
        )]
        #[case::two_minus_prefixes(
            // Represents `--1`
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(prefix PrefixMinus, loc 0, 0, 0, 3,
                    operand mkast!(prefix PrefixMinus, loc 0, 1, 0, 3,
                        operand mkast!(num 1.0, loc 0, 2, 0, 3),
                    ),
                ),
            ]),
            Value::from_num(1.0, Range::from_nums(0, 0, 0, 3))
        )]
        fn num_prefix(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::bang_bool(
            // Represents `!참`
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(prefix PrefixBang, loc 0, 0, 0, 2,
                    operand mkast!(boolean true, loc 0, 1, 0, 2),
                ),
            ]),
            Value::from_bool(false, Range::from_nums(0, 0, 0, 2))
        )]
        #[case::two_bangs_bool(
            // Represents `!!참`
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(prefix PrefixBang, loc 0, 0, 0, 3,
                    operand mkast!(prefix PrefixBang, loc 0, 1, 0, 3,
                        operand mkast!(boolean true, loc 0, 2, 0, 3),
                    ),
                ),
            ]),
            Value::from_bool(true, Range::from_nums(0, 0, 0, 3))
        )]
        fn bool_prefix(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::plus_bool(
            // Represents `+참`
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(prefix PrefixPlus, loc 0, 0, 0, 2,
                    operand mkast!(boolean true, loc 0, 1, 0, 2),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumPrefixOperand, Range::from_nums(0, 1, 0, 2)),
        )]
        #[case::minus_bool(
            // Represents `-참`
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(prefix PrefixMinus, loc 0, 0, 0, 2,
                    operand mkast!(boolean true, loc 0, 1, 0, 2),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumPrefixOperand, Range::from_nums(0, 1, 0, 2)),
        )]
        #[case::bang_num(
            // Represents `!1`
            mkast!(prog loc 0, 0, 0, 2, vec![
                mkast!(prefix PrefixBang, loc 0, 0, 0, 2,
                    operand mkast!(num 1.0, loc 0, 1, 0, 2),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidBoolPrefixOperand, Range::from_nums(0, 1, 0, 2)),
        )]
        fn wrong_type_prefix(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }
    }

    mod combinator_infix {
        use super::*;

        #[rstest]
        #[case::addition(
            // Represents `6+4`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixPlus, loc 0, 0, 0, 3,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(num 4.0, loc 0, 2, 0, 3),
                ),
            ]),
            Value::from_num(10.0, Range::from_nums(0, 0, 0, 3))
        )]
        #[case::subtraction(
            // Represents `6-4`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixMinus, loc 0, 0, 0, 3,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(num 4.0, loc 0, 2, 0, 3),
                ),
            ]),
            Value::from_num(2.0, Range::from_nums(0, 0, 0, 3))
        )]
        #[case::multiplication(
            // Represents `6*4`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(num 4.0, loc 0, 2, 0, 3),
                ),
            ]),
            Value::from_num(24.0, Range::from_nums(0, 0, 0, 3))
        )]
        #[case::division(
            // Represents `6/4`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixSlash, loc 0, 0, 0, 3,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(num 4.0, loc 0, 2, 0, 3),
                ),
            ]),
            Value::from_num(1.5, Range::from_nums(0, 0, 0, 3))
        )]
        #[case::modular(
            // Represents `6%4`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixPercent, loc 0, 0, 0, 3,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(num 4.0, loc 0, 2, 0, 3),
                ),
            ]),
            Value::from_num(2.0, Range::from_nums(0, 0, 0, 3))
        )]
        fn arithmetic_infix(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::five_kinds(
            // Represents `9*8%7-6+5/4`, which is parsed into `(((9*8)%7)-6)+(5/4)`.
            // Note that the associativity of an expression is determined in the parsing step, as represented in the AST result.
            mkast!(prog loc 0, 0, 0, 11, vec![
                mkast!(infix InfixPlus, loc 0, 0, 0, 11,
                    left mkast!(infix InfixMinus, loc 0, 0, 0, 7,
                        left mkast!(infix InfixPercent, loc 0, 0, 0, 5,
                            left mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                                left mkast!(num 9.0, loc 0, 0, 0, 1),
                                right mkast!(num 8.0, loc 0, 2, 0, 3),
                            ),
                            right mkast!(num 7.0, loc 0, 4, 0, 5),
                        ),
                        right mkast!(num 6.0, loc 0, 6, 0, 7),
                    ),
                    right mkast!(infix InfixSlash, loc 0, 8, 0, 11,
                        left mkast!(num 5.0, loc 0, 8, 0, 9),
                        right mkast!(num 4.0, loc 0, 10, 0, 11),
                    ),
                ),
            ]),
            Value::from_num(-2.75, Range::from_nums(0, 0, 0, 11))
        )]
        fn arithmetic_compound(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::conjunction_on_true_true(
            // Represents `참 그리고 참`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 7,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
            ]),
            Value::from_bool(true, Range::from_nums(0, 0, 0, 7))
        )]
        #[case::conjunction_on_true_false(
            // Represents `참 그리고 거짓`.
            mkast!(prog loc 0, 0, 0, 8, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 8,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(boolean false, loc 0, 6, 0, 8),
                ),
            ]),
            Value::from_bool(false, Range::from_nums(0, 0, 0, 8))
        )]
        #[case::conjunction_on_false_true(
            // Represents `거짓 그리고 참`.
            mkast!(prog loc 0, 0, 0, 8, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 8,
                    left mkast!(boolean false, loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 7, 0, 8),
                ),
            ]),
            Value::from_bool(false, Range::from_nums(0, 0, 0, 8))
        )]
        #[case::conjunction_on_false_false(
            // Represents `거짓 그리고 거짓`.
            mkast!(prog loc 0, 0, 0, 9, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 9,
                    left mkast!(boolean false, loc 0, 0, 0, 2),
                    right mkast!(boolean false, loc 0, 7, 0, 9),
                ),
            ]),
            Value::from_bool(false, Range::from_nums(0, 0, 0, 9))
        )]
        #[case::disjunction_on_true_true(
            // Represents `참 또는 참`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixDisjunct, loc 0, 0, 0, 6,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 5, 0, 6),
                ),
            ]),
            Value::from_bool(true, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::disjunction_on_true_false(
            // Represents `참 또는 거짓`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixDisjunct, loc 0, 0, 0, 7,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(boolean false, loc 0, 5, 0, 7),
                ),
            ]),
            Value::from_bool(true, Range::from_nums(0, 0, 0, 7))
        )]
        #[case::disjunction_on_false_true(
            // Represents `거짓 또는 참`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixDisjunct, loc 0, 0, 0, 7,
                    left mkast!(boolean false, loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
            ]),
            Value::from_bool(true, Range::from_nums(0, 0, 0, 7))
        )]
        #[case::disjunction_on_false_false(
            // Represents `거짓 또는 거짓`.
            mkast!(prog loc 0, 0, 0, 8, vec![
                mkast!(infix InfixDisjunct, loc 0, 0, 0, 8,
                    left mkast!(boolean false, loc 0, 0, 0, 2),
                    right mkast!(boolean false, loc 0, 6, 0, 8),
                ),
            ]),
            Value::from_bool(false, Range::from_nums(0, 0, 0, 8))
        )]
        fn connective_infix(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::left_bool_addition(
            // Represents `참+1`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixPlus, loc 0, 0, 0, 3,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(num 1.0, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 0, 0, 1)),
        )]
        #[case::right_bool_addition(
            // Represents `1+참`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixPlus, loc 0, 0, 0, 3,
                    left mkast!(num 1.0, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 2, 0, 3)),
        )]
        #[case::left_bool_subtraction(
            // Represents `참-1`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixMinus, loc 0, 0, 0, 3,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(num 1.0, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 0, 0, 1)),
        )]
        #[case::right_bool_subtraction(
            // Represents `1-참`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixMinus, loc 0, 0, 0, 3,
                    left mkast!(num 1.0, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 2, 0, 3)),
        )]
        #[case::left_bool_multiplication(
            // Represents `참*1`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(num 1.0, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 0, 0, 1)),
        )]
        #[case::right_bool_multiplication(
            // Represents `1*참`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                    left mkast!(num 1.0, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 2, 0, 3)),
        )]
        #[case::left_bool_division(
            // Represents `참/1`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixSlash, loc 0, 0, 0, 3,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(num 1.0, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 0, 0, 1)),
        )]
        #[case::right_bool_division(
            // Represents `1/참`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixSlash, loc 0, 0, 0, 3,
                    left mkast!(num 1.0, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 2, 0, 3)),
        )]
        #[case::left_bool_modular(
            // Represents `참%1`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixPercent, loc 0, 0, 0, 3,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(num 1.0, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 0, 0, 1)),
        )]
        #[case::right_bool_modular(
            // Represents `1%참`.
            mkast!(prog loc 0, 0, 0, 3, vec![
                mkast!(infix InfixPercent, loc 0, 0, 0, 3,
                    left mkast!(num 1.0, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 2, 0, 3),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 2, 0, 3)),
        )]
        fn arithmetic_infix_with_wrong_type_operand(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }

        #[rstest]
        #[case::left_num_conjunction(
            // Represents `1 그리고 참`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 7,
                    left mkast!(num 1.0, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidBoolInfixOperand, Range::from_nums(0, 0, 0, 1)),
        )]
        #[case::right_num_conjunction(
            // Represents `참 그리고 1`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 7,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(num 1.0, loc 0, 6, 0, 7),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidBoolInfixOperand, Range::from_nums(0, 6, 0, 7)),
        )]
        #[case::left_num_disjunction(
            // Represents `1 또는 참`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixDisjunct, loc 0, 0, 0, 6,
                    left mkast!(num 1.0, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 5, 0, 6),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidBoolInfixOperand, Range::from_nums(0, 0, 0, 1)),
        )]
        #[case::right_num_disjunction(
            // Represents `참 또는 1`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 6,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(num 1.0, loc 0, 5, 0, 6),
                ),
            ]),
            EvalError::new(EvalErrorKind::InvalidBoolInfixOperand, Range::from_nums(0, 5, 0, 6)),
        )]
        fn connective_infix_with_wrong_type_operand(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }
    }

    mod assignment_infix {
        use super::*;

        #[rstest]
        #[case::id_equals_num(
            // Represents `사과 = 1`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixEquals, loc 0, 0, 0, 6,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 1.0, loc 0, 5, 0, 6),
                ),
            ]),
            Value::from_num(1.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::id_equals_bool(
            // Represents `사과 = 참`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixEquals, loc 0, 0, 0, 6,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 5, 0, 6),
                ),
            ]),
            Value::from_bool(true, Range::from_nums(0, 0, 0, 6))
        )]
        fn equals(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::id_plus_equals_num(
            // Represents `사과 += 4`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixPlusEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(10.0, Range::from_nums(0, 0, 0, 7))
        )]
        #[case::id_minus_equals_num(
            // Represents `사과 -= 4`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixMinusEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(2.0, Range::from_nums(0, 0, 0, 7))
        )]
        #[case::id_asterisk_equals_num(
            // Represents `사과 *= 4`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixAsteriskEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(24.0, Range::from_nums(0, 0, 0, 7))
        )]
        #[case::id_slash_equals_num(
            // Represents `사과 /= 4`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixSlashEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(1.5, Range::from_nums(0, 0, 0, 7))
        )]
        #[case::id_percent_equals_num(
            // Represents `사과 %= 4`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixPercentEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(2.0, Range::from_nums(0, 0, 0, 7))
        )]
        fn combinating_equals(#[case] ast: Box<Ast>, #[case] mut env: Environment, #[case] expected: Value) {
            assert_eval!(&ast, &mut env, expected);
        }

        #[rstest]
        #[case::num_id_plus_equals_bool(
            // Represents `사과 += 참`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixPlusEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &Value::from_num(1.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 6, 0, 7)),
        )]
        #[case::num_id_minus_equals_bool(
            // Represents `사과 += 참`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixMinusEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &Value::from_num(1.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 6, 0, 7)),
        )]
        #[case::num_id_asterisk_equals_bool(
            // Represents `사과 += 참`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixAsteriskEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &Value::from_num(1.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 6, 0, 7)),
        )]
        #[case::num_id_slash_equals_bool(
            // Represents `사과 += 참`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixSlashEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &Value::from_num(1.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 6, 0, 7)),
        )]
        #[case::num_id_percent_equals_bool(
            // Represents `사과 += 참`.
            mkast!(prog loc 0, 0, 0, 7, vec![
                mkast!(infix InfixPercentEquals, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &Value::from_num(1.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 6, 0, 7)),
        )]
        fn combinating_equals_with_wrong_type(
            #[case] ast: Box<Ast>,
            #[case] mut env: Environment,
            #[case] error: EvalError,
        ) {
            assert_eval_fail!(&ast, &mut env, error);
        }
    }

    mod combinator_infix_with_env {
        use super::*;

        #[rstest]
        #[case::id_plus_num(
            // Represents `사과 + 4`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixPlus, loc 0, 0, 0, 6,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 5, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(10.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::num_plus_id(
            // Represents `6 + 사과`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixPlus, loc 0, 0, 0, 6,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(identifier "사과", loc 0, 4, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &Value::from_num(4.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(10.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::id_minus_num(
            // Represents `사과 - 4`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixMinus, loc 0, 0, 0, 6,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 5, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(2.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::num_minus_id(
            // Represents `6 - 사과`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixMinus, loc 0, 0, 0, 6,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(identifier "사과", loc 0, 4, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &Value::from_num(4.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(2.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::id_asterisk_num(
            // Represents `사과 * 4`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixAsterisk, loc 0, 0, 0, 6,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 5, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(24.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::num_asterisk_id(
            // Represents `6 * 사과`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixAsterisk, loc 0, 0, 0, 6,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(identifier "사과", loc 0, 4, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &Value::from_num(4.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(24.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::id_slash_num(
            // Represents `사과 / 4`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixSlash, loc 0, 0, 0, 6,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 5, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(1.5, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::num_slash_id(
            // Represents `6 / 사과`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixSlash, loc 0, 0, 0, 6,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(identifier "사과", loc 0, 4, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &Value::from_num(4.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(1.5, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::id_percent_num(
            // Represents `사과 % 4`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixPercent, loc 0, 0, 0, 6,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(num 4.0, loc 0, 5, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &Value::from_num(6.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(2.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::num_percent_id(
            // Represents `6 % 사과`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixPercent, loc 0, 0, 0, 6,
                    left mkast!(num 6.0, loc 0, 0, 0, 1),
                    right mkast!(identifier "사과", loc 0, 4, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &Value::from_num(4.0, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_num(2.0, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::id_conjunct_bool(
            // Represents `사과 그리고 참`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 6,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(boolean true, loc 0, 5, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `참`.
            root_env("사과", &Value::from_bool(true, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_bool(true, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::bool_conjunct_id(
            // Represents `참 그리고 사과`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixConjunct, loc 0, 0, 0, 6,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(identifier "사과", loc 0, 4, 0, 6),
                ),
            ]),
            // Represents a binding for `사과` to `참`.
            root_env("사과", &Value::from_bool(true, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_bool(true, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::id_disjunct_bool(
            // Represents `사과 그리고 거짓`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixDisjunct, loc 0, 0, 0, 7,
                    left mkast!(identifier "사과", loc 0, 0, 0, 2),
                    right mkast!(boolean false, loc 0, 5, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `거짓`.
            root_env("사과", &Value::from_bool(false, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_bool(false, Range::from_nums(0, 0, 0, 6))
        )]
        #[case::bool_disjunct_id(
            // Represents `거짓 그리고 사과`.
            mkast!(prog loc 0, 0, 0, 6, vec![
                mkast!(infix InfixDisjunct, loc 0, 0, 0, 7,
                    left mkast!(boolean false, loc 0, 0, 0, 2),
                    right mkast!(identifier "사과", loc 0, 5, 0, 7),
                ),
            ]),
            // Represents a binding for `사과` to `거짓`.
            root_env("사과", &Value::from_bool(false, Range::from_nums(0, 2, 0, 3))), // TODO: what is the meaning of the location of values?
            Value::from_bool(false, Range::from_nums(0, 0, 0, 6))
        )]
        fn expression(#[case] ast: Box<Ast>, #[case] mut env: Environment, #[case] expected: Value) {
            assert_eval!(&ast, &mut env, expected);
        }
    }

    #[rstest]
    #[case::two_numbers(
        // Represents `1 2`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(num 1.0, loc 0, 0, 0, 1),
            mkast!(num 2.0, loc 0, 2, 0, 3),
        ]),
        // Expect the evaluated result of a multiple-expression program to be the value of the last expression.
        Value::from_num(2.0, Range::from_nums(0, 0, 0, 3))
    )]
    #[case::assignment_and_identifier(
        // Represents `사과 = 1 사과`.
        mkast!(prog loc 0, 0, 0, 9, vec![
            mkast!(infix InfixEquals, loc 0, 0, 0, 6,
                left mkast!(identifier "사과", loc 0, 0, 0, 2),
                right mkast!(num 1.0, loc 0, 5, 0, 6),
            ),
            mkast!(identifier "사과", loc 0, 7, 0, 9),
        ]),
        Value::from_num(1.0, Range::from_nums(0, 0, 0, 9))
    )]
    fn multiple_expressions_program(#[case] ast: Box<Ast>, #[case] expected: Value) {
        assert_eval!(&ast, expected);
    }

    mod fixtures {
        use super::*;

        pub fn root_env(id_name: &str, id_value: &Value) -> Environment {
            let mut env = Environment::new();
            env.set(id_name, id_value);
            env
        }
    }
}
