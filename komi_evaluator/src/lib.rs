//! # Evaluator
//!
//! Reads *an abstract syntax tree (AST)* and returns a *value* as defined in the `komi_syntax` crate.
//! Note that *value* is a technical term referring to the result of evaluation.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod ast_reducer;
mod environment;
mod err;

use ast_reducer::reduce_ast;
pub use err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, Value};

type ResVal = Result<Value, EvalError>;

/// Produces a value from an AST.
struct Evaluator<'a> {
    ast: &'a Ast,
}

impl<'a> Evaluator<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { ast }
    }

    pub fn eval(&self) -> ResVal {
        reduce_ast(self.ast)
    }
}

/// Produces a value from an AST.
pub fn eval(ast: &Ast) -> ResVal {
    Evaluator::new(ast).eval()
}

#[cfg(test)]
mod tests {
    use super::{Ast, EvalError, EvalErrorKind, Value, eval};
    use komi_syntax::{AstKind, ValueKind, mkast};
    use komi_util::Range;
    use rstest::rstest;

    /// Asserts a given AST to be evaluated into the expected value.
    /// Helps write a test declaratively.
    macro_rules! assert_eval {
        ($ast:expr, $expected:expr $(,)?) => {
            assert_eq!(
                eval($ast),
                Ok($expected),
                "received a value (left) evaluated from the ast, but expected the different value (right)",
            );
        };
    }

    /// Asserts evaluating a given AST will fail.
    /// Helps write a test declaratively.
    macro_rules! assert_eval_fail {
        ($ast:expr, $expected:expr $(,)?) => {
            assert_eq!(
                eval($ast),
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
    fn single_literal(#[case] ast: Box<Ast>, #[case] expected: Value) {
        assert_eval!(&ast, expected);
    }

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
    fn multiple_expressions_program(#[case] ast: Box<Ast>, #[case] expected: Value) {
        assert_eval!(&ast, expected);
    }
}
