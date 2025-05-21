//! # Evaluator
//!
//! Reads *an abstract syntax tree (AST)* and returns a *value* as defined in the `komi_syntax` crate.
//! Note that *value* is a technical term referring to the result of evaluation.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the parser.

mod err;

pub use err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, AstKind, Value, ValueKind};
use komi_util::Range;

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
        Self::eval_ast(self.ast)
    }

    fn eval_ast(ast: &Ast) -> ResVal {
        match ast {
            Ast { kind: AstKind::Program { expressions: e }, location: loc } => Self::evaluate_expressions(e, loc),
            Ast { kind: AstKind::Number(n), location: loc } => Self::evaluate_number(*n, loc),
            Ast { kind: AstKind::Bool(b), location: loc } => Self::evaluate_bool(*b, loc),
            Ast { kind: AstKind::PrefixPlus { operand: op }, location: loc } => Self::evaluate_prefix_plus(op, loc),
            Ast { kind: AstKind::PrefixMinus { operand: op }, location: loc } => Self::evaluate_prefix_minus(op, loc),
            Ast { kind: AstKind::PrefixBang { operand: op }, location: loc } => Self::evaluate_prefix_bang(op, loc),
            Ast { kind: AstKind::InfixPlus { left, right }, location: _ } => Self::eval_infix_plus(left, right),
            Ast { kind: AstKind::InfixMinus { left, right }, location: _ } => Self::eval_infix_minus(left, right),
            Ast { kind: AstKind::InfixAsterisk { left, right }, location: _ } => Self::eval_infix_asterisk(left, right),
            Ast { kind: AstKind::InfixSlash { left, right }, location: _ } => Self::eval_infix_slash(left, right),
            Ast { kind: AstKind::InfixPercent { left, right }, location: _ } => Self::eval_infix_percent(left, right),
            Ast { kind: AstKind::InfixConjunct { left, right }, location: _ } => Self::eval_infix_conjunct(left, right),
            Ast { kind: AstKind::InfixDisjunct { left, right }, location: _ } => Self::eval_infix_disjunct(left, right),
        }
    }

    /// Returns the evaluated result of the last AST in the ASTs `expressions`.
    ///
    /// Sets its location to be `expressions_location`, since it represents the entire expressions, not a single one.
    fn evaluate_expressions(expressions: &Vec<Box<Ast>>, expressions_location: &Range) -> ResVal {
        let mut last_value = Value::from_empty(*expressions_location);

        for expression in expressions {
            last_value = Self::eval_ast(expression)?;
        }
        last_value.location = *expressions_location;
        Ok(last_value)
    }

    /// Returns the evaluated numeric result, from number `num` and its location `location`.
    fn evaluate_number(num: f64, location: &Range) -> ResVal {
        Ok(Value::new(ValueKind::Number(num), *location))
    }

    /// Returns the evaluated boolean result, from boolean `boolean` and its location `location`.
    fn evaluate_bool(boolean: bool, location: &Range) -> ResVal {
        Ok(Value::new(ValueKind::Bool(boolean), *location))
    }

    /// Returns the evaluated numeric result of the AST `operand` as an operand of the prefix plus.
    ///
    /// The location in the returned value will span from the prefix to operand.
    fn evaluate_prefix_plus(operand: &Ast, prefix_location: &Range) -> ResVal {
        Self::evaluate_num_prefix(operand, prefix_location, |v| ValueKind::Number(v))
    }

    /// Returns the evaluated numeric result of the AST `operand` as an operand of the prefix minus.
    ///
    /// The location in the returned value will span from the prefix to operand.
    fn evaluate_prefix_minus(operand: &Ast, prefix_location: &Range) -> ResVal {
        Self::evaluate_num_prefix(operand, prefix_location, |v| ValueKind::Number(-v))
    }

    /// Returns the evaluated boolean result of the AST `operand` as an operand of the prefix bang.
    ///
    /// The location in the returned value will span from the prefix to operand.
    fn evaluate_prefix_bang(operand: &Ast, prefix_location: &Range) -> ResVal {
        Self::evaluate_bool_prefix(operand, prefix_location, |v| ValueKind::Bool(!v))
    }

    fn eval_infix_plus(left: &Ast, right: &Ast) -> ResVal {
        Self::evaluate_infix(
            left,
            right,
            Self::evaluate_infix_operand_num,
            |l, r| l + r,
            |v| ValueKind::Number(v),
        )
    }

    fn eval_infix_minus(left: &Ast, right: &Ast) -> ResVal {
        Self::evaluate_infix(
            left,
            right,
            Self::evaluate_infix_operand_num,
            |l, r| l - r,
            |v| ValueKind::Number(v),
        )
    }

    fn eval_infix_asterisk(left: &Ast, right: &Ast) -> ResVal {
        Self::evaluate_infix(
            left,
            right,
            Self::evaluate_infix_operand_num,
            |l, r| l * r,
            |v| ValueKind::Number(v),
        )
    }

    fn eval_infix_slash(left: &Ast, right: &Ast) -> ResVal {
        Self::evaluate_infix(
            left,
            right,
            Self::evaluate_infix_operand_num,
            |l, r| l / r,
            |v| ValueKind::Number(v),
        )
    }

    fn eval_infix_percent(left: &Ast, right: &Ast) -> ResVal {
        Self::evaluate_infix(
            left,
            right,
            Self::evaluate_infix_operand_num,
            |l, r| l % r,
            |v| ValueKind::Number(v),
        )
    }

    fn eval_infix_conjunct(left: &Ast, right: &Ast) -> ResVal {
        let left_val = Self::evaluate_infix_operand_bool(left)?;
        let right_val = Self::evaluate_infix_operand_bool(right)?;

        let evaluated = left_val && right_val;

        let location = Range::new(left.location.begin, right.location.end);
        Ok(Value::new(ValueKind::Bool(evaluated), location))
    }

    fn eval_infix_disjunct(left: &Ast, right: &Ast) -> ResVal {
        let left_val = Self::evaluate_infix_operand_bool(left)?;
        let right_val = Self::evaluate_infix_operand_bool(right)?;

        let evaluated = left_val || right_val;

        let location = Range::new(left.location.begin, right.location.end);
        Ok(Value::new(ValueKind::Bool(evaluated), location))
    }

    /// Returns the evaluated numeric result of the AST `operand` as a leaf operand of a prefix.
    fn evaluate_prefix_operand_num(operand: &Ast) -> Result<f64, EvalError> {
        Self::evaluate_leaf_operand(operand, |value_kind| match value_kind {
            ValueKind::Number(x) => Ok(*x),
            _ => Err(EvalErrorKind::InvalidPrefixNumOperand),
        })
    }

    /// Returns the evaluated boolean result of the AST `operand` as a leaf operand of a prefix.
    fn evaluate_prefix_operand_bool(operand: &Ast) -> Result<bool, EvalError> {
        Self::evaluate_leaf_operand(operand, |value_kind| match value_kind {
            ValueKind::Bool(x) => Ok(*x),
            _ => Err(EvalErrorKind::InvalidPrefixBoolOperand),
        })
    }

    /// Returns the evaluated numeric result of the AST `operand` as a leaf operand of an infix.
    fn evaluate_infix_operand_num(operand: &Ast) -> Result<f64, EvalError> {
        Self::evaluate_leaf_operand(operand, |value_kind| match value_kind {
            ValueKind::Number(x) => Ok(*x),
            _ => Err(EvalErrorKind::InvalidAdditionOperand),
        })
    }

    /// Returns the evaluated boolean result of the AST `operand` as a leaf operand of an infix.
    fn evaluate_infix_operand_bool(operand: &Ast) -> Result<bool, EvalError> {
        Self::evaluate_leaf_operand(operand, |value_kind| match value_kind {
            ValueKind::Bool(x) => Ok(*x),
            _ => Err(EvalErrorKind::InvalidConnectiveInfixOperand),
        })
    }

    // TODO: make names consistent (num_something or something_num)
    fn evaluate_num_prefix<F>(operand: &Ast, prefix_location: &Range, get_kind: F) -> ResVal
    where
        F: Fn(f64) -> ValueKind,
    {
        Self::evaluate_prefix(operand, prefix_location, Self::evaluate_prefix_operand_num, get_kind)
    }

    fn evaluate_bool_prefix<F>(operand: &Ast, prefix_location: &Range, get_kind: F) -> ResVal
    where
        F: Fn(bool) -> ValueKind,
    {
        Self::evaluate_prefix::<bool, _, _>(operand, prefix_location, Self::evaluate_prefix_operand_bool, get_kind)
    }

    /// Returns the evaluated result of the AST `operand` as an operand of a prefix.
    ///
    /// - `evaluate_operand` determines how to evaluate the AST `operand` itself to some value `x`.
    /// - `get_kind` specifies how to get the value kind from `x`.
    ///
    /// The location in the returned value will span from the prefix to operand.
    fn evaluate_prefix<T, F, G>(operand: &Ast, prefix_location: &Range, evaluate_operand: F, get_kind: G) -> ResVal
    where
        F: Fn(&Ast) -> Result<T, EvalError>,
        G: Fn(T) -> ValueKind,
    {
        let evaluated_operand = evaluate_operand(operand)?;
        let kind = get_kind(evaluated_operand);
        let location = Range::new(prefix_location.begin, operand.location.end);
        Ok(Value::new(kind, location))
    }

    fn evaluate_infix<T, F, G, H>(
        left: &Ast,
        right: &Ast,
        evaluate_operand: F,
        evaluate_infix: G,
        make_value_kind: H,
    ) -> ResVal
    where
        F: Fn(&Ast) -> Result<T, EvalError>,
        G: Fn(T, T) -> T,
        H: Fn(T) -> ValueKind,
    {
        let left_val = evaluate_operand(left)?;
        let right_val = evaluate_operand(right)?;

        let infix_val = evaluate_infix(left_val, right_val);

        let kind = make_value_kind(infix_val);
        let location = Range::new(left.location.begin, right.location.end);

        Ok(Value::new(kind, location))
    }

    /// Returns the evalauted result of the AST `operand` as a leaf operand of a prefix or an infix.
    ///
    /// `extract` specifies the expected kind `x` of the evaluated result of `operand`.
    /// - If `x` encountered, it returns `Ok` from `x`.
    /// - Otherwise, returns `Err(e)` where `e` is `EvalError`.
    fn evaluate_leaf_operand<T, F>(operand: &Ast, extract: F) -> Result<T, EvalError>
    where
        F: Fn(&ValueKind) -> Result<T, EvalErrorKind>,
    {
        let val = Self::eval_ast(operand)?;

        match extract(&val.kind) {
            Ok(x) => Ok(x),
            Err(kind) => Err(EvalError::new(kind, val.location)),
        }
    }
}

/// Produces a value from an AST.
pub fn eval(ast: &Ast) -> ResVal {
    Evaluator::new(ast).eval()
}

#[cfg(test)]
mod tests {
    use super::{Ast, AstKind, EvalError, EvalErrorKind, Range, Value, ValueKind, eval};
    use komi_syntax::mkast;
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
        EvalError::new(EvalErrorKind::InvalidPrefixNumOperand, Range::from_nums(0, 1, 0, 2)),
    )]
    #[case::minus_bool(
        // Represents `-참`
        mkast!(prog loc 0, 0, 0, 2, vec![
            mkast!(prefix PrefixMinus, loc 0, 0, 0, 2,
                operand mkast!(boolean true, loc 0, 1, 0, 2),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidPrefixNumOperand, Range::from_nums(0, 1, 0, 2)),
    )]
    #[case::bang_num(
        // Represents `!1`
        mkast!(prog loc 0, 0, 0, 2, vec![
            mkast!(prefix PrefixBang, loc 0, 0, 0, 2,
                operand mkast!(num 1.0, loc 0, 1, 0, 2),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidPrefixBoolOperand, Range::from_nums(0, 1, 0, 2)),
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
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 0, 0, 1)),
    )]
    #[case::right_bool_addition(
        // Represents `1+참`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixPlus, loc 0, 0, 0, 3,
                left mkast!(num 1.0, loc 0, 0, 0, 1),
                right mkast!(boolean true, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 2, 0, 3)),
    )]
    #[case::left_bool_subtraction(
        // Represents `참-1`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixMinus, loc 0, 0, 0, 3,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 0, 0, 1)),
    )]
    #[case::right_bool_subtraction(
        // Represents `1-참`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixMinus, loc 0, 0, 0, 3,
                left mkast!(num 1.0, loc 0, 0, 0, 1),
                right mkast!(boolean true, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 2, 0, 3)),
    )]
    #[case::left_bool_multiplication(
        // Represents `참*1`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 0, 0, 1)),
    )]
    #[case::right_bool_multiplication(
        // Represents `1*참`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                left mkast!(num 1.0, loc 0, 0, 0, 1),
                right mkast!(boolean true, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 2, 0, 3)),
    )]
    #[case::left_bool_division(
        // Represents `참/1`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixSlash, loc 0, 0, 0, 3,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 0, 0, 1)),
    )]
    #[case::right_bool_division(
        // Represents `1/참`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixSlash, loc 0, 0, 0, 3,
                left mkast!(num 1.0, loc 0, 0, 0, 1),
                right mkast!(boolean true, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 2, 0, 3)),
    )]
    #[case::left_bool_modular(
        // Represents `참%1`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixPercent, loc 0, 0, 0, 3,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 0, 0, 1)),
    )]
    #[case::right_bool_modular(
        // Represents `1%참`.
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(infix InfixPercent, loc 0, 0, 0, 3,
                left mkast!(num 1.0, loc 0, 0, 0, 1),
                right mkast!(boolean true, loc 0, 2, 0, 3),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidAdditionOperand, Range::from_nums(0, 2, 0, 3)),
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
        EvalError::new(EvalErrorKind::InvalidConnectiveInfixOperand, Range::from_nums(0, 0, 0, 1)),
    )]
    #[case::right_num_conjunction(
        // Represents `참 그리고 1`.
        mkast!(prog loc 0, 0, 0, 7, vec![
            mkast!(infix InfixConjunct, loc 0, 0, 0, 7,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 6, 0, 7),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidConnectiveInfixOperand, Range::from_nums(0, 6, 0, 7)),
    )]
    #[case::left_num_disjunction(
        // Represents `1 또는 참`.
        mkast!(prog loc 0, 0, 0, 6, vec![
            mkast!(infix InfixDisjunct, loc 0, 0, 0, 6,
                left mkast!(num 1.0, loc 0, 0, 0, 1),
                right mkast!(boolean true, loc 0, 5, 0, 6),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidConnectiveInfixOperand, Range::from_nums(0, 0, 0, 1)),
    )]
    #[case::right_num_disjunction(
        // Represents `참 또는 1`.
        mkast!(prog loc 0, 0, 0, 6, vec![
            mkast!(infix InfixConjunct, loc 0, 0, 0, 6,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 5, 0, 6),
            ),
        ]),
        EvalError::new(EvalErrorKind::InvalidConnectiveInfixOperand, Range::from_nums(0, 5, 0, 6)),
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
