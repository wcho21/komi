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
            Ast { kind: AstKind::Program { expressions }, location } => Self::eval_program(expressions, location),
            Ast { kind: AstKind::InfixPlus { left, right }, location: _ } => Self::eval_infix_plus(left, right),
            Ast { kind: AstKind::InfixMinus { left, right }, location: _ } => Self::eval_infix_minus(left, right),
            Ast { kind: AstKind::InfixAsterisk { left, right }, location: _ } => Self::eval_infix_asterisk(left, right),
            Ast { kind: AstKind::InfixSlash { left, right }, location: _ } => Self::eval_infix_slash(left, right),
            Ast { kind: AstKind::InfixPercent { left, right }, location: _ } => Self::eval_infix_percent(left, right),
            Ast { kind: AstKind::PrefixPlus { operand }, location } => Self::eval_prefix_plus(operand, location),
            Ast { kind: AstKind::PrefixMinus { operand }, location } => Self::eval_prefix_minus(operand, location),
            Ast { kind: AstKind::Number(n), location } => Self::eval_number(n, location),
            Ast { kind: AstKind::Bool(b), location } => Self::eval_bool(b, location),
            _ => todo!(),
        }
    }

    fn eval_program(expressions: &Vec<Box<Ast>>, location: &Range) -> ResVal {
        let mut last_value = Value::from_empty(*location);

        for expression in expressions {
            last_value = Self::eval_ast(expression)?;
        }
        Ok(last_value)
    }

    fn eval_infix_plus(left: &Ast, right: &Ast) -> ResVal {
        let left_val = Self::eval_infix_operand_num(left)?;
        let right_val = Self::eval_infix_operand_num(right)?;

        let evaluated = left_val + right_val;

        let location = Range::new(left.location.begin, right.location.end);
        Ok(Value::new(ValueKind::Number(evaluated), location))
    }

    fn eval_infix_minus(left: &Ast, right: &Ast) -> ResVal {
        let left_val = Self::eval_infix_operand_num(left)?;
        let right_val = Self::eval_infix_operand_num(right)?;

        let evaluated = left_val - right_val;

        let location = Range::new(left.location.begin, right.location.end);
        Ok(Value::new(ValueKind::Number(evaluated), location))
    }

    fn eval_infix_asterisk(left: &Ast, right: &Ast) -> ResVal {
        let left_val = Self::eval_infix_operand_num(left)?;
        let right_val = Self::eval_infix_operand_num(right)?;

        let evaluated = left_val * right_val;

        let location = Range::new(left.location.begin, right.location.end);
        Ok(Value::new(ValueKind::Number(evaluated), location))
    }

    fn eval_infix_slash(left: &Ast, right: &Ast) -> ResVal {
        let left_val = Self::eval_infix_operand_num(left)?;
        let right_val = Self::eval_infix_operand_num(right)?;

        let evaluated = left_val / right_val;

        let location = Range::new(left.location.begin, right.location.end);
        Ok(Value::new(ValueKind::Number(evaluated), location))
    }

    fn eval_infix_percent(left: &Ast, right: &Ast) -> ResVal {
        let left_val = Self::eval_infix_operand_num(left)?;
        let right_val = Self::eval_infix_operand_num(right)?;

        let evaluated = left_val % right_val;

        let location = Range::new(left.location.begin, right.location.end);
        Ok(Value::new(ValueKind::Number(evaluated), location))
    }

    fn eval_prefix_plus(operand: &Ast, prefix_location: &Range) -> ResVal {
        let val = Self::eval_prefix_operand_num(operand)?;

        let location = Range::new(prefix_location.begin, operand.location.end);
        Ok(Value::new(ValueKind::Number(val), location))
    }

    fn eval_prefix_minus(operand: &Ast, prefix_location: &Range) -> ResVal {
        let operand_val = Self::eval_prefix_operand_num(operand)?;
        let val = -operand_val;

        let location = Range::new(prefix_location.begin, operand.location.end);
        Ok(Value::new(ValueKind::Number(val), location))
    }

    fn eval_number(num: &f64, location: &Range) -> ResVal {
        Ok(Value::new(ValueKind::Number(*num), *location))
    }

    fn eval_bool(boolean: &bool, location: &Range) -> ResVal {
        Ok(Value::new(ValueKind::Bool(*boolean), *location))
    }

    fn eval_infix_operand_num(operand: &Ast) -> Result<f64, EvalError> {
        let val = Self::eval_ast(operand)?;
        if let ValueKind::Number(num) = val.kind {
            Ok(num)
        } else {
            Err(EvalError::new(EvalErrorKind::InvalidAdditionOperand, val.location)) // TODO: rename error (not only for addition)
        }
    }

    fn eval_prefix_operand_num(operand: &Ast) -> Result<f64, EvalError> {
        let val = Self::eval_ast(operand)?;
        if let ValueKind::Number(num) = val.kind {
            Ok(num)
        } else {
            Err(EvalError::new(EvalErrorKind::InvalidPrefixNumOperand, val.location))
        }
    }
}

/// Produces a value from an AST.
pub fn eval(ast: &Ast) -> ResVal {
    Evaluator::new(ast).eval()
}

#[cfg(test)]
mod tests {
    use super::{Ast, AstKind, EvalError, Range, Value, ValueKind, eval};
    use komi_syntax::mkast;

    type Res = Result<(), EvalError>;

    /// Asserts a given AST to be evaluated into the expected value.
    /// Helps write a test more declaratively.
    macro_rules! assert_eval {
        ($ast:expr, $expected:expr) => {
            assert_eq!(
                eval($ast)?,
                $expected,
                "received a value (left) evaluated from the ast, but expected the different value (right)",
            );
            return Ok(())
        };
    }

    mod empty {
        use super::*;

        /// Represents ``.
        #[test]
        fn test_empty() -> Res {
            assert_eval!(
                &mkast!(prog loc 0, 0, 0, 0, vec![]),
                Value::new(ValueKind::Empty, Range::from_nums(0, 0, 0, 0))
            );
        }
    }

    mod leaves {
        use super::*;

        /// Represents `1`.
        #[test]
        fn test_num() -> Res {
            assert_eval!(
                &mkast!(prog loc 0, 0, 0, 1, vec![
                    mkast!(num 1.0, loc 0, 0, 0, 1),
                ]),
                Value::from_num(1.0, Range::from_nums(0, 0, 0, 1))
            );
        }

        /// Represents `참`.
        #[test]
        fn test_bool() -> Res {
            assert_eval!(
                &mkast!(prog loc 0, 0, 0, 1, vec![
                    mkast!(boolean true, loc 0, 0, 0, 1),
                ]),
                Value::from_bool(true, Range::from_nums(0, 0, 0, 1))
            );
        }
    }

    mod infixes {
        use super::*;

        mod simple {
            use super::*;

            /// Represents `1+2`.
            #[test]
            fn test_addition() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixPlus, loc 0, 0, 0, 3,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(num 2.0, loc 0, 2, 0, 3),
                        ),
                    ]),
                    Value::from_num(3.0, Range::from_nums(0, 0, 0, 3))
                );
            }

            /// Represents `1-2`.
            #[test]
            fn test_subtraction() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixMinus, loc 0, 0, 0, 3,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(num 2.0, loc 0, 2, 0, 3),
                        ),
                    ]),
                    Value::from_num(-1.0, Range::from_nums(0, 0, 0, 3))
                );
            }

            /// Represents `3*4`.
            #[test]
            fn test_multiplication() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                            left mkast!(num 3.0, loc 0, 0, 0, 1),
                            right mkast!(num 4.0, loc 0, 2, 0, 3),
                        ),
                    ]),
                    Value::from_num(12.0, Range::from_nums(0, 0, 0, 3))
                );
            }

            /// Represents `3/4`.
            #[test]
            fn test_division() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixSlash, loc 0, 0, 0, 3,
                            left mkast!(num 3.0, loc 0, 0, 0, 1),
                            right mkast!(num 4.0, loc 0, 2, 0, 3),
                        ),
                    ]),
                    Value::from_num(0.75, Range::from_nums(0, 0, 0, 3))
                );
            }

            /// Represents `3%4`.
            #[test]
            fn test_mod() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixPercent, loc 0, 0, 0, 3,
                            left mkast!(num 3.0, loc 0, 0, 0, 1),
                            right mkast!(num 4.0, loc 0, 2, 0, 3),
                        ),
                    ]),
                    Value::from_num(3.0, Range::from_nums(0, 0, 0, 3))
                );
            }
        }

        mod compound {
            use super::*;

            /// Represents `9*8%7-6+5/4` (parsed into `(((9*8)%7)-6)+(5/4)`.
            /// Note that the associativity of an expression is determined in the parsing step, as represented in the AST result.
            #[test]
            fn test_five_kinds() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 11, vec![
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
                );
            }

            /// Represents `+1`
            #[test]
            fn test_plus_prefix() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 2, vec![
                        mkast!(prefix PrefixPlus, loc 0, 0, 0, 2,
                            operand mkast!(num 1.0, loc 0, 1, 0, 2),
                        ),
                    ]),
                    Value::from_num(1.0, Range::from_nums(0, 0, 0, 2))
                );
            }

            /// Represents `-1`
            #[test]
            fn test_minus_prefix() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 2, vec![
                        mkast!(prefix PrefixMinus, loc 0, 0, 0, 2,
                            operand mkast!(num 1.0, loc 0, 1, 0, 2),
                        ),
                    ]),
                    Value::from_num(-1.0, Range::from_nums(0, 0, 0, 2))
                );
            }

            /// Represents `++1`
            #[test]
            fn test_two_plus_prefixes() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(prefix PrefixPlus, loc 0, 0, 0, 3,
                            operand mkast!(prefix PrefixPlus, loc 0, 1, 0, 3,
                                operand mkast!(num 1.0, loc 0, 2, 0, 3),
                            ),
                        ),
                    ]),
                    Value::from_num(1.0, Range::from_nums(0, 0, 0, 3))
                );
            }

            /// Represents `--1`
            #[test]
            fn test_two_minus_prefixes() -> Res {
                assert_eval!(
                    &mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(prefix PrefixMinus, loc 0, 0, 0, 3,
                            operand mkast!(prefix PrefixMinus, loc 0, 1, 0, 3,
                                operand mkast!(num 1.0, loc 0, 2, 0, 3),
                            ),
                        ),
                    ]),
                    Value::from_num(1.0, Range::from_nums(0, 0, 0, 3))
                );
            }
        }
    }

    // TODO: test addition fail due to wrong data type operand
}
