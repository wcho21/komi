use crate::core::err::EvalError;
use crate::core::syntax::{Ast, AstKind, Value, ValueKind};
use crate::util::Range;

type ResVal = Result<Value, EvalError>;

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
            Ast {
                kind: AstKind::Program { expressions },
                location,
            } => Self::eval_program(expressions, location),
            Ast {
                kind: AstKind::Number(n),
                location,
            } => Self::eval_number(n, location),
            Ast {
                kind: AstKind::InfixPlus { left, right },
                location: _,
            } => Self::eval_infix_plus(left, right),
            Ast {
                kind: AstKind::InfixMinus { left, right },
                location: _,
            } => Self::eval_infix_minus(left, right),
            Ast {
                kind: AstKind::InfixAsterisk { left, right },
                location: _,
            } => Self::eval_infix_asterisk(left, right),
            Ast {
                kind: AstKind::InfixSlash { left, right },
                location: _,
            } => Self::eval_infix_slash(left, right),
            Ast {
                kind: AstKind::InfixPercent { left, right },
                location: _,
            } => Self::eval_infix_percent(left, right),
        }
    }

    fn eval_program(expressions: &Vec<Ast>, location: &Range) -> ResVal {
        let mut last_value = Value::from_empty(*location);

        for expression in expressions {
            last_value = Self::eval_ast(expression)?;
        }
        Ok(last_value)
    }

    fn eval_number(num: &f64, location: &Range) -> ResVal {
        Ok(Value::new(ValueKind::Number(*num), *location))
    }

    fn eval_infix_operand_num(operand: &Ast) -> Result<f64, EvalError> {
        let val = Self::eval_ast(operand)?;
        if let ValueKind::Number(num) = val.kind {
            Ok(num)
        } else {
            Err(EvalError::BadAdditionOperand(format!("{:?}", val.kind), val.location))
        }
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
}

pub fn evaluate(ast: &Ast) -> ResVal {
    Evaluator::new(ast).eval()
}

#[cfg(test)]
mod tests {
    use super::*;

    type Res = Result<(), EvalError>;

    /// Represents ``.
    #[test]
    fn test_empty() -> Res {
        let program = Ast::new(AstKind::Program { expressions: vec![] }, Range::from_nums(0, 0, 0, 0));

        let value = evaluate(&program)?;

        let expected = Value::new(ValueKind::Empty, Range::from_nums(0, 0, 0, 0));
        assert_eq!(value, expected);
        Ok(())
    }

    /// Represents `1`.
    #[test]
    fn test_single_num() -> Res {
        let program = Ast::new(
            AstKind::Program {
                expressions: vec![Ast::new(AstKind::Number(1.0), Range::from_nums(0, 0, 0, 1))],
            },
            Range::from_nums(0, 0, 0, 1),
        );

        let value = evaluate(&program)?;

        let expected = Value::from_num(1.0, Range::from_nums(0, 0, 0, 1));
        assert_eq!(value, expected);
        Ok(())
    }

    /// Represents `1+2`.
    #[test]
    fn test_addition() -> Res {
        let program = Ast::new(
            AstKind::Program {
                expressions: vec![Ast::new(
                    AstKind::InfixPlus {
                        left: Box::new(Ast::new(AstKind::Number(1.0), Range::from_nums(0, 0, 0, 1))),
                        right: Box::new(Ast::new(AstKind::Number(2.0), Range::from_nums(0, 2, 0, 3))),
                    },
                    Range::from_nums(0, 0, 0, 3),
                )],
            },
            Range::from_nums(0, 0, 0, 1),
        );

        let value = evaluate(&program)?;

        let expected = Value::from_num(3.0, Range::from_nums(0, 0, 0, 3));
        assert_eq!(value, expected);
        Ok(())
    }

    /// Represents `3-2`.
    #[test]
    fn test_subtraction() -> Res {
        let program = Ast::new(
            AstKind::Program {
                expressions: vec![Ast::new(
                    AstKind::InfixMinus {
                        left: Box::new(Ast::new(AstKind::Number(3.0), Range::from_nums(0, 0, 0, 1))),
                        right: Box::new(Ast::new(AstKind::Number(2.0), Range::from_nums(0, 2, 0, 3))),
                    },
                    Range::from_nums(0, 0, 0, 3),
                )],
            },
            Range::from_nums(0, 0, 0, 1),
        );

        let value = evaluate(&program)?;

        let expected = Value::from_num(1.0, Range::from_nums(0, 0, 0, 3));
        assert_eq!(value, expected);
        Ok(())
    }

    /// Represents `2*3`.
    #[test]
    fn test_multiplication() -> Res {
        let program = Ast::new(
            AstKind::Program {
                expressions: vec![Ast::new(
                    AstKind::InfixAsterisk {
                        left: Box::new(Ast::new(AstKind::Number(2.0), Range::from_nums(0, 0, 0, 1))),
                        right: Box::new(Ast::new(AstKind::Number(3.0), Range::from_nums(0, 2, 0, 3))),
                    },
                    Range::from_nums(0, 0, 0, 3),
                )],
            },
            Range::from_nums(0, 0, 0, 1),
        );

        let value = evaluate(&program)?;

        let expected = Value::from_num(6.0, Range::from_nums(0, 0, 0, 3));
        assert_eq!(value, expected);
        Ok(())
    }

    /// Represents `6/3`.
    #[test]
    fn test_division() -> Res {
        let program = Ast::new(
            AstKind::Program {
                expressions: vec![Ast::new(
                    AstKind::InfixSlash {
                        left: Box::new(Ast::new(AstKind::Number(6.0), Range::from_nums(0, 0, 0, 1))),
                        right: Box::new(Ast::new(AstKind::Number(3.0), Range::from_nums(0, 2, 0, 3))),
                    },
                    Range::from_nums(0, 0, 0, 3),
                )],
            },
            Range::from_nums(0, 0, 0, 1),
        );

        let value = evaluate(&program)?;

        let expected = Value::from_num(2.0, Range::from_nums(0, 0, 0, 3));
        assert_eq!(value, expected);
        Ok(())
    }

    /// Represents `6%4`.
    #[test]
    fn test_mod() -> Res {
        let program = Ast::new(
            AstKind::Program {
                expressions: vec![Ast::new(
                    AstKind::InfixPercent {
                        left: Box::new(Ast::new(AstKind::Number(6.0), Range::from_nums(0, 0, 0, 1))),
                        right: Box::new(Ast::new(AstKind::Number(4.0), Range::from_nums(0, 2, 0, 3))),
                    },
                    Range::from_nums(0, 0, 0, 3),
                )],
            },
            Range::from_nums(0, 0, 0, 1),
        );

        let value = evaluate(&program)?;

        let expected = Value::from_num(2.0, Range::from_nums(0, 0, 0, 3));
        assert_eq!(value, expected);
        Ok(())
    }

    // TODO: test addition fail due to wrong data type operand
}
