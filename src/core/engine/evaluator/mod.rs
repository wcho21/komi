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
                kind: AstKind::Number(n),
                location,
            } => Self::eval_number(n, location),
            Ast {
                kind: AstKind::InfixPlus { left, right },
                location,
            } => Self::eval_infix_plus(left, right),
        }
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
}

pub fn evaluate(ast: &Ast) -> ResVal {
    Evaluator::new(ast).eval()
}

#[cfg(test)]
mod tests {
    use super::*;

    type Res = Result<(), EvalError>;

    const RANGE_MOCKS: &[Range] = &[Range::from_nums(0, 0, 0, 1), Range::from_nums(0, 1, 0, 2)];

    #[test]
    fn test_single_num() -> Res {
        let ast = Ast::new(AstKind::Number(1.0), RANGE_MOCKS[0]);

        let value = evaluate(&ast)?;

        let expected = Value::from_num(1.0, RANGE_MOCKS[0]);
        assert_eq!(value, expected);
        Ok(())
    }

    #[test]
    fn test_addition() -> Res {
        let ast = Ast::new(
            AstKind::InfixPlus {
                left: Box::new(Ast::new(AstKind::Number(1.0), Range::from_nums(0, 0, 0, 1))),
                right: Box::new(Ast::new(AstKind::Number(2.0), Range::from_nums(0, 2, 0, 3))),
            },
            Range::from_nums(0, 0, 0, 3),
        );

        let value = evaluate(&ast)?;

        let expected = Value::from_num(3.0, Range::from_nums(0, 0, 0, 3));
        assert_eq!(value, expected);
        Ok(())
    }

    // TODO: test addition fail due to wrong data type operand
}
