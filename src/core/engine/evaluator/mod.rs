use crate::core::err::EvalErr;
use crate::core::syntax::{Ast, AstKind, Value, ValueKind};
use crate::util::{Range, Spot};

struct Evaluator<'a> {
    ast: &'a Ast,
}

impl<'a> Evaluator<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { ast }
    }

    pub fn eval(&self) -> Result<Value, EvalErr> {
        // TODO: reduce typing type
        match self.ast {
            Ast {
                kind: AstKind::Number(n),
                location,
            } => Self::eval_number(n, location),
        }
    }

    fn eval_number(num: &f64, location: &Range) -> Result<Value, EvalErr> {
        Ok(Value::new(ValueKind::Number(*num), *location))
    }
}

pub fn evaluate(ast: &Ast) -> Result<Value, EvalErr> {
    Evaluator::new(ast).eval()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::syntax::ValueKind;
    use std::error::Error;

    const RANGE_MOCKS: &[Range] = &[
        Range::new(Spot::new(0, 0), Spot::new(1, 0)),
        Range::new(Spot::new(1, 0), Spot::new(3, 0)),
    ];

    #[test]
    fn fake_evaluate() -> Result<(), Box<dyn Error>> {
        let ast = Ast::new(AstKind::Number(1.0), RANGE_MOCKS[0]);

        let value = evaluate(&ast)?;

        let expected = Value::new(ValueKind::Number(1.0), RANGE_MOCKS[0]);
        assert_eq!(value, expected);
        Ok(())
    }
}
