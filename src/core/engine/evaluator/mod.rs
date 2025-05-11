use crate::core::syntax::{Ast, Value};
use crate::util::{Range, Spot};

pub fn evaluate(_ast: &Ast) -> Value {
    // fake implementation
    // return dummy value
    let fake_location = Range::new(Spot::new(0, 0), Spot::new(0, 0));
    Value::from_num(1.0, fake_location)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::syntax::ValueKind;

    #[test]
    fn fake_evaluate() {
        let fake_ast = Ast::from_num(1.0, Range::new(Spot::new(0, 0), Spot::new(0, 0)));
        let expected = Value::new(
            ValueKind::Number(1.0),
            Range::new(Spot::new(0, 0), Spot::new(0, 0)),
        );

        let value = evaluate(&fake_ast);

        assert_eq!(value, expected);
    }
}
