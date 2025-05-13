use crate::core::syntax::{Value, ValueKind};

pub fn represent(val: &Value) -> String {
    match val.kind {
        ValueKind::Number(n) => n.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::{Range, Spot};

    const RANGE_MOCKS: &[Range] = &[
        Range::new(Spot::new(0, 0), Spot::new(1, 0)),
        Range::new(Spot::new(1, 0), Spot::new(3, 0)),
    ];

    #[test]
    fn test_repr_num() {
        let value = Value::new(ValueKind::Number(1.0), RANGE_MOCKS[0]);

        let repr = represent(&value);

        let expected = "1";

        assert_eq!(repr, expected);
    }
}
