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

    const RANGE_MOCKS: &[Range] = &[Range::from_nums(0, 0, 0, 1), Range::from_nums(0, 1, 0, 2)];

    #[test]
    fn test_repr_num() {
        let value = Value::new(ValueKind::Number(1.0), RANGE_MOCKS[0]);

        let repr = represent(&value);

        let expected = "1";

        assert_eq!(repr, expected);
    }
}
