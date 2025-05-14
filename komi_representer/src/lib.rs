use komi_syntax::{Value, ValueKind};

pub const EMPTY_REPR: &str = "(EMPTY)";

pub fn represent(val: &Value) -> String {
    match val.kind {
        ValueKind::Number(n) => n.to_string(),
        ValueKind::Empty => EMPTY_REPR.to_string(),
    }
}

/// Note: Use the constant `EMPTY_REPR` to test the representation of the empty value, to avoid depending on the implementation detail.
#[cfg(test)]
mod tests {
    use super::*;
    use komi_util::Range;

    const RANGE_MOCKS: &[Range] = &[Range::from_nums(0, 0, 0, 1), Range::from_nums(0, 1, 0, 2)];

    mod parts {
        use super::*;

        /// Represents `1`.
        #[test]
        fn test_num() {
            let value = Value::new(ValueKind::Number(1.0), RANGE_MOCKS[0]);

            let repr = represent(&value);

            let expected = "1";

            assert_eq!(repr, expected);
        }
    }

    mod programs {
        use super::*;

        /// Represents ``.
        #[test]
        fn test_empty() {
            let value = Value::new(ValueKind::Empty, Range::from_nums(0, 0, 0, 0));

            let repr = represent(&value);

            let expected = EMPTY_REPR;

            assert_eq!(repr, expected);
        }
    }
}
