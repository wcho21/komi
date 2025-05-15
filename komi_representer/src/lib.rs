//! Representer
//!
//! Returns *a string representation* from *a value*.

use komi_syntax::{Value, ValueKind};

/// The representation of the empty value.
pub const EMPTY_REPR: &str = "(EMPTY)";

/// Produces the string representation for a given value.
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

    /// Asserts a given value to be represented into the expected representation.
    /// Helps write a test more declaratively.
    macro_rules! assert_repr {
        ($val:expr, $expected:expr) => {
            assert_eq!(
                represent($val),
                $expected,
                "received a representation (left) from the value, but expected the different representation (right)",
            );
        };
    }

    mod parts {
        use super::*;

        /// Represents `12.25`.
        #[test]
        fn test_positive_num() {
            assert_repr!(&Value::new(ValueKind::Number(12.25), RANGE_MOCKS[0]), "12.25");
        }

        /// Represents `-12.25`.
        #[test]
        fn test_negative_num() {
            assert_repr!(&Value::new(ValueKind::Number(-12.25), RANGE_MOCKS[0]), "-12.25");
        }
    }

    mod programs {
        use super::*;

        /// Represents ``.
        #[test]
        fn test_empty() {
            assert_repr!(&Value::new(ValueKind::Empty, RANGE_MOCKS[0]), EMPTY_REPR);
        }
    }
}
