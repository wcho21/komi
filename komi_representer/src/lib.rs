//! Representer
//!
//! Returns *a string representation* from *a value*.

use komi_syntax::Value;

/// Produces the string representation for a given value.
pub fn represent(val: &Value) -> String {
    val.represent()
}

/// Note: Use the constant `EMPTY_REPR` to test the representation of the empty value, to avoid depending on the implementation detail.
#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::*;
    use komi_syntax::ValueKind;
    use komi_util::{Environment, Range};

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
            assert_repr!(&Value::new(ValueKind::Number(12.25), range()), "12.25");
        }

        /// Represents `-12.25`.
        #[test]
        fn test_negative_num() {
            assert_repr!(&Value::new(ValueKind::Number(-12.25), range()), "-12.25");
        }

        /// Represents `참`.
        #[test]
        fn test_true_bool() {
            assert_repr!(&Value::new(ValueKind::Bool(true), range()), "참");
        }

        /// Represents `거짓`.
        #[test]
        fn test_false_bool() {
            assert_repr!(&Value::new(ValueKind::Bool(false), range()), "거짓");
        }

        /// Represents `함수 사과, 오렌지, 바나나 {}`.
        #[test]
        fn test_closure() {
            assert_repr!(
                &Value::new(
                    ValueKind::Closure {
                        parameters: vec![String::from("사과"), String::from("오렌지"), String::from("바나나")],
                        body: vec![],
                        env: Environment::<Value>::new(),
                    },
                    range()
                ),
                "함수 사과, 오렌지, 바나나 { ... }"
            );
        }
    }

    mod fixtures {
        use super::*;

        pub fn range() -> Range {
            Range::from_nums(0, 0, 1, 1)
        }
    }
}
