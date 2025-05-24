//! Representer
//!
//! Returns *a string representation* from *a value*.

use komi_syntax::{Value, ValueKind};

/// Predefined representations
pub const EMPTY_REPR: &str = "(EMPTY)";
pub const TRUE_REPR: &str = "참";
pub const FALSE_REPR: &str = "거짓";
pub const CLOSURE_REPR_KEYWORD: &str = "함수";
pub const CLOSURE_REPR_BODY: &str = "{ ... }";

/// Produces the string representation for a given value.
pub fn represent(val: &Value) -> String {
    match &val.kind {
        ValueKind::Number(n) => n.to_string(),
        ValueKind::Bool(b) => represent_bool(*b),
        ValueKind::Closure { parameters: p, .. } => represent_closure(p),
        ValueKind::Empty => EMPTY_REPR.to_string(),
    }
}

fn represent_bool(boolean: bool) -> String {
    match boolean {
        true => TRUE_REPR.to_string(),
        false => FALSE_REPR.to_string(),
    }
}

fn represent_closure(parameters: &Vec<String>) -> String {
    let mut parts: Vec<String> = vec![];
    parts.push(String::from(CLOSURE_REPR_KEYWORD));
    parts.push(parameters.join(", "));
    parts.push(String::from(CLOSURE_REPR_BODY));

    let repr = parts.join(" ");
    repr
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

        /// Represents `참`.
        #[test]
        fn test_true_bool() {
            assert_repr!(&Value::new(ValueKind::Bool(true), RANGE_MOCKS[0]), "참");
        }

        /// Represents `거짓`.
        #[test]
        fn test_false_bool() {
            assert_repr!(&Value::new(ValueKind::Bool(false), RANGE_MOCKS[0]), "거짓");
        }

        /// Represents `함수 사과, 오렌지, 바나나 {}`.
        #[test]
        fn test_closure() {
            assert_repr!(
                &Value::new(
                    ValueKind::Closure {
                        parameters: vec![String::from("사과"), String::from("오렌지"), String::from("바나나")],
                        body: vec![],
                    },
                    RANGE_MOCKS[0]
                ),
                "함수 사과, 오렌지, 바나나 { ... }"
            );
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
