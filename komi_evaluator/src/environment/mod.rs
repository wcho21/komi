use komi_syntax::Value;
use komi_util;

pub type Environment = komi_util::Environment<Value>;

#[cfg(test)]
mod tests {
    use super::Environment;
    use fixtures::*;
    use komi_syntax::{Value, ValueKind};
    use komi_util::location::Range;
    use rstest::rstest;

    #[rstest]
    #[case::set_and_get_some(
        // Represents `foo = 1.0`.
        vec![("foo", value())],
        "foo",
        Some(value()),
    )]
    #[case::set_and_get_none(
        // Represents no binding.
        vec![],
        "foo",
        None
    )]
    fn single_environment(#[case] to_set: Vec<(&str, Value)>, #[case] to_get: &str, #[case] expected: Option<Value>) {
        let mut env = Environment::new();
        for (k, v) in to_set {
            env.set(k, &v);
        }

        let value = env.get(to_get);

        assert_eq!(value, expected.as_ref());
    }

    #[rstest]
    #[case::set_and_get_some(
        // Represents `foo = 1.0`.
        vec![("foo", value())],
        "foo",
        Some(value()),
    )]
    #[case::set_and_get_none(
        // Represents no binding.
        vec![],
        "foo",
        None
    )]
    fn outer_environment(#[case] to_set: Vec<(&str, Value)>, #[case] to_get: &str, #[case] expected: Option<Value>) {
        let mut outer_env = Environment::new();
        for (k, v) in to_set {
            outer_env.set(k, &v);
        }
        let env = Environment::from_outer(outer_env);

        let value = env.get(to_get);

        assert_eq!(value, expected.as_ref());
    }

    mod fixtures {
        use super::*;

        pub fn value() -> Value {
            Value::new(ValueKind::Number(1.0), range())
        }

        pub fn range() -> Range {
            Range::ORIGIN
        }
    }
}
