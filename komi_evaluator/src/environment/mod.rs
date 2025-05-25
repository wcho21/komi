use komi_syntax::Value;
use komi_util;

pub type Environment = komi_util::Environment<Value>;

#[cfg(test)]
mod tests {
    use super::Environment;
    use fixtures::*;
    use komi_syntax::{Value, ValueKind};
    use komi_util::Range;
    use rstest::rstest;

    mod fixtures {
        use super::*;

        pub const VALUE_MOCK: &Value = &Value::new(ValueKind::Number(1.0), Range::from_nums(0, 0, 0, 1));
    }

    #[rstest]
    #[case::set_and_get_some(
        // Represents `foo = 1.0`.
        vec![("foo", VALUE_MOCK)],
        "foo",
        Some(VALUE_MOCK),
    )]
    #[case::set_and_get_none(
        // Represents no binding.
        vec![],
        "foo",
        None
    )]
    fn single_environment(#[case] to_set: Vec<(&str, &Value)>, #[case] to_get: &str, #[case] expected: Option<&Value>) {
        let mut env = Environment::new();
        for (k, v) in to_set {
            env.set(k, &v);
        }

        let value = env.get(to_get);

        assert_eq!(value, expected);
    }

    #[rstest]
    #[case::set_and_get_some(
        // Represents `foo = 1.0`.
        vec![("foo", VALUE_MOCK)],
        "foo",
        Some(VALUE_MOCK),
    )]
    #[case::set_and_get_none(
        // Represents no binding.
        vec![],
        "foo",
        None
    )]
    fn outer_environment(#[case] to_set: Vec<(&str, &Value)>, #[case] to_get: &str, #[case] expected: Option<&Value>) {
        let mut outer_env = Environment::new();
        for (k, v) in to_set {
            outer_env.set(k, &v);
        }
        let env = Environment::from_outer(outer_env);

        let value = env.get(to_get);

        assert_eq!(value, expected);
    }
}
