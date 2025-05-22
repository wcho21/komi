use komi_syntax::Value;
use std::collections::HashMap;

pub struct Environment {
    outer: Option<Box<Environment>>,
    table: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self { outer: None, table: HashMap::new() }
    }

    pub fn from_outer(outer: Environment) -> Self {
        Self { outer: Some(Box::new(outer)), table: HashMap::new() }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        // Return the value if found.
        if let Some(value) = self.table.get(name) {
            return Some(value);
        }

        // Since not found in the current environment, try in the outer environment.
        match &self.outer {
            Some(x) => x.get(name),
            None => None,
        }
    }

    pub fn set(&mut self, name: &str, value: &Value) -> () {
        self.table.insert(name.to_string(), value.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::Environment;
    use komi_syntax::{Value, ValueKind};
    use komi_util::Range;
    use rstest::rstest;

    const VALUE_MOCK: Value = Value::new(ValueKind::Number(1.0), Range::from_nums(0, 0, 0, 1));

    #[rstest]
    #[case::set_and_get_some(
        // Represents `foo = 1.0`.
        vec![("foo", VALUE_MOCK)],
        "foo",
        Some(&VALUE_MOCK),
    )]
    #[case::set_and_get_none(
        // Represents no binding.
        vec![],
        "foo",
        None
    )]
    fn single_environment(#[case] to_set: Vec<(&str, Value)>, #[case] to_get: &str, #[case] expected: Option<&Value>) {
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
        Some(&VALUE_MOCK),
    )]
    #[case::set_and_get_none(
        // Represents no binding.
        vec![],
        "foo",
        None
    )]
    fn outer_environment(#[case] to_set: Vec<(&str, Value)>, #[case] to_get: &str, #[case] expected: Option<&Value>) {
        let mut outer_env = Environment::new();
        for (k, v) in to_set {
            outer_env.set(k, &v);
        }
        let env = Environment::from_outer(outer_env);

        let value = env.get(to_get);

        assert_eq!(value, expected);
    }
}
