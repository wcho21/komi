use crate::environment::Environment;
use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Value, ValueKind};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Returns the evaluated result, from name `name` and its location `location`.
pub fn evaluate_identifier(name: &String, location: &Range, env: &Environment) -> ResVal {
    let Some(x) = env.get(name) else {
        return Err(EvalError::new(EvalErrorKind::UndefinedIdentifier, *location));
    };

    match x.kind {
        ValueKind::Bool(x) => Ok(Value::new(ValueKind::Bool(x), *location)),
        ValueKind::Number(x) => Ok(Value::new(ValueKind::Number(x), *location)),
        _ => todo!(),
    }
}

/// Returns the evaluated numeric result, from number `num` and its location `location`.
pub fn evaluate_num(num: f64, location: &Range) -> ResVal {
    Ok(Value::new(ValueKind::Number(num), *location))
}

/// Returns the evaluated boolean result, from boolean `boolean` and its location `location`.
pub fn evaluate_bool(boolean: bool, location: &Range) -> ResVal {
    Ok(Value::new(ValueKind::Bool(boolean), *location))
}

#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::*;
    use rstest::rstest;

    #[rstest]
    #[case::root_env(root_env(), id_name(), ID_VALUE, ID_RANGE)]
    #[case::inner_env(inner_env(), id_name(), ID_VALUE, ID_RANGE)]
    fn identifier_evaluated(
        #[case] env: Environment,
        #[case] id_name: String,
        #[case] id_value: Value,
        #[case] location: Range,
    ) {
        let evaluated = evaluate_identifier(&id_name, &location, &env);

        assert_eq!(evaluated, Ok(id_value));
    }

    #[rstest]
    #[case::root_env(root_env(), undefined_id_name(), ID_RANGE)]
    #[case::inner_env(inner_env(), undefined_id_name(), ID_RANGE)]
    fn identifier_undefined(#[case] env: Environment, #[case] id_name: String, #[case] location: Range) {
        let evaluated = evaluate_identifier(&id_name, &location, &env);

        assert_eq!(
            evaluated,
            Err(EvalError::new(EvalErrorKind::UndefinedIdentifier, location))
        );
    }

    // TODO(?): test other functions

    mod fixtures {
        use super::*;

        pub const ID_RANGE: Range = Range::from_nums(0, 0, 0, 3);
        pub const ID_VALUE: Value = Value::new(ValueKind::Number(1.0), ID_RANGE);

        pub fn id_name() -> String {
            "foo".to_string()
        }

        pub fn undefined_id_name() -> String {
            "bar".to_string()
        }

        /// Simulates a root scope, whose environment has no outer environment any more.
        pub fn root_env() -> Environment {
            let mut env = Environment::new();
            env.set(&id_name(), &ID_VALUE);

            env
        }

        /// Simulates an inner scope, whose environment has an outer environment.
        pub fn inner_env() -> Environment {
            let mut outer_env = Environment::new();
            outer_env.set(&id_name(), &ID_VALUE);

            let env = Environment::from_outer(outer_env);
            env
        }
    }
}
