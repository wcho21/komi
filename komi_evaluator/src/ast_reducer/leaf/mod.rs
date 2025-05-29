use crate::ValRes;
use crate::ast_reducer::{Exprs, Params};
use crate::environment::Environment as Env;
use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Value, ValueKind};
use komi_util::Range;

/// Returns the evaluated result, from name `name` and its location `location`.
pub fn evaluate_identifier(name: &String, location: &Range, env: &Env) -> ValRes {
    let Some(x) = env.get(name) else {
        return Err(EvalError::new(EvalErrorKind::UndefinedIdentifier, *location));
    };

    match &x.kind {
        ValueKind::Bool(x) => Ok(Value::new(ValueKind::Bool(*x), *location)),
        ValueKind::Number(x) => Ok(Value::new(ValueKind::Number(*x), *location)),
        ValueKind::Closure { parameters, body, env } => Ok(Value::new(
            ValueKind::Closure {
                parameters: parameters.clone(),
                body: body.clone(),
                env: env.clone(),
            },
            *location,
        )),
        ValueKind::BuiltinFunc(builtin_func) => Ok(Value::new(ValueKind::BuiltinFunc(*builtin_func), *location)),
    }
}

/// Returns the evaluated numeric result, from number `num` and its location `location`.
pub fn evaluate_num(num: f64, location: &Range) -> ValRes {
    Ok(Value::new(ValueKind::Number(num), *location))
}

/// Returns the evaluated boolean result, from boolean `boolean` and its location `location`.
pub fn evaluate_bool(boolean: bool, location: &Range) -> ValRes {
    Ok(Value::new(ValueKind::Bool(boolean), *location))
}

pub fn evaluate_closure(parameters: &Params, body: &Exprs, location: &Range, env: &mut Env) -> ValRes {
    Ok(Value::new(
        ValueKind::Closure {
            parameters: parameters.clone(),
            body: body.clone(),
            env: env.clone(),
        },
        *location,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::*;
    use rstest::rstest;

    #[rstest]
    #[case::root_env(root_env(), id_name(), id_value(), id_range())]
    #[case::inner_env(inner_env(), id_name(), id_value(), id_range())]
    fn identifier_evaluated(
        #[case] env: Env,
        #[case] id_name: String,
        #[case] id_value: Value,
        #[case] location: Range,
    ) {
        let evaluated = evaluate_identifier(&id_name, &location, &env);

        assert_eq!(evaluated, Ok(id_value));
    }

    #[rstest]
    #[case::root_env(root_env(), undefined_id_name(), id_range())]
    #[case::inner_env(inner_env(), undefined_id_name(), id_range())]
    fn identifier_undefined(#[case] env: Env, #[case] id_name: String, #[case] location: Range) {
        let evaluated = evaluate_identifier(&id_name, &location, &env);

        assert_eq!(
            evaluated,
            Err(EvalError::new(EvalErrorKind::UndefinedIdentifier, location))
        );
    }

    #[test]
    fn num() {
        let evaluated = evaluate_num(1.0, &range());

        assert_eq!(evaluated, Ok(Value::new(ValueKind::Number(1.0), range())));
    }

    #[test]
    fn bool() {
        let evaluated = evaluate_bool(true, &range());

        assert_eq!(evaluated, Ok(Value::new(ValueKind::Bool(true), range())));
    }

    #[test]
    fn closure() {
        let evaluated = evaluate_closure(&vec![String::from("foo")], &vec![], &range(), &mut root_env());

        assert_eq!(
            evaluated,
            Ok(Value::new(
                ValueKind::Closure {
                    parameters: vec![String::from("foo")],
                    body: vec![],
                    env: root_env()
                },
                range()
            ))
        );
    }

    mod fixtures {
        use super::*;

        pub fn id_range() -> Range {
            Range::from_nums(0, 0, 0, 3) // "foo" or "bar"
        }

        pub fn id_value() -> Value {
            Value::new(ValueKind::Number(1.0), id_range())
        }

        pub fn id_name() -> String {
            "foo".to_string()
        }

        pub fn undefined_id_name() -> String {
            "bar".to_string()
        }

        /// Simulates a root scope, whose environment has no outer environment any more.
        pub fn root_env() -> Env {
            let mut env = Env::new();
            env.set(&id_name(), &id_value());

            env
        }

        /// Simulates an inner scope, whose environment has an outer environment.
        pub fn inner_env() -> Env {
            let mut outer_env = Env::new();
            outer_env.set(&id_name(), &id_value());

            let env = Env::from_outer(outer_env);
            env
        }

        pub fn range() -> Range {
            Range::ORIGIN
        }
    }
}
