use crate::Environment as Env;
use crate::ValRes;
use komi_syntax::value::{ClosureBodyKind, Stdout, Value, ValueKind};
use komi_util::location::Range;

pub fn bind(env: &mut Env) -> () {
    // Note: a dummy location used here, since it will be determined later when evaluated as an identifier.
    env.set(
        "쓰기",
        &Value::new(
            ValueKind::Closure {
                parameters: vec![String::from("내용")],
                body: ClosureBodyKind::Native(stdout_write),
                env: Env::new(),
            },
            Range::ORIGIN,
        ),
    );
    env.set(
        "타입",
        &Value::new(
            ValueKind::Closure {
                parameters: vec![String::from("값")],
                body: ClosureBodyKind::Native(get_type),
                env: Env::new(),
            },
            Range::ORIGIN,
        ),
    );
}

fn stdout_write(location: &Range, env: &Env, stdouts: &mut Stdout) -> ValRes {
    let arg = env.get("내용").unwrap();
    let str = arg.represent();
    let str_len = str.chars().count();

    stdouts.push(str);

    Ok(Value::new(ValueKind::Number(str_len as f64), *location))
}

fn get_type(location: &Range, env: &Env, _stdouts: &mut Stdout) -> ValRes {
    let arg = env.get("값").unwrap();
    let arg_type = match arg.kind {
        ValueKind::Bool(_) => "불리언",
        ValueKind::Number(_) => "숫자",
        ValueKind::Str(_) => "문자",
        ValueKind::Closure { .. } => "함수",
    };

    Ok(Value::new(ValueKind::Str(String::from(arg_type)), *location))
}
