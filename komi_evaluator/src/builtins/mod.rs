use crate::Environment;
use komi_syntax::{Value, ValueKind};
use komi_util::Range;

type StdoutHandler = fn(&str) -> ();

pub fn bind(env: &mut Environment) -> () {
    env.set(
        "쓰기",
        &Value::new(ValueKind::BuiltinFunc(stdout_write), Range::from_nums(0, 0, 0, 0)),
    )
}

fn stdout_write(args: &Vec<Value>, stdout_handler: Option<StdoutHandler>) -> Value {
    let strs: Vec<String> = args.iter().map(|arg| arg.represent()).collect();
    let joined = strs.join(" ");

    if let Some(handler) = stdout_handler {
        handler(&joined);
    }

    Value::new(ValueKind::Number(strs.len() as f64), Range::from_nums(0, 0, 0, 0))
}
