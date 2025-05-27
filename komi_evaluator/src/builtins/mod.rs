use crate::Environment;
use komi_syntax::{Stdout, Value, ValueKind};
use komi_util::Range;

pub fn bind(env: &mut Environment) -> () {
    env.set(
        "쓰기",
        &Value::new(ValueKind::BuiltinFunc(stdout_write), Range::from_nums(0, 0, 0, 0)),
    )
}

fn stdout_write(args: &Vec<Value>, stdouts: &mut Stdout) -> Value {
    let strs: Vec<String> = args.iter().map(|arg| arg.represent()).collect();
    let joined = strs.join(" ");

    stdouts.push(joined);

    Value::new(ValueKind::Number(strs.len() as f64), Range::from_nums(0, 0, 0, 0))
}
