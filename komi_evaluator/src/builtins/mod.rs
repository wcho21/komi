use crate::Environment as Env;
use komi_syntax::value::{Stdout, Value, ValueKind};
use komi_util::location::Range;

pub fn bind(env: &mut Env) -> () {
    env.set("쓰기", &Value::new(ValueKind::BuiltinFunc(stdout_write), Range::ORIGIN))
}

fn stdout_write(args: &Vec<Value>, stdouts: &mut Stdout) -> Value {
    let strs: Vec<String> = args.iter().map(|arg| arg.represent()).collect();
    let joined = strs.join(" ");
    let joined_len = joined.chars().count();

    stdouts.push(joined);

    Value::new(ValueKind::Number(joined_len as f64), Range::ORIGIN)
}
