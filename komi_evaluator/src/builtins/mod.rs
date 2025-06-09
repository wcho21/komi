use crate::Environment as Env;
use crate::ValRes;
use komi_syntax::error::{EvalError, EvalErrorKind};
use komi_syntax::value::{Stdout, Value, ValueKind};
use komi_util::location::Range;

pub fn bind(env: &mut Env) -> () {
    env.set("쓰기", &Value::new(ValueKind::BuiltinFunc(stdout_write), Range::ORIGIN))
}

fn stdout_write(location: &Range, args: &Vec<Value>, stdouts: &mut Stdout) -> ValRes {
    if args.len() == 0 {
        return Err(EvalError::new(EvalErrorKind::BadNumArgs, *location));
    }

    let strs: Vec<String> = args.iter().map(|arg| arg.represent()).collect();
    let joined = strs.join(" ");
    let joined_len = joined.chars().count();

    stdouts.push(joined);

    Ok(Value::new(ValueKind::Number(joined_len as f64), Range::ORIGIN))
}
