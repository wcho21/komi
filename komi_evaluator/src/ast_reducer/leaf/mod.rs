use crate::err::EvalError;
use komi_syntax::{Value, ValueKind};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Returns the evaluated numeric result, from number `num` and its location `location`.
pub fn evaluate_num(num: f64, location: &Range) -> ResVal {
    Ok(Value::new(ValueKind::Number(num), *location))
}

/// Returns the evaluated boolean result, from boolean `boolean` and its location `location`.
pub fn evaluate_bool(boolean: bool, location: &Range) -> ResVal {
    Ok(Value::new(ValueKind::Bool(boolean), *location))
}
