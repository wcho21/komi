use super::util;
use crate::environment::Environment;
use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, Value, ValueKind};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Reduces the operand `operand` of a numeric infix operand to a value.
/// Its primitive value and kind are determined by `reduce_infix` and `get_kind`, respectively.
pub fn reduce_num<F, G>(
    left: &Ast,
    right: &Ast,
    location: &Range,
    env: &mut Environment,
    reduce_infix: F,
    get_kind: G,
) -> ResVal
where
    F: Fn(f64, f64) -> f64,
    G: Fn(f64) -> ValueKind,
{
    reduce(left, right, location, env, get_num_primitive, reduce_infix, get_kind)
}

/// Reduces the operand `operand` of a boolean infix operand to a value.
/// Its primitive value and kind are determined by `reduce_infix` and `get_kind`, respectively.
pub fn reduce_bool<F, G>(
    left: &Ast,
    right: &Ast,
    location: &Range,
    env: &mut Environment,
    reduce_infix: F,
    get_kind: G,
) -> ResVal
where
    F: Fn(bool, bool) -> bool,
    G: Fn(bool) -> ValueKind,
{
    reduce(left, right, location, env, get_bool_primitive, reduce_infix, get_kind)
}

fn get_num_primitive(ast: &Ast, env: &mut Environment) -> Result<f64, EvalError> {
    util::get_num_primitive_or_error(ast, EvalErrorKind::InvalidNumInfixOperand, env)
}

fn get_bool_primitive(ast: &Ast, env: &mut Environment) -> Result<bool, EvalError> {
    util::get_bool_primitive_or_error(ast, EvalErrorKind::InvalidBoolInfixOperand, env)
}

/// Reduces the operand `operand` of an infix to a value.
///
/// - `reduce_operand` determines how to reduce the `left` and `right` themselves to some values `x` and `y`, respectively.
/// - `reduce_infix` maps `x` and `y` to `z`, at the primitive level.
/// - `get_kind` specifies what kind to return from `z`.
///
/// The location is determined by `location`.
fn reduce<T, F, G, H>(
    left: &Ast,
    right: &Ast,
    location: &Range,
    env: &mut Environment,
    reduce_operand: F,
    reduce_infix: G,
    get_kind: H,
) -> ResVal
where
    F: Fn(&Ast, &mut Environment) -> Result<T, EvalError>,
    G: Fn(T, T) -> T,
    H: Fn(T) -> ValueKind,
{
    let left_val = reduce_operand(left, env)?;
    let right_val = reduce_operand(right, env)?;
    let infix_val = reduce_infix(left_val, right_val);

    let kind = get_kind(infix_val);

    Ok(Value::new(kind, *location))
}
