use crate::ast_reducer::util;
use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, Value, ValueKind};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Reduces the operand `operand` of a numeric prefix operand to a value, with its kind determined by `get_kind`.
pub fn reduce_num<F>(operand: &Ast, prefix_location: &Range, get_kind: F) -> ResVal
where
    F: Fn(f64) -> ValueKind,
{
    reduce(operand, prefix_location, get_num_primitive, get_kind)
}

/// Reduces the operand `operand` of a boolean prefix operand to a value, with its kind determined by `get_kind`.
pub fn reduce_bool<F>(operand: &Ast, prefix_location: &Range, get_kind: F) -> ResVal
where
    F: Fn(bool) -> ValueKind,
{
    reduce(operand, prefix_location, get_bool_primitive, get_kind)
}

fn get_num_primitive(ast: &Ast) -> Result<f64, EvalError> {
    util::get_num_primitive_or_error(ast, EvalErrorKind::InvalidPrefixNumOperand)
}

fn get_bool_primitive(ast: &Ast) -> Result<bool, EvalError> {
    util::get_bool_primitive_or_error(ast, EvalErrorKind::InvalidPrefixBoolOperand)
}

/// Reduces the operand `operand` of a prefix to an evaluated result.
///
/// - `reduce_operand` determines how to reduce the `operand` itself to some value `x`.
/// - `get_kind` specifies what kind to return from `x`.
///
/// The location in the returned value will span from the prefix to operand.
fn reduce<T, F, G>(operand: &Ast, prefix_location: &Range, reduce_operand: F, get_kind: G) -> ResVal
where
    F: Fn(&Ast) -> Result<T, EvalError>,
    G: Fn(T) -> ValueKind,
{
    let reduced = reduce_operand(operand)?;
    let kind = get_kind(reduced);

    let location = Range::new(prefix_location.begin, operand.location.end);

    Ok(Value::new(kind, location))
}
