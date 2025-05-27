mod prefix_reducer;

use crate::environment::Environment;
use crate::err::EvalError;
use komi_syntax::{Ast, Stdout, Value, ValueKind};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Reduces the operand `operand` of a plus prefix to a value, with its location spanning from the prefix to the operand.
pub fn reduce_plus(operand: &Box<Ast>, prefix_location: &Range, env: &mut Environment, stdouts: &mut Stdout) -> ResVal {
    prefix_reducer::reduce_num(operand, prefix_location, env, stdouts, |v| ValueKind::Number(v))
}

/// Reduces the operand `operand` of a plus prefix to a value, with its location spanning from the prefix to the operand.
pub fn reduce_minus(
    operand: &Box<Ast>,
    prefix_location: &Range,
    env: &mut Environment,
    stdouts: &mut Stdout,
) -> ResVal {
    prefix_reducer::reduce_num(operand, prefix_location, env, stdouts, |v| ValueKind::Number(-v))
}

/// Reduces the operand `operand` of a plus prefix to a value, with its location spanning from the prefix to the operand.
pub fn reduce_bang(operand: &Box<Ast>, prefix_location: &Range, env: &mut Environment, stdouts: &mut Stdout) -> ResVal {
    prefix_reducer::reduce_bool(operand, prefix_location, env, stdouts, |v| ValueKind::Bool(!v))
}
