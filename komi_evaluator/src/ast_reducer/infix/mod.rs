mod infix_reducer;

use super::util;
use crate::err::EvalError;
use komi_syntax::{Ast, Value, ValueKind};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Reduces the operands `left` and `right` of a plus infix to a value.
pub fn reduce_plus(left: &Ast, right: &Ast, infix_location: &Range) -> ResVal {
    infix_reducer::reduce_num(left, right, infix_location, |l, r| l + r, |v| ValueKind::Number(v))
}

/// Reduces the operands `left` and `right` of a minus infix to a value.
pub fn reduce_minus(left: &Ast, right: &Ast, infix_location: &Range) -> ResVal {
    infix_reducer::reduce_num(left, right, infix_location, |l, r| l - r, |v| ValueKind::Number(v))
}

/// Reduces the operands `left` and `right` of a asterisk infix to a value.
pub fn reduce_asterisk(left: &Ast, right: &Ast, infix_location: &Range) -> ResVal {
    infix_reducer::reduce_num(left, right, infix_location, |l, r| l * r, |v| ValueKind::Number(v))
}

/// Reduces the operands `left` and `right` of a slash infix to a value.
pub fn reduce_slash(left: &Ast, right: &Ast, infix_location: &Range) -> ResVal {
    infix_reducer::reduce_num(left, right, infix_location, |l, r| l / r, |v| ValueKind::Number(v))
}

/// Reduces the operands `left` and `right` of a percent infix to a value.
pub fn reduce_percent(left: &Ast, right: &Ast, infix_location: &Range) -> ResVal {
    infix_reducer::reduce_num(left, right, infix_location, |l, r| l % r, |v| ValueKind::Number(v))
}

/// Reduces the operands `left` and `right` of a conjunction infix to a value.
pub fn reduce_conjunct(left: &Ast, right: &Ast, infix_location: &Range) -> ResVal {
    infix_reducer::reduce_bool(left, right, infix_location, |l, r| l && r, |v| ValueKind::Bool(v))
}

/// Reduces the operands `left` and `right` of a disjunction infix to a value.
pub fn reduce_disjunct(left: &Ast, right: &Ast, infix_location: &Range) -> ResVal {
    infix_reducer::reduce_bool(left, right, infix_location, |l, r| l || r, |v| ValueKind::Bool(v))
}
