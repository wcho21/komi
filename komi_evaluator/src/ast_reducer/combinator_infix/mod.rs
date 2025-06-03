//! Combinator Infix
//!
//! The *combinator infix* means an infix that takes two operands and combines them into a value, such as arithmetic addition (`+`), or boolean conjunction (`&&`).

mod combinator_infix_reducer;

use super::util;
use crate::ValRes;
use crate::environment::Environment as Env;
use crate::reduce_ast;
use combinator_infix_reducer as reducer;
use komi_syntax::ast::Ast;
use komi_syntax::error::{EvalError, EvalErrorKind};
use komi_syntax::value::{Stdout, Value, ValueKind};
use komi_util::location::Range;

/// Reduces the operands `left` and `right` of a plus infix to a value.
pub fn reduce_plus(left: &Box<Ast>, right: &Box<Ast>, location: &Range, env: &mut Env, stdouts: &mut Stdout) -> ValRes {
    let left_val = reduce_ast(left, env, stdouts)?;

    match left_val.kind {
        ValueKind::Number(left_num) => reduce_plus_with_left_num(left_num, right, location, env, stdouts),
        ValueKind::Str(left_str) => reduce_plus_with_left_str(left_str, right, location, env, stdouts),
        _ => Err(EvalError::new(
            EvalErrorKind::NonNumOrStrInfixLeftOperand,
            left_val.location,
        )),
    }
}

fn reduce_plus_with_left_num(
    left_num: f64,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let right_val = reduce_ast(right, env, stdouts)?;

    match right_val.kind {
        ValueKind::Number(right_num) => Ok(Value::new(ValueKind::Number(left_num + right_num), *location)),
        _ => Err(EvalError::new(
            EvalErrorKind::NonNumInfixRightOperand,
            right_val.location,
        )),
    }
}

fn reduce_plus_with_left_str(
    left_str: String,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let right_val = reduce_ast(right, env, stdouts)?;

    match right_val.kind {
        ValueKind::Str(right_str) => Ok(Value::new(ValueKind::Str(left_str + &right_str), *location)),
        _ => Err(EvalError::new(
            EvalErrorKind::NonNumInfixRightOperand,
            right_val.location,
        )),
    }
}

/// Reduces the operands `left` and `right` of a minus infix to a value.
pub fn reduce_minus(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    reducer::reduce_num(
        left,
        right,
        location,
        env,
        stdouts,
        |l, r| l - r,
        |v| ValueKind::Number(v),
    )
}

/// Reduces the operands `left` and `right` of a asterisk infix to a value.
pub fn reduce_asterisk(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let left_val = reduce_ast(left, env, stdouts)?;

    match left_val.kind {
        ValueKind::Number(left_num) => reduce_asterisk_with_left_num(left_num, right, location, env, stdouts),
        ValueKind::Str(left_str) => reduce_asterisk_with_left_str(left_str, right, location, env, stdouts),
        _ => Err(EvalError::new(
            EvalErrorKind::NonNumOrStrInfixLeftOperand,
            left_val.location,
        )),
    }
}

fn reduce_asterisk_with_left_num(
    left_num: f64,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let right_val = reduce_ast(right, env, stdouts)?;

    match right_val.kind {
        ValueKind::Number(right_num) => Ok(Value::new(ValueKind::Number(left_num * right_num), *location)),
        _ => Err(EvalError::new(
            EvalErrorKind::NonNumInfixRightOperand,
            right_val.location,
        )),
    }
}

fn reduce_asterisk_with_left_str(
    left_str: String,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let right_val = reduce_ast(right, env, stdouts)?;

    match right_val.kind {
        ValueKind::Number(right_num) if right_num < 0.0 || right_num.fract() != 0.0 => Err(EvalError::new(
            EvalErrorKind::NonNonnegIntInfixRightOperand,
            right_val.location,
        )),
        ValueKind::Number(right_num) => {
            let right_num = right_num as usize; // Can be safely converted since a non-negative integer, as verified above
            Ok(Value::new(ValueKind::Str(left_str.repeat(right_num)), *location))
        }
        _ => Err(EvalError::new(
            EvalErrorKind::NonNumInfixRightOperand,
            right_val.location,
        )),
    }
}

/// Reduces the operands `left` and `right` of a slash infix to a value.
pub fn reduce_slash(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    reducer::reduce_num(
        left,
        right,
        location,
        env,
        stdouts,
        |l, r| l / r,
        |v| ValueKind::Number(v),
    )
}

/// Reduces the operands `left` and `right` of a percent infix to a value.
pub fn reduce_percent(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    reducer::reduce_num(
        left,
        right,
        location,
        env,
        stdouts,
        |l, r| l % r,
        |v| ValueKind::Number(v),
    )
}

/// Reduces the operands `left` and `right` of a conjunction infix to a value.
pub fn reduce_conjunct(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    reducer::reduce_bool(
        left,
        right,
        location,
        env,
        stdouts,
        |l, r| l && r,
        |v| ValueKind::Bool(v),
    )
}

/// Reduces the operands `left` and `right` of a disjunction infix to a value.
pub fn reduce_disjunct(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    reducer::reduce_bool(
        left,
        right,
        location,
        env,
        stdouts,
        |l, r| l || r,
        |v| ValueKind::Bool(v),
    )
}
