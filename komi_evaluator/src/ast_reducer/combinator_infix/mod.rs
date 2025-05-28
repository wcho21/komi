//! Combinator Infix
//!
//! The *combinator infix* means an infix that takes two operands and combines them into a value, such as arithmetic addition (`+`), or boolean conjunction (`&&`).

mod combinator_infix_reducer;

use super::util;
use crate::ValRes;
use crate::environment::Environment;
use combinator_infix_reducer as reducer;
use komi_syntax::{Ast, Stdout, ValueKind};
use komi_util::Range;

/// Reduces the operands `left` and `right` of a plus infix to a value.
pub fn reduce_plus(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Environment,
    stdouts: &mut Stdout,
) -> ValRes {
    reducer::reduce_num(
        left,
        right,
        location,
        env,
        stdouts,
        |l, r| l + r,
        |v| ValueKind::Number(v),
    )
}

/// Reduces the operands `left` and `right` of a minus infix to a value.
pub fn reduce_minus(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Environment,
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
    env: &mut Environment,
    stdouts: &mut Stdout,
) -> ValRes {
    reducer::reduce_num(
        left,
        right,
        location,
        env,
        stdouts,
        |l, r| l * r,
        |v| ValueKind::Number(v),
    )
}

/// Reduces the operands `left` and `right` of a slash infix to a value.
pub fn reduce_slash(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Environment,
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
    env: &mut Environment,
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
    env: &mut Environment,
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
    env: &mut Environment,
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
