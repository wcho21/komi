//! Assignment Infix
//!
//! The *assignment infix* means an infix that takes two operands and updates a binding in an environment, such as `=`.

mod assignment_infix_reducer;

use crate::ValRes;
use crate::ast_reducer::combinator_infix as comb;
use crate::environment::Environment as Env;
use assignment_infix_reducer as reducer;
use komi_syntax::{Ast, Stdout};
use komi_util::location::Range;

/// Reduces the operands `left` and `right` of an equals infix to a value.
pub fn reduce_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    reducer::reduce_equals(left, right, location, env, stdouts)
}

/// Reduces the operands `left` and `right` of a plus-equals infix to a value.
pub fn reduce_plus_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let comb_reduced = comb::reduce_plus(left, right, location, env, stdouts)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, location, env)
}

/// Reduces the operands `left` and `right` of a minus-equals infix to a value.
pub fn reduce_minus_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let comb_reduced = comb::reduce_minus(left, right, location, env, stdouts)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, location, env)
}

/// Reduces the operands `left` and `right` of a asterisk-equals infix to a value.
pub fn reduce_asterisk_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let comb_reduced = comb::reduce_asterisk(left, right, location, env, stdouts)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, location, env)
}

/// Reduces the operands `left` and `right` of a slash-equals infix to a value.
pub fn reduce_slash_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let comb_reduced = comb::reduce_slash(left, right, location, env, stdouts)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, location, env)
}

/// Reduces the operands `left` and `right` of a percent-equals infix to a value.
pub fn reduce_percent_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let comb_reduced = comb::reduce_percent(left, right, location, env, stdouts)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, location, env)
}
