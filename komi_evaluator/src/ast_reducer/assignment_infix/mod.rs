//! Assignment Infix
//!
//! The *assignment infix* means an infix that takes two operands and updates a binding in an environment, such as `=`.

mod assignment_infix_reducer;

use crate::ast_reducer::combinator_infix as comb;
use crate::environment::Environment;
use crate::err::EvalError;
use assignment_infix_reducer as reducer;
use komi_syntax::{Ast, Value};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Reduces the operands `left` and `right` of an equals infix to a value.
pub fn reduce_equals(left: &Ast, right: &Ast, infix_location: &Range, env: &mut Environment) -> ResVal {
    reducer::reduce_equals(left, right, infix_location, env)
}

/// Reduces the operands `left` and `right` of a plus-equals infix to a value.
pub fn reduce_plus_equals(left: &Ast, right: &Ast, infix_location: &Range, env: &mut Environment) -> ResVal {
    let comb_reduced = comb::reduce_plus(left, right, infix_location, env)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, infix_location, env)
}

/// Reduces the operands `left` and `right` of a minus-equals infix to a value.
pub fn reduce_minus_equals(left: &Ast, right: &Ast, infix_location: &Range, env: &mut Environment) -> ResVal {
    let comb_reduced = comb::reduce_minus(left, right, infix_location, env)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, infix_location, env)
}

/// Reduces the operands `left` and `right` of a asterisk-equals infix to a value.
pub fn reduce_asterisk_equals(left: &Ast, right: &Ast, infix_location: &Range, env: &mut Environment) -> ResVal {
    let comb_reduced = comb::reduce_asterisk(left, right, infix_location, env)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, infix_location, env)
}

/// Reduces the operands `left` and `right` of a slash-equals infix to a value.
pub fn reduce_slash_equals(left: &Ast, right: &Ast, infix_location: &Range, env: &mut Environment) -> ResVal {
    let comb_reduced = comb::reduce_slash(left, right, infix_location, env)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, infix_location, env)
}

/// Reduces the operands `left` and `right` of a percent-equals infix to a value.
pub fn reduce_percent_equals(left: &Ast, right: &Ast, infix_location: &Range, env: &mut Environment) -> ResVal {
    let comb_reduced = comb::reduce_percent(left, right, infix_location, env)?;

    reducer::reduce_equals_with_right_value(left, comb_reduced, infix_location, env)
}
