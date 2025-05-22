//! Assignment Infix
//!
//! The *assignment infix* means an infix that takes two operands and updates a binding in an environment, such as `=`.

mod assignment_infix_reducer;

use crate::environment::Environment;
use crate::err::EvalError;
use assignment_infix_reducer as reducer;
use komi_syntax::{Ast, Value};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Reduces the operands `left` and `right` of a equal infix to a value.
pub fn reduce_equals(left: &Ast, right: &Ast, infix_location: &Range, env: &mut Environment) -> ResVal {
    reducer::reduce_equals(left, right, infix_location, env)
}
