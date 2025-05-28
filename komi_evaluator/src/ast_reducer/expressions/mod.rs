use super::reduce_ast;
use crate::ValRes;
use crate::environment::Environment;
use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, Stdout};
use komi_util::Range;

/// Returns the evaluated result of the last AST in the ASTs `expressions`.
///
/// Sets its location to be `expressions_location`, since it represents the entire expressions, not a single one.
pub fn reduce(
    expressions: &Vec<Box<Ast>>,
    expressions_location: &Range,
    env: &mut Environment,
    stdouts: &mut Stdout,
) -> ValRes {
    let Some(first_expression) = expressions.get(0) else {
        return Err(EvalError::new(EvalErrorKind::NoExpressions, Range::ORIGIN));
    };

    let mut last_value = reduce_ast(first_expression, env, stdouts)?;
    for expression in &expressions[1..] {
        last_value = reduce_ast(expression, env, stdouts)?;
    }

    last_value.location = *expressions_location;

    Ok(last_value)
}
