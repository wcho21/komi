use super::reduce_ast;
use crate::ValRes;
use crate::ast_reducer::Exprs;
use crate::environment::Environment as Env;
use komi_syntax::Stdout;
use komi_syntax::error::{EvalError, EvalErrorKind};
use komi_util::location::Range;

/// Returns the evaluated result of the last AST in the ASTs `expressions`.
///
/// Sets its location to be `expressions_location`, since it represents the entire expressions, not a single one.
pub fn reduce(expressions: &Exprs, expressions_location: &Range, env: &mut Env, stdouts: &mut Stdout) -> ValRes {
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
