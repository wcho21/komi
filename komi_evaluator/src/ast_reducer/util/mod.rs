use super::reduce_ast;
use crate::environment::Environment;
use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, Stdout, ValueKind};

/// Reduces the AST `ast` to an evaluated result, and map it with `op`.
///
/// `op` should return `EvalErrorKind` on erroneous case, which then automatically converted into `EvalError` in the returned result.
pub fn reduce_and_map_kind<T, F>(
    ast: &Box<Ast>,
    env: &mut Environment,
    stdouts: &mut Stdout,
    op: F,
) -> Result<T, EvalError>
where
    F: Fn(&ValueKind) -> Result<T, EvalErrorKind>,
{
    let val = reduce_ast(ast, env, stdouts)?;

    match op(&val.kind) {
        Ok(x) => Ok(x),
        Err(kind) => Err(EvalError::new(kind, val.location)),
    }
}

pub fn get_num_primitive_or_error(
    ast: &Box<Ast>,
    error_kind: EvalErrorKind,
    env: &mut Environment,
    stdouts: &mut Stdout,
) -> Result<f64, EvalError> {
    reduce_and_map_kind(ast, env, stdouts, |kind| match kind {
        ValueKind::Number(x) => Ok(*x),
        _ => Err(error_kind.clone()),
    })
}

pub fn get_bool_primitive_or_error(
    ast: &Box<Ast>,
    error_kind: EvalErrorKind,
    env: &mut Environment,
    stdouts: &mut Stdout,
) -> Result<bool, EvalError> {
    reduce_and_map_kind(ast, env, stdouts, |kind| match kind {
        ValueKind::Bool(x) => Ok(*x),
        _ => Err(error_kind.clone()),
    })
}
