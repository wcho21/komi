use crate::ValRes;
use crate::ast_reducer::reduce_ast;
use crate::environment::Environment as Env;
use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, AstKind, Stdout, Value};
use komi_util::Range;

pub fn reduce_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let AstKind::Identifier(id_name) = &left.kind else {
        return Err(EvalError::new(EvalErrorKind::NonIdLeftValInAssign, left.location));
    };

    let right_val = reduce_ast(right, env, stdouts)?;
    env.set(id_name, &right_val);

    let assign_val = Value::new(right_val.kind, *location);
    Ok(assign_val)
}

pub fn reduce_equals_with_right_value(left: &Box<Ast>, right: Value, location: &Range, env: &mut Env) -> ValRes {
    let AstKind::Identifier(id_name) = &left.kind else {
        return Err(EvalError::new(EvalErrorKind::NonIdLeftValInAssign, left.location));
    };

    env.set(id_name, &right);

    let assign_val = Value::new(right.kind, *location);
    Ok(assign_val)
}
