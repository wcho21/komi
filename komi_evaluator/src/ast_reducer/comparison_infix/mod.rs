use crate::ValRes;
use crate::environment::Environment as Env;
use crate::reduce_ast;
use komi_syntax::ast::Ast;
use komi_syntax::error::{EvalError, EvalErrorKind};
use komi_syntax::value::{Stdout, Value, ValueKind};
use komi_util::location::Range;

enum Prim {
    Bool(bool),
    Number(f64),
    Str(String),
}

pub fn reduce_double_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let left_val = reduce_ast(left, env, stdouts)?;
    let left_prim = match left_val.kind {
        ValueKind::Bool(b) => Ok(Prim::Bool(b)),
        ValueKind::Number(n) => Ok(Prim::Number(n)),
        ValueKind::Str(s) => Ok(Prim::Str(s)),
        _ => Err(EvalError::new(EvalErrorKind::BadTypeEqLeftOperand, left_val.location)),
    }?;

    let right_val = reduce_ast(right, env, stdouts)?;
    let right_prim = match right_val.kind {
        ValueKind::Bool(b) => Ok(Prim::Bool(b)),
        ValueKind::Number(n) => Ok(Prim::Number(n)),
        ValueKind::Str(s) => Ok(Prim::Str(s)),
        _ => Err(EvalError::new(EvalErrorKind::BadTypeEqRightOperand, right_val.location)),
    }?;

    let infix_val = match (left_prim, right_prim) {
        (Prim::Bool(l), Prim::Bool(r)) => Ok(l == r),
        (Prim::Number(l), Prim::Number(r)) => Ok(l == r),
        (Prim::Str(l), Prim::Str(r)) => Ok(l == r),
        _ => Err(EvalError::new(EvalErrorKind::NotSameTypeInfixOperands, *location)),
    }?;
    Ok(Value::new(ValueKind::Bool(infix_val), *location))
}

pub fn reduce_bang_equals(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let left_val = reduce_ast(left, env, stdouts)?;
    let left_prim = match left_val.kind {
        ValueKind::Bool(b) => Ok(Prim::Bool(b)),
        ValueKind::Number(n) => Ok(Prim::Number(n)),
        ValueKind::Str(s) => Ok(Prim::Str(s)),
        _ => Err(EvalError::new(EvalErrorKind::BadTypeEqLeftOperand, left_val.location)),
    }?;

    let right_val = reduce_ast(right, env, stdouts)?;
    let right_prim = match right_val.kind {
        ValueKind::Bool(b) => Ok(Prim::Bool(b)),
        ValueKind::Number(n) => Ok(Prim::Number(n)),
        ValueKind::Str(s) => Ok(Prim::Str(s)),
        _ => Err(EvalError::new(EvalErrorKind::BadTypeEqRightOperand, right_val.location)),
    }?;

    let infix_val = match (left_prim, right_prim) {
        (Prim::Bool(l), Prim::Bool(r)) => Ok(l != r),
        (Prim::Number(l), Prim::Number(r)) => Ok(l != r),
        (Prim::Str(l), Prim::Str(r)) => Ok(l != r),
        _ => Err(EvalError::new(EvalErrorKind::NotSameTypeInfixOperands, *location)),
    }?;
    Ok(Value::new(ValueKind::Bool(infix_val), *location))
}

pub fn reduce_lbracket(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let left_val = reduce_ast(left, env, stdouts)?;
    let ValueKind::Number(left_prim) = left_val.kind else {
        return Err(EvalError::new(EvalErrorKind::BadTypeOrdLeftOperand, left_val.location));
    };

    let right_val = reduce_ast(right, env, stdouts)?;
    let ValueKind::Number(right_prim) = right_val.kind else {
        return Err(EvalError::new(
            EvalErrorKind::BadTypeOrdRightOperand,
            right_val.location,
        ));
    };

    let infix_val = left_prim < right_prim;
    Ok(Value::new(ValueKind::Bool(infix_val), *location))
}

pub fn reduce_rbracket(
    left: &Box<Ast>,
    right: &Box<Ast>,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let left_val = reduce_ast(left, env, stdouts)?;
    let ValueKind::Number(left_prim) = left_val.kind else {
        return Err(EvalError::new(EvalErrorKind::BadTypeOrdLeftOperand, left_val.location));
    };

    let right_val = reduce_ast(right, env, stdouts)?;
    let ValueKind::Number(right_prim) = right_val.kind else {
        return Err(EvalError::new(
            EvalErrorKind::BadTypeOrdRightOperand,
            right_val.location,
        ));
    };

    let infix_val = left_prim > right_prim;
    Ok(Value::new(ValueKind::Bool(infix_val), *location))
}
