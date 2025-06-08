use crate::ValRes;
use crate::environment::Environment as Env;
use crate::reduce_ast;
use komi_syntax::ast::Ast;
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
        ValueKind::Bool(b) => Prim::Bool(b),
        ValueKind::Number(n) => Prim::Number(n),
        ValueKind::Str(s) => Prim::Str(s),
        _ => todo!(),
    };

    let right_val = reduce_ast(right, env, stdouts)?;
    let right_prim = match right_val.kind {
        ValueKind::Bool(b) => Prim::Bool(b),
        ValueKind::Number(n) => Prim::Number(n),
        ValueKind::Str(s) => Prim::Str(s),
        _ => todo!(),
    };

    let infix_val = match (left_prim, right_prim) {
        (Prim::Bool(l), Prim::Bool(r)) => l == r,
        (Prim::Number(l), Prim::Number(r)) => l == r,
        (Prim::Str(l), Prim::Str(r)) => l == r,
        _ => todo!(),
    };
    Ok(Value::new(ValueKind::Bool(infix_val), *location))
}
