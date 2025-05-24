use crate::ast_reducer::reduce_ast;
use crate::environment::Environment;
use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, AstKind, Value};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

pub fn reduce_equals(left: &Ast, right: &Ast, location: &Range, env: &mut Environment) -> ResVal {
    let AstKind::Identifier(id_name) = &left.kind else {
        return Err(EvalError::new(EvalErrorKind::InvalidAssignmentLeftValue, left.location));
    };

    let right_val = reduce_ast(right, env)?;
    env.set(id_name, &right_val);

    let assign_val = Value::new(right_val.kind, *location);
    Ok(assign_val)
}

pub fn reduce_equals_with_right_value(left: &Ast, right: Value, location: &Range, env: &mut Environment) -> ResVal {
    let AstKind::Identifier(id_name) = &left.kind else {
        return Err(EvalError::new(EvalErrorKind::InvalidAssignmentLeftValue, left.location));
    };

    env.set(id_name, &right);

    let assign_val = Value::new(right.kind, *location);
    Ok(assign_val)
}
