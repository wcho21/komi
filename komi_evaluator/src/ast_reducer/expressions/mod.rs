use super::reduce_ast;
use crate::environment::Environment;
use crate::err::EvalError;
use komi_syntax::{Ast, Value};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

/// Returns the evaluated result of the last AST in the ASTs `expressions`.
///
/// Sets its location to be `expressions_location`, since it represents the entire expressions, not a single one.
pub fn reduce(expressions: &Vec<Box<Ast>>, expressions_location: &Range, env: &Environment) -> ResVal {
    let mut last_value = Value::from_empty(*expressions_location);

    for expression in expressions {
        last_value = reduce_ast(expression, env)?;
    }
    last_value.location = *expressions_location;
    Ok(last_value)
}
