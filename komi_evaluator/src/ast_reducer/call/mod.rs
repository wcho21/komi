use crate::environment::Environment;
use crate::err::{EvalError, EvalErrorKind};
use crate::reduce_ast;
use komi_syntax::{Ast, AstKind, Value, ValueKind};
use komi_util::Range;

// TODO: rename type to ValRes and export to publish
type ResVal = Result<Value, EvalError>;

pub fn evaluate(target: &Box<Ast>, arguments: &Vec<Box<Ast>>, location: &Range, env: &mut Environment) -> ResVal {
    let target_closure = reduce_ast(target, env)?;

    let ValueKind::Closure { parameters, body, mut env } = target_closure.kind else {
        return Err(EvalError::new(EvalErrorKind::InvalidCallTarget, target.location));
    };

    let arg_vals_res: Result<Vec<Value>, EvalError> = arguments.iter().map(|arg| reduce_ast(arg, &mut env)).collect();
    let arg_vals = arg_vals_res?;

    let mut inner_env = Environment::from_outer(env);
    for (param, arg) in parameters.iter().zip(arg_vals.iter()) {
        inner_env.set(param, arg);
    }

    let body_location = Range::new(body[0].location.begin, body[body.len() - 1].location.end);
    let body_as_prog = Box::new(Ast::new(AstKind::Program { expressions: body }, body_location));
    let mut val = reduce_ast(&body_as_prog, &mut inner_env)?;

    val.location = *location;
    Ok(val)
}
