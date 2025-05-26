use crate::environment::Environment;
use crate::err::{EvalError, EvalErrorKind};
use crate::reduce_ast;
use komi_syntax::{Ast, AstKind, Value, ValueKind};
use komi_util::Range;

use komi_syntax::BuiltinFunc;
// TODO: publish StdoutHandler in root
type StdoutHandler = fn(&str) -> ();
// TODO: rename type to ValRes and export to publish
type ResVal = Result<Value, EvalError>;

pub fn evaluate(
    target: &Box<Ast>,
    arguments: &Vec<Box<Ast>>,
    location: &Range,
    env: &mut Environment,
    stdout_handler: Option<StdoutHandler>,
) -> ResVal {
    let target_closure = reduce_ast(target, env, None)?;

    match target_closure.kind {
        ValueKind::Closure { parameters, body, env: closure_env } => {
            evaluate_closure(parameters, arguments, body, env, closure_env, location)
        }
        ValueKind::BuiltinFunc(builtin_func) => evaluate_builtin_func(builtin_func, arguments, env, stdout_handler),
        _ => Err(EvalError::new(EvalErrorKind::InvalidCallTarget, target.location)),
    }
}

fn evaluate_builtin_func(
    builtin_func: BuiltinFunc,
    arguments: &Vec<Box<Ast>>,
    env: &mut Environment,
    stdout_handler: Option<StdoutHandler>,
) -> ResVal {
    let arg_vals_res: Result<Vec<Value>, EvalError> = arguments.iter().map(|arg| reduce_ast(arg, env, None)).collect();
    let arg_vals = arg_vals_res?;

    Ok(builtin_func(&arg_vals, stdout_handler))
}

fn evaluate_closure(
    parameters: Vec<String>,
    arguments: &Vec<Box<Ast>>,
    body: Vec<Box<Ast>>,
    outer_env: &mut Environment,
    closure_env: Environment,
    location: &Range,
) -> ResVal {
    let arg_vals_res: Result<Vec<Value>, EvalError> =
        arguments.iter().map(|arg| reduce_ast(arg, outer_env, None)).collect();
    let arg_vals = arg_vals_res?;

    let mut inner_env = Environment::from_outer(closure_env);
    for (param, arg) in parameters.iter().zip(arg_vals.iter()) {
        inner_env.set(param, arg);
    }

    let body_location = Range::new(body[0].location.begin, body[body.len() - 1].location.end);
    let body_as_prog = Box::new(Ast::new(AstKind::Program { expressions: body }, body_location));
    let mut val = reduce_ast(&body_as_prog, &mut inner_env, None)?;

    val.location = *location;
    Ok(val)
}
