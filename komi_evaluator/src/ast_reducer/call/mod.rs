use crate::ast_reducer::{Args, Exprs, Params};
use crate::environment::Environment as Env;
use crate::{ValRes, ValsRes, reduce_ast};
use komi_syntax::ast::{Ast, AstKind};
use komi_syntax::error::{EvalError, EvalErrorKind};
use komi_syntax::value::{BuiltinFunc, ClosureBodyKind, Stdout, Value, ValueKind};
use komi_util::location::Range;

pub fn evaluate(target: &Box<Ast>, arguments: &Args, location: &Range, env: &mut Env, stdouts: &mut Stdout) -> ValRes {
    let target_closure = reduce_ast(target, env, stdouts)?;

    match target_closure.kind {
        ValueKind::Closure { parameters, body, env: closure_env } => {
            evaluate_closure(parameters, arguments, body, env, closure_env, location, stdouts)
        }
        ValueKind::BuiltinFunc(builtin_func) => evaluate_builtin_func(builtin_func, arguments, env, location, stdouts),
        ValueKind::ClosureAlt { parameters, body, env: closure_env } => {
            evaluate_closure_alt(parameters, arguments, body, env, closure_env, location, stdouts)
        }
        _ => Err(EvalError::new(EvalErrorKind::InvalidCallTarget, target.location)),
    }
}

fn evaluate_closure_alt(
    parameters: Params,
    arguments: &Args,
    body: ClosureBodyKind,
    outer_env: &mut Env,
    closure_env: Env,
    location: &Range,
    stdouts: &mut Stdout,
) -> ValRes {
    if arguments.len() != parameters.len() {
        return Err(EvalError::new(EvalErrorKind::BadNumArgs, *location));
    }

    let arg_vals_res: ValsRes = arguments
        .iter()
        .map(|arg| reduce_ast(arg, outer_env, stdouts))
        .collect();
    let arg_vals = arg_vals_res?;

    let mut inner_env = Env::from_outer(closure_env);
    for (param, arg) in parameters.iter().zip(arg_vals.iter()) {
        inner_env.set(param, arg);
    }

    let mut val = evaluate_closure_alt_body(body, &mut inner_env, location, &arg_vals, stdouts)?;
    val.location = *location;
    Ok(val)
}

fn evaluate_closure_alt_body(
    body: ClosureBodyKind,
    env: &mut Env,
    location: &Range,
    arguments: &Vec<Value>,
    stdouts: &mut Stdout,
) -> ValRes {
    match body {
        ClosureBodyKind::Ast(body) => {
            let body_location = Range::new(body[0].location.begin, body[body.len() - 1].location.end);
            let body_as_prog = Box::new(Ast::new(AstKind::Program { expressions: body }, body_location));
            let val = reduce_ast(&body_as_prog, env, stdouts)?;
            Ok(val)
        }
        ClosureBodyKind::Native(f) => {
            let val = f(location, arguments, stdouts)?;
            Ok(val)
        }
    }
}

fn evaluate_builtin_func(
    builtin_func: BuiltinFunc,
    arguments: &Args,
    env: &mut Env,
    location: &Range,
    stdouts: &mut Stdout,
) -> ValRes {
    let arg_vals_res: ValsRes = arguments.iter().map(|arg| reduce_ast(arg, env, stdouts)).collect();
    let arg_vals = arg_vals_res?;

    builtin_func(location, &arg_vals, stdouts)
}

fn evaluate_closure(
    parameters: Params,
    arguments: &Args,
    body: Exprs,
    outer_env: &mut Env,
    closure_env: Env,
    location: &Range,
    stdouts: &mut Stdout,
) -> ValRes {
    if arguments.len() != parameters.len() {
        return Err(EvalError::new(EvalErrorKind::BadNumArgs, *location));
    }

    let arg_vals_res: ValsRes = arguments
        .iter()
        .map(|arg| reduce_ast(arg, outer_env, stdouts))
        .collect();
    let arg_vals = arg_vals_res?;

    let mut inner_env = Env::from_outer(closure_env);
    for (param, arg) in parameters.iter().zip(arg_vals.iter()) {
        inner_env.set(param, arg);
    }

    let body_location = Range::new(body[0].location.begin, body[body.len() - 1].location.end);
    let body_as_prog = Box::new(Ast::new(AstKind::Program { expressions: body }, body_location));
    let mut val = reduce_ast(&body_as_prog, &mut inner_env, stdouts)?;

    val.location = *location;
    Ok(val)
}
