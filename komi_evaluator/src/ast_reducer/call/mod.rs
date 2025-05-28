use crate::ast_reducer::{Args, Exprs, Params};
use crate::environment::Environment as Env;
use crate::err::{EvalError, EvalErrorKind};
use crate::{ValRes, ValsRes, reduce_ast};
use komi_syntax::{Ast, AstKind, BuiltinFunc, Stdout, ValueKind};
use komi_util::Range;

pub fn evaluate(target: &Box<Ast>, arguments: &Args, location: &Range, env: &mut Env, stdouts: &mut Stdout) -> ValRes {
    let target_closure = reduce_ast(target, env, stdouts)?;

    match target_closure.kind {
        ValueKind::Closure { parameters, body, env: closure_env } => {
            evaluate_closure(parameters, arguments, body, env, closure_env, location, stdouts)
        }
        ValueKind::BuiltinFunc(builtin_func) => evaluate_builtin_func(builtin_func, arguments, env, stdouts),
        _ => Err(EvalError::new(EvalErrorKind::InvalidCallTarget, target.location)),
    }
}

fn evaluate_builtin_func(builtin_func: BuiltinFunc, arguments: &Args, env: &mut Env, stdouts: &mut Stdout) -> ValRes {
    let arg_vals_res: ValsRes = arguments.iter().map(|arg| reduce_ast(arg, env, stdouts)).collect();
    let arg_vals = arg_vals_res?;

    Ok(builtin_func(&arg_vals, stdouts))
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
