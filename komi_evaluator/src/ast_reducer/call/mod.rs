use crate::ast_reducer::{Args, Params};
use crate::environment::Environment as Env;
use crate::{ValRes, ValsRes, reduce_ast};
use komi_syntax::ast::{Ast, AstKind};
use komi_syntax::error::{EvalError, EvalErrorKind};
use komi_syntax::value::{ClosureBodyKind, Stdout, ValueKind};
use komi_util::location::Range;

pub fn evaluate(target: &Box<Ast>, arguments: &Args, location: &Range, env: &mut Env, stdouts: &mut Stdout) -> ValRes {
    let target_closure = reduce_ast(target, env, stdouts)?;

    let ValueKind::Closure { parameters, body, env: closure_env } = target_closure.kind else {
        return Err(EvalError::new(EvalErrorKind::InvalidCallTarget, target.location));
    };

    evaluate_closure(parameters, arguments, body, env, closure_env, location, stdouts)
}

fn evaluate_closure(
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

    let mut val = evaluate_closure_body(body, &mut inner_env, location, stdouts)?;
    val.location = *location;
    Ok(val)
}

fn evaluate_closure_body(body: ClosureBodyKind, env: &mut Env, location: &Range, stdouts: &mut Stdout) -> ValRes {
    match body {
        ClosureBodyKind::Ast(body) => {
            let body_location = Range::new(body[0].location.begin, body[body.len() - 1].location.end);
            let body_as_prog = Box::new(Ast::new(AstKind::Program { expressions: body }, body_location));
            let val = reduce_ast(&body_as_prog, env, stdouts)?;
            Ok(val)
        }
        ClosureBodyKind::Native(f) => {
            let val = f(location, env, stdouts)?;
            Ok(val)
        }
    }
}
