use crate::ValRes;
use crate::environment::Environment as Env;
use crate::reduce_ast;
use komi_syntax::ast::Ast;
use komi_syntax::error::{EvalError, EvalErrorKind};
use komi_syntax::value::{Stdout, Value, ValueKind};
use komi_util::location::Range;

pub fn reduce(left: &Box<Ast>, right: &Box<Ast>, location: &Range, env: &mut Env, stdouts: &mut Stdout) -> ValRes {
    let left_val = reduce_ast(left, env, stdouts)?;
    let right_val = reduce_ast(right, env, stdouts)?;
    let ValueKind::Closure { parameters: closure_params, body, env: closure_env } = right_val.kind else {
        return Err(EvalError::new(
            EvalErrorKind::NotClosureRightOperand,
            right_val.location,
        ));
    };
    if closure_params.len() == 0 {
        return Err(EvalError::new(EvalErrorKind::NoParamsToBind, right_val.location));
    }

    let mut env = closure_env.clone();
    env.set(&closure_params[0], &left_val);
    let params: Vec<String> = closure_params.into_iter().skip(1).collect();

    Ok(Value::new(
        ValueKind::Closure { parameters: params, body: body.clone(), env },
        *location,
    ))
}
