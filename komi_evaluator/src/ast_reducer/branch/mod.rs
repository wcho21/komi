use crate::ast_reducer::Exprs;
use crate::ast_reducer::expressions as expr;
use crate::environment::Environment as Env;
use crate::{ValRes, reduce_ast};
use komi_syntax::ast::Ast;
use komi_syntax::error::{EvalError, EvalErrorKind};
use komi_syntax::value::{Stdout, ValueKind};
use komi_util::location::Range;

pub fn evaluate(
    predicate: &Box<Ast>,
    consequence: &Exprs,
    alternative: &Exprs,
    location: &Range,
    env: &mut Env,
    stdouts: &mut Stdout,
) -> ValRes {
    let pred_value = reduce_ast(predicate, env, stdouts)?;
    let ValueKind::Bool(pred) = pred_value.kind else {
        return Err(EvalError::new(EvalErrorKind::NonBoolPred, pred_value.location));
    };

    expr::reduce(if pred { consequence } else { alternative }, location, env, stdouts)
}
