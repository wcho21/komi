mod expressions;
mod infix;
mod leaf;
mod prefix;
mod util;

use crate::err::EvalError;
use komi_syntax::{Ast, AstKind, Value};

type ResVal = Result<Value, EvalError>;

pub fn reduce_ast(ast: &Ast) -> ResVal {
    // Design principle: once you read something, pass it as an argument.
    // This avoids unnecessary repeated reading in subfunctions.
    // Moreover, if you delay determining the kind of what you read, the decision is only postponed to subfunctions.
    // You'll have to handle exceptional cases that could have been avoided.

    let loc = ast.location;
    match &ast.kind {
        AstKind::Program { expressions: e } => expressions::reduce(&e, &loc),
        AstKind::Number(x) => leaf::evaluate_num(*x, &loc),
        AstKind::Bool(x) => leaf::evaluate_bool(*x, &loc),
        AstKind::PrefixPlus { operand: op } => prefix::reduce_plus(&op, &loc),
        AstKind::PrefixMinus { operand: op } => prefix::reduce_minus(&op, &loc),
        AstKind::PrefixBang { operand: op } => prefix::reduce_bang(&op, &loc),
        AstKind::InfixPlus { left: l, right: r } => infix::reduce_plus(&l, &r, &loc),
        AstKind::InfixMinus { left: l, right: r } => infix::reduce_minus(&l, &r, &loc),
        AstKind::InfixAsterisk { left: l, right: r } => infix::reduce_asterisk(&l, &r, &loc),
        AstKind::InfixSlash { left: l, right: r } => infix::reduce_slash(&l, &r, &loc),
        AstKind::InfixPercent { left: l, right: r } => infix::reduce_percent(&l, &r, &loc),
        AstKind::InfixConjunct { left: l, right: r } => infix::reduce_conjunct(&l, &r, &loc),
        AstKind::InfixDisjunct { left: l, right: r } => infix::reduce_disjunct(&l, &r, &loc),
    }
}
