use crate::err::{EvalError, EvalErrorKind};
use komi_syntax::{Ast, AstKind, Value, ValueKind};
use komi_util::Range;

type ResVal = Result<Value, EvalError>;

pub fn reduce_ast(ast: &Ast) -> ResVal {
    let loc = ast.location;
    match &ast.kind {
        AstKind::Program { expressions: e } => evaluate_expressions(&e, &loc),
        AstKind::Number(x) => evaluate_number(*x, &loc),
        AstKind::Bool(x) => evaluate_bool(*x, &loc),
        AstKind::PrefixPlus { operand: op } => evaluate_prefix_plus(&op, &loc),
        AstKind::PrefixMinus { operand: op } => evaluate_prefix_minus(&op, &loc),
        AstKind::PrefixBang { operand: op } => evaluate_prefix_bang(&op, &loc),
        AstKind::InfixPlus { left, right } => eval_infix_plus(&left, &right),
        AstKind::InfixMinus { left, right } => eval_infix_minus(&left, &right),
        AstKind::InfixAsterisk { left, right } => eval_infix_asterisk(&left, &right),
        AstKind::InfixSlash { left, right } => eval_infix_slash(&left, &right),
        AstKind::InfixPercent { left, right } => eval_infix_percent(&left, &right),
        AstKind::InfixConjunct { left, right } => eval_infix_conjunct(&left, &right),
        AstKind::InfixDisjunct { left, right } => eval_infix_disjunct(&left, &right),
    }
}

/// Returns the evaluated result of the last AST in the ASTs `expressions`.
///
/// Sets its location to be `expressions_location`, since it represents the entire expressions, not a single one.
fn evaluate_expressions(expressions: &Vec<Box<Ast>>, expressions_location: &Range) -> ResVal {
    let mut last_value = Value::from_empty(*expressions_location);

    for expression in expressions {
        last_value = reduce_ast(expression)?;
    }
    last_value.location = *expressions_location;
    Ok(last_value)
}

/// Returns the evaluated numeric result, from number `num` and its location `location`.
fn evaluate_number(num: f64, location: &Range) -> ResVal {
    Ok(Value::new(ValueKind::Number(num), *location))
}

/// Returns the evaluated boolean result, from boolean `boolean` and its location `location`.
fn evaluate_bool(boolean: bool, location: &Range) -> ResVal {
    Ok(Value::new(ValueKind::Bool(boolean), *location))
}

/// Returns the evaluated numeric result of the AST `operand` as an operand of the prefix plus.
///
/// The location in the returned value will span from the prefix to operand.
fn evaluate_prefix_plus(operand: &Ast, prefix_location: &Range) -> ResVal {
    evaluate_num_prefix(operand, prefix_location, |v| ValueKind::Number(v))
}

/// Returns the evaluated numeric result of the AST `operand` as an operand of the prefix minus.
///
/// The location in the returned value will span from the prefix to operand.
fn evaluate_prefix_minus(operand: &Ast, prefix_location: &Range) -> ResVal {
    evaluate_num_prefix(operand, prefix_location, |v| ValueKind::Number(-v))
}

/// Returns the evaluated boolean result of the AST `operand` as an operand of the prefix bang.
///
/// The location in the returned value will span from the prefix to operand.
fn evaluate_prefix_bang(operand: &Ast, prefix_location: &Range) -> ResVal {
    evaluate_bool_prefix(operand, prefix_location, |v| ValueKind::Bool(!v))
}

fn eval_infix_plus(left: &Ast, right: &Ast) -> ResVal {
    evaluate_num_infix(left, right, |l, r| l + r, |v| ValueKind::Number(v))
}

fn eval_infix_minus(left: &Ast, right: &Ast) -> ResVal {
    evaluate_num_infix(left, right, |l, r| l - r, |v| ValueKind::Number(v))
}

fn eval_infix_asterisk(left: &Ast, right: &Ast) -> ResVal {
    evaluate_num_infix(left, right, |l, r| l * r, |v| ValueKind::Number(v))
}

fn eval_infix_slash(left: &Ast, right: &Ast) -> ResVal {
    evaluate_num_infix(left, right, |l, r| l / r, |v| ValueKind::Number(v))
}

fn eval_infix_percent(left: &Ast, right: &Ast) -> ResVal {
    evaluate_num_infix(left, right, |l, r| l % r, |v| ValueKind::Number(v))
}

fn eval_infix_conjunct(left: &Ast, right: &Ast) -> ResVal {
    evaluate_bool_infix(left, right, |l, r| l && r, |v| ValueKind::Bool(v))
}

fn eval_infix_disjunct(left: &Ast, right: &Ast) -> ResVal {
    evaluate_bool_infix(left, right, |l, r| l || r, |v| ValueKind::Bool(v))
}

/// Returns the evaluated numeric result of the AST `operand` as a leaf operand of a prefix.
fn evaluate_num_prefix_operand(operand: &Ast) -> Result<f64, EvalError> {
    evaluate_leaf_operand(operand, |value_kind| match value_kind {
        ValueKind::Number(x) => Ok(*x),
        _ => Err(EvalErrorKind::InvalidPrefixNumOperand),
    })
}

/// Returns the evaluated boolean result of the AST `operand` as a leaf operand of a prefix.
fn evaluate_bool_prefix_operand(operand: &Ast) -> Result<bool, EvalError> {
    evaluate_leaf_operand(operand, |value_kind| match value_kind {
        ValueKind::Bool(x) => Ok(*x),
        _ => Err(EvalErrorKind::InvalidPrefixBoolOperand),
    })
}

/// Returns the evaluated numeric result of the AST `operand` as a leaf operand of an infix.
fn evaluate_num_infix_operand(operand: &Ast) -> Result<f64, EvalError> {
    evaluate_leaf_operand(operand, |value_kind| match value_kind {
        ValueKind::Number(x) => Ok(*x),
        _ => Err(EvalErrorKind::InvalidAdditionOperand),
    })
}

/// Returns the evaluated boolean result of the AST `operand` as a leaf operand of an infix.
fn evaluate_bool_infix_operand(operand: &Ast) -> Result<bool, EvalError> {
    evaluate_leaf_operand(operand, |value_kind| match value_kind {
        ValueKind::Bool(x) => Ok(*x),
        _ => Err(EvalErrorKind::InvalidConnectiveInfixOperand),
    })
}

fn evaluate_num_prefix<F>(operand: &Ast, prefix_location: &Range, get_kind: F) -> ResVal
where
    F: Fn(f64) -> ValueKind,
{
    evaluate_prefix(operand, prefix_location, evaluate_num_prefix_operand, get_kind)
}

fn evaluate_bool_prefix<F>(operand: &Ast, prefix_location: &Range, get_kind: F) -> ResVal
where
    F: Fn(bool) -> ValueKind,
{
    evaluate_prefix::<bool, _, _>(operand, prefix_location, evaluate_bool_prefix_operand, get_kind)
}

/// Returns the evaluated result of the AST `operand` as an operand of a prefix.
///
/// - `evaluate_operand` determines how to evaluate the AST `operand` itself to some value `x`.
/// - `get_kind` specifies how to get the value kind from `x`.
///
/// The location in the returned value will span from the prefix to operand.
fn evaluate_prefix<T, F, G>(operand: &Ast, prefix_location: &Range, evaluate_operand: F, get_kind: G) -> ResVal
where
    F: Fn(&Ast) -> Result<T, EvalError>,
    G: Fn(T) -> ValueKind,
{
    let evaluated_operand = evaluate_operand(operand)?;
    let kind = get_kind(evaluated_operand);
    let location = Range::new(prefix_location.begin, operand.location.end);
    Ok(Value::new(kind, location))
}

fn evaluate_bool_infix<F, G>(left: &Ast, right: &Ast, evaluate_infix: F, make_value_kind: G) -> ResVal
where
    F: Fn(bool, bool) -> bool,
    G: Fn(bool) -> ValueKind,
{
    evaluate_infix_(
        left,
        right,
        evaluate_bool_infix_operand,
        evaluate_infix,
        make_value_kind,
    )
}

fn evaluate_num_infix<F, G>(left: &Ast, right: &Ast, evaluate_infix: F, make_value_kind: G) -> ResVal
where
    F: Fn(f64, f64) -> f64,
    G: Fn(f64) -> ValueKind,
{
    evaluate_infix_(left, right, evaluate_num_infix_operand, evaluate_infix, make_value_kind)
}

// TODO: document
// TODO(?): factor out into separate file
pub fn evaluate_infix_<T, F, G, H>(
    left: &Ast,
    right: &Ast,
    evaluate_operand: F,
    evaluate_infix: G,
    make_value_kind: H,
) -> ResVal
where
    F: Fn(&Ast) -> Result<T, EvalError>,
    G: Fn(T, T) -> T,
    H: Fn(T) -> ValueKind,
{
    let left_val = evaluate_operand(left)?;
    let right_val = evaluate_operand(right)?;

    let infix_val = evaluate_infix(left_val, right_val);

    let kind = make_value_kind(infix_val);
    let location = Range::new(left.location.begin, right.location.end);

    Ok(Value::new(kind, location))
}

/// Returns the evalauted result of the AST `operand` as a leaf operand of a prefix or an infix.
///
/// `extract` specifies the expected kind `x` of the evaluated result of `operand`.
/// - If `x` encountered, it returns `Ok` from `x`.
/// - Otherwise, returns `Err(e)` where `e` is `EvalError`.
fn evaluate_leaf_operand<T, F>(operand: &Ast, extract: F) -> Result<T, EvalError>
where
    F: Fn(&ValueKind) -> Result<T, EvalErrorKind>,
{
    let val = reduce_ast(operand)?;

    match extract(&val.kind) {
        Ok(x) => Ok(x),
        Err(kind) => Err(EvalError::new(kind, val.location)),
    }
}
