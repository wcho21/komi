use crate::Ast;
use komi_util::{Environment, Range};

/// Kinds of values produced during evaluation.
/// Serves as the interface between an evaluator and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum ValueKind {
    Number(f64),
    Bool(bool),
    Closure {
        parameters: Vec<String>,
        body: Vec<Box<Ast>>,
        env: Environment<Value>,
    },
    BuiltinFunc(BuiltinFunc),
    Empty,
}

/// A representation of the value produced during evaluation.
#[derive(Debug, PartialEq, Clone)]
pub struct Value {
    pub kind: ValueKind,
    pub location: Range,
}

/// Predefined representations
pub const EMPTY_REPR: &str = "(EMPTY)";
pub const TRUE_REPR: &str = "참";
pub const FALSE_REPR: &str = "거짓";
pub const CLOSURE_REPR_KEYWORD: &str = "함수";
pub const CLOSURE_REPR_BODY: &str = "{ ... }";
pub const BUILTIN_FUNC_REPR: &str = "(내장 함수)";

impl Value {
    // TODO: move representing logic into here (see `komi_representer`)
    pub fn represent(&self) -> String {
        match &self.kind {
            ValueKind::Number(n) => n.to_string(),
            ValueKind::Bool(b) => represent_bool(*b),
            ValueKind::Closure { parameters: p, .. } => represent_closure(p),
            ValueKind::BuiltinFunc(_) => BUILTIN_FUNC_REPR.to_string(),
            ValueKind::Empty => EMPTY_REPR.to_string(),
        }
    }
}

// TODO: move representing logic into here (see `komi_representer`)
fn represent_bool(boolean: bool) -> String {
    match boolean {
        true => TRUE_REPR.to_string(),
        false => FALSE_REPR.to_string(),
    }
}

// TODO: move representing logic into here (see `komi_representer`)
fn represent_closure(parameters: &Vec<String>) -> String {
    let mut parts: Vec<String> = vec![];
    parts.push(String::from(CLOSURE_REPR_KEYWORD));
    parts.push(parameters.join(", "));
    parts.push(String::from(CLOSURE_REPR_BODY));

    let repr = parts.join(" ");
    repr
}

type StdoutHandler = fn(&str) -> ();
pub type BuiltinFunc = fn(&Vec<Value>, Option<StdoutHandler>) -> Value;

impl Value {
    pub const fn new(kind: ValueKind, location: Range) -> Self {
        Value { kind, location }
    }

    // TODO: change from-function into macro
    pub fn from_num(num: f64, location: Range) -> Self {
        Value::new(ValueKind::Number(num), location)
    }

    pub fn from_bool(boolean: bool, location: Range) -> Self {
        Value::new(ValueKind::Bool(boolean), location)
    }

    // TODO: remove empty value, replace it with lex error instead
    pub fn from_empty(location: Range) -> Self {
        Value::new(ValueKind::Empty, location)
    }
}
