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
}

/// A representation of the value produced during evaluation.
#[derive(Debug, PartialEq, Clone)]
pub struct Value {
    pub kind: ValueKind,
    pub location: Range,
}

/// Predefined representations
pub const TRUE_REPR: &str = "참";
pub const FALSE_REPR: &str = "거짓";
pub const CLOSURE_REPR_KEYWORD: &str = "함수";
pub const CLOSURE_REPR_BODY: &str = "{ ... }";
pub const BUILTIN_FUNC_REPR: &str = "(내장 함수)";

impl Value {
    pub fn represent(&self) -> String {
        match &self.kind {
            ValueKind::Number(n) => n.to_string(),
            ValueKind::Bool(b) => represent_bool(*b),
            ValueKind::Closure { parameters: p, .. } => represent_closure(p),
            ValueKind::BuiltinFunc(_) => BUILTIN_FUNC_REPR.to_string(),
        }
    }
}

fn represent_bool(boolean: bool) -> String {
    match boolean {
        true => TRUE_REPR.to_string(),
        false => FALSE_REPR.to_string(),
    }
}

fn represent_closure(parameters: &Vec<String>) -> String {
    let mut parts: Vec<String> = vec![];
    parts.push(String::from(CLOSURE_REPR_KEYWORD));
    parts.push(parameters.join(", "));
    parts.push(String::from(CLOSURE_REPR_BODY));

    let repr = parts.join(" ");
    repr
}

pub type Stdout = Vec<String>;
pub type BuiltinFunc = fn(&Vec<Value>, &mut Stdout) -> Value;

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
}
