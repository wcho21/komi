mod representation;

use crate::Ast;
use komi_util::{Environment, Range};
use representation::Representer;

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

impl Value {
    pub fn represent(&self) -> String {
        Representer::represent(&self)
    }
}

pub type Stdout = Vec<String>;
pub type BuiltinFunc = fn(&Vec<Value>, &mut Stdout) -> Value;

#[macro_export]
macro_rules! mkval {
    ($val:expr, $loc:expr) => {
        Value::new($val, $loc)
    };
}

impl Value {
    pub const fn new(kind: ValueKind, location: Range) -> Self {
        Value { kind, location }
    }
}
