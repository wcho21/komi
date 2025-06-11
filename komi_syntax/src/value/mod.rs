mod representation;

use crate::ast::Ast;
use crate::error::EvalError;
// TODO: separate environment from this file (is environment syntax?)
use komi_util::environment::Environment as BaseEnvironment;
use komi_util::location::Range;
use representation::Representer;

pub type Environment = BaseEnvironment<Value>;

/// Kinds of values produced during evaluation.
/// Serves as the interface between an evaluator and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum ValueKind {
    Number(f64),
    Bool(bool),
    Str(String),
    Closure {
        parameters: Vec<String>,
        body: ClosureBodyKind,
        env: Environment,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ClosureBodyKind {
    Ast(Vec<Box<Ast>>),
    Native(BuiltinFunc),
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
pub type BuiltinFunc = fn(&Range, &Environment, &mut Stdout) -> Result<Value, EvalError>;

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
