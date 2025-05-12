use crate::util::Range;

/// Kinds of values produced during evaluation.
/// Serves as the interface between an evaluator and its user.
#[derive(Debug, PartialEq)]
pub enum ValueKind {
    Number(f64),
}

/// A representation of the value produced during evaluation.
#[derive(Debug, PartialEq)]
pub struct Value {
    pub kind: ValueKind,
    pub location: Range,
}

impl Value {
    pub fn new(kind: ValueKind, location: Range) -> Self {
        Value { kind, location }
    }

    pub fn from_num(num: f64, location: Range) -> Self {
        Value::new(ValueKind::Number(num), location)
    }
}
