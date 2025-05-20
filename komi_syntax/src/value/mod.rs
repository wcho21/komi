use komi_util::Range;

/// Kinds of values produced during evaluation.
/// Serves as the interface between an evaluator and its user.
#[derive(Debug, PartialEq)]
pub enum ValueKind {
    Number(f64),
    Bool(bool),
    Empty,
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

    pub fn from_bool(boolean: bool, location: Range) -> Self {
        Value::new(ValueKind::Bool(boolean), location)
    }

    // TODO: remove empty value, replace it with lex error instead
    pub fn from_empty(location: Range) -> Self {
        Value::new(ValueKind::Empty, location)
    }
}
