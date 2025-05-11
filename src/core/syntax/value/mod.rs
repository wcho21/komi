use crate::util::Range;

#[derive(Debug, PartialEq)]
pub enum ValueKind {
    Number(f64),
}

#[derive(Debug, PartialEq)]
pub struct Value {
    kind: ValueKind,
    location: Range,
}

impl Value {
    pub fn new(kind: ValueKind, location: Range) -> Self {
        Value { kind, location }
    }

    pub fn from_num(num: f64, location: Range) -> Self {
        Value::new(ValueKind::Number(num), location)
    }
}
