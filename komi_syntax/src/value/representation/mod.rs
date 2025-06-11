use super::{Value, ValueKind};

pub struct Representer {}

impl Representer {
    /// Predefined representations
    pub const TRUE: &str = "참";
    pub const FALSE: &str = "거짓";
    pub const CLOSURE_KEYWORD: &str = "함수";
    pub const CLOSURE_BODY: &str = "{ ... }";

    pub fn represent(val: &Value) -> String {
        match &val.kind {
            ValueKind::Number(n) => Self::represent_number(*n),
            ValueKind::Bool(b) => Self::represent_bool(*b),
            ValueKind::Str(s) => Self::represent_str(s),
            ValueKind::Closure { parameters: p, .. } => Self::represent_closure(p),
        }
    }

    fn represent_number(num: f64) -> String {
        num.to_string()
    }

    fn represent_bool(boolean: bool) -> String {
        match boolean {
            true => Self::TRUE.to_string(),
            false => Self::FALSE.to_string(),
        }
    }

    fn represent_str(string: &String) -> String {
        string.clone()
    }

    fn represent_closure(parameters: &Vec<String>) -> String {
        let mut parts: Vec<String> = vec![];
        parts.push(String::from(Self::CLOSURE_KEYWORD));
        parts.push(parameters.join(", "));
        parts.push(String::from(Self::CLOSURE_BODY));

        let repr = parts.join(" ");
        repr
    }
}
