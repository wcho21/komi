use crate::core::syntax::Value;

pub fn represent(_val: &Value) -> String {
    // fake implementation
    // return dummy representation
    String::from("1")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::{Range, Spot};

    #[test]
    fn fake_represent() {
        let fake_value = Value::from_num(1.0, Range::new(Spot::new(0, 0), Spot::new(0, 0)));
        let expected = String::from("1");

        let representation = represent(&fake_value);

        assert_eq!(representation, expected);
    }
}
