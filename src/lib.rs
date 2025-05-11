mod util;

pub fn execute(_source: &str) -> String {
    // fake implementation
    String::from("1")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_execute() {
        let source = "1";
        let executed = execute(source);

        assert_eq!(executed, "1");
    }
}
