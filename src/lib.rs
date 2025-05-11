mod core;
mod util;

pub fn execute(source: &str) -> String {
    core::execute(source)
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
