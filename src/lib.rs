mod core;
mod util;

use util::exec_fmt;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn execute(source: &str) -> String {
    match core::execute(source) {
        Ok(s) => exec_fmt::fmt_ok(&s),
        Err(e) => exec_fmt::fmt_err(e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_execute() {
        let source = "1";
        let executed = execute(source);

        assert_eq!(executed, exec_fmt::fmt_ok("1"));
    }

    #[test]
    fn test_execute_fail() {
        let source = " ";
        let executed = execute(source);

        assert_err(&executed);
    }

    fn assert_err(executed: &str) -> () {
        assert!(
            is_err(&executed),
            "Expected an error, but received '{}'",
            executed
        );
    }

    fn is_err(executed: &str) -> bool {
        executed.starts_with("{ \"err\": \"")
    }
}
