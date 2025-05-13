mod core;
mod util;

use util::exec_fmt::format;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn execute(source: &str) -> String {
    format(core::execute(source))
}

#[cfg(test)]
mod tests {
    use super::*;
    use util::exec_fmt::is_err_format;

    #[test]
    fn test_execute() {
        let source = "1";
        let executed = execute(source);

        assert_eq!(executed, "komi v1 ok 1")
    }

    #[test]
    fn test_execute_fail() {
        let source = " ";
        let executed = execute(source);

        assert!(executed.starts_with("komi v1 err"));
    }
}
