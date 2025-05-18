pub mod util;

use util::exec_fmt::format;
use util::res_converter::convert;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[deprecated]
pub fn execute(source: &str) -> String {
    format(komi::execute(source))
}

#[wasm_bindgen]
pub fn get_execution_result(source: &str) -> Result<JsValue, JsValue> {
    convert(&komi::execute(source))
}

#[cfg(test)]
mod tests {
    use super::*;
    use komi::EMPTY_REPR;

    #[test]
    fn test_execute_num() {
        let source = "1";
        let executed = execute(source);

        assert_eq!(executed, "komi v1 ok 1")
    }

    #[test]
    fn test_execute_empty() {
        let source = " ";
        let executed = execute(source);

        assert_eq!(executed, format!("komi v1 ok {}", EMPTY_REPR))
    }
}
