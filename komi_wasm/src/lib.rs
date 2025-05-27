pub mod util;

use util::res_converter::{convert, convert_with_stdout};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[deprecated]
pub fn get_execution_result(source: &str) -> Result<JsValue, JsValue> {
    convert(&komi::execute(source))
}

#[wasm_bindgen]
pub fn get_execution_result_and_stdout(source: &str) -> Result<JsValue, JsValue> {
    convert_with_stdout(&komi::execute_and_get_stdout(source))
}
