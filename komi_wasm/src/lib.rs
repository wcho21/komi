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
