pub mod util;

use util::res_converter::convert;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn get_execution_result(source: &str) -> Result<JsValue, JsValue> {
    convert(&komi::execute(source))
}
