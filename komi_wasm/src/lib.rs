pub mod util;

use util::res_converter::JsConverter;
use wasm_bindgen::prelude::*;

pub type JsRes = Result<JsValue, JsValue>;

#[wasm_bindgen]
pub fn execute(source: &str) -> JsRes {
    let exec_out = komi::execute(source);
    JsConverter::convert(exec_out)
}
