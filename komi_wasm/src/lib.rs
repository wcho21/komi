pub mod util;

use util::res_converter::convert;
use wasm_bindgen::prelude::*;

pub type JsRes = Result<JsValue, JsValue>;

#[wasm_bindgen]
pub fn execute(source: &str) -> JsRes {
    convert(&komi::execute(source))
}
