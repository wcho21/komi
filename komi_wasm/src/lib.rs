pub mod util;

use util::res_converter::JsConverter;
pub use util::res_converter::{JsExecError, JsExecOut};
use wasm_bindgen::prelude::*;

/// Returns an object {@link ExecOut} if execution succeeds; throws an error {@link ExecError} if it fails.
#[wasm_bindgen]
pub fn execute(source: &str) -> Result<JsExecOut, JsExecError> {
    let exec_out = komi::execute(source);
    JsConverter::convert(exec_out)
}
