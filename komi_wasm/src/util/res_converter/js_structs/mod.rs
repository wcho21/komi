use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen(getter_with_clone, js_name = "ExecOut")]
#[derive(Debug)]
pub struct JsExecOut {
    #[wasm_bindgen(readonly)]
    pub value: String,
    #[wasm_bindgen(readonly)]
    pub stdout: String,
}

#[wasm_bindgen(getter_with_clone, js_name = "ExecError")]
#[derive(Debug)]
pub struct JsExecError {
    #[wasm_bindgen(readonly)]
    pub name: String,
    #[wasm_bindgen(readonly)]
    pub message: String,
    #[wasm_bindgen(readonly)]
    pub cause: JsExecErrorCause,
}

#[wasm_bindgen(getter_with_clone, js_name = "ExecErrorCause")]
#[derive(Debug, Clone)]
pub struct JsExecErrorCause {
    #[wasm_bindgen(getter_with_clone, readonly)]
    pub location: JsRange,
}

#[wasm_bindgen(getter_with_clone, js_name = "Range")]
#[derive(Debug, Clone)]
pub struct JsRange {
    #[wasm_bindgen(getter_with_clone, readonly)]
    pub begin: JsSpot,
    #[wasm_bindgen(getter_with_clone, readonly)]
    pub end: JsSpot,
}

#[wasm_bindgen(getter_with_clone, js_name = "Spot")]
#[derive(Debug, Clone)]
pub struct JsSpot {
    #[wasm_bindgen(readonly)]
    pub row: u32,
    #[wasm_bindgen(readonly)]
    pub col: u32,
}
