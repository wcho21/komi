use crate::util::js_val::{convert_range_to_js_object, make_js_err};
use js_sys::Error;
use komi::{ExecError, ExecResult};
use komi_util::EngineError;
use komi_util::unpacker::unpack_engine_error;
use std::fmt::Display;
use wasm_bindgen::JsValue;

pub fn convert(exec_res: &ExecResult) -> Result<JsValue, JsValue> {
    match exec_res {
        Ok(s) => Ok(convert_ok(s)),
        Err(e) => Err(convert_err(&e)),
    }
}

fn convert_ok(ok_str: &str) -> JsValue {
    JsValue::from_str(ok_str)
}

fn convert_err(err: &ExecError) -> JsValue {
    match err {
        ExecError::Lex(e) => convert_err_to_js_err(e, "LexError").into(),
        ExecError::Parse(e) => convert_err_to_js_err(e, "ParseError").into(),
        ExecError::Eval(e) => convert_err_to_js_err(e, "EvalError").into(),
    }
}

fn convert_err_to_js_err<T: Display>(err: &EngineError<T>, name: &str) -> Error {
    let (kind, location) = unpack_engine_error(&err);

    let message = format!("{}", kind);
    let cause_location = convert_range_to_js_object(location).unwrap();
    let js_err = make_js_err(&name, &message, &cause_location);
    js_err
}
