pub mod obj;

use js_sys::{Error, Object};
use komi_util::location::{Range, Spot};
use komi_util::unpacker::unpack_spot;
use wasm_bindgen::JsValue;

pub fn convert_spot_to_js_object(spot: &Spot) -> Result<Object, JsValue> {
    let obj = Object::new();
    let (row, col) = unpack_spot(spot);

    obj::set_uint_property(&obj, "row", row)?;
    obj::set_uint_property(&obj, "col", col)?;

    Ok(obj)
}

pub fn convert_range_to_js_object(location: &Range) -> Result<Object, JsValue> {
    let obj = Object::new();

    let begin = convert_spot_to_js_object(&location.begin)?;
    let end = convert_spot_to_js_object(&location.end)?;

    obj::set_object_property(&obj, "begin", &begin)?;
    obj::set_object_property(&obj, "end", &end)?;

    Ok(obj)
}

/// Converts an execution result to a JavaScript object with two fields `value` and `stdout`.
/// These fields will be the string values from `repr` and `stdout`, respectively.
pub fn convert_repr_and_stdout_to_js_val(repr: &str, stdout: &str) -> Result<JsValue, JsValue> {
    let obj = Object::new();
    obj::set_string_property(&obj, "value", repr)?;
    obj::set_string_property(&obj, "stdout", stdout)?;

    Ok(obj.into())
}

pub fn convert_str_and_location_to_js_val(
    name: &str,
    message: &str,
    cause_location: &Range,
) -> Result<JsValue, JsValue> {
    let cause = Object::new();
    let js_range_obj = convert_range_to_js_object(cause_location)?;
    obj::set_object_property(&cause, "location", &js_range_obj)?;

    let js_err = Error::new(message);
    js_err.set_name(name);
    js_err.set_cause(&cause);

    Ok(js_err.into())
}
