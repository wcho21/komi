use js_sys::{Error, JsString, Number, Object, Reflect};
use komi_util::unpacker::unpack_spot;
use komi_util::{Range, Spot};
use wasm_bindgen::JsValue;

pub fn set_uint_property(obj: &Object, key: &str, value: u32) -> Result<bool, JsValue> {
    Reflect::set(obj, &JsString::from(key), &Number::from(value))
}

pub fn set_object_property(obj: &Object, key: &str, value: &Object) -> Result<bool, JsValue> {
    Reflect::set(obj, &JsString::from(key), value)
}

pub fn get_property(obj: &JsValue, key: &str) -> Result<JsValue, JsValue> {
    Reflect::get(obj, &JsString::from(key))
}

pub fn convert_spot_to_js_object(spot: &Spot) -> Result<Object, JsValue> {
    let obj = Object::new();
    let (row, col) = unpack_spot(spot);

    set_uint_property(&obj, "row", row)?;
    set_uint_property(&obj, "col", col)?;

    Ok(obj)
}

pub fn convert_range_to_js_object(location: &Range) -> Result<Object, JsValue> {
    let obj = Object::new();

    let begin = convert_spot_to_js_object(&location.begin)?;
    let end = convert_spot_to_js_object(&location.end)?;

    set_object_property(&obj, "begin", &begin)?;
    set_object_property(&obj, "end", &end)?;

    Ok(obj)
}

pub fn make_js_err(name: &str, message: &str, location: &JsValue) -> Error {
    let js_err = Error::new(message);

    js_err.set_name(name);

    let cause = Object::new();
    let _ = Reflect::set(&cause, &JsString::from("location"), &location);
    js_err.set_cause(&cause);

    js_err
}
