use js_sys::{JsString, Number, Object, Reflect};
use wasm_bindgen::JsValue;

pub fn set_uint_property(obj: &Object, key: &str, value: u32) -> Result<bool, JsValue> {
    Reflect::set(obj, &JsString::from(key), &Number::from(value))
}

pub fn set_object_property(obj: &Object, key: &str, value: &Object) -> Result<bool, JsValue> {
    Reflect::set(obj, &JsString::from(key), value)
}

pub fn set_string_property(obj: &Object, key: &str, value: &str) -> Result<bool, JsValue> {
    Reflect::set(obj, &JsString::from(key), &JsString::from(value))
}

pub fn get_property(obj: &JsValue, key: &str) -> Result<JsValue, JsValue> {
    Reflect::get(obj, &JsString::from(key))
}
