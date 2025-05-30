#[allow(dead_code)] // Suppress warnings from #[wasm_bindgen_test] test codes.
mod tests {
    use fixtures::*;
    use js_sys::{Error, Number};
    use komi::ExecOut;
    use komi_wasm::util::js_val::obj;
    use komi_wasm::util::res_converter::convert;
    use wasm_bindgen::JsValue;
    use wasm_bindgen_test::*;

    /// The converted result is expected to be a JavaScript value (`JsValue`), which is a JavaScript object.
    /// The interface will be as below, in TypeScript syntax:
    ///
    /// ```ts
    /// {
    ///   value: string;
    ///   stdout: string;
    /// }
    /// ```
    #[wasm_bindgen_test]
    fn test_convert_ok() -> Result<(), JsValue> {
        // Suppose the execution returns Ok
        let exec_out = Ok(ExecOut::new(value(), stdout()));

        // The converted result is expected to be a JavaScript value (`JsValue`)
        let converted = convert(&exec_out)?;

        // `converted` should have `value` field
        let converted_value = obj::get_property(&converted, "value")?;
        let expected_value = JsValue::from_str(&value());
        assert_eq!(converted_value, expected_value);

        // `converted` should have `stdout` field
        let converted_stdout = obj::get_property(&converted, "stdout")?;
        let expected_stdout = JsValue::from_str(&stdout());
        assert_eq!(converted_stdout, expected_stdout);

        Ok(())
    }

    /// The converted error is expected to be a JavaScript value (`JsValue`), which is a JavaScript `Error` object.
    /// The interface of the `Error` will be as below, in TypeScript syntax:
    ///
    /// ```ts
    /// {
    ///   name: string;
    ///   message: string;
    ///   cause: {
    ///     location: {
    ///       begin: {
    ///         col: number;
    ///         row: number;
    ///       },
    ///       end: {
    ///         col: number;
    ///         row: number;
    ///       },
    ///     };
    ///   };
    /// }
    /// ```
    ///
    /// For the details of JavaScript `Error`, see [MDN documentation].
    ///
    /// [MDN documentation]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error
    #[wasm_bindgen_test]
    fn test_convert_err() -> Result<(), JsValue> {
        let exec_out = Err(exec_err());

        let converted = convert(&exec_out);

        // `converted` should have `cause` field (according to the JavaScript Error class)
        let converted_cause = Error::from(converted.unwrap_err()).cause();

        // The `cause` field should have `location` field
        let converted_location = obj::get_property(&converted_cause, "location")?;

        // The `location` field should have `begin` and `end` fields
        let converted_begin = obj::get_property(&converted_location, "begin")?;
        let converted_end = obj::get_property(&converted_location, "end")?;

        // Each field should have `row` and `col` fields.
        let converted_begin_row = obj::get_property(&converted_begin, "row")?;
        let converted_begin_col = obj::get_property(&converted_begin, "col")?;
        let converted_end_row = obj::get_property(&converted_end, "row")?;
        let converted_end_col = obj::get_property(&converted_end, "col")?;

        assert_eq!(Number::from(converted_begin_row), Number::from(cause_begin_row()));
        assert_eq!(Number::from(converted_begin_col), Number::from(cause_begin_col()));
        assert_eq!(Number::from(converted_end_row), Number::from(cause_end_row()));
        assert_eq!(Number::from(converted_end_col), Number::from(cause_end_col()));

        Ok(())
    }

    mod fixtures {
        use komi::{ExecError, LexError, LexErrorKind};
        use komi_util::Range;

        pub fn value() -> String {
            String::from("value fixture")
        }

        pub fn stdout() -> String {
            String::from("stdout fixture")
        }

        pub fn exec_err() -> ExecError {
            ExecError::Lex(lex_err())
        }

        pub fn lex_err() -> LexError {
            LexError::new(LexErrorKind::NoSource, cause_range())
        }

        pub fn cause_range() -> Range {
            Range::from_nums(cause_begin_row(), cause_begin_col(), cause_end_row(), cause_end_col())
        }

        pub fn cause_begin_row() -> u32 {
            1
        }

        pub fn cause_begin_col() -> u32 {
            2
        }

        pub fn cause_end_row() -> u32 {
            3
        }

        pub fn cause_end_col() -> u32 {
            4
        }
    }
}
