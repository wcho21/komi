use js_sys::{Error, JsString, Number};
use komi_wasm::get_execution_result;
use komi_wasm::util::js_val::get_property;
use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

macro_rules! assert_exec {
    ($src:expr, $expected:expr) => {
        let res = get_execution_result($src)?;

        assert_eq!(JsString::from(res), JsString::from($expected));
    };
}

macro_rules! assert_error {
    ($src:expr, $name:expr, $msg:expr, $br:expr, $bc:expr, $er:expr, $ec:expr) => {
        let res = get_execution_result($src);

        assert!(res.is_err(), "expected an error, but it isn't.");

        let err = Error::from(res.unwrap_err());
        assert_eq!(
            err.name(),
            JsString::from($name),
            "expected the name (left), but received a different name (right)",
        );
        assert_eq!(
            err.message(),
            JsString::from($msg),
            "expected the message (left), but received a different message (right)",
        );

        let cause = err.cause();
        let location = get_property(&cause, "location")?;
        let begin = get_property(&location, "begin")?;
        let end = get_property(&location, "end")?;

        let begin_row = get_property(&begin, "row")?;
        let begin_col = get_property(&begin, "col")?;
        let end_row = get_property(&end, "row")?;
        let end_col = get_property(&end, "col")?;

        assert_eq!(
            Number::from(begin_row),
            Number::from($br as u32),
            "expected the begin_row (left), but received a different number (right)"
        );
        assert_eq!(
            Number::from(begin_col),
            Number::from($bc as u32),
            "expected the begin_col (left), but received a different number (right)"
        );
        assert_eq!(
            Number::from(end_row),
            Number::from($er as u32),
            "expected the end_row (left), but received a different number (right)"
        );
        assert_eq!(
            Number::from(end_col),
            Number::from($ec as u32),
            "expected the end_col (left), but received a different number (right)"
        );
    };
}

mod tests {
    use super::*;

    mod ok {
        use super::*;

        mod num {
            use super::*;

            #[wasm_bindgen_test]
            fn test_single_literal() -> Result<(), JsValue> {
                assert_exec!("12.25", "12.25");
                Ok(())
            }

            #[wasm_bindgen_test]
            fn test_plus_prefix() -> Result<(), JsValue> {
                assert_exec!("+12.25", "12.25");
                Ok(())
            }

            #[wasm_bindgen_test]
            fn test_minus_prefix() -> Result<(), JsValue> {
                assert_exec!("-12.25", "-12.25");
                Ok(())
            }

            #[wasm_bindgen_test]
            fn test_arithmetic_expression() -> Result<(), JsValue> {
                assert_exec!("(1.5 - 2.5) * 3 / 4 + 5 % 6", "4.25");
                Ok(())
            }

            #[wasm_bindgen_test]
            fn test_nested_grouping() -> Result<(), JsValue> {
                // Note that the expression will be parsed into `(((8 - 4) - 2) - 1)` if without grouping.
                assert_exec!("8 - (4 - (2 - 1))", "5");
                Ok(())
            }
        }
    }

    mod lex_errors {
        use super::*;

        #[wasm_bindgen_test]
        fn test_illegal_char() -> Result<(), JsValue> {
            // "^" is illegal char
            assert_error!("^", "LexError", "IllegalChar", 0, 0, 0, "^".len());
            Ok(())
        }

        #[wasm_bindgen_test]
        fn test_illegal_num_literal() -> Result<(), JsValue> {
            assert_error!("12.", "LexError", "IllegalNumLiteral", 0, 0, 0, "12.".len());
            Ok(())
        }
    }

    mod parse_errors {
        use super::*;

        #[wasm_bindgen_test]
        fn test_invalid_expr_start() -> Result<(), JsValue> {
            assert_error!("*", "ParseError", "InvalidExprStart", 0, 0, 0, "*".len());
            Ok(())
        }

        #[wasm_bindgen_test]
        fn test_lparen_not_closed() -> Result<(), JsValue> {
            assert_error!("(12+3", "ParseError", "LParenNotClosed", 0, 0, 0, "(12+3".len());
            Ok(())
        }

        #[wasm_bindgen_test]
        fn test_no_infix_right_operand() -> Result<(), JsValue> {
            assert_error!("1+", "ParseError", "NoInfixRightOperand", 0, 0, 0, "1+".len());
            Ok(())
        }

        #[wasm_bindgen_test]
        fn test_no_prefix_operand() -> Result<(), JsValue> {
            assert_error!("+", "ParseError", "NoPrefixOperand", 0, 0, 0, "+".len());
            Ok(())
        }
    }
}
