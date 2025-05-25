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

macro_rules! test_exec {
    ($name:ident, $src:expr, $expected:expr) => {
        #[wasm_bindgen_test]
        fn $name() -> Result<(), JsValue> {
            assert_exec!($src, $expected);
            Ok(())
        }
    };
}

macro_rules! test_error {
    ($name:ident, $src:expr, $err_name:literal, $err_msg:literal, $br:expr, $bc:expr, $er:expr, $ec:expr) => {
        #[wasm_bindgen_test]
        fn $name() -> Result<(), JsValue> {
            assert_error!($src, $err_name, $err_msg, $br, $bc, $er, $ec);
            Ok(())
        }
    };
}

#[allow(dead_code)] // Suppress warnings from #[wasm_bindgen_test] test codes.
mod tests {
    use super::*;

    mod ok {
        use super::*;

        mod closure {
            use super::*;

            test_exec!(
                closure,
                "함수 사과, 오렌지, 바나나 {}",
                "함수 사과, 오렌지, 바나나 { ... }"
            );
        }

        mod num {
            use super::*;

            mod literals {
                use super::*;

                test_exec!(num, "12.25", "12.25");
            }

            mod prefixes {
                use super::*;

                test_exec!(plus, "+12.25", "12.25");
                test_exec!(minus, "-12.25", "-12.25");
                test_exec!(consecutive, "++--12.25", "12.25");
            }

            mod infixes {
                use super::*;

                test_exec!(addition, "6+4", "10");
                test_exec!(subtraction, "6-4", "2");
                test_exec!(multiplication, "6*4", "24");
                test_exec!(division, "6/4", "1.5");
                test_exec!(modular, "6%4", "2");
            }

            mod compound {
                use super::*;

                test_exec!(expression, "(1.5 - 2.5) * 3 / 4 + 5 % 6", "4.25");
                // Note that the expression will be parsed into `(((8 - 4) - 2) - 1)` if without grouping.
                test_exec!(nested_grouping, "8 - (4 - (2 - 1))", "5");
            }
        }

        mod bool {
            use super::*;

            mod literals {
                use super::*;

                test_exec!(the_true, "참", "참");
                test_exec!(the_false, "거짓", "거짓");
            }

            mod prefixes {
                use super::*;

                test_exec!(negation, "!거짓", "참");
                test_exec!(consecutive, "!!!거짓", "참");
            }

            mod infixes {
                use super::*;

                mod conjunction {
                    use super::*;

                    test_exec!(true_and_true, "참 그리고 참", "참");
                    test_exec!(true_and_false, "참 그리고 거짓", "거짓");
                    test_exec!(false_and_true, "거짓 그리고 참", "거짓");
                    test_exec!(false_and_false, "거짓 그리고 거짓", "거짓");
                }

                mod disjunction {
                    use super::*;

                    test_exec!(true_or_true, "참 또는 참", "참");
                    test_exec!(true_or_false, "참 또는 거짓", "참");
                    test_exec!(false_or_true, "거짓 또는 참", "참");
                    test_exec!(false_or_false, "거짓 또는 거짓", "거짓");
                }

                mod compound {
                    use super::*;

                    test_exec!(negation_on_conjunction, "!(참 또는 참)", "거짓");
                }
            }
        }

        mod assignment {
            use super::*;

            test_exec!(assignment, "사과=1", "1");
            test_exec!(addition_assignment, "사과=6 사과+=4 사과", "10");
            test_exec!(subtraction_assignment, "사과=6 사과-=4 사과", "2");
            test_exec!(multiplication_assignment, "사과=6 사과*=4 사과", "24");
            test_exec!(division_assignment, "사과=6 사과/=4 사과", "1.5");
            test_exec!(modular_assignment, "사과=6 사과%=4 사과", "2");

            test_error!(
                mixed_type,
                "사과=참 사과+=1",
                "EvalError",
                "InvalidNumInfixOperand",
                0,
                5,
                0,
                7
            );
        }
    }

    mod lex_errors {
        use super::*;

        // "^" represents an illegal char.
        test_error!(illegal_char, "^", "LexError", "IllegalChar", 0, 0, 0, 1);
        test_error!(arithmetic_plus, "12.", "LexError", "IllegalNumLiteral", 0, 0, 0, 3);
    }

    mod parse_errors {
        use super::*;

        test_error!(arithmetic_plus, "+", "ParseError", "NoPrefixOperand", 0, 0, 0, 1);
        test_error!(arithmetic_asterisk, "*", "ParseError", "InvalidExprStart", 0, 0, 0, 1);
        test_error!(paren_not_closed, "(12+3", "ParseError", "LParenNotClosed", 0, 0, 0, 5);
        test_error!(no_operand, "1+", "ParseError", "NoInfixRightOperand", 0, 0, 0, 2);
    }
}
