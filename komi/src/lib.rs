mod err;

pub use err::ExecError;
use komi_evaluator::eval;
use komi_lexer::lex;
use komi_parser::parse;
pub use komi_representer::EMPTY_REPR;
use komi_representer::represent;

pub type ExecResult = Result<String, ExecError>;

pub fn execute(source: &str) -> ExecResult {
    let tokens = lex(&source)?;
    let ast = parse(&tokens)?;
    let value = eval(&ast)?;
    let representation = represent(&value);

    Ok(representation)
}

#[cfg(test)]
mod tests {
    use super::{EMPTY_REPR, ExecError, execute};
    use komi_lexer::LexError;
    use komi_util::Range;

    type Res = Result<(), ExecError>;

    mod empty {
        use super::*;

        #[test]
        fn test_empty() -> Res {
            let source = "";

            let repr = execute(&source)?;

            let expected = format!("{EMPTY_REPR}");
            assert_eq!(repr, expected);
            Ok(())
        }

        #[test]
        fn test_whitespaces() -> Res {
            let source = "  \t\t\r\r\n\n\r\n\r\n";

            let repr = execute(&source)?;

            let expected = format!("{EMPTY_REPR}");
            assert_eq!(repr, expected);
            Ok(())
        }

        #[test]
        fn test_comment() -> Res {
            let source = "# some comment";

            let repr = execute(&source)?;

            let expected = format!("{EMPTY_REPR}");
            assert_eq!(repr, expected);
            Ok(())
        }

        #[test]
        fn test_multi_line_comment() -> Res {
            let source = "# line 1\r\n# line 2";

            let repr = execute(&source)?;

            let expected = format!("{EMPTY_REPR}");
            assert_eq!(repr, expected);
            Ok(())
        }
    }

    mod single_literals {
        use super::*;

        #[test]
        fn test_number_without_decimal() -> Res {
            let source = "12";

            let repr = execute(source)?;

            assert_eq!(repr, "12");
            Ok(())
        }

        #[test]
        fn test_number_with_decimal() -> Res {
            let source = "12.25";

            let repr = execute(source)?;

            assert_eq!(repr, "12.25");
            Ok(())
        }

        #[test]
        fn test_number_ending_with_dot() -> Res {
            let source = "12.";

            let repr = execute(source)?;

            assert_eq!(repr, "12");
            Ok(())
        }

        #[test]
        #[ignore] // TODO
        fn test_number_beginning_with_dot() -> Res {
            let source = ".25";

            let repr = execute(source)?;

            assert_eq!(repr, "0.25");
            Ok(())
        }

        #[test]
        #[ignore] // TODO
        fn test_number_with_plus() -> Res {
            let source = "+12";

            let repr = execute(source)?;

            assert_eq!(repr, "12");
            Ok(())
        }

        #[test]
        #[ignore] // TODO
        fn test_number_with_minus() -> Res {
            let source = "-12";

            let repr = execute(source)?;

            assert_eq!(repr, "12");
            Ok(())
        }
    }

    mod arithmetic_expressions {
        use super::*;

        #[test]
        fn test_addition() -> Res {
            let source = "1 + 2";

            let repr = execute(source)?;

            assert_eq!(repr, "3");
            Ok(())
        }

        #[test]
        fn test_subtraction() -> Res {
            let source = "1 - 2";

            let repr = execute(source)?;

            assert_eq!(repr, "-1");
            Ok(())
        }

        #[test]
        fn test_multiplcation() -> Res {
            let source = "3 * 4";

            let repr = execute(source)?;

            assert_eq!(repr, "12");
            Ok(())
        }

        #[test]
        fn test_division() -> Res {
            let source = "3 / 4";

            let repr = execute(source)?;

            assert_eq!(repr, "0.75");
            Ok(())
        }

        #[test]
        fn test_mod() -> Res {
            let source = "7 % 4";

            let repr = execute(source)?;

            assert_eq!(repr, "3");
            Ok(())
        }

        #[test]
        fn test_complex_arithmetic_expression() -> Res {
            let source = "9 * 8 % 7 - 6 + 5 / 4";

            let repr = execute(source)?;

            assert_eq!(repr, "-2.75");
            Ok(())
        }

        #[test]
        #[ignore] // TODO
        fn test_grouping() -> Res {
            let source = "8 - (4 - 2)";

            let repr = execute(source)?;

            assert_eq!(repr, "6");
            Ok(())
        }

        #[test]
        #[ignore] // TODO
        fn test_nested_grouping() -> Res {
            let source = "16 - (8 - (4 - 2))";

            let repr = execute(source)?;

            assert_eq!(repr, "10");
            Ok(())
        }
    }

    mod fail {
        use super::*;

        #[test]
        fn test_dot() -> Res {
            let source = ".";

            let repr = execute(source);

            let expected = Err(ExecError::Lex(LexError::IllegalChar {
                char: ".".to_string(),
                location: Range::from_nums(0, 0, 0, 1),
            }));
            assert_eq!(repr, expected);
            Ok(())
        }

        #[test]
        fn test_two_dots() -> Res {
            let source = "..";

            let repr = execute(source);

            let expected = Err(ExecError::Lex(LexError::IllegalChar {
                char: ".".to_string(),
                location: Range::from_nums(0, 0, 0, 1),
            }));
            assert_eq!(repr, expected);
            Ok(())
        }

        #[test]
        #[ignore] // TODO
        fn test_two_pluses() -> Res {
            let source = "++";

            let repr = execute(source);

            let expected = Err(ExecError::Lex(LexError::IllegalChar {
                char: "+".to_string(),
                location: Range::from_nums(0, 1, 0, 2),
            }));
            assert_eq!(repr, expected);
            Ok(())
        }

        #[test]
        #[ignore] // TODO
        fn test_two_minuses() -> Res {
            let source = "--";

            let repr = execute(source);

            let expected = Err(ExecError::Lex(LexError::IllegalChar {
                char: "-".to_string(),
                location: Range::from_nums(0, 1, 0, 2),
            }));
            assert_eq!(repr, expected);
            Ok(())
        }

        #[test]
        fn test_two_pluses_infix() -> Res {
            let source = "12++34";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_two_minuses_infix() -> Res {
            let source = "12--34";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_two_asterisks_infix() -> Res {
            let source = "12**34";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_two_slashes_infix() -> Res {
            let source = "12//34";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_two_percents_infix() -> Res {
            let source = "12%%34";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_plus_minus_infix() -> Res {
            let source = "12+-34";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_asterisk_slash_infix() -> Res {
            let source = "12*/34";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_percent_plus_infix() -> Res {
            let source = "12%+34";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_plus() -> Res {
            let source = "+";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_minus() -> Res {
            let source = "-";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_asterisk() -> Res {
            let source = "*";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_slash() -> Res {
            let source = "/";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }

        #[test]
        fn test_percent() -> Res {
            let source = "%";

            let repr = execute(source);

            // TODO: specify error
            assert!(matches!(repr, Err(ExecError::Parse(_))));
            Ok(())
        }
    }
}
