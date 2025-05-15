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
    use komi_lexer::{LexError, LexErrorKind};
    use komi_util::Range;

    type Res = Result<(), ExecError>;

    /// Asserts a given source to be interpreted into the expected result.
    /// Helps write a test more declaratively.
    macro_rules! assert_exec {
        ($source:expr, $expected:expr) => {
            assert_eq!(
                execute($source)?,
                $expected,
                "received a value (left) interpreted from the source '{}', but expected the different result (right)",
                $source,
            );
            return Ok(())
        };
    }

    /// Asserts executing a given source will fail.
    /// Helps write a test more declaratively.
    macro_rules! assert_exec_fail {
        ($source:expr, $expected:expr) => {
            assert_eq!(
                execute($source),
                Err($expected),
                "received a result (left), but expected executing the source '{}' to fail (right)",
                $source,
            );
            return Ok(())
        };
    }

    /// Asserts, with matching, executing a given source will fail.
    /// Helps write a test more declaratively.
    macro_rules! assert_exec_fail_match {
        ($source:expr, $expected:pat) => {
            assert!(
                matches!(execute($source), Err($expected)),
                "received a result (left), but expected executing the source '{}' to fail (right)",
                $source,
            );
            return Ok(())
        };
    }

    mod empty {
        use super::*;

        #[test]
        fn test_empty() -> Res {
            assert_exec!("", format!("{EMPTY_REPR}"));
        }

        #[test]
        fn test_whitespaces() -> Res {
            assert_exec!("  \t\t\r\r\n\n\r\n\r\n", format!("{EMPTY_REPR}"));
        }

        #[test]
        fn test_comment() -> Res {
            assert_exec!("# some comment", format!("{EMPTY_REPR}"));
        }

        #[test]
        fn test_multi_line_comment() -> Res {
            assert_exec!("# line ,\r\n# line 2", format!("{EMPTY_REPR}"));
        }
    }

    mod single_literals {
        use super::*;

        #[test]
        fn test_number_without_decimal() -> Res {
            assert_exec!("12", "12");
        }

        #[test]
        fn test_number_with_decimal() -> Res {
            assert_exec!("12.25", "12.25");
        }

        #[test]
        #[ignore] // TODO
        fn test_number_ending_with_dot() -> Res {
            assert_exec!("12.", "12");
        }

        #[test]
        #[ignore] // TODO
        fn test_number_beginning_with_dot() -> Res {
            assert_exec!(".25", "0.25");
        }

        #[test]
        #[ignore] // TODO
        fn test_number_with_plus() -> Res {
            assert_exec!("+12", "12");
        }

        #[test]
        #[ignore] // TODO
        fn test_number_with_minus() -> Res {
            assert_exec!("-12", "-12");
        }
    }

    mod arithmetic_expressions {
        use super::*;

        #[test]
        fn test_addition() -> Res {
            assert_exec!("1 + 2", "3");
        }

        #[test]
        fn test_subtraction() -> Res {
            assert_exec!("1 - 2", "-1");
        }

        #[test]
        fn test_multiplcation() -> Res {
            assert_exec!("3 * 4", "12");
        }

        #[test]
        fn test_division() -> Res {
            assert_exec!("3 / 4", "0.75");
        }

        #[test]
        fn test_mod() -> Res {
            assert_exec!("7 % 4", "3");
        }

        #[test]
        fn test_complex_arithmetic_expression() -> Res {
            assert_exec!("9 * 8 % 7 - 6 + 5 / 4", "-2.75");
        }

        #[test]
        #[ignore] // TODO
        fn test_grouping() -> Res {
            assert_exec!("8 - (4 - 2)", "6");
        }

        #[test]
        #[ignore] // TODO
        fn test_nested_grouping() -> Res {
            assert_exec!("16 - (8 - (4 - 2))", "10");
        }
    }

    mod fail {
        use super::*;

        #[test]
        fn test_dot() -> Res {
            assert_exec_fail!(
                ".",
                ExecError::Lex(LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1),))
            );
        }

        #[test]
        fn test_two_dots() -> Res {
            assert_exec_fail!(
                "..",
                ExecError::Lex(LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1),))
            );
        }

        #[test]
        #[ignore] // TODO
        fn test_two_pluses() -> Res {
            assert_exec_fail!(
                "++",
                ExecError::Lex(LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 1, 0, 2)))
            );
        }

        #[test]
        #[ignore] // TODO
        fn test_two_minuses() -> Res {
            assert_exec_fail!(
                "--",
                ExecError::Lex(LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 1, 0, 2)))
            );
        }

        #[test]
        #[ignore] // TODO
        fn test_two_pluses_infix() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("12++34", ExecError::Parse(_));
        }

        #[test]
        #[ignore] // TODO
        fn test_two_minuses_infix() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("12--34", ExecError::Parse(_));
        }

        #[test]
        fn test_two_asterisks_infix() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("12**34", ExecError::Parse(_));
        }

        #[test]
        fn test_two_slashes_infix() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("12//34", ExecError::Parse(_));
        }

        #[test]
        fn test_two_percents_infix() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("12%%34", ExecError::Parse(_));
        }

        #[test]
        #[ignore] // TODO
        fn test_plus_minus_infix() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("12+-34", ExecError::Parse(_));
        }

        #[test]
        fn test_asterisk_slash_infix() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("12*/34", ExecError::Parse(_));
        }

        #[test]
        #[ignore] // TODO
        fn test_percent_plus_infix() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("12%+34", ExecError::Parse(_));
        }

        #[test]
        fn test_plus() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("+", ExecError::Parse(_));
        }

        #[test]
        fn test_minus() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("-", ExecError::Parse(_));
        }

        #[test]
        fn test_asterisk() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("*", ExecError::Parse(_));
        }

        #[test]
        fn test_slash() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("/", ExecError::Parse(_));
        }

        #[test]
        fn test_percent() -> Res {
            // TODO: specify error
            assert_exec_fail_match!("%", ExecError::Parse(_));
        }
    }
}
