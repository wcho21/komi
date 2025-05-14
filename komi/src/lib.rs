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
    }

    mod math {
        use super::*;

        #[test]
        fn test_single_number_literal_without_decimal() -> Res {
            let source = "12";
            let executed = execute(source)?;

            assert_eq!(executed, "12");
            Ok(())
        }

        #[test]
        fn test_single_number_literal_with_decimal() -> Res {
            let source = "12.25";
            let executed = execute(source)?;

            assert_eq!(executed, "12.25");
            Ok(())
        }

        #[test]
        fn test_addition() -> Res {
            let source = "1 + 2";
            let executed = execute(source)?;

            assert_eq!(executed, "3");
            Ok(())
        }

        #[test]
        fn test_arithmetic_expression() -> Res {
            let source = "9*8%7-6+5/4";
            let executed = execute(source)?;

            assert_eq!(executed, "-2.75");
            Ok(())
        }
    }
}
