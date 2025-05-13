mod engine;
pub mod err;
mod syntax;

pub use err::ExecError;

pub type ExecResult = Result<String, ExecError>;

pub fn execute(source: &str) -> ExecResult {
    let tokens = engine::lex(&source)?;
    let ast = engine::parse(&tokens)?;
    let value = engine::evaluate(&ast)?;
    let representation = engine::represent(&value);

    Ok(representation)
}

#[cfg(test)]
mod tests {
    use super::*;

    type Res = Result<(), ExecError>;

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
    fn test_fail() -> Res {
        let source = " ";
        let executed = execute(source);

        assert!(matches!(executed, Err(ExecError::Lex(_))));
        Ok(())
    }
}
