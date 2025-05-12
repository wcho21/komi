mod engine;
mod err;
mod syntax;

pub use err::ExecErr;

pub fn execute(source: &str) -> Result<String, ExecErr> {
    let tokens = engine::lex(&source)?;
    let ast = engine::parse(&tokens);
    let value = engine::evaluate(&ast);
    let representation = engine::represent(&value);

    Ok(representation)
}

#[cfg(test)]
mod tests {
    use super::*;

    type Res = Result<(), ExecErr<'static>>;

    #[test]
    fn should_work() -> Res {
        let source = "1";
        let executed = execute(source)?;

        assert_eq!(executed, "1");
        Ok(())
    }

    #[test]
    fn test_fail() -> Res {
        let source = " ";
        let executed = execute(source);

        assert!(matches!(executed, Err(ExecErr::Lex(_))));
        Ok(())
    }
}
