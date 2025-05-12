mod engine;
mod err;
mod syntax;

use err::ExecErr;

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

    #[test]
    fn should_work() -> Result<(), ExecErr<'static>> {
        let source = "1";
        let executed = execute(source)?;

        assert_eq!(executed, "1");
        Ok(())
    }
}
