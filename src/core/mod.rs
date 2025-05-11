mod engine;
mod syntax;

pub fn execute(source: &str) -> String {
    let tokens = engine::lex(&source);
    let ast = engine::parse(&tokens);
    let value = engine::evaluate(&ast);
    let representation = engine::represent(&value);

    representation
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_work() {
        let source = "1";
        let executed = execute(source);

        assert_eq!(executed, "1");
    }
}
