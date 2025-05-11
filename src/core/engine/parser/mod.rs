use crate::core::syntax::{Ast, Token};
use crate::util::{Range, Spot};

pub fn parse(_tokens: &Vec<Token>) -> Ast {
    // fake implementation
    // return dummy ast
    let fake_location = Range::new(Spot::new(0, 0), Spot::new(0, 0));
    Ast::from_num(1.0, fake_location)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::syntax::AstKind;

    #[test]
    fn fake_parse() {
        let fake_tokens = vec![];
        let expected = Ast::new(
            AstKind::Number(1.0),
            Range::new(Spot::new(0, 0), Spot::new(0, 0)),
        );

        let tokens = parse(&fake_tokens);

        assert_eq!(tokens, expected);
    }
}
