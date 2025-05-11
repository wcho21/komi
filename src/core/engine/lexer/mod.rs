use crate::core::syntax::{Token, TokenKind};
use crate::util::{Range, Spot};

pub fn lex(_source: &str) -> Vec<Token> {
    // fake implementation
    // return dummy token
    let fake_location = Range::new(Spot::new(0, 0), Spot::new(0, 0));
    vec![Token::new(TokenKind::Number(1.0), fake_location)]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fake_lex() {
        let fake_source = String::from("");
        let expected = vec![Token::new(
            TokenKind::Number(1.0),
            Range::new(Spot::new(0, 0), Spot::new(0, 0)),
        )];

        let tokens = lex(&fake_source);

        assert_eq!(tokens, expected);
    }
}
