mod token_scanner;

use crate::core::err::ParseError;
use crate::core::syntax::{Ast, AstKind, Token, TokenKind};
use crate::util::{Range, Spot, Scanner};
use token_scanner::TokenScanner;

type ResAst = Result<Ast, ParseError>;

struct Parser<'a> {
    scanner: TokenScanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            scanner: TokenScanner::new(tokens),
        }
    }

    pub fn parse(&self) -> ResAst {
        self.parse_program()
    }

    fn parse_program(&self) -> ResAst {
        self.parse_num()
    }

    fn parse_num(&self) -> ResAst {
        match self.scanner.read() {
            Some(Token {
                kind: TokenKind::Number(n),
                location,
            }) => Ok(Ast::new(AstKind::Number(*n), *location)),
            _ => Err(ParseError::Unexpected(
                "Unexpected".to_string(),
                Range::new(Spot::new(0, 0), Spot::new(0, 0)),
            )),
        }
    }
}

pub fn parse(tokens: &Vec<Token>) -> ResAst {
    Parser::new(tokens).parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::syntax::AstKind;

    type Res = Result<(), ParseError>;

    const RANGE_MOCKS: &[Range] = &[
        Range::new(Spot::new(0, 0), Spot::new(1, 0)),
        Range::new(Spot::new(1, 0), Spot::new(3, 0)),
    ];

    const TOKEN_MOCKS: &[Token] = &[
        Token::new(TokenKind::Number(1.0), RANGE_MOCKS[0]),
        Token::new(TokenKind::Number(2.0), RANGE_MOCKS[1]),
    ];

    #[test]
    fn parse_num() -> Res {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let ast = parse(&tokens)?;

        let expected = Ast::new(AstKind::Number(1.0), RANGE_MOCKS[0]);
        assert_eq!(ast, expected);
        Ok(())
    }
}
