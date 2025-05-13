mod token_scanner;

use crate::core::err::ParseError;
use crate::core::syntax::{Ast, AstKind, Bp, Token, TokenKind};
use crate::util::{Range, Scanner, Spot};
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

    pub fn parse(&mut self) -> ResAst {
        self.parse_program()
    }

    fn parse_program(&mut self) -> ResAst {
        self.parse_expression(Bp::get_lowest())
    }

    fn parse_expression(&mut self, threshold_bp: &Bp) -> ResAst {
        let mut top = self.parse_expression_start()?;

        loop {
            let infix = match self.parse_expression_middle(&top, threshold_bp) {
                Some(x) => x?,
                None => {
                    break;
                }
            };

            top = infix;
        }

        Ok(top)
    }

    fn parse_expression_start(&mut self) -> ResAst {
        self.parse_num()
    }

    fn parse_expression_middle(&mut self, left: &Ast, threshold_bp: &Bp) -> Option<ResAst> {
        match self.scanner.read() {
            Some(Token { kind, location }) => match kind {
                TokenKind::Plus => {
                    let bp = Bp::get_summative();
                    if threshold_bp.right >= bp.left {
                        return None;
                    }

                    self.scanner.advance();
                    let right = match self.parse_expression(bp) {
                        Ok(x) => x,
                        x => return Some(x),
                    };

                    let parsed_ast = Ast::from_infix_plus(left.clone(), right);
                    Some(Ok(parsed_ast))
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_num(&mut self) -> ResAst {
        match self.scanner.read() {
            Some(Token {
                kind: TokenKind::Number(n),
                location,
            }) => {
                self.scanner.advance();
                Ok(Ast::new(AstKind::Number(*n), *location))
            }
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
    fn test_parse_num() -> Res {
        let tokens = vec![TOKEN_MOCKS[0], TOKEN_MOCKS[1]];

        let ast = parse(&tokens)?;

        let expected = Ast::new(AstKind::Number(1.0), RANGE_MOCKS[0]);
        assert_eq!(ast, expected);
        Ok(())
    }

    mod infixes {
        use super::*;

        #[test]
        fn test_parse_infix_plus() -> Res {
            let tokens = vec![
                Token::new(
                    TokenKind::Number(1.0),
                    Range::new(Spot::new(0, 0), Spot::new(1, 0)),
                ),
                Token::new(
                    TokenKind::Plus,
                    Range::new(Spot::new(1, 0), Spot::new(2, 0)),
                ),
                Token::new(
                    TokenKind::Number(2.0),
                    Range::new(Spot::new(2, 0), Spot::new(3, 0)),
                ),
            ];

            let ast = parse(&tokens)?;

            let expected = Ast::new(
                AstKind::InfixPlus {
                    left: Box::new(Ast::new(
                        AstKind::Number(1.0),
                        Range::new(Spot::new(0, 0), Spot::new(1, 0)),
                    )),
                    right: Box::new(Ast::new(
                        AstKind::Number(2.0),
                        Range::new(Spot::new(2, 0), Spot::new(3, 0)),
                    )),
                },
                Range::new(Spot::new(0, 0), Spot::new(3, 0)),
            );
            assert_eq!(ast, expected);
            Ok(())
        }
    }
}
