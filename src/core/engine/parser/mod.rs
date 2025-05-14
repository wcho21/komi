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
        let mut expressions: Vec<Ast> = vec![];

        while self.scanner.read() != None {
            let e = self.parse_expression(Bp::get_lowest())?;
            expressions.push(e);
        }

        return Ok(Ast::from_program(expressions));
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
            Some(Token { kind, location: _ }) => match kind {
                TokenKind::Plus => {
                    let bp = Bp::get_additive();
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
                TokenKind::Asterisk => {
                    let bp = Bp::get_multiplicative();
                    if threshold_bp.right >= bp.left {
                        return None;
                    }

                    self.scanner.advance();
                    let right = match self.parse_expression(bp) {
                        Ok(x) => x,
                        x => return Some(x),
                    };

                    let parsed_ast = Ast::from_infix_asterisk(left.clone(), right);
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
                Ok(Ast::from_num(*n, *location))
            }
            _ => Err(ParseError::Unexpected(
                "Unexpected".to_string(),
                Range::new(Spot::new(0, 0), Spot::new(0, 0)), // TODO: fix location
            )),
        }
    }
}

pub fn parse(tokens: &Vec<Token>) -> ResAst {
    Parser::new(tokens).parse()
}

/// Note: The `expected` variable in each test case should be build manually, not by using helper functions such as `from_program()`,
/// since its value would otherwise depend on the internal logic of those functions.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::syntax::AstKind;

    type Res = Result<(), ParseError>;

    mod empty {
        use super::*;

        #[test]
        fn test_parse_empty() -> Res {
            let tokens = vec![];

            let ast = parse(&tokens)?;

            let expected = Ast::new(AstKind::Program { expressions: vec![] }, Range::from_nums(0, 0, 0, 0));
            assert_eq!(ast, expected);
            Ok(())
        }
    }

    mod leaves {
        use super::*;

        #[test]
        fn test_parse_num() -> Res {
            let tokens = vec![Token::new(TokenKind::Number(1.0), Range::from_nums(0, 0, 0, 1))];

            let ast = parse(&tokens)?;

            let expected = Ast::new(
                AstKind::Program {
                    expressions: vec![Ast::new(AstKind::Number(1.0), Range::from_nums(0, 0, 0, 1))],
                },
                Range::from_nums(0, 0, 0, 1),
            );
            assert_eq!(ast, expected);
            Ok(())
        }
    }

    mod infixes {
        use super::*;

        #[test]
        fn test_parse_infix_plus() -> Res {
            let tokens = vec![
                Token::new(TokenKind::Number(1.0), Range::from_nums(0, 0, 0, 1)),
                Token::new(TokenKind::Plus, Range::from_nums(0, 1, 0, 2)),
                Token::new(TokenKind::Number(2.0), Range::from_nums(0, 2, 0, 3)),
            ];

            let ast = parse(&tokens)?;

            let expected = Ast::new(
                AstKind::Program {
                    expressions: vec![Ast::new(
                        AstKind::InfixPlus {
                            left: Box::new(Ast::new(AstKind::Number(1.0), Range::from_nums(0, 0, 0, 1))),
                            right: Box::new(Ast::new(AstKind::Number(2.0), Range::from_nums(0, 2, 0, 3))),
                        },
                        Range::from_nums(0, 0, 0, 3),
                    )],
                },
                Range::from_nums(0, 0, 0, 3),
            );
            assert_eq!(ast, expected);
            Ok(())
        }

        #[test]
        fn test_parse_infix_asterisk() -> Res {
            let tokens = vec![
                Token::new(TokenKind::Number(1.0), Range::from_nums(0, 0, 0, 1)),
                Token::new(TokenKind::Asterisk, Range::from_nums(0, 1, 0, 2)),
                Token::new(TokenKind::Number(2.0), Range::from_nums(0, 2, 0, 3)),
            ];

            let ast = parse(&tokens)?;

            let expected = Ast::new(
                AstKind::Program {
                    expressions: vec![Ast::new(
                        AstKind::InfixAsterisk {
                            left: Box::new(Ast::new(AstKind::Number(1.0), Range::from_nums(0, 0, 0, 1))),
                            right: Box::new(Ast::new(AstKind::Number(2.0), Range::from_nums(0, 2, 0, 3))),
                        },
                        Range::from_nums(0, 0, 0, 3),
                    )],
                },
                Range::from_nums(0, 0, 0, 3),
            );
            assert_eq!(ast, expected);
            Ok(())
        }
    }
}
