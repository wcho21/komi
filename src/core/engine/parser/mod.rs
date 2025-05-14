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
            let token = if let Some(x) = self.scanner.read() {
                x
            } else {
                break;
            };

            let bp = Bp::get_from_token(token);
            if threshold_bp.right >= bp.left {
                break;
            }

            self.scanner.advance();
            top = self.parse_infix_expression(&top, token)?;
        }

        Ok(top)
    }

    fn parse_expression_start(&mut self) -> ResAst {
        self.parse_num()
    }

    fn parse_infix_expression(&mut self, left: &Ast, infix: &Token) -> ResAst {
        match infix.kind {
            TokenKind::Plus => {
                let right = self.parse_expression(Bp::get_additive())?;
                let expression = Ast::from_infix_plus(left.clone(), right);
                Ok(expression)
            }
            TokenKind::Minus => {
                let right = self.parse_expression(Bp::get_additive())?;
                let expression = Ast::from_infix_minus(left.clone(), right);
                Ok(expression)
            }
            TokenKind::Asterisk => {
                let right = self.parse_expression(Bp::get_multiplicative())?;
                let expression = Ast::from_infix_asterisk(left.clone(), right);
                Ok(expression)
            }
            TokenKind::Slash => {
                let right = self.parse_expression(Bp::get_multiplicative())?;
                let expression = Ast::from_infix_slash(left.clone(), right);
                Ok(expression)
            }
            TokenKind::Percent => {
                let right = self.parse_expression(Bp::get_multiplicative())?;
                let expression = Ast::from_infix_percent(left.clone(), right);
                Ok(expression)
            }
            _ => panic!("todo"),
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
        fn test_parse_infix_minus() -> Res {
            let tokens = vec![
                Token::new(TokenKind::Number(1.0), Range::from_nums(0, 0, 0, 1)),
                Token::new(TokenKind::Minus, Range::from_nums(0, 1, 0, 2)),
                Token::new(TokenKind::Number(2.0), Range::from_nums(0, 2, 0, 3)),
            ];

            let ast = parse(&tokens)?;

            let expected = Ast::new(
                AstKind::Program {
                    expressions: vec![Ast::new(
                        AstKind::InfixMinus {
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

        #[test]
        fn test_parse_infix_slash() -> Res {
            let tokens = vec![
                Token::new(TokenKind::Number(1.0), Range::from_nums(0, 0, 0, 1)),
                Token::new(TokenKind::Slash, Range::from_nums(0, 1, 0, 2)),
                Token::new(TokenKind::Number(2.0), Range::from_nums(0, 2, 0, 3)),
            ];

            let ast = parse(&tokens)?;

            let expected = Ast::new(
                AstKind::Program {
                    expressions: vec![Ast::new(
                        AstKind::InfixSlash {
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
        fn test_parse_infix_percent() -> Res {
            let tokens = vec![
                Token::new(TokenKind::Number(1.0), Range::from_nums(0, 0, 0, 1)),
                Token::new(TokenKind::Percent, Range::from_nums(0, 1, 0, 2)),
                Token::new(TokenKind::Number(2.0), Range::from_nums(0, 2, 0, 3)),
            ];

            let ast = parse(&tokens)?;

            let expected = Ast::new(
                AstKind::Program {
                    expressions: vec![Ast::new(
                        AstKind::InfixPercent {
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
