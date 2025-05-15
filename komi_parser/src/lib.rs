//! # Parser
//!
//! Reads *tokens* and returns *an abstract syntax tree (AST)* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the lexer and evaluator.

mod err;
mod token_scanner;

pub use err::{ParseError, ParseErrorKind};
use komi_syntax::{Ast, Bp, Token, TokenKind};
use komi_util::{Range, Scanner};
use token_scanner::TokenScanner;

type ResAst = Result<Box<Ast>, ParseError>;

/// Produces an AST from tokens.
struct Parser<'a> {
    scanner: TokenScanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self { scanner: TokenScanner::new(tokens) }
    }

    pub fn parse(&mut self) -> ResAst {
        self.parse_program()
    }

    fn parse_program(&mut self) -> ResAst {
        let mut expressions: Vec<Box<Ast>> = vec![];

        while let Some(x) = self.scanner.read() {
            let e = self.parse_expression(x, Bp::get_lowest())?;
            expressions.push(e);
        }

        return Ok(Box::new(Ast::from_program(expressions)));
    }

    fn parse_expression(&mut self, first_token: &'a Token, threshold_bp: &Bp) -> ResAst {
        let mut top = self.parse_expression_start(first_token)?;

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
            top = self.parse_infix_expression(top, token)?;
        }

        Ok(top)
    }

    fn parse_expression_start(&mut self, first_token: &'a Token) -> ResAst {
        match first_token.kind {
            TokenKind::Number(n) => {
                self.scanner.advance();
                self.make_num_ast(n, first_token.location)
            }
            TokenKind::LParen => {
                self.scanner.advance();
                self.parse_grouped_expression(first_token)
            }
            _ => Err(ParseError::new(
                ParseErrorKind::Unexpected,
                "".to_string(),
                Range::from_nums(0, 0, 0, 0),
            )),
        }
    }

    fn parse_infix_expression(&mut self, left: Box<Ast>, infix: &'a Token) -> ResAst {
        match infix.kind {
            TokenKind::Plus => {
                let bp = Bp::get_additive();
                self.read_right_and_make_infix_ast(left, bp, Ast::from_infix_plus)
            }
            TokenKind::Minus => {
                let bp = Bp::get_additive();
                self.read_right_and_make_infix_ast(left, bp, Ast::from_infix_minus)
            }
            TokenKind::Asterisk => {
                let bp = Bp::get_multiplicative();
                self.read_right_and_make_infix_ast(left, bp, Ast::from_infix_asterisk)
            }
            TokenKind::Slash => {
                let bp = Bp::get_multiplicative();
                self.read_right_and_make_infix_ast(left, bp, Ast::from_infix_slash)
            }
            TokenKind::Percent => {
                let bp = Bp::get_multiplicative();
                self.read_right_and_make_infix_ast(left, bp, Ast::from_infix_percent)
            }
            _ => panic!("todo"),
        }
    }

    fn parse_grouped_expression(&mut self, first_token: &'a Token) -> ResAst {
        let mut grouped_ast = match self.scanner.read() {
            Some(x) => self.parse_expression(x, Bp::get_lowest())?,
            None => panic!("no token in grouped? (todo make error kind)"),
        };

        match self.scanner.read() {
            Some(x) if x.kind == TokenKind::RParen => {
                grouped_ast.location.begin = first_token.location.begin;
                grouped_ast.location.end = self.scanner.locate().end;
                self.scanner.advance();
                Ok(grouped_ast)
            }
            _ => panic!("no matching right paren (todo make error kind)"),
        }
    }

    fn read_right_and_make_infix_ast(&mut self, left: Box<Ast>, bp: &Bp, make_ast: fn(Ast, Ast) -> Ast) -> ResAst {
        if let Some(x) = self.scanner.read() {
            let right = self.parse_expression(x, bp)?;
            let infix_ast = Box::new(make_ast(*left, *right));
            Ok(infix_ast)
        } else {
            panic!("no right");
        }
    }

    fn make_num_ast(&mut self, num: f64, location: Range) -> ResAst {
        Ok(Box::new(Ast::from_num(num, location)))
    }
}

/// Produces an AST from tokens.
pub fn parse(tokens: &Vec<Token>) -> ResAst {
    Parser::new(tokens).parse()
}

/// Note: The `expected` variable in each test case should be build manually, not by using helper functions such as `from_program()`,
/// since its value would otherwise depend on the internal logic of those functions.
#[cfg(test)]
mod tests {
    use super::{Ast, ParseError, Range, Token, TokenKind, parse};
    use komi_syntax::{AstKind, mkast, mktoken};

    type Res = Result<(), ParseError>;

    /// Asserts given tokens to be parsed into the expected AST.
    /// Helps write a test more declaratively.
    macro_rules! assert_parse {
        ($tokens:expr, $expected:expr) => {
            assert_eq!(
                parse($tokens)?,
                $expected,
                "received an ast (left) parsed from the tokens, but expected the different ast (right)",
            );
            return Ok(())
        };
    }

    mod empty {
        use super::*;

        /// Represents ``.
        #[test]
        fn test_empty() -> Res {
            assert_parse!(&vec![], mkast!(prog loc 0, 0, 0, 0, vec![]));
        }
    }

    mod leaves {
        use super::*;

        /// Represents `1`.
        #[test]
        fn test_num() -> Res {
            assert_parse!(
                &vec![mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1)],
                mkast!(prog loc 0, 0, 0, 1, vec![
                    mkast!(num 1.0, loc 0, 0, 0, 1),
                ])
            );
        }
    }

    mod infixes {
        use super::*;

        mod simple {
            use super::*;

            /// Represents `1+2`.
            #[test]
            fn test_plus() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Plus, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                    ],
                    mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixPlus, loc 0, 0, 0, 3,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(num 2.0, loc 0, 2, 0, 3),
                        ),
                    ])
                );
            }

            /// Represents `1-2`.
            #[test]
            fn test_minus() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Minus, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                    ],
                    mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixMinus, loc 0, 0, 0, 3,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(num 2.0, loc 0, 2, 0, 3),
                        ),
                    ])
                );
            }

            /// Represents `1*2`.
            #[test]
            fn test_asterisk() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Asterisk, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                    ],
                    mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(num 2.0, loc 0, 2, 0, 3),
                        ),
                    ])
                );
            }

            /// Represents `1/2`.
            #[test]
            fn test_slash() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Slash, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                    ],
                    mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixSlash, loc 0, 0, 0, 3,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(num 2.0, loc 0, 2, 0, 3),
                        ),
                    ])
                );
            }

            /// Represents `1%2`.
            #[test]
            fn test_percent() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Percent, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                    ],
                    mkast!(prog loc 0, 0, 0, 3, vec![
                        mkast!(infix InfixPercent, loc 0, 0, 0, 3,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(num 2.0, loc 0, 2, 0, 3),
                        ),
                    ])
                );
            }
        }

        mod associativity {
            use super::*;

            /// Represents `1+2+3`, and expects to be parsed into `(1+2)+3`.
            #[test]
            fn test_plus_left_assoc() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Plus, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Plus, loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
                    ],
                    mkast!(prog loc 0, 0, 0, 5, vec![
                        mkast!(infix InfixPlus, loc 0, 0, 0, 5,
                            left mkast!(infix InfixPlus, loc 0, 0, 0, 3,
                                left mkast!(num 1.0, loc 0, 0, 0, 1),
                                right mkast!(num 2.0, loc 0, 2, 0, 3),
                            ),
                            right mkast!(num 3.0, loc 0, 4, 0, 5),
                        ),
                    ])
                );
            }

            /// Represents `1-2-3`, and expects to be parsed into `(1-2)-3`.
            #[test]
            fn test_minus_left_assoc() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Minus, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Minus, loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
                    ],
                    mkast!(prog loc 0, 0, 0, 5, vec![
                        mkast!(infix InfixMinus, loc 0, 0, 0, 5,
                            left mkast!(infix InfixMinus, loc 0, 0, 0, 3,
                                left mkast!(num 1.0, loc 0, 0, 0, 1),
                                right mkast!(num 2.0, loc 0, 2, 0, 3),
                            ),
                            right mkast!(num 3.0, loc 0, 4, 0, 5),
                        ),
                    ])
                );
            }

            /// Represents `1*2*3`, and expects to be parsed into `(1*2)*3`.
            #[test]
            fn test_asterisk_left_assoc() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Asterisk, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Asterisk, loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
                    ],
                    mkast!(prog loc 0, 0, 0, 5, vec![
                        mkast!(infix InfixAsterisk, loc 0, 0, 0, 5,
                            left mkast!(infix InfixAsterisk, loc 0, 0, 0, 3,
                                left mkast!(num 1.0, loc 0, 0, 0, 1),
                                right mkast!(num 2.0, loc 0, 2, 0, 3),
                            ),
                            right mkast!(num 3.0, loc 0, 4, 0, 5),
                        ),
                    ])
                );
            }

            /// Represents `1/2/3`, and expects to be parsed into `(1/2)/3`.
            #[test]
            fn test_slash_left_assoc() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Slash, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Slash, loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
                    ],
                    mkast!(prog loc 0, 0, 0, 5, vec![
                        mkast!(infix InfixSlash, loc 0, 0, 0, 5,
                            left mkast!(infix InfixSlash, loc 0, 0, 0, 3,
                                left mkast!(num 1.0, loc 0, 0, 0, 1),
                                right mkast!(num 2.0, loc 0, 2, 0, 3),
                            ),
                            right mkast!(num 3.0, loc 0, 4, 0, 5),
                        ),
                    ])
                );
            }

            /// Represents `1%2%3`, and expects to be parsed into `(1%2)%3`.
            #[test]
            fn test_percent_left_assoc() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Percent, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Percent, loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
                    ],
                    mkast!(prog loc 0, 0, 0, 5, vec![
                        mkast!(infix InfixPercent, loc 0, 0, 0, 5,
                            left mkast!(infix InfixPercent, loc 0, 0, 0, 3,
                                left mkast!(num 1.0, loc 0, 0, 0, 1),
                                right mkast!(num 2.0, loc 0, 2, 0, 3),
                            ),
                            right mkast!(num 3.0, loc 0, 4, 0, 5),
                        ),
                    ])
                );
            }
        }

        mod priority {
            use super::*;

            /// Represents `1+2*3`, and expects to be parsed into `1+(2*3)`.
            #[test]
            fn test_asterisk_prioritized_over_plus() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Plus, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Asterisk, loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
                    ],
                    mkast!(prog loc 0, 0, 0, 5, vec![
                        mkast!(infix InfixPlus, loc 0, 0, 0, 5,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(infix InfixAsterisk, loc 0, 2, 0, 5,
                                left mkast!(num 2.0, loc 0, 2, 0, 3),
                                right mkast!(num 3.0, loc 0, 4, 0, 5),
                            ),
                        ),
                    ])
                );
            }

            /// Represents `1-2/3`, and expects to be parsed into `1-(2/3)`.
            #[test]
            fn test_slash_prioritized_over_minus() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Minus, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Slash, loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
                    ],
                    mkast!(prog loc 0, 0, 0, 5, vec![
                        mkast!(infix InfixMinus, loc 0, 0, 0, 5,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(infix InfixSlash, loc 0, 2, 0, 5,
                                left mkast!(num 2.0, loc 0, 2, 0, 3),
                                right mkast!(num 3.0, loc 0, 4, 0, 5),
                            ),
                        ),
                    ])
                );
            }

            /// Represents `1+2%3`, and expects to be parsed into `1+(2%3)`.
            #[test]
            fn test_percent_prioritized_over_plus() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Plus, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Percent, loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
                    ],
                    mkast!(prog loc 0, 0, 0, 5, vec![
                        mkast!(infix InfixPlus, loc 0, 0, 0, 5,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(infix InfixPercent, loc 0, 2, 0, 5,
                                left mkast!(num 2.0, loc 0, 2, 0, 3),
                                right mkast!(num 3.0, loc 0, 4, 0, 5),
                            ),
                        ),
                    ])
                );
            }
        }

        mod grouping {
            use super::*;

            /// Represents `(1-2)*3`
            #[test]
            fn test_grouping() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::LParen, loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
                        mktoken!(TokenKind::Minus, loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Number(2.0), loc 0, 3, 0, 4),
                        mktoken!(TokenKind::RParen, loc 0, 4, 0, 5),
                        mktoken!(TokenKind::Asterisk, loc 0, 5, 0, 6),
                        mktoken!(TokenKind::Number(3.0), loc 0, 6, 0, 7),
                    ],
                    mkast!(prog loc 0, 0, 0, 7, vec![
                        mkast!(infix InfixAsterisk, loc 0, 0, 0, 7,
                            left mkast!(infix InfixMinus, loc 0, 0, 0, 5,
                                left mkast!(num 1.0, loc 0, 1, 0, 2),
                                right mkast!(num 2.0, loc 0, 3, 0, 4),
                            ),
                            right mkast!(num 3.0, loc 0, 6, 0, 7),
                        ),
                    ])
                );
            }

            /// Represents `1-(2*(3-4))`
            #[test]
            fn test_nested_grouping() -> Res {
                assert_parse!(
                    &vec![
                        mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
                        mktoken!(TokenKind::Minus, loc 0, 1, 0, 2),
                        mktoken!(TokenKind::LParen, loc 0, 2, 0, 3),
                        mktoken!(TokenKind::Number(2.0), loc 0, 3, 0, 4),
                        mktoken!(TokenKind::Asterisk, loc 0, 4, 0, 5),
                        mktoken!(TokenKind::LParen, loc 0, 5, 0, 6),
                        mktoken!(TokenKind::Number(3.0), loc 0, 6, 0, 7),
                        mktoken!(TokenKind::Minus, loc 0, 7, 0, 8),
                        mktoken!(TokenKind::Number(4.0), loc 0, 8, 0, 9),
                        mktoken!(TokenKind::RParen, loc 0, 9, 0, 10),
                        mktoken!(TokenKind::RParen, loc 0, 10, 0, 11),
                    ],
                    mkast!(prog loc 0, 0, 0, 11, vec![
                        mkast!(infix InfixMinus, loc 0, 0, 0, 11,
                            left mkast!(num 1.0, loc 0, 0, 0, 1),
                            right mkast!(infix InfixAsterisk, loc 0, 2, 0, 11,
                                left mkast!(num 2.0, loc 0, 3, 0, 4),
                                right mkast!(infix InfixMinus, loc 0, 5, 0, 10,
                                    left mkast!(num 3.0, loc 0, 6, 0, 7),
                                    right mkast!(num 4.0, loc 0, 8, 0, 9),
                                ),
                            ),
                        ),
                    ])
                );
            }
        }
    }
}
