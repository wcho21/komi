//! # Parser
//!
//! Reads *tokens* and returns *an abstract syntax tree (AST)* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the lexer and evaluator.

mod err;
mod token_scanner;

pub use err::{ParseError, ParseErrorKind};
use komi_syntax::bp;
use komi_syntax::{Ast, AstKind, Bp, Token, TokenKind};
use komi_util::{Range, Scanner};
use token_scanner::TokenScanner;

type ResAst = Result<Box<Ast>, ParseError>;
type MakePrefixAst<'a> = fn(Box<Ast>, &'a Range) -> Ast;
type MakeInfixAst = fn(Box<Ast>, Box<Ast>) -> Ast;

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

        while let Some(token) = self.scanner.read() {
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
                self.make_num_ast(n, &first_token.location)
            }
            TokenKind::Bool(b) => {
                self.scanner.advance();
                self.make_bool_ast(b, &first_token.location)
            }
            TokenKind::Plus => {
                self.scanner.advance();
                self.parse_plus_prefix_expression(&first_token.location)
            }
            TokenKind::Minus => {
                self.scanner.advance();
                self.parse_minus_prefix_expression(&first_token.location)
            }
            TokenKind::Bang => {
                self.scanner.advance();
                self.parse_bang_prefix_expression(&first_token.location)
            }
            TokenKind::LParen => {
                self.scanner.advance();
                self.parse_grouped_expression(first_token)
            }
            _ => {
                let location = first_token.location;
                Err(ParseError::new(ParseErrorKind::InvalidExprStart, location))
            }
        }
    }

    fn parse_plus_prefix_expression(&mut self, prefix_location: &'a Range) -> ResAst {
        self.read_operand_and_make_prefix_ast(prefix_location, Ast::from_prefix_plus)
    }

    fn parse_minus_prefix_expression(&mut self, prefix_location: &'a Range) -> ResAst {
        self.read_operand_and_make_prefix_ast(prefix_location, Ast::from_prefix_minus)
    }

    fn parse_bang_prefix_expression(&mut self, prefix_location: &'a Range) -> ResAst {
        self.read_operand_and_make_prefix_ast(prefix_location, Ast::from_prefix_bang)
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
            // TODO: choose which has better readability between this one below and the other one using `from_*` functions like above
            TokenKind::Conjunct => self.read_right_and_make_infix_ast(left, &bp::CONNECTIVE_BP, |left, right| {
                let location = Range::new(left.location.begin, right.location.end);
                let kind = AstKind::InfixConjunct { left, right };
                Ast::new(kind, location)
            }),
            TokenKind::Disjunct => self.read_right_and_make_infix_ast(left, &bp::CONNECTIVE_BP, |left, right| {
                let location = Range::new(left.location.begin, right.location.end);
                let kind = AstKind::InfixDisjunct { left, right };
                Ast::new(kind, location)
            }),
            _ => panic!("todo"), // TODO: can this happen? return unexpected error if not
        }
    }

    fn parse_grouped_expression(&mut self, first_token: &'a Token) -> ResAst {
        let mut grouped_ast = match self.scanner.read() {
            Some(x) => self.parse_expression(x, Bp::get_lowest()),
            None => Err(ParseError::new(ParseErrorKind::LParenNotClosed, first_token.location)),
        }?;

        match self.scanner.read() {
            Some(x) if x.kind == TokenKind::RParen => {
                let location = Range::new(first_token.location.begin, self.scanner.locate().end);
                grouped_ast.location = location;
                self.scanner.advance();
                Ok(grouped_ast)
            }
            _ => {
                let location = Range::new(first_token.location.begin, self.scanner.locate().end);
                Err(ParseError::new(ParseErrorKind::LParenNotClosed, location))
            }
        }
    }

    fn read_operand_and_make_prefix_ast(&mut self, prefix_location: &'a Range, make_ast: MakePrefixAst<'a>) -> ResAst {
        if let Some(x) = self.scanner.read() {
            let operand_ast = self.parse_expression(x, Bp::get_prefix())?;
            let prefix_ast = Box::new(make_ast(operand_ast, prefix_location));
            Ok(prefix_ast)
        } else {
            let location = Range::new(prefix_location.begin, self.scanner.locate().end);
            Err(ParseError::new(ParseErrorKind::NoPrefixOperand, location))
        }
    }

    fn read_right_and_make_infix_ast(&mut self, left: Box<Ast>, bp: &Bp, make_ast: MakeInfixAst) -> ResAst {
        // TODO: write exceptional case (else case here) first
        if let Some(x) = self.scanner.read() {
            let right = self.parse_expression(x, bp)?;
            let infix_ast = Box::new(make_ast(left, right));
            Ok(infix_ast)
        } else {
            let location = Range::new(left.location.begin, self.scanner.locate().end);
            Err(ParseError::new(ParseErrorKind::NoInfixRightOperand, location))
        }
    }

    fn make_num_ast(&mut self, num: f64, location: &Range) -> ResAst {
        Ok(Box::new(Ast::from_num(num, location)))
    }

    fn make_bool_ast(&mut self, boolean: bool, location: &Range) -> ResAst {
        Ok(Box::new(Ast::from_bool(boolean, location)))
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
    use super::{Ast, ParseError, ParseErrorKind, Range, Token, TokenKind, parse};
    use komi_syntax::{AstKind, mkast, mktoken};
    use rstest::rstest;

    /// Asserts given tokens to be parsed into the expected AST.
    /// Helps write a test declaratively.
    macro_rules! assert_parse {
        ($tokens:expr, $expected:expr $(,)?) => {
            assert_eq!(
                parse($tokens),
                Ok($expected),
                "received an ast (left) parsed from the tokens, but expected the different ast (right)",
            );
        };
    }

    /// Asserts parsing given tokens will fail.
    /// Helps write a test declaratively.
    macro_rules! assert_parse_fail {
        ($tokens:expr, $expected:expr $(,)?) => {
            assert_eq!(
                parse($tokens),
                Err($expected),
                "received a result (left), but expected an error (right)",
            );
        };
    }

    #[test]
    fn empty() {
        // Represents ``.
        assert_parse!(&vec![], mkast!(prog loc 0, 0, 0, 0, vec![]));
    }

    #[rstest]
    #[case::num(
        // Represents `1`.
        vec![
            mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1)
        ],
        mkast!(prog loc 0, 0, 0, 1, vec![
            mkast!(num 1.0, loc 0, 0, 0, 1),
        ])
    )]
    #[case::bool(
        // Represents `참`.
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1)
        ],
        mkast!(prog loc 0, 0, 0, 1, vec![
            mkast!(boolean true, loc 0, 0, 0, 1),
        ])
    )]
    fn single_literal(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::plus_num(
        // Represents `+1`.
        vec![
            mktoken!(TokenKind::Plus, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
        ],
        mkast!(prog loc 0, 0, 0, 2, vec![
            mkast!(prefix PrefixPlus, loc 0, 0, 0, 2,
                operand mkast!(num 1.0, loc 0, 1, 0, 2),
            ),
        ])
    )]
    #[case::minus_num(
        // Represents `-1`.
        vec![
            mktoken!(TokenKind::Minus, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
        ],
        mkast!(prog loc 0, 0, 0, 2, vec![
            mkast!(prefix PrefixMinus, loc 0, 0, 0, 2,
                operand mkast!(num 1.0, loc 0, 1, 0, 2),
            ),
        ])
    )]
    #[case::two_pluses_num(
        // Represents `++1`.
        vec![
            mktoken!(TokenKind::Plus, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Plus, loc 0, 1, 0, 2),
            mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
        ],
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(prefix PrefixPlus, loc 0, 0, 0, 3,
                operand mkast!(prefix PrefixPlus, loc 0, 1, 0, 3,
                    operand mkast!(num 1.0, loc 0, 2, 0, 3),
                ),
            ),
        ])
    )]
    #[case::two_minuses_num(
        // Represents `--1`.
        vec![
            mktoken!(TokenKind::Minus, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Minus, loc 0, 1, 0, 2),
            mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
        ],
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(prefix PrefixMinus, loc 0, 0, 0, 3,
                operand mkast!(prefix PrefixMinus, loc 0, 1, 0, 3,
                    operand mkast!(num 1.0, loc 0, 2, 0, 3),
                ),
            ),
        ])
    )]
    #[case::bang_bool(
        // Represents `!참`.
        vec![
            mktoken!(TokenKind::Bang, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Bool(true), loc 0, 1, 0, 2),
        ],
        mkast!(prog loc 0, 0, 0, 2, vec![
            mkast!(prefix PrefixBang, loc 0, 0, 0, 2,
                operand mkast!(boolean true, loc 0, 1, 0, 2),
            ),
        ])
    )]
    #[case::two_bang_bool(
        // Represents `!!참`.
        vec![
            mktoken!(TokenKind::Bang, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Bang, loc 0, 1, 0, 2),
            mktoken!(TokenKind::Bool(true), loc 0, 2, 0, 3),
        ],
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(prefix PrefixBang, loc 0, 0, 0, 3,
                operand mkast!(prefix PrefixBang, loc 0, 1, 0, 3,
                    operand mkast!(boolean true, loc 0, 2, 0, 3),
                ),
            ),
        ])
    )]
    fn prefix(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::plus(
        // Represents `1+2`.
        vec![
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
    )]
    #[case::minus(
        // Represents `1-2`.
        vec![
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
    )]
    #[case::asterisk(
        // Represents `1*2`.
        vec![
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
    )]
    #[case::slash(
        // Represents `1/2`.
        vec![
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
    )]
    #[case::percent(
        // Represents `1%2`.
        vec![
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
    )]
    #[case::conjunct(
        // Represents `참 그리고 거짓`.
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Conjunct, loc 0, 2, 0, 5),
            mktoken!(TokenKind::Bool(false), loc 0, 6, 0, 8),
        ],
        mkast!(prog loc 0, 0, 0, 8, vec![
            mkast!(infix InfixConjunct, loc 0, 0, 0, 8,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(boolean false, loc 0, 6, 0, 8),
            ),
        ])
    )]
    #[case::disjunct(
        // Represents `참 또는 거짓`.
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Disjunct, loc 0, 2, 0, 4),
            mktoken!(TokenKind::Bool(false), loc 0, 5, 0, 7),
        ],
        mkast!(prog loc 0, 0, 0, 7, vec![
            mkast!(infix InfixDisjunct, loc 0, 0, 0, 7,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(boolean false, loc 0, 5, 0, 7),
            ),
        ])
    )]
    fn infix(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::asterisk_without_left(
        // Represents `*1`.
        vec![
            mktoken!(TokenKind::Asterisk, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::slash_without_left(
        // Represents `/1`.
        vec![
            mktoken!(TokenKind::Slash, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::percent_without_left(
        // Represents `%1`.
        vec![
            mktoken!(TokenKind::Percent, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::conjunct_without_left(
        // Represents `그리고 참`.
        vec![
            mktoken!(TokenKind::Conjunct, loc 0, 0, 0, 3),
            mktoken!(TokenKind::Bool(true), loc 0, 4, 0, 5),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 3))
    )]
    #[case::disjunct_without_left(
        // Represents `또는 참`.
        vec![
            mktoken!(TokenKind::Conjunct, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Bool(true), loc 0, 3, 0, 4),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    fn infix_no_left_operand(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::plus_without_right(
        // Represents `1+`.
        vec![
            mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Plus, loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::minus_without_right(
        // Represents `1-`.
        vec![
            mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Minus, loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::asterisk_without_right(
        // Represents `1*`.
        vec![
            mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Asterisk, loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::slash_without_right(
        // Represents `1/`.
        vec![
            mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Slash, loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::percent_without_right(
        // Represents `1%`.
        vec![
            mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Percent, loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::conjunct_without_right(
        // Represents `참 그리고`.
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Conjunct, loc 0, 2, 0, 5),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 5))
    )]
    #[case::disjunct_without_right(
        // Represents `참 또는`.
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Disjunct, loc 0, 2, 0, 4),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 4))
    )]
    fn infix_no_right_operand(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::plus(
        // Represents `+`.
        vec![mktoken!(TokenKind::Plus, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::minus(
        // Represents `-`.
        vec![mktoken!(TokenKind::Minus, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::asterisk(
        // Represents `*`.
        vec![mktoken!(TokenKind::Asterisk, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::slash(
        // Represents `/`.
        vec![mktoken!(TokenKind::Slash, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::percent(
        // Represents `%`.
        vec![mktoken!(TokenKind::Percent, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::bang(
        // Represents `!`.
        vec![mktoken!(TokenKind::Bang, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::conjunct(
        // Represents `그리고`.
        vec![mktoken!(TokenKind::Conjunct, loc 0, 0, 0, 3)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 3))
    )]
    #[case::disjunct(
        // Represents `또는`.
        vec![mktoken!(TokenKind::Disjunct, loc 0, 0, 0, 2)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    fn single_token(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::lparen(
        // Represents `(`.
        vec![mktoken!(TokenKind::LParen, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::LParenNotClosed, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::rparen(
        // Represents `)`.
        vec![mktoken!(TokenKind::RParen, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    fn parenthesis(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::two_pluses(
        // Represents `1+2+3`, and expects to be parsed into `(1+2)+3`.
        vec![
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
    )]
    #[case::two_minuses(
    // Represents `1-2-3`, and expects to be parsed into `(1-2)-3`.
        vec![
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
    )]
    #[case::two_asterisks(
        // Represents `1*2*3`, and expects to be parsed into `(1*2)*3`.
        vec![
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
    )]
    #[case::two_slashes(
        // Represents `1/2/3`, and expects to be parsed into `(1/2)/3`.
        vec![
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
    )]
    #[case::two_percents(
        // Represents `1%2%3`, and expects to be parsed into `(1%2)%3`.
        vec![
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
    )]
    #[case::two_conjuncts(
        // Represents `참 그리고 참 그리고 참`, and expects to be parsed into `(참 그리고 참) 그리고 참`.
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Conjunct, loc 0, 2, 0, 5),
            mktoken!(TokenKind::Bool(true), loc 0, 6, 0, 7),
            mktoken!(TokenKind::Conjunct, loc 0, 8, 0, 11),
            mktoken!(TokenKind::Bool(true), loc 0, 12, 0, 13),
        ],
        mkast!(prog loc 0, 0, 0, 13, vec![
            mkast!(infix InfixConjunct, loc 0, 0, 0, 13,
                left mkast!(infix InfixConjunct, loc 0, 0, 0, 7,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 6, 0, 7),
                ),
                right mkast!(boolean true, loc 0, 12, 0, 13),
            ),
        ])
    )]
    #[case::two_disjuncts(
        // Represents `참 그리고 참 그리고 참`, and expects to be parsed into `(참 그리고 참) 그리고 참`.
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Disjunct, loc 0, 2, 0, 4),
            mktoken!(TokenKind::Bool(true), loc 0, 5, 0, 6),
            mktoken!(TokenKind::Disjunct, loc 0, 7, 0, 9),
            mktoken!(TokenKind::Bool(true), loc 0, 10, 0, 11),
        ],
        mkast!(prog loc 0, 0, 0, 11, vec![
            mkast!(infix InfixDisjunct, loc 0, 0, 0, 11,
                left mkast!(infix InfixDisjunct, loc 0, 0, 0, 6,
                    left mkast!(boolean true, loc 0, 0, 0, 1),
                    right mkast!(boolean true, loc 0, 5, 0, 6),
                ),
                right mkast!(boolean true, loc 0, 10, 0, 11),
            ),
        ])
    )]
    fn left_associativity(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::asterisk_prioritized_over_plus(
        // Represents `1+2*3`, and expects to be parsed into `1+(2*3)`.
        vec![
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
    )]
    #[case::slash_prioritized_over_minus(
        // Represents `1-2/3`, and expects to be parsed into `1-(2/3)`.
        vec![
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
    )]
    #[case::percent_prioritized_over_plus(
        // Represents `1+2%3`, and expects to be parsed into `1+(2%3)`.
        vec![
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
    )]
    fn priority(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::arithmetic_grouping(
        // Represents `(1-2)*3`
        vec![
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
    )]
    #[case::arithmetic_nested_grouping(
        // Represents `1-(2*(3-4))`
        vec![
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
    )]
    #[case::connective_grouping(
        // Represents `참 또는 (참 그리고 참)`
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Disjunct, loc 0, 2, 0, 4),
            mktoken!(TokenKind::LParen, loc 0, 5, 0, 6),
            mktoken!(TokenKind::Bool(true), loc 0, 6, 0, 7),
            mktoken!(TokenKind::Conjunct, loc 0, 8, 0, 10),
            mktoken!(TokenKind::Bool(true), loc 0, 11, 0, 12),
            mktoken!(TokenKind::RParen, loc 0, 12, 0, 13),
        ],
        mkast!(prog loc 0, 0, 0, 13, vec![
            mkast!(infix InfixDisjunct, loc 0, 0, 0, 13,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(infix InfixConjunct, loc 0, 5, 0, 13,
                    left mkast!(boolean true, loc 0, 6, 0, 7),
                    right mkast!(boolean true, loc 0, 11, 0, 12),
                ),
            ),
        ])
    )]
    #[case::connective_nested_grouping(
        // Represents `참 또는 (참 그리고 (참 또는 참))`
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Disjunct, loc 0, 2, 0, 4),
            mktoken!(TokenKind::LParen, loc 0, 5, 0, 6),
            mktoken!(TokenKind::Bool(true), loc 0, 6, 0, 7),
            mktoken!(TokenKind::Conjunct, loc 0, 8, 0, 10),
            mktoken!(TokenKind::LParen, loc 0, 11, 0, 12),
            mktoken!(TokenKind::Bool(true), loc 0, 12, 0, 13),
            mktoken!(TokenKind::Conjunct, loc 0, 14, 0, 17),
            mktoken!(TokenKind::Bool(true), loc 0, 18, 0, 19),
            mktoken!(TokenKind::RParen, loc 0, 19, 0, 20),
            mktoken!(TokenKind::RParen, loc 0, 20, 0, 21),
        ],
        mkast!(prog loc 0, 0, 0, 21, vec![
            mkast!(infix InfixDisjunct, loc 0, 0, 0, 21,
                left mkast!(boolean true, loc 0, 0, 0, 1),
                right mkast!(infix InfixConjunct, loc 0, 5, 0, 21,
                    left mkast!(boolean true, loc 0, 6, 0, 7),
                    right mkast!(infix InfixConjunct, loc 0, 11, 0, 20,
                        left mkast!(boolean true, loc 0, 12, 0, 13),
                        right mkast!(boolean true, loc 0, 18, 0, 19),
                    ),
                ),
            ),
        ])
    )]
    fn grouping(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::lparen_not_closed_and_end(
        // Represents `(1+2`.
        vec![
            mktoken!(TokenKind::LParen, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
            mktoken!(TokenKind::Plus, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(2.0), loc 0, 3, 0, 4),
        ],
        ParseError::new(ParseErrorKind::LParenNotClosed, Range::from_nums(0, 0, 0, 4))
    )]
    #[case::lparen_not_closed_and_something(
        // Represents `(1+2 3`.
        vec![
            mktoken!(TokenKind::LParen, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
            mktoken!(TokenKind::Plus, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(2.0), loc 0, 3, 0, 4),
            mktoken!(TokenKind::Number(3.0), loc 0, 4, 0, 5),
        ],
        ParseError::new(ParseErrorKind::LParenNotClosed, Range::from_nums(0, 0, 0, 5))
    )]
    fn unmatched_parenthesis(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::two_numbers(
        // Represents `1 2`.
        vec![
            mktoken!(TokenKind::Number(1.0), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(2.0), loc 0, 2, 0, 3),
        ],
        mkast!(prog loc 0, 0, 0, 3, vec![
            mkast!(num 1.0, loc 0, 0, 0, 1),
            mkast!(num 2.0, loc 0, 2, 0, 3),
        ])
    )]
    fn multiple_tokens(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}
