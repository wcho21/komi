//! # Parser
//!
//! Reads *tokens* and returns *an abstract syntax tree (AST)* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the lexer and evaluator.

mod err;
mod token_scanner;

pub use err::{ParseError, ParseErrorKind};
use komi_syntax::{Ast, AstKind, Bp, Token, TokenKind};
use komi_util::{Range, Scanner, range};
use token_scanner::TokenScanner;

type ResAst = Result<Box<Ast>, ParseError>;

/// Produces an AST from tokens.
struct Parser<'a> {
    scanner: TokenScanner<'a>,
}

macro_rules! read_right_and_make_infix_ast {
    ($self:ident, $left:ident, $bp:ident, $kind:ident) => {{
        let bp = &Bp::$bp;
        $self.read_right_and_make_infix_ast($left, bp, |left, right| AstKind::$kind { left, right })
    }};
}

impl<'a> Parser<'a> {
    // Design principle: once you read a token to use, advance the scanner and pass the data of the token as an argument.

    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self { scanner: TokenScanner::new(tokens) }
    }

    pub fn parse(&mut self) -> ResAst {
        self.parse_program()
    }

    fn parse_program(&mut self) -> ResAst {
        let mut expressions: Vec<Box<Ast>> = vec![];

        while let Some(x) = self.scanner.read_and_advance() {
            let e = self.parse_expression(x, &Bp::LOWEST)?;
            expressions.push(e);
        }

        self.make_program_ast(expressions)
    }

    fn parse_expression(&mut self, first_token: &'a Token, threshold_bp: &Bp) -> ResAst {
        let mut top = self.parse_expression_start(first_token)?;

        while let Some(token) = self.scanner.read() {
            let bp = Bp::get_from_token(token);
            if threshold_bp.right >= bp.left {
                break;
            }

            self.scanner.advance();
            top = self.parse_expression_middle(top, token)?;
        }

        Ok(top)
    }

    fn parse_expression_start(&mut self, first_token: &'a Token) -> ResAst {
        match &first_token.kind {
            TokenKind::Number(n) => self.make_num_ast(*n, &first_token.location),
            TokenKind::Bool(b) => self.make_bool_ast(*b, &first_token.location),
            TokenKind::Identifier(i) => self.make_identifier_ast(i, &first_token.location),
            TokenKind::Plus => self.parse_plus_prefix_expression(&first_token.location),
            TokenKind::Minus => self.parse_minus_prefix_expression(&first_token.location),
            TokenKind::Bang => self.parse_bang_prefix_expression(&first_token.location),
            TokenKind::LParen => self.parse_grouped_expression(first_token),
            TokenKind::Closure => self.parse_closure_expression(&first_token.location),
            _ => {
                let location = first_token.location;
                Err(ParseError::new(ParseErrorKind::InvalidExprStart, location))
            }
        }
    }

    /// Parses characters into a closure-expression AST, with the location `keyword_location` of the closure keyword.
    /// Should be called after the scanner has advanced past the closure keyword.
    fn parse_closure_expression(&mut self, keyword_location: &'a Range) -> ResAst {
        let mut parameters: Vec<String> = vec![];

        let token_location = self.scanner.locate();
        let mut token = self.scanner.read_and_advance();
        if let Some(Token { kind: TokenKind::Identifier(id), .. }) = &token {
            parameters.push(String::from(id));
            parameters.append(&mut self.parse_closure_expression_parameters()?);
            token = self.scanner.read_and_advance();
        }
        if token.is_none() || token.unwrap().kind != TokenKind::LBrace {
            // Should be an rbrace if no identifier appears. Return an error if not.
            return Err(ParseError::new(ParseErrorKind::InvalidFuncParam, token_location));
        }

        let body = self.parse_closure_expression_body()?;

        let token_location = self.scanner.locate();
        let token = self.scanner.read_and_advance();
        if token.is_none() || token.unwrap().kind != TokenKind::RBrace {
            return Err(ParseError::new(ParseErrorKind::FuncBodyNotClosed, token_location));
        }

        let closure_location = Range::new(keyword_location.begin, token_location.end);
        self.make_closure_ast(parameters, body, &closure_location)
    }

    /// Should be called after the scanner has advanced past a left brace.
    /// Stops at the end or a right brace.
    fn parse_closure_expression_body(&mut self) -> Result<Vec<Box<Ast>>, ParseError> {
        let mut expressions: Vec<Box<Ast>> = vec![];

        while let Some(token) = self.scanner.read() {
            if token.kind == TokenKind::RBrace {
                break;
            }

            self.scanner.advance();
            let expression = self.parse_expression(token, &Bp::LOWEST)?;
            expressions.push(expression);
        }

        Ok(expressions)
    }

    fn parse_closure_expression_parameters(&mut self) -> Result<Vec<String>, ParseError> {
        let mut parameters: Vec<String> = vec![];

        while let Some(Token { kind: TokenKind::Comma, .. }) = self.scanner.read() {
            self.scanner.advance();

            let token_location = self.scanner.locate();
            let token = self.scanner.read_and_advance();
            if let Some(Token { kind: TokenKind::Identifier(id), .. }) = &token {
                parameters.push(String::from(id));
            } else {
                return Err(ParseError::new(ParseErrorKind::InvalidFuncParam, token_location));
            }
        }

        Ok(parameters)
    }

    fn parse_plus_prefix_expression(&mut self, prefix_location: &'a Range) -> ResAst {
        let get_kind = |operand| AstKind::PrefixPlus { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_minus_prefix_expression(&mut self, prefix_location: &'a Range) -> ResAst {
        let get_kind = |operand| AstKind::PrefixMinus { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_bang_prefix_expression(&mut self, prefix_location: &'a Range) -> ResAst {
        let get_kind = |operand| AstKind::PrefixBang { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_expression_middle(&mut self, left: Box<Ast>, infix: &'a Token) -> ResAst {
        // Determine the AST kind by `get_kind` and the binding power of the infix by `bp`.
        match infix.kind {
            TokenKind::Plus => read_right_and_make_infix_ast!(self, left, ADDITIVE, InfixPlus),
            TokenKind::Minus => read_right_and_make_infix_ast!(self, left, ADDITIVE, InfixMinus),
            TokenKind::Asterisk => read_right_and_make_infix_ast!(self, left, MULTIPLICATIVE, InfixAsterisk),
            TokenKind::Slash => read_right_and_make_infix_ast!(self, left, MULTIPLICATIVE, InfixSlash),
            TokenKind::Percent => read_right_and_make_infix_ast!(self, left, MULTIPLICATIVE, InfixPercent),
            TokenKind::Conjunct => read_right_and_make_infix_ast!(self, left, CONNECTIVE, InfixConjunct),
            TokenKind::Disjunct => read_right_and_make_infix_ast!(self, left, CONNECTIVE, InfixDisjunct),
            TokenKind::Equals => read_right_and_make_infix_ast!(self, left, ASSIGNMENT, InfixEquals),
            TokenKind::PlusEquals => read_right_and_make_infix_ast!(self, left, ASSIGNMENT, InfixPlusEquals),
            TokenKind::MinusEquals => read_right_and_make_infix_ast!(self, left, ASSIGNMENT, InfixMinusEquals),
            TokenKind::AsteriskEquals => read_right_and_make_infix_ast!(self, left, ASSIGNMENT, InfixAsteriskEquals),
            TokenKind::SlashEquals => read_right_and_make_infix_ast!(self, left, ASSIGNMENT, InfixSlashEquals),
            TokenKind::PercentEquals => read_right_and_make_infix_ast!(self, left, ASSIGNMENT, InfixPercentEquals),
            TokenKind::LParen => self.read_right_and_make_call_ast(left),
            _ => panic!("todo"), // NOTE: this undetermined cases came from calling of the parse_expression(), and bp says nothing about the token kinds explicitly
        }
    }

    fn parse_grouped_expression(&mut self, first_token: &'a Token) -> ResAst {
        let mut grouped_ast = match self.scanner.read_and_advance() {
            Some(x) => self.parse_expression(x, &Bp::LOWEST),
            None => Err(ParseError::new(ParseErrorKind::LParenNotClosed, first_token.location)),
        }?;

        let rparen_location = self.scanner.locate();
        match self.scanner.read_and_advance() {
            Some(x) if x.kind == TokenKind::RParen => {
                let location = Range::new(first_token.location.begin, rparen_location.end);
                grouped_ast.location = location;
                Ok(grouped_ast)
            }
            _ => {
                let location = Range::new(first_token.location.begin, rparen_location.end);
                Err(ParseError::new(ParseErrorKind::LParenNotClosed, location))
            }
        }
    }

    fn read_operand_and_make_prefix_ast<F>(&mut self, prefix_location: &'a Range, get_kind: F) -> ResAst
    where
        F: Fn(Box<Ast>) -> AstKind,
    {
        // Return an error if end
        let Some(x) = self.scanner.read_and_advance() else {
            let location = Range::new(prefix_location.begin, self.scanner.locate().end);
            return Err(ParseError::new(ParseErrorKind::NoPrefixOperand, location));
        };

        let operand = self.parse_expression(x, &Bp::PREFIX)?;

        let location = Range::new(prefix_location.begin, operand.location.end);
        let kind = get_kind(operand);
        let prefix = Box::new(Ast::new(kind, location));
        Ok(prefix)
    }

    fn read_right_and_make_call_ast(&mut self, left: Box<Ast>) -> ResAst {
        // Return an error if end
        let Some(token) = self.scanner.read_and_advance() else {
            let location = Range::new(left.location.begin, self.scanner.locate().end);
            return Err(ParseError::new(ParseErrorKind::InvalidCallArgs, location));
        };

        let arguments = self.read_call_arguments(token)?;

        let location = Range::new(left.location.begin, self.scanner.locate().begin);
        let kind = AstKind::Call { target: left, arguments };
        let call = Box::new(Ast::new(kind, location));

        Ok(call)
    }

    fn read_call_arguments(&mut self, first_token: &'a Token) -> Result<Vec<Box<Ast>>, ParseError> {
        // This function will read the pattern `first_arg [, arg]*`

        let mut arguments: Vec<Box<Ast>> = vec![];

        if first_token.kind == TokenKind::RParen {
            return Ok(arguments);
        }

        let first_arg = self.parse_expression(first_token, &Bp::LOWEST)?;
        arguments.push(first_arg);

        while let Some(token) = self.scanner.read_and_advance() {
            // Successfully break if end of arguments
            if token.kind == TokenKind::RParen {
                break;
            }

            // Return error if invalid syntax due to a missing comma
            if token.kind != TokenKind::Comma {
                // TODO: return more specific error like `NoCommaCallArgs`
                return Err(ParseError::new(ParseErrorKind::InvalidCallArgs, token.location));
            }

            // Return error if end of source while reading arguments
            let next_token_location = self.scanner.locate();
            let Some(next_token) = self.scanner.read_and_advance() else {
                return Err(ParseError::new(ParseErrorKind::InvalidCallArgs, next_token_location));
            };

            let arg = self.parse_expression(next_token, &Bp::LOWEST)?;
            arguments.push(arg);
        }

        Ok(arguments)
    }

    fn read_right_and_make_infix_ast<F>(&mut self, left: Box<Ast>, bp: &Bp, get_kind: F) -> ResAst
    where
        F: Fn(Box<Ast>, Box<Ast>) -> AstKind,
    {
        // Return an error if end
        let Some(x) = self.scanner.read_and_advance() else {
            let location = Range::new(left.location.begin, self.scanner.locate().end);
            return Err(ParseError::new(ParseErrorKind::NoInfixRightOperand, location));
        };

        let right = self.parse_expression(x, bp)?;

        let location = Range::new(left.location.begin, right.location.end);
        let kind = get_kind(left, right);
        let infix = Box::new(Ast::new(kind, location));
        Ok(infix)
    }

    fn make_num_ast(&self, num: f64, location: &Range) -> ResAst {
        Ok(Box::new(Ast::new(AstKind::Number(num), *location)))
    }

    fn make_bool_ast(&self, boolean: bool, location: &Range) -> ResAst {
        Ok(Box::new(Ast::new(AstKind::Bool(boolean), *location)))
    }

    fn make_identifier_ast(&self, identifier: &str, location: &Range) -> ResAst {
        Ok(Box::new(Ast::new(
            AstKind::Identifier(identifier.to_owned()),
            *location,
        )))
    }

    fn make_closure_ast(&self, parameters: Vec<String>, expressions: Vec<Box<Ast>>, location: &Range) -> ResAst {
        Ok(Box::new(Ast::new(
            AstKind::Closure { parameters, body: expressions },
            *location,
        )))
    }

    fn make_program_ast(&self, expressions: Vec<Box<Ast>>) -> ResAst {
        let location = self.locate_expressions(&expressions);

        Ok(Box::new(Ast::new(AstKind::Program { expressions }, location)))
    }

    fn locate_expressions(&self, expressions: &Vec<Box<Ast>>) -> Range {
        if expressions.len() == 0 {
            return range::ORIGIN;
        }

        Range {
            begin: expressions[0].location.begin,
            end: expressions[expressions.len() - 1].location.end,
        }
    }
}

/// Produces an AST from tokens.
pub fn parse(tokens: &Vec<Token>) -> ResAst {
    Parser::new(tokens).parse()
}

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
    #[case::identifier(
        // Represents `a`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1)
        ],
        mkast!(prog loc 0, 0, 0, 1, vec![
            mkast!(identifier "a", loc 0, 0, 0, 1),
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
    #[case::equals(
        // Represents `a = 1`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Equals, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixEquals, loc 0, 0, 0, 5,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 4, 0, 5),
            ),
        ])
    )]
    #[case::plus_equals(
        // Represents `a += 1`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::PlusEquals, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixPlusEquals, loc 0, 0, 0, 5,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 4, 0, 5),
            ),
        ])
    )]
    #[case::minus_equals(
        // Represents `a -= 1`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::MinusEquals, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixMinusEquals, loc 0, 0, 0, 5,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 4, 0, 5),
            ),
        ])
    )]
    #[case::asterisk_equals(
        // Represents `a *= 1`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::AsteriskEquals, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixAsteriskEquals, loc 0, 0, 0, 5,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 4, 0, 5),
            ),
        ])
    )]
    #[case::slash_equals(
        // Represents `a /= 1`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::SlashEquals, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixSlashEquals, loc 0, 0, 0, 5,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 4, 0, 5),
            ),
        ])
    )]
    #[case::percent_equals(
        // Represents `a %= 1`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::PercentEquals, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixPercentEquals, loc 0, 0, 0, 5,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(num 1.0, loc 0, 4, 0, 5),
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
    #[case::equals_without_left(
        // Represents `=1`.
        vec![
            mktoken!(TokenKind::Equals, loc 0, 0, 0, 1),
            mktoken!(TokenKind::Number(1.0), loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::plus_equals_without_left(
        // Represents `+=1`.
        vec![
            mktoken!(TokenKind::PlusEquals, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::minus_equals_without_left(
        // Represents `-=1`.
        vec![
            mktoken!(TokenKind::MinusEquals, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::asterisk_equals_without_left(
        // Represents `*=1`.
        vec![
            mktoken!(TokenKind::AsteriskEquals, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::slash_equals_without_left(
        // Represents `/=1`.
        vec![
            mktoken!(TokenKind::SlashEquals, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::percent_equals_without_left(
        // Represents `%=1`.
        vec![
            mktoken!(TokenKind::PercentEquals, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
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
    #[case::equals_without_right(
        // Represents `a=`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Equals, loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::plus_equals_without_right(
        // Represents `a+=`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::PlusEquals, loc 0, 1, 0, 2),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::minus_equals_without_right(
        // Represents `a-=`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::MinusEquals, loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 3))
    )]
    #[case::asterisk_equals_without_right(
        // Represents `a*=`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::AsteriskEquals, loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 3))
    )]
    #[case::slash_equals_without_right(
        // Represents `a/=`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::SlashEquals, loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 3))
    )]
    #[case::percent_equals_without_right(
        // Represents `a%=`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::PercentEquals, loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::NoInfixRightOperand, Range::from_nums(0, 0, 0, 3))
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
    #[case::equals(
        // Represents `=`.
        vec![mktoken!(TokenKind::Equals, loc 0, 0, 0, 1)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1))
    )]
    #[case::plus_equals(
        // Represents `+=`.
        vec![mktoken!(TokenKind::PlusEquals, loc 0, 0, 0, 2)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::minus_equals(
        // Represents `-=`.
        vec![mktoken!(TokenKind::MinusEquals, loc 0, 0, 0, 2)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::asterisk_equals(
        // Represents `*=`.
        vec![mktoken!(TokenKind::AsteriskEquals, loc 0, 0, 0, 2)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::slash_equals(
        // Represents `/=`.
        vec![mktoken!(TokenKind::SlashEquals, loc 0, 0, 0, 2)],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2))
    )]
    #[case::percent_equals(
        // Represents `%=`.
        vec![mktoken!(TokenKind::PercentEquals, loc 0, 0, 0, 2)],
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
    #[case::two_equals(
        // Represents `a=b=c`, and expects to be parsed into `a=(b=c)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Equals, loc 0, 1, 0, 2),
            mktoken!(TokenKind::Identifier(String::from("b")), loc 0, 2, 0, 3),
            mktoken!(TokenKind::Equals, loc 0, 3, 0, 4),
            mktoken!(TokenKind::Identifier(String::from("c")), loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixEquals, loc 0, 0, 0, 5,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(infix InfixEquals, loc 0, 2, 0, 5,
                    left mkast!(identifier "b", loc 0, 2, 0, 3),
                    right mkast!(identifier "c", loc 0, 4, 0, 5),
                ),
            ),
        ])
    )]
    #[case::two_plus_equals(
        // Represents `a+=b+=c`, and expects to be parsed into `a+=(b+=c)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::PlusEquals, loc 0, 1, 0, 3),
            mktoken!(TokenKind::Identifier(String::from("b")), loc 0, 3, 0, 4),
            mktoken!(TokenKind::PlusEquals, loc 0, 4, 0, 6),
            mktoken!(TokenKind::Identifier(String::from("c")), loc 0, 6, 0, 7),
        ],
        mkast!(prog loc 0, 0, 0, 7, vec![
            mkast!(infix InfixPlusEquals, loc 0, 0, 0, 7,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(infix InfixPlusEquals, loc 0, 3, 0, 7,
                    left mkast!(identifier "b", loc 0, 3, 0, 4),
                    right mkast!(identifier "c", loc 0, 6, 0, 7),
                ),
            ),
        ])
    )]
    #[case::two_minus_equals(
        // Represents `a-=b-=c`, and expects to be parsed into `a-=(b-=c)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::MinusEquals, loc 0, 1, 0, 3),
            mktoken!(TokenKind::Identifier(String::from("b")), loc 0, 3, 0, 4),
            mktoken!(TokenKind::MinusEquals, loc 0, 4, 0, 6),
            mktoken!(TokenKind::Identifier(String::from("c")), loc 0, 6, 0, 7),
        ],
        mkast!(prog loc 0, 0, 0, 7, vec![
            mkast!(infix InfixMinusEquals, loc 0, 0, 0, 7,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(infix InfixMinusEquals, loc 0, 3, 0, 7,
                    left mkast!(identifier "b", loc 0, 3, 0, 4),
                    right mkast!(identifier "c", loc 0, 6, 0, 7),
                ),
            ),
        ])
    )]
    #[case::two_asterisk_equals(
        // Represents `a*=b*=c`, and expects to be parsed into `a*=(b*=c)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::AsteriskEquals, loc 0, 1, 0, 3),
            mktoken!(TokenKind::Identifier(String::from("b")), loc 0, 3, 0, 4),
            mktoken!(TokenKind::AsteriskEquals, loc 0, 4, 0, 6),
            mktoken!(TokenKind::Identifier(String::from("c")), loc 0, 6, 0, 7),
        ],
        mkast!(prog loc 0, 0, 0, 7, vec![
            mkast!(infix InfixAsteriskEquals, loc 0, 0, 0, 7,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(infix InfixAsteriskEquals, loc 0, 3, 0, 7,
                    left mkast!(identifier "b", loc 0, 3, 0, 4),
                    right mkast!(identifier "c", loc 0, 6, 0, 7),
                ),
            ),
        ])
    )]
    #[case::two_slash_equals(
        // Represents `a/=b/=c`, and expects to be parsed into `a/=(b/=c)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::SlashEquals, loc 0, 1, 0, 3),
            mktoken!(TokenKind::Identifier(String::from("b")), loc 0, 3, 0, 4),
            mktoken!(TokenKind::SlashEquals, loc 0, 4, 0, 6),
            mktoken!(TokenKind::Identifier(String::from("c")), loc 0, 6, 0, 7),
        ],
        mkast!(prog loc 0, 0, 0, 7, vec![
            mkast!(infix InfixSlashEquals, loc 0, 0, 0, 7,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(infix InfixSlashEquals, loc 0, 3, 0, 7,
                    left mkast!(identifier "b", loc 0, 3, 0, 4),
                    right mkast!(identifier "c", loc 0, 6, 0, 7),
                ),
            ),
        ])
    )]
    #[case::two_percent_equals(
        // Represents `a%=b%=c`, and expects to be parsed into `a%=(b%=c)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::PercentEquals, loc 0, 1, 0, 3),
            mktoken!(TokenKind::Identifier(String::from("b")), loc 0, 3, 0, 4),
            mktoken!(TokenKind::PercentEquals, loc 0, 4, 0, 6),
            mktoken!(TokenKind::Identifier(String::from("c")), loc 0, 6, 0, 7),
        ],
        mkast!(prog loc 0, 0, 0, 7, vec![
            mkast!(infix InfixPercentEquals, loc 0, 0, 0, 7,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(infix InfixPercentEquals, loc 0, 3, 0, 7,
                    left mkast!(identifier "b", loc 0, 3, 0, 4),
                    right mkast!(identifier "c", loc 0, 6, 0, 7),
                ),
            ),
        ])
    )]
    fn right_associativity(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
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
    #[case::plus_prioritized_over_equals(
        // Represents `a=1+2`, and expects to be parsed into `a=(1+2)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("a")), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Equals, loc 0, 1, 0, 2),
            mktoken!(TokenKind::Number(1.0), loc 0, 2, 0, 3),
            mktoken!(TokenKind::Plus, loc 0, 3, 0, 4),
            mktoken!(TokenKind::Number(2.0), loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(infix InfixEquals, loc 0, 0, 0, 5,
                left mkast!(identifier "a", loc 0, 0, 0, 1),
                right mkast!(infix InfixPlus, loc 0, 2, 0, 5,
                    left mkast!(num 1.0, loc 0, 2, 0, 3),
                    right mkast!(num 2.0, loc 0, 4, 0, 5),
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

    // TODO: test ending with parameter, comma and double comma in parameter list without body
    #[rstest]
    // TODO: closure cannot have the empty body, should be an error
    #[case::no_parameters_and_empty_body(
        // Represents `함수 {}`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::LBrace, loc 0, 3, 0, 4),
            mktoken!(TokenKind::RBrace, loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(closure loc 0, 0, 0, 5,
                param vec![],
                body vec![],
            ),
        ])
    )]
    #[case::single_parameter_and_empty_body(
        // Represents `함수 사과 {}`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 3, 0, 5),
            mktoken!(TokenKind::LBrace, loc 0, 6, 0, 7),
            mktoken!(TokenKind::RBrace, loc 0, 7, 0, 8),
        ],
        mkast!(prog loc 0, 0, 0, 8, vec![
            mkast!(closure loc 0, 0, 0, 8,
                param vec![String::from("사과")],
                body vec![],
            ),
        ])
    )]
    #[case::multiple_parameters_and_empty_body(
        // Represents `함수 사과, 오렌지, 바나나 {}`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 3, 0, 5),
            mktoken!(TokenKind::Comma, loc 0, 5, 0, 6),
            mktoken!(TokenKind::Identifier(String::from("오렌지")), loc 0, 7, 0, 10),
            mktoken!(TokenKind::Comma, loc 0, 10, 0, 11),
            mktoken!(TokenKind::Identifier(String::from("바나나")), loc 0, 12, 0, 15),
            mktoken!(TokenKind::LBrace, loc 0, 16, 0, 17),
            mktoken!(TokenKind::RBrace, loc 0, 17, 0, 18),
        ],
        mkast!(prog loc 0, 0, 0, 18, vec![
            mkast!(closure loc 0, 0, 0, 18,
                param vec![String::from("사과"), String::from("오렌지"), String::from("바나나")],
                body vec![],
            ),
        ])
    )]
    #[case::no_parameters_and_single_expression(
        // Represents `함수 { 1 }`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::LBrace, loc 0, 3, 0, 4),
            mktoken!(TokenKind::Number(1.0), loc 0, 5, 0, 6),
            mktoken!(TokenKind::RBrace, loc 0, 7, 0, 8),
        ],
        mkast!(prog loc 0, 0, 0, 8, vec![
            mkast!(closure loc 0, 0, 0, 8,
                param vec![],
                body vec![
                    mkast!(num 1.0, loc 0, 5, 0, 6),
                ],
            ),
        ])
    )]
    #[case::no_parameters_and_multiple_expression(
        // Represents `함수 { 1 2 3 }`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::LBrace, loc 0, 3, 0, 4),
            mktoken!(TokenKind::Number(1.0), loc 0, 5, 0, 6),
            mktoken!(TokenKind::Number(2.0), loc 0, 7, 0, 8),
            mktoken!(TokenKind::Number(3.0), loc 0, 9, 0, 10),
            mktoken!(TokenKind::RBrace, loc 0, 11, 0, 12),
        ],
        mkast!(prog loc 0, 0, 0, 12, vec![
            mkast!(closure loc 0, 0, 0, 12,
                param vec![],
                body vec![
                    mkast!(num 1.0, loc 0, 5, 0, 6),
                    mkast!(num 2.0, loc 0, 7, 0, 8),
                    mkast!(num 3.0, loc 0, 9, 0, 10),
                ],
            ),
        ])
    )]
    #[case::multiple_parameters_and_multiple_expression(
        // Represents `함수 사과, 오렌지, 바나나 { 1 2 3 }`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 3, 0, 5),
            mktoken!(TokenKind::Comma, loc 0, 5, 0, 6),
            mktoken!(TokenKind::Identifier(String::from("오렌지")), loc 0, 7, 0, 10),
            mktoken!(TokenKind::Comma, loc 0, 10, 0, 11),
            mktoken!(TokenKind::Identifier(String::from("바나나")), loc 0, 12, 0, 15),
            mktoken!(TokenKind::LBrace, loc 0, 16, 0, 17),
            mktoken!(TokenKind::Number(1.0), loc 0, 18, 0, 19),
            mktoken!(TokenKind::Number(2.0), loc 0, 20, 0, 21),
            mktoken!(TokenKind::Number(3.0), loc 0, 22, 0, 23),
            mktoken!(TokenKind::RBrace, loc 0, 24, 0, 25),
        ],
        mkast!(prog loc 0, 0, 0, 25, vec![
            mkast!(closure loc 0, 0, 0, 25,
                param vec![String::from("사과"), String::from("오렌지"), String::from("바나나")],
                body vec![
                    mkast!(num 1.0, loc 0, 18, 0, 19),
                    mkast!(num 2.0, loc 0, 20, 0, 21),
                    mkast!(num 3.0, loc 0, 22, 0, 23),
                ],
            ),
        ])
    )]
    fn closure(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::end_with_keyword(
        // Represents `함수`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
        ],
        ParseError::new(ParseErrorKind::InvalidFuncParam, Range::from_nums(0, 2, 0, 2))
    )]
    #[case::invalid_parameters(
        // Represents `함수 +`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::Plus, loc 0, 3, 0, 4),
        ],
        ParseError::new(ParseErrorKind::InvalidFuncParam, Range::from_nums(0, 3, 0, 4))
    )]
    #[case::empty_body_not_closed(
        // Represents `함수 {`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::LBrace, loc 0, 3, 0, 4),
        ],
        ParseError::new(ParseErrorKind::FuncBodyNotClosed, Range::from_nums(0, 4, 0, 4))
    )]
    #[case::nonempty_body_not_closed(
        // Represents `함수 { 1`.
        vec![
            mktoken!(TokenKind::Closure, loc 0, 0, 0, 2),
            mktoken!(TokenKind::LBrace, loc 0, 3, 0, 4),
            mktoken!(TokenKind::Number(1.0), loc 0, 5, 0, 6),
        ],
        ParseError::new(ParseErrorKind::FuncBodyNotClosed, Range::from_nums(0, 6, 0, 6))
    )]
    fn invalid_closure(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::id_with_no_args(
        // Represents `사과()`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::LParen, loc 0, 2, 0, 3),
            mktoken!(TokenKind::RParen, loc 0, 3, 0, 4),
        ],
        mkast!(prog loc 0, 0, 0, 4, vec![
            mkast!(call loc 0, 0, 0, 4,
                target mkast!(identifier "사과", loc 0, 0, 0, 2),
                args vec![],
            ),
        ])
    )]
    #[case::id_with_single_arg(
        // Represents `사과(1)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::LParen, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 3, 0, 4),
            mktoken!(TokenKind::RParen, loc 0, 4, 0, 5),
        ],
        mkast!(prog loc 0, 0, 0, 5, vec![
            mkast!(call loc 0, 0, 0, 5,
                target mkast!(identifier "사과", loc 0, 0, 0, 2),
                args vec![
                    mkast!(num 1.0, loc 0, 3, 0, 4),
                ],
            ),
        ])
    )]
    #[case::id_with_multiple_args(
        // Represents `사과(1,2,3)`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::LParen, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 3, 0, 4),
            mktoken!(TokenKind::Comma, loc 0, 4, 0, 5),
            mktoken!(TokenKind::Number(2.0), loc 0, 5, 0, 6),
            mktoken!(TokenKind::Comma, loc 0, 6, 0, 7),
            mktoken!(TokenKind::Number(3.0), loc 0, 7, 0, 8),
            mktoken!(TokenKind::RParen, loc 0, 8, 0, 9),
        ],
        mkast!(prog loc 0, 0, 0, 9, vec![
            mkast!(call loc 0, 0, 0, 9,
                target mkast!(identifier "사과", loc 0, 0, 0, 2),
                args vec![
                    mkast!(num 1.0, loc 0, 3, 0, 4),
                    mkast!(num 2.0, loc 0, 5, 0, 6),
                    mkast!(num 3.0, loc 0, 7, 0, 8),
                ],
            ),
        ])
    )]
    fn call(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::end_with_lparen(
        // Represents `사과(`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::LParen, loc 0, 2, 0, 3),
        ],
        ParseError::new(ParseErrorKind::InvalidCallArgs, Range::from_nums(0, 0, 0, 3))
    )]
    #[case::comma_after_lparen(
        // Represents `사과(,`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::LParen, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Comma, loc 0, 3, 0, 4),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 3, 0, 4))
    )]
    #[case::two_comma_after_arg(
        // Represents `사과(1,,`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::LParen, loc 0, 2, 0, 3),
            mktoken!(TokenKind::Number(1.0), loc 0, 3, 0, 4),
            mktoken!(TokenKind::Comma, loc 0, 4, 0, 5),
            mktoken!(TokenKind::Comma, loc 0, 5, 0, 6),
        ],
        ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 5, 0, 6))
    )]
    fn invalid_call(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
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
    #[case::two_bools(
        // Represents `참 거짓`.
        vec![
            mktoken!(TokenKind::Bool(true), loc 0, 0, 0, 1),
            mktoken!(TokenKind::Bool(false), loc 0, 2, 0, 4),
        ],
        mkast!(prog loc 0, 0, 0, 4, vec![
            mkast!(boolean true, loc 0, 0, 0, 1),
            mkast!(boolean false, loc 0, 2, 0, 4),
        ])
    )]
    #[case::two_identifiers(
        // Represents `사과 오렌지`.
        vec![
            mktoken!(TokenKind::Identifier(String::from("사과")), loc 0, 0, 0, 2),
            mktoken!(TokenKind::Identifier(String::from("오렌지")), loc 0, 3, 0, 6),
        ],
        mkast!(prog loc 0, 0, 0, 6, vec![
            mkast!(identifier "사과", loc 0, 0, 0, 2),
            mkast!(identifier "오렌지", loc 0, 3, 0, 6),
        ])
    )]
    fn multiple_tokens(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}
