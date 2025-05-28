//! # Parser
//!
//! Reads *tokens* and returns *an abstract syntax tree (AST)* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the lexer and evaluator.

mod err;
mod token_scanner;
mod util;

pub use err::{ParseError, ParseErrorKind};
use komi_syntax::{Ast, AstKind, Bp, Token, TokenKind};
use komi_util::{Range, Scanner};
use token_scanner::TokenScanner;

type AstRes = Result<Box<Ast>, ParseError>;
type Args = Vec<Box<Ast>>;
type Params = Vec<String>;
type Exprs = Vec<Box<Ast>>;
type ArgsRes = Result<Args, ParseError>;
type ParamsRes = Result<Params, ParseError>;
type ExprsRes = Result<Exprs, ParseError>;

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

    pub fn parse(&mut self) -> AstRes {
        self.parse_program()
    }

    fn parse_program(&mut self) -> AstRes {
        let expressions = self.parse_expressions()?;

        self.make_program_ast(expressions)
    }

    fn parse_expressions(&mut self) -> ExprsRes {
        let mut expressions: Exprs = vec![];

        while let Some(x) = self.scanner.read_and_advance() {
            let e = self.parse_expression(x, &Bp::LOWEST)?;
            expressions.push(e);
        }

        Ok(expressions)
    }

    fn parse_expression(&mut self, first_token: &'a Token, threshold_bp: &Bp) -> AstRes {
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

    fn parse_expression_start(&mut self, first_token: &'a Token) -> AstRes {
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
    fn parse_closure_expression(&mut self, keyword_location: &'a Range) -> AstRes {
        let mut parameters: Params = vec![];

        let token_location = self.scanner.locate();
        let mut token = self.scanner.read_and_advance();
        if let Some(Token { kind: TokenKind::Identifier(id), .. }) = &token {
            parameters.push(String::from(id));
            parameters.append(&mut self.parse_closure_expression_parameters()?);
            token = self.scanner.read_and_advance();
        }
        if token.is_none() || token.unwrap().kind != TokenKind::LBrace {
            // Should be an rbrace if no identifier appears. Return an error if not.
            return Err(ParseError::new(ParseErrorKind::InvalidClosureParam, token_location));
        }

        let body = self.parse_closure_expression_body()?;

        let token_location = self.scanner.locate();
        let token = self.scanner.read_and_advance();
        if token.is_none() || token.unwrap().kind != TokenKind::RBrace {
            return Err(ParseError::new(ParseErrorKind::ClosureBodyNotClosed, token_location));
        }

        let closure_location = Range::new(keyword_location.begin, token_location.end);
        self.make_closure_ast(parameters, body, &closure_location)
    }

    /// Should be called after the scanner has advanced past a left brace.
    /// Stops at the end or a right brace.
    fn parse_closure_expression_body(&mut self) -> ExprsRes {
        let mut expressions: Exprs = vec![];

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

    fn parse_closure_expression_parameters(&mut self) -> ParamsRes {
        let mut parameters: Params = vec![];

        while let Some(Token { kind: TokenKind::Comma, .. }) = self.scanner.read() {
            self.scanner.advance();

            let token_location = self.scanner.locate();
            let token = self.scanner.read_and_advance();
            if let Some(Token { kind: TokenKind::Identifier(id), .. }) = &token {
                parameters.push(String::from(id));
            } else {
                return Err(ParseError::new(ParseErrorKind::InvalidClosureParam, token_location));
            }
        }

        Ok(parameters)
    }

    fn parse_plus_prefix_expression(&mut self, prefix_location: &'a Range) -> AstRes {
        let get_kind = |operand| AstKind::PrefixPlus { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_minus_prefix_expression(&mut self, prefix_location: &'a Range) -> AstRes {
        let get_kind = |operand| AstKind::PrefixMinus { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_bang_prefix_expression(&mut self, prefix_location: &'a Range) -> AstRes {
        let get_kind = |operand| AstKind::PrefixBang { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_expression_middle(&mut self, left: Box<Ast>, infix: &'a Token) -> AstRes {
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

    fn parse_grouped_expression(&mut self, first_token: &'a Token) -> AstRes {
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

    fn read_operand_and_make_prefix_ast<F>(&mut self, prefix_location: &'a Range, get_kind: F) -> AstRes
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

    fn read_right_and_make_call_ast(&mut self, left: Box<Ast>) -> AstRes {
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

    fn read_call_arguments(&mut self, first_token: &'a Token) -> ArgsRes {
        // This function will read the pattern `first_arg [, arg]*`

        let mut arguments: Args = vec![];

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

    fn read_right_and_make_infix_ast<F>(&mut self, left: Box<Ast>, bp: &Bp, get_kind: F) -> AstRes
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

    fn make_num_ast(&self, num: f64, location: &Range) -> AstRes {
        Ok(Box::new(Ast::new(AstKind::Number(num), *location)))
    }

    fn make_bool_ast(&self, boolean: bool, location: &Range) -> AstRes {
        Ok(Box::new(Ast::new(AstKind::Bool(boolean), *location)))
    }

    fn make_identifier_ast(&self, identifier: &str, location: &Range) -> AstRes {
        Ok(Box::new(Ast::new(
            AstKind::Identifier(identifier.to_owned()),
            *location,
        )))
    }

    fn make_closure_ast(&self, parameters: Params, expressions: Exprs, location: &Range) -> AstRes {
        Ok(Box::new(Ast::new(
            AstKind::Closure { parameters, body: expressions },
            *location,
        )))
    }

    fn make_program_ast(&self, expressions: Exprs) -> AstRes {
        let location = util::locate_expressions(&expressions);

        Ok(Box::new(Ast::new(AstKind::Program { expressions }, location)))
    }
}

/// Produces an AST from tokens.
pub fn parse(tokens: &Vec<Token>) -> AstRes {
    Parser::new(tokens).parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use komi_syntax::{AstKind, mkast, mktoken};
    use komi_util::str_loc;
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

    /// Makes a `ParseError`.
    /// The first argument is the error kind `ParseErrorKind`.
    /// The second argument is the error location `Range`.
    macro_rules! mkerr {
        ($kind:ident, $range:expr) => {
            ParseError::new(ParseErrorKind::$kind, $range)
        };
    }

    #[test]
    fn empty() {
        // Represents ``.
        assert_parse!(&vec![], mkast!(prog loc str_loc!("", ""), vec![]));
    }

    #[rstest]
    #[case::num(
        // Represents `1`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0)
            )
        ],
        mkast!(prog loc str_loc!("", "1"), vec![
            mkast!(num 1.0, loc str_loc!("", "1")),
        ])
    )]
    #[case::bool(
        // Represents `참`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true)
            )
        ],
        mkast!(prog loc str_loc!("", "참"), vec![
            mkast!(boolean true, loc str_loc!("", "참")),
        ])
    )]
    #[case::identifier(
        // Represents `a`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a"))
            )
        ],
        mkast!(prog loc str_loc!("", "a"), vec![
            mkast!(identifier "a", loc str_loc!("", "a")),
        ])
    )]
    fn single_literal(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::plus_num(
        // Represents `+1`.
        vec![
            mktoken!(str_loc!("", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("+", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "+1"), vec![
            mkast!(prefix PrefixPlus, loc str_loc!("", "+1"),
                operand mkast!(num 1.0, loc str_loc!("+", "1")),
            ),
        ])
    )]
    #[case::minus_num(
        // Represents `-1`.
        vec![
            mktoken!(str_loc!("", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("-", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "-1"), vec![
            mkast!(prefix PrefixMinus, loc str_loc!("", "-1"),
                operand mkast!(num 1.0, loc str_loc!("-", "1")),
            ),
        ])
    )]
    #[case::two_pluses_num(
        // Represents `++1`.
        vec![
            mktoken!(str_loc!("", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("+", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("++", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "++1"), vec![
            mkast!(prefix PrefixPlus, loc str_loc!("", "++1"),
                operand mkast!(prefix PrefixPlus, loc str_loc!("+", "+1"),
                    operand mkast!(num 1.0, loc str_loc!("++", "1")),
                ),
            ),
        ])
    )]
    #[case::two_minuses_num(
        // Represents `--1`.
        vec![
            mktoken!(str_loc!("", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("-", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("--", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "--1"), vec![
            mkast!(prefix PrefixMinus, loc str_loc!("", "--1"),
                operand mkast!(prefix PrefixMinus, loc str_loc!("-", "-1"),
                    operand mkast!(num 1.0, loc str_loc!("--", "1")),
                ),
            ),
        ])
    )]
    #[case::bang_bool(
        // Represents `!참`.
        vec![
            mktoken!(str_loc!("", "!"),
                TokenKind::Bang,
            ),
            mktoken!(str_loc!("!", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkast!(prog loc str_loc!("", "!참"), vec![
            mkast!(prefix PrefixBang, loc str_loc!("", "!참"),
                operand mkast!(boolean true, loc str_loc!("!", "참")),
            ),
        ])
    )]
    #[case::two_bang_bool(
        // Represents `!!참`.
        vec![
            mktoken!(str_loc!("", "!"),
                TokenKind::Bang,
            ),
            mktoken!(str_loc!("!", "!"),
                TokenKind::Bang,
            ),
            mktoken!(str_loc!("!!", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkast!(prog loc str_loc!("", "!!참"), vec![
            mkast!(prefix PrefixBang, loc str_loc!("", "!!참"),
                operand mkast!(prefix PrefixBang, loc str_loc!("!", "!참"),
                    operand mkast!(boolean true, loc str_loc!("!!", "참")),
                ),
            ),
        ])
    )]
    fn prefix(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::plus(
        // Represents `1 + 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 + 2"), vec![
            mkast!(infix InfixPlus, loc str_loc!("", "1 + 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 + ", "2")),
            ),
        ])
    )]
    #[case::minus(
        // Represents `1 - 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 - 2"), vec![
            mkast!(infix InfixMinus, loc str_loc!("", "1 - 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 - ", "2")),
            ),
        ])
    )]
    #[case::asterisk(
        // Represents `1 * 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 * ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 * 2"), vec![
            mkast!(infix InfixAsterisk, loc str_loc!("", "1 * 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 * ", "2")),
            ),
        ])
    )]
    #[case::slash(
        // Represents `1 / 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("1 / ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 / 2"), vec![
            mkast!(infix InfixSlash, loc str_loc!("", "1 / 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 / ", "2")),
            ),
        ])
    )]
    #[case::percent(
        // Represents `1 % 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("1 % ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 % 2"), vec![
            mkast!(infix InfixPercent, loc str_loc!("", "1 % 2"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(num 2.0, loc str_loc!("1 % ", "2")),
            ),
        ])
    )]
    #[case::conjunct(
        // Represents `참 그리고 거짓`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("참 그리고 ", "거짓"),
                TokenKind::Bool(false),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 그리고 거짓"), vec![
            mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 거짓"),
                left mkast!(boolean true, loc str_loc!("", "참")),
                right mkast!(boolean false, loc str_loc!("참 그리고 ", "거짓")),
            ),
        ])
    )]
    #[case::disjunct(
        // Represents `참 또는 거짓`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 ", "거짓"),
                TokenKind::Bool(false),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 또는 거짓"), vec![
            mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 거짓"),
                left mkast!(boolean true, loc str_loc!("", "참")),
                right mkast!(boolean false, loc str_loc!("참 또는 ", "거짓")),
            ),
        ])
    )]
    #[case::equals(
        // Represents `a = 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("a = ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a = 1"), vec![
            mkast!(infix InfixEquals, loc str_loc!("", "a = 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a = ", "1")),
            ),
        ])
    )]
    #[case::plus_equals(
        // Represents `a += 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "+="),
                TokenKind::PlusEquals,
            ),
            mktoken!(str_loc!("a += ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a += 1"), vec![
            mkast!(infix InfixPlusEquals, loc str_loc!("", "a += 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a += ", "1")),
            ),
        ])
    )]
    #[case::minus_equals(
        // Represents `a -= 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "-="),
                TokenKind::MinusEquals,
            ),
            mktoken!(str_loc!("a -= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a -= 1"), vec![
            mkast!(infix InfixMinusEquals, loc str_loc!("", "a -= 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a -= ", "1")),
            ),
        ])
    )]
    #[case::asterisk_equals(
        // Represents `a *= 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "*="),
                TokenKind::AsteriskEquals,
            ),
            mktoken!(str_loc!("a *= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a *= 1"), vec![
            mkast!(infix InfixAsteriskEquals, loc str_loc!("", "a *= 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a *= ", "1")),
            ),
        ])
    )]
    #[case::slash_equals(
        // Represents `a /= 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "/="),
                TokenKind::SlashEquals,
            ),
            mktoken!(str_loc!("a /= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a /= 1"), vec![
            mkast!(infix InfixSlashEquals, loc str_loc!("", "a /= 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a /= ", "1")),
            ),
        ])
    )]
    #[case::percent_equals(
        // Represents `a %= 1`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "%="),
                TokenKind::PercentEquals,
            ),
            mktoken!(str_loc!("a %= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a %= 1"), vec![
            mkast!(infix InfixPercentEquals, loc str_loc!("", "a %= 1"),
                left mkast!(identifier String::from("a"), loc str_loc!("", "a")),
                right mkast!(num 1.0, loc str_loc!("a %= ", "1")),
            ),
        ])
    )]
    fn infix(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::asterisk_without_left(
        // Represents `* 1`.
        vec![
            mktoken!(str_loc!("", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("* ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "*")),
    )]
    #[case::slash_without_left(
        // Represents `/ 1`.
        vec![
            mktoken!(str_loc!("", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("/ ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "/")),
    )]
    #[case::percent_without_left(
        // Represents `% 1`.
        vec![
            mktoken!(str_loc!("", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("% ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "%")),
    )]
    #[case::conjunct_without_left(
        // Represents `그리고 참`.
        vec![
            mktoken!(str_loc!("", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("그리고 ", "그리고 참"),
                TokenKind::Bool(true),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "그리고")),
    )]
    #[case::disjunct_without_left(
        // Represents `또는 참`.
        vec![
            mktoken!(str_loc!("", "또는"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("또는 ", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "또는")),
    )]
    #[case::equals_without_left(
        // Represents `= 1`.
        vec![
            mktoken!(str_loc!("", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "=")),
    )]
    #[case::plus_equals_without_left(
        // Represents `+= 1`.
        vec![
            mktoken!(str_loc!("", "+="),
                TokenKind::PlusEquals,
            ),
            mktoken!(str_loc!("+= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "+=")),
    )]
    #[case::minus_equals_without_left(
        // Represents `-= 1`.
        vec![
            mktoken!(str_loc!("", "-="),
                TokenKind::MinusEquals,
            ),
            mktoken!(str_loc!("-= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "-=")),
    )]
    #[case::asterisk_equals_without_left(
        // Represents `*= 1`.
        vec![
            mktoken!(str_loc!("", "*="),
                TokenKind::AsteriskEquals,
            ),
            mktoken!(str_loc!("*= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "*=")),
    )]
    #[case::slash_equals_without_left(
        // Represents `/= 1`.
        vec![
            mktoken!(str_loc!("", "/="),
                TokenKind::SlashEquals,
            ),
            mktoken!(str_loc!("/= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "/=")),
    )]
    #[case::percent_equals_without_left(
        // Represents `%= 1`.
        vec![
            mktoken!(str_loc!("", "%="),
                TokenKind::PercentEquals,
            ),
            mktoken!(str_loc!("%= ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("", "%=")),
    )]
    fn infix_no_left_operand(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::plus_without_right(
        // Represents `1 +`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 +")),
    )]
    #[case::minus_without_right(
        // Represents `1 -`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "-"),
                TokenKind::Minus,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 -")),
    )]
    #[case::asterisk_without_right(
        // Represents `1 *`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "*"),
                TokenKind::Asterisk,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 *")),
    )]
    #[case::slash_without_right(
        // Represents `1 /`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "/"),
                TokenKind::Slash,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 /")),
    )]
    #[case::percent_without_right(
        // Represents `1 %`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "%"),
                TokenKind::Percent,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "1 %")),
    )]
    #[case::conjunct_without_right(
        // Represents `참 그리고`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "그리고"),
                TokenKind::Conjunct,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "참 그리고")),
    )]
    #[case::disjunct_without_right(
        // Represents `참 또는`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "또는"),
                TokenKind::Disjunct,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "참 또는")),
    )]
    #[case::equals_without_right(
        // Represents `a =`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "="),
                TokenKind::Equals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a =")),
    )]
    #[case::plus_equals_without_right(
        // Represents `a +=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "+="),
                TokenKind::PlusEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a +=")),
    )]
    #[case::minus_equals_without_right(
        // Represents `a -=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "-="),
                TokenKind::MinusEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a -=")),
    )]
    #[case::asterisk_equals_without_right(
        // Represents `a *=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "*="),
                TokenKind::AsteriskEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a *=")),
    )]
    #[case::slash_equals_without_right(
        // Represents `a /=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "/="),
                TokenKind::SlashEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a /=")),
    )]
    #[case::percent_equals_without_right(
        // Represents `a %=`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a " , "%="),
                TokenKind::PercentEquals,
            ),
        ],
        mkerr!(NoInfixRightOperand, str_loc!("", "a %=")),
    )]
    fn infix_no_right_operand(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::plus(
        // Represents `+`.
        vec![
            mktoken!(str_loc!("", "+"),
                TokenKind::Plus,
            )
        ],
        mkerr!(NoPrefixOperand, str_loc!("", "+"))
    )]
    #[case::minus(
        // Represents `-`.
        vec![
            mktoken!(str_loc!("", "-"),
                TokenKind::Minus,
            )
        ],
        mkerr!(NoPrefixOperand, str_loc!("", "-"))
    )]
    #[case::asterisk(
        // Represents `*`.
        vec![
            mktoken!(str_loc!("", "*"),
                TokenKind::Asterisk,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "*"))
    )]
    #[case::slash(
        // Represents `/`.
        vec![
            mktoken!(str_loc!("", "/"),
                TokenKind::Slash,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "/"))
    )]
    #[case::percent(
        // Represents `%`.
        vec![
            mktoken!(str_loc!("", "%"),
                TokenKind::Percent,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "%"))
    )]
    #[case::bang(
        // Represents `!`.
        vec![
            mktoken!(str_loc!("", "!"),
                TokenKind::Bang,
            )
        ],
        mkerr!(NoPrefixOperand, str_loc!("", "!"))
    )]
    #[case::conjunct(
        // Represents `그리고`.
        vec![
            mktoken!(str_loc!("", "그리고"),
                TokenKind::Conjunct,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "그리고"))
    )]
    #[case::disjunct(
        // Represents `또는`.
        vec![
            mktoken!(str_loc!("", "또는"),
                TokenKind::Disjunct,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "또는"))
    )]
    #[case::equals(
        // Represents `=`.
        vec![
            mktoken!(str_loc!("", "="),
                TokenKind::Equals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "="))
    )]
    #[case::plus_equals(
        // Represents `+=`.
        vec![
            mktoken!(str_loc!("", "+="),
                TokenKind::PlusEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "+="))
    )]
    #[case::minus_equals(
        // Represents `-=`.
        vec![
            mktoken!(str_loc!("", "-="),
                TokenKind::MinusEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "-="))
    )]
    #[case::asterisk_equals(
        // Represents `*=`.
        vec![
            mktoken!(str_loc!("", "*="),
                TokenKind::AsteriskEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "*="))
    )]
    #[case::slash_equals(
        // Represents `/=`.
        vec![
            mktoken!(str_loc!("", "/="),
                TokenKind::SlashEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "/="))
    )]
    #[case::percent_equals(
        // Represents `%=`.
        vec![
            mktoken!(str_loc!("", "%="),
                TokenKind::PercentEquals,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", "%="))
    )]
    fn single_token(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::lparen(
        // Represents `(`.
        vec![
            mktoken!(str_loc!("", "("),
                TokenKind::LParen,
            )
        ],
        mkerr!(LParenNotClosed, str_loc!("", "("))
    )]
    #[case::rparen(
        // Represents `)`.
        vec![
            mktoken!(str_loc!("", ")"),
                TokenKind::RParen,
            )
        ],
        mkerr!(InvalidExprStart, str_loc!("", ")"))
    )]
    fn parenthesis(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::two_pluses(
        // Represents `1 + 2 + 3`, and expects to be parsed into `(1 + 2) + 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 + 2 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + 2 + ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 + 2 + 3"), vec![
            mkast!(infix InfixPlus, loc str_loc!("", "1 + 2 + 3"),
                left mkast!(infix InfixPlus, loc str_loc!("", "1 + 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 + ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 + 2 + ", "3")),
            ),
        ])
    )]
    #[case::two_minuses(
        // Represents `1 - 2 - 3`, and expects to be parsed into `(1 - 2) - 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 - 2 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - 2 - ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 - 2 - 3"), vec![
            mkast!(infix InfixMinus, loc str_loc!("", "1 - 2 - 3"),
                left mkast!(infix InfixMinus, loc str_loc!("", "1 - 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 - ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 - 2 - ", "3")),
            ),
        ])
    )]
    #[case::two_asterisks(
        // Represents `1 * 2 * 3`, and expects to be parsed into `(1 * 2) * 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 * ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 * 2 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 * 2 * ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 * 2 * 3"), vec![
            mkast!(infix InfixAsterisk, loc str_loc!("", "1 * 2 * 3"),
                left mkast!(infix InfixAsterisk, loc str_loc!("", "1 * 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 * ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 * 2 * ", "3")),
            ),
        ])
    )]
    #[case::two_slashes(
        // Represents `1 / 2 / 3`, and expects to be parsed into `(1 / 2) / 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("1 / ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 / 2 ", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("1 / 2 / ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 / 2 / 3"), vec![
            mkast!(infix InfixSlash, loc str_loc!("", "1 / 2 / 3"),
                left mkast!(infix InfixSlash, loc str_loc!("", "1 / 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 / ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 / 2 / ", "3")),
            ),
        ])
    )]
    #[case::two_percents(
        // Represents `1 % 2 % 3`, and expects to be parsed into `(1 % 2) % 3`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("1 % ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 % 2 ", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("1 % 2 % ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 % 2 % 3"), vec![
            mkast!(infix InfixPercent, loc str_loc!("", "1 % 2 % 3"),
                left mkast!(infix InfixPercent, loc str_loc!("", "1 % 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 % ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("1 % 2 % ", "3")),
            ),
        ])
    )]
    #[case::two_conjuncts(
        // Represents `참 그리고 참 그리고 참`, and expects to be parsed into `(참 그리고 참) 그리고 참`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("참 그리고 ", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 그리고 참 ", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("참 그리고 참 그리고 ", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 그리고 참 그리고 참"), vec![
            mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 참 그리고 참"),
                left mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 참"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean true, loc str_loc!("참 그리고 ", "참")),
                ),
                right mkast!(boolean true, loc str_loc!("참 그리고 참 그리고 ", "참")),
            ),
        ])
    )]
    #[case::two_disjuncts(
        // Represents `참 또는 참 또는 참`, and expects to be parsed into `(참 또는 참) 또는 참`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 ", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 또는 참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 참 또는 ", "참"),
                TokenKind::Bool(true),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 또는 참 또는 참"), vec![
            mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 참 또는 참"),
                left mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 참"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean true, loc str_loc!("참 또는 ", "참")),
                ),
                right mkast!(boolean true, loc str_loc!("참 또는 참 또는 ", "참")),
            ),
        ])
    )]
    fn left_associativity(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::two_equals(
        // Represents `a = b = c`, and expects to be parsed into `a = (b = c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("a = ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a = b ", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("a = b = ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a = b = c"), vec![
            mkast!(infix InfixEquals, loc str_loc!("", "a = b = c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixEquals, loc str_loc!("a = ", "b = c"),
                    left mkast!(identifier "b", loc str_loc!("a = ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a = b = ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_plus_equals(
        // Represents `a += b += c`, and expects to be parsed into `a += (b += c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "+="),
                TokenKind::PlusEquals,
            ),
            mktoken!(str_loc!("a += ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a += b ", "+="),
                TokenKind::PlusEquals,
            ),
            mktoken!(str_loc!("a += b += ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a += b += c"), vec![
            mkast!(infix InfixPlusEquals, loc str_loc!("", "a += b += c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixPlusEquals, loc str_loc!("a += ", "b += c"),
                    left mkast!(identifier "b", loc str_loc!("a += ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a += b += ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_minus_equals(
        // Represents `a -= b -= c`, and expects to be parsed into `a -= (b -= c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "-="),
                TokenKind::MinusEquals,
            ),
            mktoken!(str_loc!("a -= ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a -= b ", "-="),
                TokenKind::MinusEquals,
            ),
            mktoken!(str_loc!("a -= b -= ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a -= b -= c"), vec![
            mkast!(infix InfixMinusEquals, loc str_loc!("", "a -= b -= c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixMinusEquals, loc str_loc!("a -= ", "b -= c"),
                    left mkast!(identifier "b", loc str_loc!("a -= ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a -= b -= ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_asterisk_equals(
        // Represents `a *= b *= c`, and expects to be parsed into `a *= (b *= c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "*="),
                TokenKind::AsteriskEquals,
            ),
            mktoken!(str_loc!("a *= ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a *= b ", "*="),
                TokenKind::AsteriskEquals,
            ),
            mktoken!(str_loc!("a *= b *= ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a *= b *= c"), vec![
            mkast!(infix InfixAsteriskEquals, loc str_loc!("", "a *= b *= c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixAsteriskEquals, loc str_loc!("a *= ", "b *= c"),
                    left mkast!(identifier "b", loc str_loc!("a *= ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a *= b *= ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_slash_equals(
        // Represents `a /= b /= c`, and expects to be parsed into `a /= (b /= c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "/="),
                TokenKind::SlashEquals,
            ),
            mktoken!(str_loc!("a /= ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a /= b ", "/="),
                TokenKind::SlashEquals,
            ),
            mktoken!(str_loc!("a /= b /= ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a /= b /= c"), vec![
            mkast!(infix InfixSlashEquals, loc str_loc!("", "a /= b /= c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixSlashEquals, loc str_loc!("a /= ", "b /= c"),
                    left mkast!(identifier "b", loc str_loc!("a /= ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a /= b /= ", "c")),
                ),
            ),
        ])
    )]
    #[case::two_percent_equals(
        // Represents `a %= b %= c`, and expects to be parsed into `a %= (b %= c)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "%="),
                TokenKind::PercentEquals,
            ),
            mktoken!(str_loc!("a %= ", "b"),
                TokenKind::Identifier(String::from("b")),
            ),
            mktoken!(str_loc!("a %= b ", "%="),
                TokenKind::PercentEquals,
            ),
            mktoken!(str_loc!("a %= b %= ", "c"),
                TokenKind::Identifier(String::from("c")),
            ),
        ],
        mkast!(prog loc str_loc!("", "a %= b %= c"), vec![
            mkast!(infix InfixPercentEquals, loc str_loc!("", "a %= b %= c"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixPercentEquals, loc str_loc!("a %= ", "b %= c"),
                    left mkast!(identifier "b", loc str_loc!("a %= ", "b")),
                    right mkast!(identifier "c", loc str_loc!("a %= b %= ", "c")),
                ),
            ),
        ])
    )]
    fn right_associativity(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::asterisk_prioritized_over_plus(
        // Represents `1 + 2 * 3`, and expects to be parsed into `1 + (2 * 3)`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 + 2 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 + 2 * ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 + 2 * 3"), vec![
            mkast!(infix InfixPlus, loc str_loc!("", "1 + 2 * 3"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(infix InfixAsterisk, loc str_loc!("1 + ", "2 * 3"),
                    left mkast!(num 2.0, loc str_loc!("1 + ", "2")),
                    right mkast!(num 3.0, loc str_loc!("1 + 2 * ", "3")),
                ),
            ),
        ])
    )]
    #[case::slash_prioritized_over_minus(
        // Represents `1 - 2 / 3`, and expects to be parsed into `1 - (2 / 3)`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 - 2 ", "/"),
                TokenKind::Slash,
            ),
            mktoken!(str_loc!("1 - 2 / ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 - 2 / 3"), vec![
            mkast!(infix InfixMinus, loc str_loc!("", "1 - 2 / 3"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(infix InfixSlash, loc str_loc!("1 - ", "2 / 3"),
                    left mkast!(num 2.0, loc str_loc!("1 - ", "2")),
                    right mkast!(num 3.0, loc str_loc!("1 - 2 / ", "3")),
                ),
            ),
        ])
    )]
    #[case::percent_prioritized_over_plus(
        // Represents `1 + 2 % 3`, and expects to be parsed into `1 + (2 % 3)`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("1 + ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 + 2 ", "%"),
                TokenKind::Percent,
            ),
            mktoken!(str_loc!("1 + 2 % ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 + 2 % 3"), vec![
            mkast!(infix InfixPlus, loc str_loc!("", "1 + 2 % 3"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(infix InfixPercent, loc str_loc!("1 + ", "2 % 3"),
                    left mkast!(num 2.0, loc str_loc!("1 + ", "2")),
                    right mkast!(num 3.0, loc str_loc!("1 + 2 % ", "3")),
                ),
            ),
        ])
    )]
    #[case::plus_prioritized_over_equals(
        // Represents `a = 1 + 2`, and expects to be parsed into `a = (1 + 2)`.
        vec![
            mktoken!(str_loc!("", "a"),
                TokenKind::Identifier(String::from("a")),
            ),
            mktoken!(str_loc!("a ", "="),
                TokenKind::Equals,
            ),
            mktoken!(str_loc!("a = ", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("a = 1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("a = 1 + ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "a = 1 + 2"), vec![
            mkast!(infix InfixEquals, loc str_loc!("", "a = 1 + 2"),
                left mkast!(identifier "a", loc str_loc!("", "a")),
                right mkast!(infix InfixPlus, loc str_loc!("a = ", "1 + 2"),
                    left mkast!(num 1.0, loc str_loc!("a = ", "1")),
                    right mkast!(num 2.0, loc str_loc!("a = 1 + ", "2")),
                ),
            ),
        ])
    )]
    fn priority(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }

    #[rstest]
    #[case::arithmetic_grouping(
        // Represents `(1 - 2) * 3`
        vec![
            mktoken!(str_loc!("", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("(1 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("(1 - ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("(1 - 2", ")"),
                TokenKind::RParen,
            ),
            mktoken!(str_loc!("(1 - 2) ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("(1 - 2) * ", "3"),
                TokenKind::Number(3.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "(1 - 2) * 3"), vec![
            mkast!(infix InfixAsterisk, loc str_loc!("", "(1 - 2) * 3"),
                left mkast!(infix InfixMinus, loc str_loc!("", "(1 - 2)"),
                    left mkast!(num 1.0, loc str_loc!("(", "1")),
                    right mkast!(num 2.0, loc str_loc!("(1 - ", "2")),
                ),
                right mkast!(num 3.0, loc str_loc!("(1 - 2) * ", "3")),
            ),
        ])
    )]
    #[case::arithmetic_nested_grouping(
        // Represents `1 - (2 * (3 - 4))`
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - ", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("1 - (", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("1 - (2 ", "*"),
                TokenKind::Asterisk,
            ),
            mktoken!(str_loc!("1 - (2 * ", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("1 - (2 * (", "3"),
                TokenKind::Number(3.0),
            ),
            mktoken!(str_loc!("1 - (2 * (3 ", "-"),
                TokenKind::Minus,
            ),
            mktoken!(str_loc!("1 - (2 * (3 - ", "4"),
                TokenKind::Number(4.0),
            ),
            mktoken!(str_loc!("1 - (2 * (3 - 4", ")"),
                TokenKind::RParen,
            ),
            mktoken!(str_loc!("1 - (2 * (3 - 4)", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "1 - (2 * (3 - 4))"), vec![
            mkast!(infix InfixMinus, loc str_loc!("", "1 - (2 * (3 - 4))"),
                left mkast!(num 1.0, loc str_loc!("", "1")),
                right mkast!(infix InfixAsterisk, loc str_loc!("1 - ", "(2 * (3 - 4))"),
                    left mkast!(num 2.0, loc str_loc!("1 - (", "2")),
                    right mkast!(infix InfixMinus, loc str_loc!("1 - (2 * ", "(3 - 4)"),
                        left mkast!(num 3.0, loc str_loc!("1 - (2 * (", "3")),
                        right mkast!(num 4.0, loc str_loc!("1 - (2 * (3 - ", "4")),
                    ),
                ),
            ),
        ])
    )]
    #[case::connective_grouping(
        // Represents `참 또는 (참 그리고 참)`
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 ", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("참 또는 (", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 또는 (참 ", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("참 또는 (참 그리고 ", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 또는 (참 그리고 참", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "참 또는 (참 그리고 참)"), vec![
            mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 (참 그리고 참)"), 
                left mkast!(boolean true, loc str_loc!("", "참")),
                right mkast!(infix InfixConjunct, loc str_loc!("참 또는 ", "(참 그리고 참)"),
                    left mkast!(boolean true, loc str_loc!("참 또는 (", "참")),
                    right mkast!(boolean true, loc str_loc!("참 또는 (참 그리고 ", "참")),
                ),
            ),
        ])
    )]
    #[case::connective_nested_grouping(
        // Represents `참 또는 (거짓 그리고 (참 또는 참))`
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 ", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("참 또는 (", "거짓"),
                TokenKind::Bool(false),
            ),
            mktoken!(str_loc!("참 또는 (거짓 ", "그리고"),
                TokenKind::Conjunct,
            ),
            mktoken!(str_loc!("참 또는 (거짓 그리고 ", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("참 또는 (거짓 그리고 (", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 또는 (거짓 그리고 (참 ", "또는"),
                TokenKind::Disjunct,
            ),
            mktoken!(str_loc!("참 또는 (거짓 그리고 (참 또는 ", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 또는 (거짓 그리고 (참 또는 참", ")"),
                TokenKind::RParen,
            ),
            mktoken!(str_loc!("참 또는 (거짓 그리고 (참 또는 참)", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "참 또는 (거짓 그리고 (참 또는 참))"), vec![
            mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 (거짓 그리고 (참 또는 참))"), 
                left mkast!(boolean true, loc str_loc!("", "참")),
                right mkast!(infix InfixConjunct, loc str_loc!("참 또는 ", "(거짓 그리고 (참 또는 참))"),
                    left mkast!(boolean false, loc str_loc!("참 또는 (", "거짓")),
                    right mkast!(infix InfixDisjunct, loc str_loc!("참 또는 (거짓 그리고 ", "(참 또는 참)"),
                        left mkast!(boolean true, loc str_loc!("참 또는 (거짓 그리고 (", "참")),
                        right mkast!(boolean true, loc str_loc!("참 또는 (거짓 그리고 (참 또는 ", "참")),
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
        // Represents `(1 + 2`.
        vec![
            mktoken!(str_loc!("", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("(1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("(1 + ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkerr!(LParenNotClosed, str_loc!("", "(1 + 2"))
    )]
    #[case::lparen_not_closed_and_something(
        // Represents `(1 + 2 3`.
        vec![
            mktoken!(str_loc!("", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("(1 ", "+"),
                TokenKind::Plus,
            ),
            mktoken!(str_loc!("(1 + ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("(1 + 2 ", "3"),
                TokenKind::Number(2.0),
            ),
        ],
        mkerr!(LParenNotClosed, str_loc!("", "(1 + 2 3"))
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
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "{"),
                TokenKind::LBrace,
            ),
            mktoken!(str_loc!("함수 {", "}"),
                TokenKind::RBrace,
            ),
        ],
        mkast!(prog loc str_loc!("", "함수 {}"), vec![
            mkast!(closure loc str_loc!("", "함수 {}"),
                params vec![],
                body vec![],
            ),
        ])
    )]
    #[case::single_parameter_and_empty_body(
        // Represents `함수 사과 {}`.
        vec![
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("함수 사과 ", "{"),
                TokenKind::LBrace,
            ),
            mktoken!(str_loc!("함수 사과 {", "}"),
                TokenKind::RBrace,
            ),
        ],
        mkast!(prog loc str_loc!("", "함수 사과 {}"), vec![
            mkast!(closure loc str_loc!("", "함수 사과 {}"),
                params vec![String::from("사과")],
                body vec![],
            ),
        ])
    )]
    #[case::multiple_parameters_and_empty_body(
        // Represents `함수 사과, 오렌지, 바나나 {}`.
        vec![
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("함수 사과", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("함수 사과, ", "오렌지"),
                TokenKind::Identifier(String::from("오렌지")),
            ),
            mktoken!(str_loc!("함수 사과, 오렌지", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, ", "바나나"),
                TokenKind::Identifier(String::from("바나나")),
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, 바나나 ", "{"),
                TokenKind::LBrace,
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, 바나나 {", "}"),
                TokenKind::RBrace,
            ),
        ],
        mkast!(prog loc str_loc!("", "함수 사과, 오렌지, 바나나 {}"), vec![
            mkast!(closure loc str_loc!("", "함수 사과, 오렌지, 바나나 {}"),
                params vec![
                    String::from("사과"),
                    String::from("오렌지"),
                    String::from("바나나"),
                ],
                body vec![],
            ),
        ])
    )]
    #[case::no_parameters_and_single_expression(
        // Represents `함수 { 1 }`.
        vec![
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "{"),
                TokenKind::LBrace,
            ),
            mktoken!(str_loc!("함수 { ", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("함수 { 1 ", "}"),
                TokenKind::RBrace,
            ),
        ],
        mkast!(prog loc str_loc!("", "함수 { 1 }"), vec![
            mkast!(closure loc str_loc!("", "함수 { 1 }"),
                params vec![],
                body vec![
                    mkast!(num 1.0, loc str_loc!("함수 { ", "1")),
                ],
            ),
        ])
    )]
    #[case::no_parameters_and_multiple_expression(
        // Represents `함수 { 1 2 3 }`.
        vec![
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "{"),
                TokenKind::LBrace,
            ),
            mktoken!(str_loc!("함수 { ", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("함수 { 1 ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("함수 { 1 2 ", "3"),
                TokenKind::Number(3.0),
            ),
            mktoken!(str_loc!("함수 { 1 2 3 ", "}"),
                TokenKind::RBrace,
            ),
        ],
        mkast!(prog loc str_loc!("", "함수 { 1 2 3 }"), vec![
            mkast!(closure loc str_loc!("", "함수 { 1 2 3 }"),
                params vec![],
                body vec![
                    mkast!(num 1.0, loc str_loc!("함수 { ", "1")),
                    mkast!(num 2.0, loc str_loc!("함수 { 1 ", "2")),
                    mkast!(num 3.0, loc str_loc!("함수 { 1 2 ", "3")),
                ],
            ),
        ])
    )]
    #[case::multiple_parameters_and_multiple_expression(
        // Represents `함수 사과, 오렌지, 바나나 { 1 2 3 }`.
        vec![
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("함수 사과", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("함수 사과, ", "오렌지"),
                TokenKind::Identifier(String::from("오렌지")),
            ),
            mktoken!(str_loc!("함수 사과, 오렌지", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, ", "바나나"),
                TokenKind::Identifier(String::from("바나나")),
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, 바나나 ", "{"),
                TokenKind::LBrace,
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, 바나나 { ", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, 바나나 { 1 ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, 바나나 { 1 2 ", "3"),
                TokenKind::Number(3.0),
            ),
            mktoken!(str_loc!("함수 사과, 오렌지, 바나나 { 1 2 3 ", "}"),
                TokenKind::RBrace,
            ),
        ],
        mkast!(prog loc str_loc!("", "함수 사과, 오렌지, 바나나 { 1 2 3 }"), vec![
            mkast!(closure loc str_loc!("", "함수 사과, 오렌지, 바나나 { 1 2 3 }"),
                params vec![
                    String::from("사과"),
                    String::from("오렌지"),
                    String::from("바나나"),
                ],
                body vec![
                    mkast!(num 1.0, loc str_loc!("함수 사과, 오렌지, 바나나 { ", "1")),
                    mkast!(num 2.0, loc str_loc!("함수 사과, 오렌지, 바나나 { 1 ", "2")),
                    mkast!(num 3.0, loc str_loc!("함수 사과, 오렌지, 바나나 { 1 2 ", "3")),
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
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
        ],
        mkerr!(InvalidClosureParam, str_loc!("함수", ""))
    )]
    #[case::invalid_parameters(
        // Represents `함수 +`.
        vec![
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "+"),
                TokenKind::Plus,
            ),
        ],
        mkerr!(InvalidClosureParam, str_loc!("함수 ", "+"))
    )]
    #[case::empty_body_not_closed(
        // Represents `함수 {`.
        vec![
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "{"),
                TokenKind::LBrace,
            ),
        ],
        mkerr!(ClosureBodyNotClosed, str_loc!("함수 {", ""))
    )]
    #[case::nonempty_body_not_closed(
        // Represents `함수 { 1`.
        vec![
            mktoken!(str_loc!("", "함수"),
                TokenKind::Closure,
            ),
            mktoken!(str_loc!("함수 ", "{"),
                TokenKind::LBrace,
            ),
            mktoken!(str_loc!("함수 { ", "1"),
                TokenKind::Number(1.0),
            ),
        ],
        mkerr!(ClosureBodyNotClosed, str_loc!("함수 { 1", ""))
    )]
    fn invalid_closure(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::id_with_no_args(
        // Represents `사과()`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "사과()"), vec![
            mkast!(call loc str_loc!("", "사과()"),
                target mkast!(identifier "사과", loc str_loc!("", "사과")),
                args vec![],
            ),
        ])
    )]
    #[case::id_with_single_arg(
        // Represents `사과(1)`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("사과(1", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "사과(1)"), vec![
            mkast!(call loc str_loc!("", "사과(1)"),
                target mkast!(identifier "사과", loc str_loc!("", "사과")),
                args vec![
                    mkast!(num 1.0, loc str_loc!("사과(", "1")),
                ],
            ),
        ])
    )]
    #[case::id_with_multiple_args(
        // Represents `사과(1, 2, 3)`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("사과(1 ", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(1, ", "2"),
                TokenKind::Number(2.0),
            ),
            mktoken!(str_loc!("사과(1, 2", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(1, 2, ", "3"),
                TokenKind::Number(3.0),
            ),
            mktoken!(str_loc!("사과(1, 2, 3", ")"),
                TokenKind::RParen,
            ),
        ],
        mkast!(prog loc str_loc!("", "사과(1, 2, 3)"), vec![
            mkast!(call loc str_loc!("", "사과(1, 2, 3)"),
                target mkast!(identifier "사과", loc str_loc!("", "사과")),
                args vec![
                    mkast!(num 1.0, loc str_loc!("사과(", "1")),
                    mkast!(num 2.0, loc str_loc!("사과(1, ", "2")),
                    mkast!(num 3.0, loc str_loc!("사과(1, 2, ", "3")),
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
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
        ],
        mkerr!(InvalidCallArgs, str_loc!("", "사과("))
    )]
    #[case::comma_after_lparen(
        // Represents `사과(,`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", ","),
                TokenKind::Comma,
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("사과(", ","))
    )]
    #[case::two_comma_after_arg(
        // Represents `사과(1,,`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과", "("),
                TokenKind::LParen,
            ),
            mktoken!(str_loc!("사과(", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("사과(1", ","),
                TokenKind::Comma,
            ),
            mktoken!(str_loc!("사과(1,", ","),
                TokenKind::Comma,
            ),
        ],
        mkerr!(InvalidExprStart, str_loc!("사과(1,", ","))
    )]
    fn invalid_call(#[case] tokens: Vec<Token>, #[case] error: ParseError) {
        assert_parse_fail!(&tokens, error);
    }

    #[rstest]
    #[case::two_numbers(
        // Represents `1 2`.
        vec![
            mktoken!(str_loc!("", "1"),
                TokenKind::Number(1.0),
            ),
            mktoken!(str_loc!("1 ", "2"),
                TokenKind::Number(2.0),
            ),
        ],
        mkast!(prog loc str_loc!("", "1 2"), vec![
            mkast!(num 1.0, loc str_loc!("", "1")),
            mkast!(num 2.0, loc str_loc!("1 ", "2")),
        ])
    )]
    #[case::two_bools(
        // Represents `참 거짓`.
        vec![
            mktoken!(str_loc!("", "참"),
                TokenKind::Bool(true),
            ),
            mktoken!(str_loc!("참 ", "거짓"),
                TokenKind::Bool(false),
            ),
        ],
        mkast!(prog loc str_loc!("", "참 거짓"), vec![
            mkast!(boolean true, loc str_loc!("", "참")),
            mkast!(boolean false, loc str_loc!("참 ", "거짓")),
        ])
    )]
    #[case::two_identifiers(
        // Represents `사과 오렌지`.
        vec![
            mktoken!(str_loc!("", "사과"),
                TokenKind::Identifier(String::from("사과")),
            ),
            mktoken!(str_loc!("사과 ", "오렌지"),
                TokenKind::Identifier(String::from("오렌지")),
            ),
        ],
        mkast!(prog loc str_loc!("", "사과 오렌지"), vec![
            mkast!(identifier "사과", loc str_loc!("", "사과")),
            mkast!(identifier "오렌지", loc str_loc!("사과 ", "오렌지")),
        ])
    )]
    fn multiple_tokens(#[case] tokens: Vec<Token>, #[case] expected: Box<Ast>) {
        assert_parse!(&tokens, expected);
    }
}
