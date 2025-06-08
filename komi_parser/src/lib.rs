//! # Parser
//!
//! Reads *tokens* and returns *an abstract syntax tree (AST)* as defined in the `komi_syntax` crate.
//! Designed to be loosely coupled, so it does not rely on the implementation details of the lexer and evaluator.

mod tests;
mod token_scanner;
mod util;

use komi_syntax::ast::{Ast, AstKind};
use komi_syntax::bp::Bp;
use komi_syntax::error::{ParseError, ParseErrorKind};
use komi_syntax::token::{Token, TokenKind};
use komi_util::location::Range;
use komi_util::scanner::Scanner;
use komi_util::str_segment::StrSegment;
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

    fn parse_expression(&mut self, first_token: &Token, threshold_bp: &Bp) -> AstRes {
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

    fn parse_expression_start(&mut self, first_token: &Token) -> AstRes {
        match &first_token.kind {
            TokenKind::Number(n) => self.make_num_ast(*n, &first_token.location),
            TokenKind::Bool(b) => self.make_bool_ast(*b, &first_token.location),
            TokenKind::Str(s) => self.make_str_ast(s, &first_token.location),
            TokenKind::Identifier(i) => self.make_identifier_ast(i, &first_token.location),
            TokenKind::Plus => self.parse_plus_prefix_expression(&first_token.location),
            TokenKind::Minus => self.parse_minus_prefix_expression(&first_token.location),
            TokenKind::Bang => self.parse_bang_prefix_expression(&first_token.location),
            TokenKind::LParen => self.parse_grouped_expression(first_token),
            TokenKind::Closure => self.parse_closure_expression(&first_token.location),
            TokenKind::IfBranch => self.parse_branch_expression(&first_token.location),
            _ => {
                let location = first_token.location;
                Err(ParseError::new(ParseErrorKind::InvalidExprStart, location))
            }
        }
    }

    fn parse_branch_expression(&mut self, keyword_location: &Range) -> AstRes {
        // Parse a predicate
        let predicate = self.parse_branch_expression_predicate(keyword_location)?;

        // Parse a consequence
        let consequence = self.parse_branch_expression_consequence(keyword_location)?;

        // Expect an else-branch keyword
        let else_keyword_location = self.scanner.locate();
        let Some(token) = self.scanner.read_and_advance() else {
            let branch_location = Range::new(keyword_location.begin, else_keyword_location.end);
            return Err(ParseError::new(ParseErrorKind::NoAlternBlock, branch_location));
        };
        if token.kind != TokenKind::ElseBranch {
            return Err(ParseError::new(ParseErrorKind::NoAlternKeyword, else_keyword_location));
        }

        // Expect an if-branch keyword or opening brace
        let next_token_location = self.scanner.locate();
        let Some(next_token) = self.scanner.read_and_advance() else {
            let branch_location = Range::new(else_keyword_location.begin, next_token_location.end);
            return Err(ParseError::new(ParseErrorKind::NoAlternBlock, branch_location));
        };
        if next_token.kind == TokenKind::IfBranch {
            let nested_branch = self.parse_branch_expression(&next_token_location)?;
            let branch_location = Range::new(keyword_location.begin, nested_branch.location.end);
            let branch = Ast::new(
                AstKind::Branch { predicate, consequence, alternative: vec![nested_branch] },
                branch_location,
            );
            return Ok(Box::new(branch));
        }
        if next_token.kind != TokenKind::LBrace {
            return Err(ParseError::new(
                ParseErrorKind::NoOpeningBraceInAltern,
                next_token.location,
            ));
        }

        let alternative = self.parse_brace_block()?;

        // Expect a closing brace
        let altern_closing_brace_location = self.scanner.locate();
        let Some(_token) = self.scanner.read_and_advance() else {
            let else_location = Range::new(next_token_location.begin, altern_closing_brace_location.end);
            return Err(ParseError::new(ParseErrorKind::NoClosingBraceInAltern, else_location));
        };

        // Expect a non-empty alternative
        if alternative.len() == 0 {
            let else_location = Range::new(next_token_location.begin, altern_closing_brace_location.end);
            return Err(ParseError::new(ParseErrorKind::NoExprInAltern, else_location));
        }

        let branch_location = Range::new(keyword_location.begin, altern_closing_brace_location.end);
        let branch = Ast::new(AstKind::Branch { predicate, consequence, alternative }, branch_location);
        Ok(Box::new(branch))
    }

    /// Should be called after the scanner has advanced past the if-branch keyword.
    /// Stops at the character past the end of a predicate expression.
    fn parse_branch_expression_predicate(&mut self, keyword_location: &Range) -> AstRes {
        let first_location = self.scanner.locate();
        let Some(first_token) = self.scanner.read_and_advance() else {
            let branch_location = Range::new(keyword_location.begin, first_location.end);
            return Err(ParseError::new(ParseErrorKind::NoPredicate, branch_location));
        };

        let predicate = self.parse_expression(first_token, &Bp::LOWEST)?;
        Ok(predicate)
    }

    /// Should be called after the scanner has advanced past the end of a predicate expression.
    /// Stops at the character past the closing brace `}`.
    fn parse_branch_expression_consequence(&mut self, keyword_location: &Range) -> ExprsRes {
        // Expect an opening brace
        let conseq_opening_brace_location = self.scanner.locate();
        let Some(token) = self.scanner.read_and_advance() else {
            let branch_location = Range::new(keyword_location.begin, conseq_opening_brace_location.end);
            return Err(ParseError::new(ParseErrorKind::NoConseqBlock, branch_location));
        };
        if token.kind != TokenKind::LBrace {
            let branch_location = conseq_opening_brace_location;
            return Err(ParseError::new(ParseErrorKind::NoOpeningBraceInConseq, branch_location));
        }

        let consequence = self.parse_brace_block()?;

        // Expect a closing brace
        let conseq_closing_brace_location = self.scanner.locate();
        let Some(_token) = self.scanner.read_and_advance() else {
            let conseq_location = Range::new(conseq_opening_brace_location.begin, conseq_closing_brace_location.end);
            return Err(ParseError::new(ParseErrorKind::NoClosingBraceInConseq, conseq_location));
        };
        // Expect a non-empty consequence
        if consequence.len() == 0 {
            let conseq_location = Range::new(conseq_opening_brace_location.begin, conseq_closing_brace_location.end);
            return Err(ParseError::new(ParseErrorKind::NoExprConseq, conseq_location));
        }

        Ok(consequence)
    }

    /// Parses characters into a closure-expression AST, with the location `keyword_location` of the closure keyword.
    /// Should be called after the scanner has advanced past the closure keyword.
    fn parse_closure_expression(&mut self, keyword_location: &Range) -> AstRes {
        let parameters = self.parse_closure_expression_params(keyword_location)?;
        let body = self.parse_closure_expression_body()?;

        let token_location = self.scanner.locate();
        let token = self.scanner.read_and_advance();
        if token.is_none() || token.unwrap().kind != TokenKind::RBrace {
            return Err(ParseError::new(
                ParseErrorKind::NoClosingBraceInClosureBody,
                token_location,
            ));
        }
        // Check if the body is empty here, to locate the closure.
        if body.len() == 0 {
            let closure_location = Range::new(keyword_location.begin, token_location.end);
            return Err(ParseError::new(ParseErrorKind::NoExprInClosureBody, closure_location));
        }

        let closure_location = Range::new(keyword_location.begin, token_location.end);
        self.make_closure_ast(parameters, body, &closure_location)
    }

    /// Should be called after the scanner has advanced past a left brace.
    /// Stops at the end or a right brace `}`, so the caller should validate the end character is `}`.
    ///
    /// It possibly returns an empty vector for parsed expressions, which is also should be validated by the caller.
    fn parse_closure_expression_body(&mut self) -> ExprsRes {
        let expressions = self.parse_brace_block()?;
        Ok(expressions)
    }

    /// Should be called after the scanner has advanced past a left brace.
    /// Stops at the end or a right brace `}`, so the caller should validate the end character is `}`.
    ///
    /// It possibly returns an empty vector for parsed expressions, which is also should be validated by the caller.
    fn parse_brace_block(&mut self) -> ExprsRes {
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

    fn parse_closure_expression_params(&mut self, keyword_location: &Range) -> ParamsRes {
        let Some(first_token) = self.scanner.read_and_advance() else {
            let closure_location = Range::new(keyword_location.begin, self.scanner.locate().end);
            return Err(ParseError::new(ParseErrorKind::NoClosureParams, closure_location));
        };

        // Return if no parameters
        let mut parameters: Params = vec![];
        if first_token.kind == TokenKind::LBrace {
            return Ok(parameters);
        }

        // Expect the pattern `<first_param> [, <param>]* {`
        let Token { kind: TokenKind::Identifier(first_param), .. } = &first_token else {
            return Err(ParseError::new(
                ParseErrorKind::NonIdClosureParams,
                first_token.location,
            ));
        };
        parameters.push(String::from(first_param));

        loop {
            // First part: Expect a comma `,` or the left brace `{`
            let Some(token) = self.scanner.read_and_advance() else {
                let closure_location = Range::new(keyword_location.begin, self.scanner.locate().end);
                return Err(ParseError::new(ParseErrorKind::NoClosureBody, closure_location));
            };

            // Successfully break if end of parameters.
            if token.kind == TokenKind::LBrace {
                break;
            }

            // Return error if comma missing
            if token.kind != TokenKind::Comma {
                return Err(ParseError::new(ParseErrorKind::NoCommaInClosureParams, token.location));
            }

            // Second part: Expect an identifier as a parameter
            let Some(next_token) = self.scanner.read_and_advance() else {
                let closure_location = Range::new(keyword_location.begin, self.scanner.locate().end);
                return Err(ParseError::new(ParseErrorKind::NoClosureBody, closure_location));
            };
            let Token { kind: TokenKind::Identifier(param), .. } = next_token else {
                return Err(ParseError::new(ParseErrorKind::NonIdClosureParams, next_token.location));
            };

            parameters.push(param.to_owned());
        }

        Ok(parameters)
    }

    fn parse_plus_prefix_expression(&mut self, prefix_location: &Range) -> AstRes {
        let get_kind = |operand| AstKind::PrefixPlus { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_minus_prefix_expression(&mut self, prefix_location: &Range) -> AstRes {
        let get_kind = |operand| AstKind::PrefixMinus { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_bang_prefix_expression(&mut self, prefix_location: &Range) -> AstRes {
        let get_kind = |operand| AstKind::PrefixBang { operand };
        self.read_operand_and_make_prefix_ast(prefix_location, get_kind)
    }

    fn parse_expression_middle(&mut self, left: Box<Ast>, infix: &Token) -> AstRes {
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
            TokenKind::DoubleEquals => read_right_and_make_infix_ast!(self, left, COMPARISON, InfixDoubleEquals),
            TokenKind::BangEquals => read_right_and_make_infix_ast!(self, left, COMPARISON, InfixBangEquals),
            TokenKind::LBracketEquals => read_right_and_make_infix_ast!(self, left, COMPARISON, InfixLBracketEquals),
            TokenKind::RBracketEquals => read_right_and_make_infix_ast!(self, left, COMPARISON, InfixRBracketEquals),
            TokenKind::LBracket => read_right_and_make_infix_ast!(self, left, COMPARISON, InfixLBracket),
            TokenKind::RBracket => read_right_and_make_infix_ast!(self, left, COMPARISON, InfixRBracket),
            TokenKind::LParen => self.read_right_and_make_call_ast(left),
            _ => Err(ParseError::new(ParseErrorKind::UnexpectedExprInfix, infix.location)),
        }
    }

    fn parse_grouped_expression(&mut self, first_token: &Token) -> AstRes {
        let mut grouped_ast = match self.scanner.read_and_advance() {
            Some(x) => self.parse_expression(x, &Bp::LOWEST),
            None => Err(ParseError::new(
                ParseErrorKind::NoClosingParenInGroup,
                first_token.location,
            )),
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
                Err(ParseError::new(ParseErrorKind::NoClosingParenInGroup, location))
            }
        }
    }

    fn read_operand_and_make_prefix_ast<F>(&mut self, prefix_location: &Range, get_kind: F) -> AstRes
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

    /// Call this after advancing the scanner past the left parenthesis `(`.
    /// The scanner stops at the character immediately after the right parenthesis `)`.
    fn read_right_and_make_call_ast(&mut self, left: Box<Ast>) -> AstRes {
        let arguments = self.parse_call_arguments(&left.location)?;

        let rparen_location_end = self.scanner.locate().end;
        let location = Range::new(left.location.begin, rparen_location_end);
        self.scanner.advance();

        let kind = AstKind::Call { target: left, arguments };

        let call = Box::new(Ast::new(kind, location));
        Ok(call)
    }

    /// Call this after advancing the scanner past the character immediately after the left parenthesis `(`.
    /// The scanner stops at the character immediately before the right parenthesis `)`, for the scanner to locate the `)` later.
    fn parse_call_arguments(&mut self, call_target_location: &Range) -> ArgsRes {
        // Return an error if end
        let Some(first_token) = self.scanner.read() else {
            let call_location = Range::new(call_target_location.begin, self.scanner.locate().end);
            return Err(ParseError::new(ParseErrorKind::NoClosingParenInCallArgs, call_location));
        };

        // Return if no arguments
        let mut arguments: Args = vec![];
        if first_token.kind == TokenKind::RParen {
            return Ok(arguments);
        }

        self.scanner.advance(); // Advance past the `first_token`

        // Expect the pattern `<first_arg> [, <arg>]* )`
        let first_arg = self.parse_expression(first_token, &Bp::LOWEST)?;
        arguments.push(first_arg);

        loop {
            // First part: Expect a comma `,` or the right parenthesis `)`

            // Return error if end of source where the right parenthesis `)` would be
            let Some(token) = self.scanner.read() else {
                let call_location = Range::new(call_target_location.begin, self.scanner.locate().end);
                return Err(ParseError::new(ParseErrorKind::NoClosingParenInCallArgs, call_location));
            };

            // Successfully break if end of arguments
            if token.kind == TokenKind::RParen {
                break;
            }

            // Return error if comma missing
            if token.kind != TokenKind::Comma {
                return Err(ParseError::new(ParseErrorKind::NoCommaInCallArgs, token.location));
            }
            self.scanner.advance(); // Advance past the comma.

            // Second part: Expect an argument

            // Return error if end of source while reading arguments
            let Some(next_token) = self.scanner.read_and_advance() else {
                let call_location = Range::new(call_target_location.begin, self.scanner.locate().end);
                return Err(ParseError::new(ParseErrorKind::NoClosingParenInCallArgs, call_location));
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

    fn make_str_ast(&self, str: &Vec<StrSegment>, location: &Range) -> AstRes {
        Ok(Box::new(Ast::new(AstKind::Str(str.to_vec()), *location)))
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

/// Asserts given tokens to be parsed into the expected AST.
/// Helps write a test declaratively.
#[macro_export]
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
#[macro_export]
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
#[macro_export]
macro_rules! mkerr {
    ($kind:ident, $range:expr) => {
        ParseError::new(ParseErrorKind::$kind, $range)
    };
}
