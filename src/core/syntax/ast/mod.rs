use crate::util::{Range, range};

/// Kinds of AST produced during parsing.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum AstKind {
    Program { expressions: Vec<Ast> },
    Number(f64),
    InfixPlus { left: Box<Ast>, right: Box<Ast> },
}

/// An abstract syntax tree, or AST produced during parsing.
#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    pub kind: AstKind,
    pub location: Range,
}

impl Ast {
    pub fn new(kind: AstKind, location: Range) -> Self {
        Ast { kind, location }
    }

    pub fn from_program(expressions: Vec<Ast>) -> Self {
        let location = Self::locate_expressions(&expressions);

        Ast::new(AstKind::Program { expressions }, location)
    }

    pub fn from_num(num: f64, location: Range) -> Self {
        Ast::new(AstKind::Number(num), location)
    }

    pub fn from_infix_plus(left: Ast, right: Ast) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixPlus {
            left: Box::new(left),
            right: Box::new(right),
        };
        Ast::new(kind, location)
    }

    fn locate_expressions(expressions: &Vec<Ast>) -> Range {
        if expressions.len() == 0 {
            return range::ORIGIN;
        }

        Range {
            begin: expressions[0].location.begin,
            end: expressions[expressions.len() - 1].location.end,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let range = Range::from_nums(1, 2, 3, 4);
        let ast = Ast::new(AstKind::Number(1.0), range.clone());

        assert_eq!(
            ast,
            Ast {
                kind: AstKind::Number(1.0),
                location: range
            }
        )
    }
}
