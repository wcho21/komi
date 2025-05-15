use komi_util::{Range, range};

/// Kinds of AST produced during parsing.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum AstKind {
    Program { expressions: Vec<Box<Ast>> },
    Number(f64),
    PrefixPlus { operand: Box<Ast> },
    PrefixMinus { operand: Box<Ast> },
    InfixPlus { left: Box<Ast>, right: Box<Ast> },
    InfixMinus { left: Box<Ast>, right: Box<Ast> },
    InfixAsterisk { left: Box<Ast>, right: Box<Ast> },
    InfixSlash { left: Box<Ast>, right: Box<Ast> },
    InfixPercent { left: Box<Ast>, right: Box<Ast> },
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

    pub fn from_program(expressions: Vec<Box<Ast>>) -> Self {
        let location = Self::locate_expressions(&expressions);

        Ast::new(AstKind::Program { expressions }, location)
    }

    pub fn from_num(num: f64, location: Range) -> Self {
        Ast::new(AstKind::Number(num), location)
    }

    pub fn from_prefix_plus(operand: Ast, prefix_location: Range) -> Self {
        let location = Range::new(prefix_location.begin, operand.location.end);
        Ast::new(AstKind::PrefixPlus { operand: Box::new(operand) }, location)
    }

    pub fn from_prefix_minus(operand: Ast, prefix_location: Range) -> Self {
        let location = Range::new(prefix_location.begin, operand.location.end);
        Ast::new(AstKind::PrefixMinus { operand: Box::new(operand) }, location)
    }

    pub fn from_infix_plus(left: Ast, right: Ast) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixPlus { left: Box::new(left), right: Box::new(right) };
        Ast::new(kind, location)
    }

    pub fn from_infix_minus(left: Ast, right: Ast) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixMinus { left: Box::new(left), right: Box::new(right) };
        Ast::new(kind, location)
    }

    pub fn from_infix_asterisk(left: Ast, right: Ast) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixAsterisk { left: Box::new(left), right: Box::new(right) };
        Ast::new(kind, location)
    }

    pub fn from_infix_slash(left: Ast, right: Ast) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixSlash { left: Box::new(left), right: Box::new(right) };
        Ast::new(kind, location)
    }

    pub fn from_infix_percent(left: Ast, right: Ast) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixPercent { left: Box::new(left), right: Box::new(right) };
        Ast::new(kind, location)
    }

    fn locate_expressions(expressions: &Vec<Box<Ast>>) -> Range {
        if expressions.len() == 0 {
            return range::ORIGIN;
        }

        Range {
            begin: expressions[0].location.begin,
            end: expressions[expressions.len() - 1].location.end,
        }
    }
}

/// Makes an AST with the kind, and the location specified by four numbers.
#[macro_export]
macro_rules! mkast {
    (prog loc $br:expr, $bc:expr, $er:expr, $ec: expr, $exprs:expr) => {
        Box::new(Ast::new(
            AstKind::Program { expressions: $exprs },
            Range::from_nums($br as u64, $bc as u64, $er as u64, $ec as u64),
        ))
    };
    (prefix $kind:ident, loc $br:expr, $bc:expr, $er:expr, $ec: expr, operand $oprnd:expr $(,)?) => {
        Box::new(Ast::new(
            AstKind::$kind { operand: $oprnd },
            Range::from_nums($br as u64, $bc as u64, $er as u64, $ec as u64),
        ))
    };
    (infix $kind:ident, loc $br:expr, $bc:expr, $er:expr, $ec: expr, left $left:expr, right $right:expr $(,)?) => {
        Box::new(Ast::new(
            AstKind::$kind { left: $left, right: $right },
            Range::from_nums($br as u64, $bc as u64, $er as u64, $ec as u64),
        ))
    };
    (num $val:expr, loc $br:expr, $bc:expr, $er:expr, $ec: expr) => {
        Box::new(Ast::new(AstKind::Number($val), Range::from_nums($br, $bc, $er, $ec)))
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let range = Range::from_nums(1, 2, 3, 4);
        let ast = Ast::new(AstKind::Number(1.0), range.clone());

        assert_eq!(ast, Ast { kind: AstKind::Number(1.0), location: range })
    }
}
