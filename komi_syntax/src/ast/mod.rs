use komi_util::{Range, range};

/// Kinds of AST produced during parsing.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum AstKind {
    Program { expressions: Vec<Box<Ast>> },
    Number(f64),
    Bool(bool),
    PrefixPlus { operand: Box<Ast> },
    PrefixMinus { operand: Box<Ast> },
    PrefixBang { operand: Box<Ast> },
    InfixPlus { left: Box<Ast>, right: Box<Ast> },
    InfixMinus { left: Box<Ast>, right: Box<Ast> },
    InfixAsterisk { left: Box<Ast>, right: Box<Ast> },
    InfixSlash { left: Box<Ast>, right: Box<Ast> },
    InfixPercent { left: Box<Ast>, right: Box<Ast> },
    InfixConjunct { left: Box<Ast>, right: Box<Ast> },
    InfixDisjunct { left: Box<Ast>, right: Box<Ast> },
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

    // TODO: really need to use `from_*()` functions, instead of `new()`??
    pub fn from_program(expressions: Vec<Box<Ast>>) -> Self {
        let location = Self::locate_expressions(&expressions);

        Ast::new(AstKind::Program { expressions }, location)
    }

    pub fn from_num(num: f64, location: &Range) -> Self {
        Ast::new(AstKind::Number(num), *location)
    }

    pub fn from_bool(boolean: bool, location: &Range) -> Self {
        Ast::new(AstKind::Bool(boolean), *location)
    }

    pub fn from_prefix_plus(operand: Box<Ast>, prefix_location: &Range) -> Self {
        let location = Range::new(prefix_location.begin, operand.location.end);
        Ast::new(AstKind::PrefixPlus { operand }, location)
    }

    pub fn from_prefix_minus(operand: Box<Ast>, prefix_location: &Range) -> Self {
        let location = Range::new(prefix_location.begin, operand.location.end);
        Ast::new(AstKind::PrefixMinus { operand }, location)
    }

    pub fn from_prefix_bang(operand: Box<Ast>, prefix_location: &Range) -> Self {
        let location = Range::new(prefix_location.begin, operand.location.end);
        Ast::new(AstKind::PrefixBang { operand }, location)
    }

    pub fn from_infix_plus(left: Box<Ast>, right: Box<Ast>) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixPlus { left, right };
        Ast::new(kind, location)
    }

    pub fn from_infix_minus(left: Box<Ast>, right: Box<Ast>) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixMinus { left, right };
        Ast::new(kind, location)
    }

    pub fn from_infix_asterisk(left: Box<Ast>, right: Box<Ast>) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixAsterisk { left, right };
        Ast::new(kind, location)
    }

    pub fn from_infix_slash(left: Box<Ast>, right: Box<Ast>) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixSlash { left, right };
        Ast::new(kind, location)
    }

    pub fn from_infix_percent(left: Box<Ast>, right: Box<Ast>) -> Self {
        let location = Range::new(left.clone().location.begin, right.clone().location.end);
        let kind = AstKind::InfixPercent { left, right };
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

/// Makes an AST with the kind and the location specified by four numbers.
/// Helps write an AST declaratively.
#[macro_export]
macro_rules! mkast {
    (prog loc $br:expr, $bc:expr, $er:expr, $ec: expr, $exprs:expr) => {
        Box::new(Ast::new(
            AstKind::Program { expressions: $exprs },
            Range::from_nums($br as u32, $bc as u32, $er as u32, $ec as u32),
        ))
    };
    (prefix $kind:ident, loc $br:expr, $bc:expr, $er:expr, $ec: expr, operand $oprnd:expr $(,)?) => {
        Box::new(Ast::new(
            AstKind::$kind { operand: $oprnd },
            Range::from_nums($br as u32, $bc as u32, $er as u32, $ec as u32),
        ))
    };
    (infix $kind:ident, loc $br:expr, $bc:expr, $er:expr, $ec: expr, left $left:expr, right $right:expr $(,)?) => {
        Box::new(Ast::new(
            AstKind::$kind { left: $left, right: $right },
            Range::from_nums($br as u32, $bc as u32, $er as u32, $ec as u32),
        ))
    };
    (num $val:expr, loc $br:expr, $bc:expr, $er:expr, $ec: expr) => {
        Box::new(Ast::new(AstKind::Number($val), Range::from_nums($br, $bc, $er, $ec)))
    };
    (boolean $val:expr, loc $br:expr, $bc:expr, $er:expr, $ec: expr) => {
        Box::new(Ast::new(AstKind::Bool($val), Range::from_nums($br, $bc, $er, $ec)))
    };
}

/// Test code as a specification.
/// Each test case shows which AST the function returns for a given location.
#[cfg(test)]
mod tests {
    use super::*;

    const RANGE_MOCKS: &[Range] = &[Range::from_nums(1, 2, 3, 4), Range::from_nums(2, 3, 4, 5)];
    const AST_MOCKS: &[Ast] = &[
        Ast { kind: AstKind::Number(1.0), location: RANGE_MOCKS[0] },
        Ast { kind: AstKind::Number(2.0), location: RANGE_MOCKS[1] },
    ];
    const AST_KIND_MOCK: AstKind = AstKind::Number(1.0);

    #[test]
    fn test_new() {
        let ast = Ast::new(AST_KIND_MOCK, RANGE_MOCKS[0]);

        let expected = Ast { kind: AST_KIND_MOCK, location: RANGE_MOCKS[0] };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_program() {
        let expressions = vec![Box::new(AST_MOCKS[0].clone())];

        let ast = Ast::from_program(expressions.clone());

        let expected = Ast {
            kind: AstKind::Program { expressions },
            location: RANGE_MOCKS[0],
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_num() {
        let ast = Ast::from_num(1.0, &RANGE_MOCKS[0]);

        let expected = Ast { kind: AstKind::Number(1.0), location: RANGE_MOCKS[0] };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_bool() {
        let ast = Ast::from_bool(true, &RANGE_MOCKS[0]);

        let expected = Ast { kind: AstKind::Bool(true), location: RANGE_MOCKS[0] };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_prefix_plus() {
        let operand = Box::new(AST_MOCKS[0].clone());

        let ast = Ast::from_prefix_plus(operand.clone(), &RANGE_MOCKS[0]);

        let expected = Ast {
            kind: AstKind::PrefixPlus { operand },
            location: RANGE_MOCKS[0],
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_prefix_minus() {
        let operand = Box::new(AST_MOCKS[0].clone());

        let ast = Ast::from_prefix_minus(operand.clone(), &RANGE_MOCKS[0]);

        let expected = Ast {
            kind: AstKind::PrefixMinus { operand },
            location: RANGE_MOCKS[0],
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_prefix_bang() {
        let operand = Box::new(AST_MOCKS[0].clone());

        let ast = Ast::from_prefix_bang(operand.clone(), &RANGE_MOCKS[0]);

        let expected = Ast {
            kind: AstKind::PrefixBang { operand },
            location: RANGE_MOCKS[0],
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_infix_plus() {
        let left = Box::new(AST_MOCKS[0].clone());
        let right = Box::new(AST_MOCKS[1].clone());

        let ast = Ast::from_infix_plus(left.clone(), right.clone());

        let expected = Ast {
            kind: AstKind::InfixPlus { left: left.clone(), right: right.clone() },
            location: Range { begin: left.location.begin, end: right.location.end },
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_infix_minus() {
        let left = Box::new(AST_MOCKS[0].clone());
        let right = Box::new(AST_MOCKS[1].clone());

        let ast = Ast::from_infix_minus(left.clone(), right.clone());

        let expected = Ast {
            kind: AstKind::InfixMinus { left: left.clone(), right: right.clone() },
            location: Range { begin: left.location.begin, end: right.location.end },
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_infix_asterisk() {
        let left = Box::new(AST_MOCKS[0].clone());
        let right = Box::new(AST_MOCKS[1].clone());

        let ast = Ast::from_infix_asterisk(left.clone(), right.clone());

        let expected = Ast {
            kind: AstKind::InfixAsterisk { left: left.clone(), right: right.clone() },
            location: Range { begin: left.location.begin, end: right.location.end },
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_infix_slash() {
        let left = Box::new(AST_MOCKS[0].clone());
        let right = Box::new(AST_MOCKS[1].clone());

        let ast = Ast::from_infix_slash(left.clone(), right.clone());

        let expected = Ast {
            kind: AstKind::InfixSlash { left: left.clone(), right: right.clone() },
            location: Range { begin: left.location.begin, end: right.location.end },
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_from_infix_percent() {
        let left = Box::new(AST_MOCKS[0].clone());
        let right = Box::new(AST_MOCKS[1].clone());

        let ast = Ast::from_infix_percent(left.clone(), right.clone());

        let expected = Ast {
            kind: AstKind::InfixPercent { left: left.clone(), right: right.clone() },
            location: Range { begin: left.location.begin, end: right.location.end },
        };
        assert_eq!(ast, expected);
    }
}
