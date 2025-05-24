use komi_util::Range;

/// Kinds of AST produced during parsing.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum AstKind {
    Program { expressions: Vec<Box<Ast>> },
    Number(f64),
    Bool(bool),
    Identifier(String),
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
    InfixEquals { left: Box<Ast>, right: Box<Ast> },
    InfixPlusEquals { left: Box<Ast>, right: Box<Ast> },
    InfixMinusEquals { left: Box<Ast>, right: Box<Ast> },
    InfixAsteriskEquals { left: Box<Ast>, right: Box<Ast> },
    InfixSlashEquals { left: Box<Ast>, right: Box<Ast> },
    InfixPercentEquals { left: Box<Ast>, right: Box<Ast> },
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
    (identifier $name:expr, loc $br:expr, $bc:expr, $er:expr, $ec: expr) => {
        Box::new(Ast::new(
            AstKind::Identifier(String::from($name)),
            Range::from_nums($br, $bc, $er, $ec),
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
    use fixtures::*;

    #[test]
    fn test_new() {
        let ast = Ast::new(AST_KIND_MOCK, RANGE_MOCK);

        let expected = Ast { kind: AST_KIND_MOCK, location: RANGE_MOCK };
        assert_eq!(ast, expected);
    }

    mod fixtures {
        use super::*;

        pub const RANGE_MOCK: Range = Range::from_nums(1, 2, 3, 4);
        pub const AST_KIND_MOCK: AstKind = AstKind::Number(1.0);
    }
}
