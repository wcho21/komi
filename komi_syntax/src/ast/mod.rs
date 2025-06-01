use komi_util::location::Range;
use komi_util::str_segment::StrSegment;

type Expr = Box<Ast>;
type Exprs = Vec<Expr>;
type Id = String;
type Ids = Vec<Id>;
type StrSegments = Vec<StrSegment>;

/// Kinds of AST produced during parsing.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq, Clone)]
pub enum AstKind {
    Program { expressions: Exprs },
    Number(f64),
    Bool(bool),
    Str(StrSegments),
    Identifier(Id),
    PrefixPlus { operand: Expr },
    PrefixMinus { operand: Expr },
    PrefixBang { operand: Expr },
    InfixPlus { left: Expr, right: Expr },
    InfixMinus { left: Expr, right: Expr },
    InfixAsterisk { left: Expr, right: Expr },
    InfixSlash { left: Expr, right: Expr },
    InfixPercent { left: Expr, right: Expr },
    InfixConjunct { left: Expr, right: Expr },
    InfixDisjunct { left: Expr, right: Expr },
    InfixEquals { left: Expr, right: Expr },
    InfixPlusEquals { left: Expr, right: Expr },
    InfixMinusEquals { left: Expr, right: Expr },
    InfixAsteriskEquals { left: Expr, right: Expr },
    InfixSlashEquals { left: Expr, right: Expr },
    InfixPercentEquals { left: Expr, right: Expr },
    Closure { parameters: Ids, body: Exprs },
    Call { target: Expr, arguments: Exprs },
    Branch { predicate: Expr, consequence: Exprs, alternative: Exprs },
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
    (prog loc $range:expr, $exprs:expr) => {
        Box::new(Ast::new(AstKind::Program { expressions: $exprs }, $range))
    };
    (prefix $kind:ident, loc $br:expr, $bc:expr, $er:expr, $ec: expr, operand $oprnd:expr $(,)?) => {
        Box::new(Ast::new(
            AstKind::$kind { operand: $oprnd },
            Range::from_nums($br as u32, $bc as u32, $er as u32, $ec as u32),
        ))
    };
    (prefix $kind:ident, loc $range:expr, operand $oprnd:expr $(,)?) => {
        Box::new(Ast::new(AstKind::$kind { operand: $oprnd }, $range))
    };
    (infix $kind:ident, loc $br:expr, $bc:expr, $er:expr, $ec: expr, left $left:expr, right $right:expr $(,)?) => {
        Box::new(Ast::new(
            AstKind::$kind { left: $left, right: $right },
            Range::from_nums($br as u32, $bc as u32, $er as u32, $ec as u32),
        ))
    };
    (infix $kind:ident, loc $range:expr, left $left:expr, right $right:expr $(,)?) => {
        Box::new(Ast::new(AstKind::$kind { left: $left, right: $right }, $range))
    };
    (identifier $name:expr, loc $br:expr, $bc:expr, $er:expr, $ec: expr) => {
        Box::new(Ast::new(
            AstKind::Identifier(String::from($name)),
            Range::from_nums($br, $bc, $er, $ec),
        ))
    };
    (identifier $name:expr, loc $range:expr) => {
        Box::new(Ast::new(AstKind::Identifier(String::from($name)), $range))
    };
    (closure loc $br:expr, $bc:expr, $er:expr, $ec: expr, params $param:expr, body $body:expr $(,)?) => {
        Box::new(Ast::new(
            AstKind::Closure { parameters: $param, body: $body },
            Range::from_nums($br, $bc, $er, $ec),
        ))
    };
    (closure loc $range:expr, params $param:expr, body $body:expr $(,)?) => {
        Box::new(Ast::new(AstKind::Closure { parameters: $param, body: $body }, $range))
    };
    (call loc $br:expr, $bc:expr, $er:expr, $ec: expr, target $target:expr, args $args:expr $(,)?) => {
        Box::new(Ast::new(
            AstKind::Call { target: $target, arguments: $args },
            Range::from_nums($br, $bc, $er, $ec),
        ))
    };
    (call loc $range:expr, target $target:expr, args $args:expr $(,)?) => {
        Box::new(Ast::new(AstKind::Call { target: $target, arguments: $args }, $range))
    };
    (num $val:expr, loc $br:expr, $bc:expr, $er:expr, $ec: expr) => {
        Box::new(Ast::new(AstKind::Number($val), Range::from_nums($br, $bc, $er, $ec)))
    };
    (num $val:expr, loc $range:expr) => {
        Box::new(Ast::new(AstKind::Number($val), $range))
    };
    (boolean $val:expr, loc $br:expr, $bc:expr, $er:expr, $ec: expr) => {
        Box::new(Ast::new(AstKind::Bool($val), Range::from_nums($br, $bc, $er, $ec)))
    };
    (boolean $val:expr, loc $range:expr) => {
        Box::new(Ast::new(AstKind::Bool($val), $range))
    };
    (string loc $range:expr, $val:expr $(,)?) => {
        Box::new(Ast::new(AstKind::Str($val), $range))
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::*;

    #[test]
    fn test_new() {
        let ast = Ast::new(ast_kind(), range());

        let expected = Ast { kind: ast_kind(), location: range() };
        assert_eq!(ast, expected);
    }

    mod fixtures {
        use super::*;

        pub fn range() -> Range {
            Range::from_nums(0, 0, 1, 1)
        }

        pub fn ast_kind() -> AstKind {
            AstKind::Number(1.0)
        }
    }
}
