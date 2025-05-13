use crate::util::Range;

/// Kinds of AST produced during parsing.
/// Serves as the interface between a parser and its user.
#[derive(Debug, PartialEq)]
pub enum AstKind {
    Number(f64),
}

/// An abstract syntax tree, or AST produced during parsing.
#[derive(Debug, PartialEq)]
pub struct Ast {
    pub kind: AstKind,
    pub location: Range,
}

impl Ast {
    pub fn new(kind: AstKind, location: Range) -> Self {
        Ast { kind, location }
    }

    pub fn from_num(num: f64, location: Range) -> Self {
        Ast::new(AstKind::Number(num), location)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::Spot;

    #[test]
    fn test_new() {
        let range = Range::new(Spot::new(1, 2), Spot::new(3, 4));
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
