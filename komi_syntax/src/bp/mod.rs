use crate::{Token, TokenKind};

/// Binding powers for an infix token.
pub struct Bp {
    pub left: u8,
    pub right: u8,
}

static LOWEST_BP: Bp = Bp { left: 0o0, right: 0o1 };
// TODO: use the static bp instead of `get_*()` functions, to reduce bundled size
pub const CONNECTIVE_BP: Bp = Bp { left: 0o20, right: 0o21 };
static ADDITIVE_BP: Bp = Bp { left: 0o30, right: 0o31 };
static MULTIPLICATIVE_BP: Bp = Bp { left: 0o40, right: 0o41 };
static PREFIX_BP: Bp = Bp { left: 0o70, right: 0o71 };

impl Bp {
    pub fn get_lowest() -> &'static Bp {
        &LOWEST_BP
    }

    pub fn get_additive() -> &'static Bp {
        &ADDITIVE_BP
    }

    pub fn get_multiplicative() -> &'static Bp {
        &MULTIPLICATIVE_BP
    }

    pub fn get_prefix() -> &'static Bp {
        &PREFIX_BP
    }

    pub fn get_from_token(token: &Token) -> &'static Bp {
        match token.kind {
            TokenKind::Plus | TokenKind::Minus => &ADDITIVE_BP,
            TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => &MULTIPLICATIVE_BP,
            TokenKind::Conjunct | TokenKind::Disjunct => &CONNECTIVE_BP,
            _ => &LOWEST_BP,
        }
    }
}
