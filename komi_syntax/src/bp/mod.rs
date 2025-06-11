use crate::token::{Token, TokenKind};

/// Binding powers for an infix token.
pub struct Bp {
    pub left: u8,
    pub right: u8,
}

impl Bp {
    // Note that a token will be parsed into a right associative infix if the `left` is greater than the `right`, and vice versa.
    pub const LOWEST: Self = Self { left: 0o0, right: 0o1 };
    pub const ASSIGNMENT: Self = Self { left: 0o11, right: 0o10 };
    pub const CONNECTIVE: Self = Self { left: 0o20, right: 0o21 };
    pub const COMPARISON: Self = Self { left: 0o30, right: 0o31 };
    pub const ADDITIVE: Self = Self { left: 0o40, right: 0o41 };
    pub const MULTIPLICATIVE: Self = Self { left: 0o50, right: 0o51 };
    pub const PREFIX: Self = Self { left: 0o60, right: 0o61 };
    pub const CALL: Self = Self { left: 0o70, right: 0o71 };
    pub const BIND: Self = Self { left: 0o72, right: 0o73 };

    pub fn get_from_token(token: &Token) -> &Self {
        match token.kind {
            TokenKind::Plus | TokenKind::Minus => &Self::ADDITIVE,
            TokenKind::Equals
            | TokenKind::PlusEquals
            | TokenKind::MinusEquals
            | TokenKind::AsteriskEquals
            | TokenKind::SlashEquals
            | TokenKind::PercentEquals => &Self::ASSIGNMENT,
            TokenKind::LBracket
            | TokenKind::RBracket
            | TokenKind::LBracketEquals
            | TokenKind::RBracketEquals
            | TokenKind::DoubleEquals
            | TokenKind::BangEquals => &Self::COMPARISON,
            TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => &Self::MULTIPLICATIVE,
            TokenKind::Conjunct | TokenKind::Disjunct => &Self::CONNECTIVE,
            TokenKind::LParen => &Self::CALL,
            TokenKind::Dot => &Self::BIND,
            _ => &Self::LOWEST,
        }
    }
}
