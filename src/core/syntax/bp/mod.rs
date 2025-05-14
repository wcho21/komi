/// Binding powers for an infix token.
pub struct Bp {
    pub left: u8,
    pub right: u8,
}

static LOWEST_BP: Bp = Bp { left: 0o0, right: 0o1 };
static ADDITIVE_BP: Bp = Bp {
    left: 0o30,
    right: 0o31,
};
static MULTIPLICATIVE_BP: Bp = Bp {
    left: 0o40,
    right: 0o41,
};

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
}
