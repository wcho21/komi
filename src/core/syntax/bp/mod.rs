/// Binding powers for an infix token.
pub struct Bp {
    pub left: u8,
    pub right: u8,
}

static LOWEST_BP: Bp = Bp { left: 0o0, right: 0o1 };
static SUMMATIVE_BP: Bp = Bp {
    left: 0o30,
    right: 0o31,
};

impl Bp {
    pub fn get_summative() -> &'static Bp {
        &SUMMATIVE_BP
    }

    pub fn get_lowest() -> &'static Bp {
        &LOWEST_BP
    }
}
