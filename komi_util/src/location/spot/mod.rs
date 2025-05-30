//! A single coordinate.

/// A position representing the coordinate of a character in multi-line text.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spot {
    pub row: u32,
    pub col: u32,
}

impl Spot {
    pub const ORIGIN: Spot = Spot::new(0, 0);

    pub const fn new(row: u32, col: u32) -> Self {
        Spot { row, col }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let spot = Spot::new(1, 2);
        let expected = Spot { row: 1, col: 2 };

        assert_eq!(spot, expected)
    }
}
