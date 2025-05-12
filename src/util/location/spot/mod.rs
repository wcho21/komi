/// A position representing the coordinate of a character in multi-line text.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spot {
    row: u64,
    col: u64,
}

impl Spot {
    pub const fn new(row: u64, col: u64) -> Self {
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
