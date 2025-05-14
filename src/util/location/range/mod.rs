use super::spot;
use super::spot::Spot;

/// A range representing the span of multiple characters in a multi-line text.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Range {
    pub begin: Spot,
    pub end: Spot,
}

impl Range {
    pub const fn new(begin: Spot, end: Spot) -> Self {
        Range { begin, end }
    }

    pub const fn from_nums(begin_row: u64, begin_col: u64, end_row: u64, end_col: u64) -> Self {
        let begin = Spot::new(begin_row, begin_col);
        let end = Spot::new(end_row, end_col);

        Range::new(begin, end)
    }
}

pub const ORIGIN: Range = Range::new(spot::ORIGIN, spot::ORIGIN);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let spot1 = Spot::new(1, 2);
        let spot2 = Spot::new(3, 4);
        let expected = Range { begin: spot1.clone(), end: spot2.clone() };

        let range = Range::new(spot1, spot2);

        assert_eq!(range, expected)
    }
}
