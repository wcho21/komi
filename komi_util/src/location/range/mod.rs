use super::spot::Spot;

/// A range representing the span of multiple characters in a multi-line text.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Range {
    pub begin: Spot,
    pub end: Spot,
}

impl Range {
    pub const ORIGIN: Range = Range::new(Spot::ORIGIN, Spot::ORIGIN);

    pub const fn new(begin: Spot, end: Spot) -> Self {
        Range { begin, end }
    }

    pub const fn from_nums(begin_row: u32, begin_col: u32, end_row: u32, end_col: u32) -> Self {
        let begin = Spot::new(begin_row, begin_col);
        let end = Spot::new(end_row, end_col);

        Range::new(begin, end)
    }
}

/// Makes a `Range` value to locate a string.
/// - The first argument is the base string, which is a string from the beginning to the character just before the string to locate.
/// - The second argument is the string to locate.
/// - The third argument is the number of newlines after the base string. Optional, and zero by default.
///
/// # Examples
///
/// ```
/// use komi_util::location::Range;
/// use komi_util::str_loc;
///
/// // Location of `"bar"` in `"foobar"`.
/// assert_eq!(str_loc!("foo", "bar"), Range::from_nums(0, 3, 0, 6));
///
/// // Location of `"\r\nbar"` in `"foo\r\nbar"`.
/// // Since the macro doesn't recognize the newlines, you have to specify the number of the newlines `1`.
/// assert_eq!(str_loc!("foo", "bar", 1), Range::from_nums(0, 3, 1, 3));
/// ```
#[macro_export]
macro_rules! str_loc {
    ($base:literal, $str:literal) => {
        Range::from_nums(
            0,
            $base.chars().count() as u32,
            0,
            ($base.chars().count() + $str.chars().count()) as u32,
        )
    };
    ($base:literal, $str:literal, $newlines:literal) => {
        Range::from_nums(
            0,
            $base.chars().count() as u32,
            $newlines as u32,
            $str.chars().count() as u32,
        )
    };
}

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
