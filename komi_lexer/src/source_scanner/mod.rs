use super::utf8_tape::Utf8Tape;
use komi_util::{Range, Scanner, Tape};

/// A character scaner from a source.
/// It reads characters one by one, but treats CRLF ("\r\n") as a single character.
pub struct SourceScanner<'a> {
    tape: Utf8Tape<'a>,
    col: u64,
    row: u64,
}

impl<'a> SourceScanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { tape: Utf8Tape::new(source), col: 0, row: 0 }
    }
}

impl<'a> Scanner for SourceScanner<'a> {
    type Item = Option<&'a str>;

    /// Reads next character unit.
    /// Note that if CRLF (`"\r\n"`) encountered, returned as a unit.
    fn read(&self) -> Option<&'a str> {
        match self.tape.get_current() {
            Some("\r") => match self.tape.peek_next() {
                Some("\n") => Some("\r\n"),
                _ => Some("\r"),
            },
            x => x,
        }
    }

    /// Moves the internal pointer to the next unit.
    fn advance(&mut self) -> () {
        match self.read() {
            Some("\r") | Some("\n") => {
                self.row += 1;
                self.col = 0;
                self.tape.advance();
            }
            Some("\r\n") => {
                self.row += 1;
                self.col = 0;
                self.tape.advance();
                self.tape.advance();
            }
            None => {}
            _ => {
                self.col += 1;
                self.tape.advance();
            }
        }
    }

    /// Returns the current location, which is specific by `begin` and `end` as a half-open interval `[begin, end)`.
    /// Note that an empty interval returned if the end of the source.
    fn locate(&self) -> Range {
        match self.read() {
            None => Range::from_nums(self.row, self.col, self.row, self.col),
            _ => Range::from_nums(self.row, self.col, self.row, self.col + 1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read() {
        let source = "ab";
        let scanner = SourceScanner::new(source);

        assert_eq!(scanner.read(), Some("a"));
    }

    #[test]
    fn test_read_twice() {
        let source = "ab";
        let scanner = SourceScanner::new(source);

        assert_eq!(scanner.read(), Some("a"));
        assert_eq!(scanner.read(), Some("a"));
    }

    #[test]
    fn test_read_empty() {
        let source = "";
        let scanner = SourceScanner::new(source);

        assert_eq!(scanner.read(), None);
    }

    #[test]
    fn test_advance() {
        let source = "ab";
        let mut scanner = SourceScanner::new(source);

        scanner.advance();
        assert_eq!(scanner.read(), Some("b"));
    }

    #[test]
    fn test_read_lf() {
        let source = "\n\n";
        let mut scanner = SourceScanner::new(source);

        assert_eq!(scanner.read(), Some("\n"));
        assert_eq!(scanner.read(), Some("\n"));
        scanner.advance();
        assert_eq!(scanner.read(), Some("\n"));
        assert_eq!(scanner.read(), Some("\n"));
        scanner.advance();
        assert_eq!(scanner.read(), None);
    }

    #[test]
    fn test_read_cr() {
        let source = "\r\r";
        let mut scanner = SourceScanner::new(source);

        assert_eq!(scanner.read(), Some("\r"));
        assert_eq!(scanner.read(), Some("\r"));
        scanner.advance();
        assert_eq!(scanner.read(), Some("\r"));
        assert_eq!(scanner.read(), Some("\r"));
        scanner.advance();
        assert_eq!(scanner.read(), None);
    }

    #[test]
    fn test_read_crlf() {
        let source = "\r\n\r\n";
        let mut scanner = SourceScanner::new(source);

        assert_eq!(scanner.read(), Some("\r\n"));
        assert_eq!(scanner.read(), Some("\r\n"));
        scanner.advance();
        assert_eq!(scanner.read(), Some("\r\n"));
        assert_eq!(scanner.read(), Some("\r\n"));
        scanner.advance();
        assert_eq!(scanner.read(), None);
    }

    #[test]
    fn test_locate_col_changes() {
        let source = "ab";
        let mut scanner = SourceScanner::new(source);

        assert_eq!(scanner.read(), Some("a"));
        assert_eq!(scanner.locate(), Range::from_nums(0, 0, 0, 1),);
        scanner.advance();
        assert_eq!(scanner.read(), Some("b"));
        assert_eq!(scanner.locate(), Range::from_nums(0, 1, 0, 2),);
        scanner.advance();
        assert_eq!(scanner.read(), None);
        assert_eq!(scanner.locate(), Range::from_nums(0, 2, 0, 2),);
        scanner.advance();
        assert_eq!(scanner.read(), None);
        assert_eq!(scanner.locate(), Range::from_nums(0, 2, 0, 2),);
    }

    #[test]
    fn test_locate_row_changes() {
        let source = "\r\n\n\r";
        let mut scanner = SourceScanner::new(source);

        assert_eq!(scanner.read(), Some("\r\n"));
        assert_eq!(scanner.locate(), Range::from_nums(0, 0, 0, 1),);
        scanner.advance();
        assert_eq!(scanner.read(), Some("\n"));
        assert_eq!(scanner.locate(), Range::from_nums(1, 0, 1, 1),);
        scanner.advance();
        assert_eq!(scanner.read(), Some("\r"));
        assert_eq!(scanner.locate(), Range::from_nums(2, 0, 2, 1),);
        scanner.advance();
        assert_eq!(scanner.read(), None);
        assert_eq!(scanner.locate(), Range::from_nums(3, 0, 3, 0),);
        scanner.advance();
        assert_eq!(scanner.read(), None);
        assert_eq!(scanner.locate(), Range::from_nums(3, 0, 3, 0),);
    }

    #[test]
    fn test_locate_col_reset() {
        let source = "a\r\nb";
        let mut scanner = SourceScanner::new(source);

        scanner.advance();
        assert_eq!(scanner.read(), Some("\r\n"));
        assert_eq!(scanner.locate(), Range::from_nums(0, 1, 0, 2),);
        scanner.advance();
        assert_eq!(scanner.read(), Some("b"));
        assert_eq!(scanner.locate(), Range::from_nums(1, 0, 1, 1),);
    }
}
