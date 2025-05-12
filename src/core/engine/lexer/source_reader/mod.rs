use super::utf8_tape::Utf8Tape;
use crate::util::{Spot, Tape};

/// A character reader from a source.
/// It reads characters one by one, but treats CRLF ("\r\n") as a single character.
pub struct SourceReader<'a> {
    tape: Utf8Tape<'a>,
    col: u64,
    row: u64,
}

impl<'a> SourceReader<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            tape: Utf8Tape::new(source),
            col: 0,
            row: 0,
        }
    }

    pub fn read(&self) -> Option<&'a str> {
        match self.tape.get_current() {
            Some("\r") => match self.tape.peek_next() {
                Some("\n") => Some("\r\n"),
                _ => Some("\r"),
            },
            x => x,
        }
    }

    pub fn advance(&mut self) -> () {
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

    pub fn spot(&self) -> Spot {
        Spot::new(self.row, self.col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read() {
        let source = "ab";
        let reader = SourceReader::new(source);

        assert_eq!(reader.read(), Some("a"));
    }

    #[test]
    fn test_read_twice() {
        let source = "ab";
        let reader = SourceReader::new(source);

        assert_eq!(reader.read(), Some("a"));
        assert_eq!(reader.read(), Some("a"));
    }

    #[test]
    fn test_read_empty() {
        let source = "";
        let reader = SourceReader::new(source);

        assert_eq!(reader.read(), None);
    }

    #[test]
    fn test_advance() {
        let source = "ab";
        let mut reader = SourceReader::new(source);

        reader.advance();
        assert_eq!(reader.read(), Some("b"));
    }

    #[test]
    fn test_read_lf() {
        let source = "\n\n";
        let mut reader = SourceReader::new(source);

        assert_eq!(reader.read(), Some("\n"));
        assert_eq!(reader.read(), Some("\n"));
        reader.advance();
        assert_eq!(reader.read(), Some("\n"));
        assert_eq!(reader.read(), Some("\n"));
        reader.advance();
        assert_eq!(reader.read(), None);
    }

    #[test]
    fn test_read_cr() {
        let source = "\r\r";
        let mut reader = SourceReader::new(source);

        assert_eq!(reader.read(), Some("\r"));
        assert_eq!(reader.read(), Some("\r"));
        reader.advance();
        assert_eq!(reader.read(), Some("\r"));
        assert_eq!(reader.read(), Some("\r"));
        reader.advance();
        assert_eq!(reader.read(), None);
    }

    #[test]
    fn test_read_crlf() {
        let source = "\r\n\r\n";
        let mut reader = SourceReader::new(source);

        assert_eq!(reader.read(), Some("\r\n"));
        assert_eq!(reader.read(), Some("\r\n"));
        reader.advance();
        assert_eq!(reader.read(), Some("\r\n"));
        assert_eq!(reader.read(), Some("\r\n"));
        reader.advance();
        assert_eq!(reader.read(), None);
    }

    #[test]
    fn test_spot_col() {
        let source = "ab";
        let mut reader = SourceReader::new(source);

        assert_eq!(reader.read(), Some("a"));
        assert_eq!(reader.spot(), Spot::new(0, 0));
        reader.advance();
        assert_eq!(reader.read(), Some("b"));
        assert_eq!(reader.spot(), Spot::new(0, 1));
        reader.advance();
        assert_eq!(reader.read(), None);
        assert_eq!(reader.spot(), Spot::new(0, 2));
        reader.advance();
        assert_eq!(reader.read(), None);
        assert_eq!(reader.spot(), Spot::new(0, 2));
    }

    #[test]
    fn test_spot_row() {
        let source = "\r\n\n\r";
        let mut reader = SourceReader::new(source);

        assert_eq!(reader.read(), Some("\r\n"));
        assert_eq!(reader.spot(), Spot::new(0, 0));
        reader.advance();
        assert_eq!(reader.read(), Some("\n"));
        assert_eq!(reader.spot(), Spot::new(1, 0));
        reader.advance();
        assert_eq!(reader.read(), Some("\r"));
        assert_eq!(reader.spot(), Spot::new(2, 0));
        reader.advance();
        assert_eq!(reader.read(), None);
        assert_eq!(reader.spot(), Spot::new(3, 0));
        reader.advance();
        assert_eq!(reader.read(), None);
        assert_eq!(reader.spot(), Spot::new(3, 0));
    }

    #[test]
    fn test_spot_col_reset() {
        let source = "a\r\nb";
        let mut reader = SourceReader::new(source);

        reader.advance();
        assert_eq!(reader.read(), Some("\r\n"));
        assert_eq!(reader.spot(), Spot::new(0, 1));
        reader.advance();
        assert_eq!(reader.read(), Some("b"));
        assert_eq!(reader.spot(), Spot::new(1, 0));
    }
}
