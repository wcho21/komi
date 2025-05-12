use crate::util::Tape;

/// A UTF-8 character tape from a string.
pub struct Utf8Tape<'a> {
    chars: Vec<&'a str>,
    base_index: usize,
}

impl<'a> Utf8Tape<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut chars: Vec<&str> = vec![];
        let mut base: usize = 0;

        for c in source.chars() {
            let size: usize = c.len_utf8();
            let s: &str = &source[base..(base + size)];

            chars.push(s);

            base += size;
        }

        Self {
            chars,
            base_index: 0,
        }
    }
}

impl<'a> Tape for Utf8Tape<'a> {
    type Item = &'a str;

    fn get_current(&self) -> Option<Self::Item> {
        self.chars.get(self.base_index).map(|s| *s)
    }
    fn peek_next(&self) -> Option<Self::Item> {
        self.chars.get(self.base_index + 1).map(|s| *s)
    }
    fn advance(&mut self) -> () {
        if self.base_index == self.chars.len() {
            return ();
        }

        self.base_index += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_current_for_empty() {
        let source = "";

        let tape = Utf8Tape::new(&source);

        assert_eq!(tape.get_current(), None);
    }

    #[test]
    fn test_get_current_twice() {
        let source = "ab";

        let tape = Utf8Tape::new(&source);

        assert_eq!(tape.get_current(), Some("a"));
        assert_eq!(tape.get_current(), Some("a"));
    }

    #[test]
    fn test_peek_next() {
        let source = "ab";

        let tape = Utf8Tape::new(&source);

        assert_eq!(tape.peek_next(), Some("b"));
    }

    #[test]
    fn test_peek_next_twice() {
        let source = "ab";

        let tape = Utf8Tape::new(&source);

        assert_eq!(tape.peek_next(), Some("b"));
        assert_eq!(tape.peek_next(), Some("b"));
    }

    #[test]
    fn test_peek_next_for_empty() {
        let source = "";

        let tape = Utf8Tape::new(&source);

        assert_eq!(tape.peek_next(), None);
    }

    #[test]
    fn test_advance() {
        let source = "ab";

        let mut tape = Utf8Tape::new(&source);

        tape.advance();
        assert_eq!(tape.get_current(), Some("b"));
        assert_eq!(tape.peek_next(), None);
    }

    #[test]
    fn test_advance_for_empty() {
        let source = "";

        let mut tape = Utf8Tape::new(&source);

        tape.advance();
        assert_eq!(tape.get_current(), None);
        assert_eq!(tape.peek_next(), None);
    }
}
