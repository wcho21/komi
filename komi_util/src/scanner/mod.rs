use crate::location::Range;

/// Represents a structure that reads units one by one with spanning the location.
pub trait Scanner {
    type Item;

    // Returns the item at the current location.
    fn read(&self) -> Self::Item;

    // Advances past the current item.
    fn advance(&mut self) -> ();

    // Returns the location of the current item.
    fn locate(&self) -> Range;

    // Returns the item at the current location, and advances.
    // Same as calling `read()` and `locate()`.
    fn read_and_advance(&mut self) -> Self::Item {
        let item = self.read();
        self.advance();
        item
    }
}
