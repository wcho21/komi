/// Represents a structure that reads items one by one, with the ability to peek at the next item without advancing.
pub trait Tape {
    type Item;

    fn get_current(&self) -> Option<Self::Item>;
    fn peek_next(&self) -> Option<Self::Item>;
    fn advance(&mut self) -> ();
}
