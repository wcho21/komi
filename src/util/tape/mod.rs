/// A tape-like behaviour that reads items one by one, with the ability to peek at the next item without advancing.
pub trait Tape {
    type Item;

    fn get_current(&self) -> Option<Self::Item>;
    fn peak_next(&self) -> Option<Self::Item>;
    fn advance(&mut self) -> ();
}
