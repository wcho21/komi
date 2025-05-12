use crate::util::Range;

/// Reading units one by one with spanning the location.
pub trait Scanner {
    type Item;

    fn read(&self) -> Option<Self::Item>;
    fn advance(&mut self) -> ();
    fn locate(&self) -> Range;
}
