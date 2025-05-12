pub trait Tape {
    type Item;

    fn get_current(&self) -> Option<Self::Item>;
    fn peak_next(&self) -> Option<Self::Item>;
    fn advance(&mut self) -> ();
}
