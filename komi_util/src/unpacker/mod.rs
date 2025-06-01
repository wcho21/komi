use crate::error::EngineError;
use crate::location::{Range, Spot};

/// Returns `(spot.row, spot.col)` from the `spot`.
pub fn unpack_spot(spot: &Spot) -> (u32, u32) {
    (spot.row, spot.col)
}

/// Returns `(&err.kind, &err.location)` from the `err`.
pub fn unpack_engine_error<T>(err: &EngineError<T>) -> (&T, &Range) {
    (&err.kind, &err.location)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod unpack_spot {
        use super::*;

        #[test]
        fn test() {
            let spot = Spot::new(1, 2);

            let (row, col) = unpack_spot(&spot);

            assert_eq!(row, 1);
            assert_eq!(col, 2);
        }
    }

    mod unpack_engine_error {
        use super::*;

        #[test]
        fn test() {
            let error_kind = "foo";
            let error = EngineError::new(error_kind, Range::from_nums(1, 2, 3, 4));

            let (kind, location) = unpack_engine_error(&error);

            assert_eq!(*kind, error_kind);
            assert_eq!(*location, Range::from_nums(1, 2, 3, 4));
        }
    }
}
