use super::spot::Spot;

#[derive(Debug, PartialEq, Clone)]
pub struct Range {
    begin: Spot,
    end: Spot,
}

impl Range {
    pub fn new(begin: Spot, end: Spot) -> Self {
        Range { begin, end }
    }
}

pub fn from_spot(spot: &Spot) -> Range {
    Range::new(spot.clone(), spot.clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let spot1 = Spot::new(1, 2);
        let spot2 = Spot::new(3, 4);
        let expected = Range {
            begin: spot1.clone(),
            end: spot2.clone(),
        };

        let range = Range::new(spot1, spot2);

        assert_eq!(range, expected)
    }
}
