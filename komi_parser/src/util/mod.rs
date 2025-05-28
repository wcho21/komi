use crate::Exprs;
use komi_util::Range;

pub fn locate_expressions(expressions: &Exprs) -> Range {
    if expressions.len() == 0 {
        return Range::ORIGIN;
    }

    Range {
        begin: expressions[0].location.begin,
        end: expressions[expressions.len() - 1].location.end,
    }
}
