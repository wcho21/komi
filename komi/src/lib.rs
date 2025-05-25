mod err;

pub use err::ExecError;
use komi_evaluator::eval;
use komi_lexer::lex;
use komi_parser::parse;
pub use komi_representer::EMPTY_REPR;
use komi_representer::represent;

pub type ExecResult = Result<String, ExecError>;

pub fn execute(source: &str) -> ExecResult {
    let tokens = lex(&source)?;
    let ast = parse(&tokens)?;
    let value = eval(&ast)?;
    let representation = represent(&value);

    Ok(representation)
}

#[cfg(test)]
mod tests {
    use super::{EMPTY_REPR, ExecError, execute};
    use komi_evaluator::{EvalError, EvalErrorKind};
    use komi_lexer::{LexError, LexErrorKind};
    use komi_parser::{ParseError, ParseErrorKind};
    use komi_util::Range;
    use rstest::rstest;

    /// Asserts a given source to be interpreted into the expected result.
    /// Helps write a test declaratively.
    macro_rules! assert_exec {
        ($source:expr, $expected:expr) => {
            assert_eq!(
                execute($source),
                Ok($expected),
                "received a value (left) interpreted from the source '{}', but expected the different result (right)",
                $source,
            );
        };
    }

    /// Asserts executing a given source will fail.
    /// Helps write a test declaratively.
    macro_rules! assert_exec_fail {
        ($source:expr, $expected:expr) => {
            assert_eq!(
                execute($source),
                Err($expected),
                "received a result (left), but expected executing the source '{}' to fail (right)",
                $source,
            );
        };
    }

    #[rstest]
    #[case::empty("", format!("{EMPTY_REPR}"))]
    #[case::whitespaces("  \t\t\r\r\n\n\r\n\r\n", format!("{EMPTY_REPR}"))]
    #[case::comments("# some comment", format!("{EMPTY_REPR}"))]
    fn empty(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::number_without_decimal("12", "12")]
    #[case::number_with_decimal("12.25", "12.25")]
    #[case::bool_true("참", "참")]
    #[case::bool_false("거짓", "거짓")]
    #[case::closure("함수 사과, 오렌지, 바나나 {}", "함수 사과, 오렌지, 바나나 { ... }")]
    fn single_literal(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::number_beginning_with_dot(
        ".12",
        ExecError::Lex(LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))
    )]
    #[case::number_ending_with_dot(
        "12.",
        ExecError::Lex(LexError::new(LexErrorKind::IllegalNumLiteral, Range::from_nums(0, 0, 0, 3)))
    )]
    fn bad_literal(#[case] source: &str, #[case] error: ExecError) {
        assert_exec_fail!(source, error);
    }

    #[rstest]
    #[case::positive_num("+12", "12")]
    #[case::doubly_positive_num("++12", "12")]
    #[case::negative_num("-12", "-12")]
    #[case::doubly_negative_num("--12", "12")]
    fn arithmetic_prefix(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::negation("!참", "거짓")]
    #[case::doubly_negation("!!참", "참")]
    fn boolean_prefix(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::plus(
        "+",
        ExecError::Parse(ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 0, 0, 1)))
    )]
    #[case::minus(
        "-",
        ExecError::Parse(ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 0, 0, 1)))
    )]
    #[case::asterisk(
        "*",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1)))
    )]
    #[case::slash(
        "/",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1)))
    )]
    #[case::percent(
        "%",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 1)))
    )]
    #[case::bang(
        "!",
        ExecError::Parse(ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 0, 0, 1)))
    )]
    #[case::conjunction(
        "그리고",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 3)))
    )]
    #[case::disjunction(
        "또는",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 0, 0, 2)))
    )]
    #[case::dot(
        ".",
        ExecError::Lex(LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))
    )]
    fn single(#[case] source: &str, #[case] error: ExecError) {
        assert_exec_fail!(source, error);
    }

    #[rstest]
    #[case::two_pluses(
        "++",
        ExecError::Parse(ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 1, 0, 2)))
    )]
    #[case::two_minuses(
        "--",
        ExecError::Parse(ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 1, 0, 2)))
    )]
    #[case::two_bangs(
        "!!",
        ExecError::Parse(ParseError::new(ParseErrorKind::NoPrefixOperand, Range::from_nums(0, 1, 0, 2)))
    )]
    #[case::two_dots(
        "..",
        ExecError::Lex(LexError::new(LexErrorKind::IllegalChar, Range::from_nums(0, 0, 0, 1)))
    )]
    fn double(#[case] source: &str, #[case] error: ExecError) {
        assert_exec_fail!(source, error);
    }

    #[rstest]
    #[case::addition("6 + 4", "10")]
    #[case::subtraction("6 - 4", "2")]
    #[case::multiplication("6 * 4", "24")]
    #[case::division("6 / 4", "1.5")]
    #[case::modular("6 % 4", "2")]
    fn arithmetic_infix(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::five_kinds("9 * 8 % 7 - 6 + 5 / 4", "-2.75")]
    #[case::grouping("16 - (8 - (4 - 2))", "10")]
    fn arithmetic_infix_compound(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::two_pluses("6 + + 4", "10")] // Evaluated as `6 + (+4)`.
    #[case::two_minuses("6 - - 4", "10")] // Evaluated as `6 - (-4)`.
    #[case::plus_minus("6 + - 4", "2")] // Evaluated as `6 + (-4)`.
    #[case::minus_plus("6 - + 4", "2")] // Evaluated as `6 - (+4)`.
    #[case::asterisk_plus("6 * + 4", "24")] // Evaluated as `6 * (+4)`.
    #[case::asterisk_minus("6 * - 4", "-24")] // Evaluated as `6 * (-4)`.
    #[case::slash_plus("6 / + 4", "1.5")] // Evaluated as `6 / (+4)`.
    #[case::slash_minus("6 / - 4", "-1.5")] // Evaluated as `6 / (-4)`.
    #[case::percent_plus("6 % + 4", "2")] // Evaluated as `6 % (+4)`.
    #[case::percent_minus("6 % - 4", "2")] // Evaluated as `6 % (-4)`.
    fn legal_two_arithmetic_infixes(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::two_asterisks(
        "6 * * 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::two_slashes(
        "6 / / 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::two_percents(
        "6 % % 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::plus_asterisk(
        "6 + * 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::minus_asterisk(
        "6 - * 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::plus_slash(
        "6 + / 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::minus_slash(
        "6 - / 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::plus_percent(
        "6 + % 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::minus_percent(
        "6 - % 4",
        ExecError::Parse(ParseError::new(ParseErrorKind::InvalidExprStart, Range::from_nums(0, 4, 0, 5)))
    )]
    fn illegal_two_arithmetic_infixes(#[case] source: &str, #[case] error: ExecError) {
        assert_exec_fail!(source, error);
    }

    #[rstest]
    #[case::conjunction("참 그리고 거짓", "거짓")]
    #[case::disjunction("참 또는 거짓", "참")]
    fn connective_infix(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::two_kinds("참 그리고 거짓 또는 참", "참")]
    #[case::grouping("(참 그리고 거짓) 또는 (참 그리고 (참 그리고 참))", "참")]
    fn connective_infix_compound(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::three_kinds("!(참 그리고 거짓)", "참")]
    fn boolean_compount(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::assignment("a=1", "1")]
    #[case::identifier("a=1 a", "1")]
    #[case::as_arithmetic_operand("a=1 a+1", "2")]
    #[case::as_boolean_operand("a=참 !a", "거짓")]
    #[case::as_two_arithmetic_operands("a=1 b=2 a+b", "3")]
    #[case::addition_assignment("a=6 a+=4", "10")]
    #[case::subtraction_assignment("a=6 a-=4", "2")]
    #[case::multiplication_assignment("a=6 a*=4", "24")]
    #[case::division_assignment("a=6 a/=4", "1.5")]
    #[case::modular_assignment("a=6 a%=4", "2")]
    fn assignment_and_identifier(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }

    #[rstest]
    #[case::addition_assignment_to_bool_id(
        "a=참 a+=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::subtraction_assignment_to_bool_id(
        "a=참 a-=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::multiplication_assignment_to_bool_id(
        "a=참 a*=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::division_assignment_to_bool_id(
        "a=참 a/=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::modular_assignment_to_bool_id(
        "a=참 a%=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::addition_assignment_with_bool(
        "a=1 a+=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    #[case::subtraction_assignment_with_bool(
        "a=1 a-=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    #[case::multiplication_assignment_with_bool(
        "a=1 a*=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    #[case::division_assignment_with_bool(
        "a=1 a/=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    #[case::modular_assignment_with_bool(
        "a=1 a%=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::InvalidNumInfixOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    fn assignment_with_wrong_type(#[case] source: &str, #[case] error: ExecError) {
        assert_exec_fail!(source, error);
    }

    #[rstest]
    #[case::two_numbers("1 2", "2")] // Evaluated as the value of the last expression.
    fn multiple_expressions(#[case] source: &str, #[case] expected: String) {
        assert_exec!(source, expected);
    }
}
