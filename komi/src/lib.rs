use komi_evaluator::Evaluator;
use komi_lexer::lex;
use komi_parser::parse;
use komi_syntax::error::ExecError;

pub type ExecRes = Result<ExecOut, ExecError>;

#[derive(Debug)]
pub struct ExecOut {
    pub representation: String,
    pub stdout: String,
}

impl ExecOut {
    pub fn new(representation: String, stdout: String) -> Self {
        Self { representation, stdout }
    }
}

pub fn execute(source: &str) -> ExecRes {
    let tokens = lex(&source)?;
    let ast = parse(&tokens)?;

    let mut evaluator = Evaluator::new(&ast);
    let value = evaluator.eval()?;
    let representation = value.represent();
    let stdout = evaluator.flush();

    let exec_out = ExecOut::new(representation, stdout);
    Ok(exec_out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use komi_syntax::error::{EvalError, EvalErrorKind, LexError, LexErrorKind, ParseError, ParseErrorKind};
    use komi_util::location::Range;
    use rstest::rstest;

    /// Asserts a given source to be interpreted into the expected result with stdout.
    /// Helps write a test declaratively.
    macro_rules! assert_out {
        ($source:expr, $expected_repr:expr, $expected_stdout:expr) => {{
            let out = execute($source).unwrap();
            let repr = out.representation;
            let stdout = out.stdout;

            assert_eq!($expected_repr, repr, "expected a value (left), but it isn't (right)",);
            assert_eq!(
                $expected_stdout, stdout,
                "expected a stdout (left), but it isn't (right)",
            );
        }};
    }

    /// Asserts executing a given source will fail.
    /// Helps write a test declaratively.
    macro_rules! assert_fail {
        ($source:expr, $expected:expr) => {{
            let err = execute($source).unwrap_err();
            assert_eq!(err, $expected, "received a result (left), but it isn't (right)",);
        }};
    }

    #[rstest]
    #[case::empty(
        "",
        ExecError::Lex(LexError::new(LexErrorKind::NoSource, Range::from_nums(0, 0, 0, 0)))
    )]
    #[case::whitespaces(
        "  \t\t\n\n\r\r\r\n\r\n",
        ExecError::Lex(LexError::new(LexErrorKind::NoSource, Range::from_nums(0, 0, 6, 0)))
    )]
    #[case::comments(
        "# some comment",
        ExecError::Lex(LexError::new(LexErrorKind::NoSource, Range::from_nums(0, 0, 0, 14)))
    )]
    fn empty(#[case] source: &str, #[case] error: ExecError) {
        assert_fail!(source, error);
    }

    #[rstest]
    #[case::number_without_decimal("12", "12")]
    #[case::number_with_decimal("12.25", "12.25")]
    #[case::bool_true("참", "참")]
    #[case::bool_false("거짓", "거짓")]
    #[case::str_without_interploation("\"사과 오렌지\"", "사과 오렌지")]
    #[case::closure("함수 사과, 오렌지, 바나나 { 1 }", "함수 사과, 오렌지, 바나나 { ... }")]
    fn single_literal(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
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
        assert_fail!(source, error);
    }

    #[rstest]
    #[case::positive_num("+12", "12")]
    #[case::doubly_positive_num("++12", "12")]
    #[case::negative_num("-12", "-12")]
    #[case::doubly_negative_num("--12", "12")]
    fn arithmetic_prefix(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
    }

    #[rstest]
    #[case::negation("!참", "거짓")]
    #[case::doubly_negation("!!참", "참")]
    fn boolean_prefix(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
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
        assert_fail!(source, error);
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
        assert_fail!(source, error);
    }

    #[rstest]
    #[case::addition("6 + 4", "10")]
    #[case::subtraction("6 - 4", "2")]
    #[case::multiplication("6 * 4", "24")]
    #[case::division("6 / 4", "1.5")]
    #[case::modular("6 % 4", "2")]
    fn arithmetic_infix(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
    }

    #[rstest]
    #[case::five_kinds("9 * 8 % 7 - 6 + 5 / 4", "-2.75")]
    #[case::grouping("16 - (8 - (4 - 2))", "10")]
    fn arithmetic_infix_compound(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
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
        assert_out!(source, expected, "");
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
        assert_fail!(source, error);
    }

    #[rstest]
    #[case::conjunction("참 그리고 거짓", "거짓")]
    #[case::disjunction("참 또는 거짓", "참")]
    fn connective_infix(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
    }

    #[rstest]
    #[case::two_kinds("참 그리고 거짓 또는 참", "참")]
    #[case::grouping("(참 그리고 거짓) 또는 (참 그리고 (참 그리고 참))", "참")]
    fn connective_infix_compound(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
    }

    #[rstest]
    #[case::three_kinds("!(참 그리고 거짓)", "참")]
    fn boolean_compount(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
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
        assert_out!(source, expected, "");
    }

    #[rstest]
    #[case::addition_assignment_to_bool_id(
        "a=참 a+=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumOrStrInfixLeftOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::subtraction_assignment_to_bool_id(
        "a=참 a-=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumInfixLeftOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::multiplication_assignment_to_bool_id(
        "a=참 a*=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumOrStrInfixLeftOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::division_assignment_to_bool_id(
        "a=참 a/=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumInfixLeftOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::modular_assignment_to_bool_id(
        "a=참 a%=1",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumInfixLeftOperand, Range::from_nums(0, 4, 0, 5)))
    )]
    #[case::addition_assignment_with_bool(
        "a=1 a+=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumInfixRightOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    #[case::subtraction_assignment_with_bool(
        "a=1 a-=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumInfixRightOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    #[case::multiplication_assignment_with_bool(
        "a=1 a*=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumInfixRightOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    #[case::division_assignment_with_bool(
        "a=1 a/=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumInfixRightOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    #[case::modular_assignment_with_bool(
        "a=1 a%=참",
        ExecError::Eval(EvalError::new(EvalErrorKind::NonNumInfixRightOperand, Range::from_nums(0, 7, 0, 8)))
    )]
    fn assignment_with_wrong_type(#[case] source: &str, #[case] error: ExecError) {
        assert_fail!(source, error);
    }

    #[rstest]
    #[case::curring_call("사과 = 42 오렌지 = 참 \"{사과}{오렌지}\"", "42참")]
    fn string_interpolation(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
    }

    // TODO: test closure captures

    #[rstest]
    #[case::immediate_closure_call("함수{1}()", "1")]
    #[case::immediate_closure_call_call("함수{함수{1}}()()", "1")]
    #[case::id_call("사과 = 함수{1} 사과()", "1")]
    #[case::curring_call("사과 = 함수 오렌지{함수 바나나{오렌지+바나나}} 사과(1)(2)", "3")]
    fn calls(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
    }

    #[rstest]
    #[case::two_way("만약 참 { 1 } 아니면 { 2 }", "1")]
    #[case::three_way("만약 거짓 { 1 } 아니면 만약 거짓 { 2 } 아니면 { 3 }", "3")]
    #[case::four_way("만약 거짓 { 1 } 아니면 만약 거짓 { 2 } 아니면 만약 거짓 { 3 } 아니면 { 4 }", "4")]
    fn branch(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
    }

    #[rstest]
    #[case::two_numbers("1 2", "2")] // Evaluated as the value of the last expression.
    fn multiple_expressions(#[case] source: &str, #[case] expected: String) {
        assert_out!(source, expected, "");
    }

    #[rstest]
    #[case::num_without_decimal("쓰기(1)", "1", "1")]
    #[case::num_without_decimal("쓰기(12.25)", "5", "12.25")]
    #[case::str_ascii("쓰기(\"foo\")", "3", "foo")]
    #[case::str_hangul("쓰기(\"사과\")", "2", "사과")]
    fn stdout(#[case] source: &str, #[case] expected_repr: String, #[case] expected_stdout: String) {
        assert_out!(source, expected_repr, expected_stdout);
    }
}
