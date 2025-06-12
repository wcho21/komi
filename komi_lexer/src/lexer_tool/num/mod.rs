use crate::{LexError, LexErrorKind, SourceScanner, TokenRes};
use komi_syntax::token::{Token, TokenKind as Kind};
use komi_util::char_validator;
use komi_util::location::Range;
use komi_util::scanner::Scanner;

/// Returns a number literal token if successfully lexed, or error otherwise.
///
/// Call this after advancing the scanner past the initial character, with its location passed as `first_location`.
/// The scanner stops at the first non-digit character, after reading a number with or without a decimal part.
// TODO: spilt into sub functions and use match to simplify representation of the logic
pub fn lex_num(
    scanner: &mut SourceScanner,
    pushback_buffer: &mut Vec<(&str, Range)>,
    first_location: Range,
    first_char: &str,
) -> TokenRes {
    let mut lexeme = String::new();
    let begin = first_location.begin;

    // Read the whole number part
    lexeme.push_str(&read_digits(scanner, first_char));
    let whole_part_end = scanner.locate().begin;

    // Return a token if not a dot
    let Some(".") = scanner.read() else {
        let token = lex_num_lexeme(lexeme, Range::new(begin, whole_part_end));

        return Ok(token);
    };

    // Advance past the dot
    let dot_location = scanner.locate();
    scanner.advance();

    // Return an error if end or not a digit, to prohibit invalid literals such as `12.` or `12.+`
    let Some(x) = scanner.read() else {
        let location = Range::new(begin, scanner.locate().end);
        return Err(LexError::new(LexErrorKind::IllegalNumLiteral, location));
    };

    let char_in_id_domain = char_validator::is_char_in_identifier_domain(x);
    let digit_char = char_validator::is_digit_char(x);
    if !digit_char && char_in_id_domain {
        pushback_buffer.push((".", dot_location));
        let token = lex_num_lexeme(lexeme, Range::new(begin, whole_part_end));
        return Ok(token);
    }
    if !digit_char {
        let location = Range::new(begin, scanner.locate().end);
        return Err(LexError::new(LexErrorKind::IllegalNumLiteral, location));
    }
    scanner.advance();

    // Append the previous dot
    lexeme.push_str(".");

    // Read the decimal part
    lexeme.push_str(&read_digits(scanner, x));

    // Parse into a number and return a token
    let end = scanner.locate().begin;
    let token = lex_num_lexeme(lexeme, Range::new(begin, end));

    Ok(token)
}

fn read_digits(scanner: &mut SourceScanner, first_char: &str) -> String {
    let mut digits = first_char.to_string();

    while let Some(x) = scanner.read() {
        if !char_validator::is_digit_char(x) {
            break;
        }

        digits.push_str(x);
        scanner.advance();
    }

    digits
}

fn lex_num_lexeme(lexeme: String, location: Range) -> Token {
    let num = lexeme.parse::<f64>().unwrap();

    Token::new(Kind::Number(num), location)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::*;
    use komi_syntax::mktoken;
    use komi_util::str_loc;
    use rstest::rstest;

    #[rstest]
    #[case::whole_number(
        "123",
        mktoken!(str_loc!("", "123"),
            Kind::Number(123.0),
        )
    )]
    #[case::whole_number_and_non_digit(
        "123+",
        mktoken!(str_loc!("", "123"),
            Kind::Number(123.0),
        )
    )]
    #[case::with_decimal(
        "12.25",
        mktoken!(str_loc!("", "12.25"),
            Kind::Number(12.25),
        )
    )]
    #[case::with_decimal_and_non_digit(
        "12.25+",
        mktoken!(str_loc!("", "12.25"),
            Kind::Number(12.25),
        )
    )]
    #[case::with_decimal(
        "12.사과",
        mktoken!(str_loc!("", "12"),
            Kind::Number(12.0),
        )
    )]
    fn ok(#[case] source: &str, #[case] expected: Token) {
        let mut scanner = SourceScanner::new(source);
        let mut pushback_buffer = vec![];
        let first_char = scanner.read_and_advance().unwrap();

        let token = lex_num(&mut scanner, &mut pushback_buffer, range(), first_char);

        assert_eq!(token, Ok(expected));
    }

    mod fixtures {
        use super::*;

        pub fn range() -> Range {
            Range::from_nums(0, 0, 0, 0)
        }
    }
}
