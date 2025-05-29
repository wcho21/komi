/// Returns true if the string is a digit char; false otherwise.
pub fn is_digit_char(s: &str) -> bool {
    s.len() == 1 && s.chars().next().unwrap().is_ascii_digit()
}

/// Returns true if the string is a whitespace char; false otherwise.
///
/// Specifically, the whitespace includes:
/// - ASCII whitespace, including newlines.
/// - CRLF (`"\r\n"`).
pub fn is_whitespace_char(s: &str) -> bool {
    (s.len() == 1 && s.chars().next().unwrap().is_ascii_whitespace()) || (s == "\r\n")
}

/// Returns true if the given string `s` is a character within the identifier domain; false otherwise.
///
/// The identifier domain is a set of characters that the lexer recognizes as valid for identifiers.
/// Specifically, it includes:
/// - ASCII alphabets and number characters.
/// - Hangul, from `U+AC00` (`가`) to `U+D7A3` (`힣`)
// TODO: include underbar (`_`)
pub fn is_char_in_identifier_domain(s: &str) -> bool {
    is_char_ascii_alphanumeric(s) || is_char_in_hangul_identifier_domain(s)
}

fn is_char_ascii_alphanumeric(s: &str) -> bool {
    s.len() == 1 && s.chars().next().unwrap().is_ascii_alphanumeric()
}

/// Returns true if the given character is within the Unicode range for Hangul (from `U+AC00` to `U+D7AC`); false otherwise.
fn is_char_in_hangul_identifier_domain(s: &str) -> bool {
    // Note that a hangul character takes 3 bytes in utf-8
    if s.len() != 3 {
        return false;
    }

    let char = s.chars().next().unwrap();
    let codepoint = u32::from(char);
    0xAC00 <= codepoint && codepoint <= 0xD7A3
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::digit("1", true)]
    #[case::two_digits("12", false)]
    #[case::non_digit("x", false)]
    fn test_is_digit(#[case] s: &str, #[case] expected: bool) {
        assert_eq!(is_digit_char(s), expected);
    }

    #[rstest]
    #[case::digit("1", false)]
    #[case::space(" ", true)]
    #[case::two_spaces("  ", false)]
    #[case::cr("\r", true)]
    #[case::lf("\n", true)]
    #[case::crlf("\r\n", true)]
    #[case::crcr("\r\r", false)]
    #[case::lflf("\n\n", false)]
    #[case::crlfcrlf("\r\n\r\n", false)]
    fn test_is_whitespace(#[case] s: &str, #[case] expected: bool) {
        assert_eq!(is_whitespace_char(s), expected);
    }

    #[rstest]
    #[case::num("1", true)]
    #[case::alphabet("a", true)]
    #[case::hangul_ga("가", true)]
    #[case::hangul_na("나", true)]
    #[case::hangul_da("다", true)]
    #[case::hangul_hit("힣", true)]
    #[case::two_nums("12", false)]
    #[case::two_alphabet("ab", false)]
    #[case::two_hanguls("가나", false)]
    #[case::space(" ", false)]
    #[case::cr("\r", false)]
    #[case::lf("\n", false)]
    #[case::crlf("\r\n", false)]
    #[case::below_hangul_boundary(&String::from(char::from_u32(0xABFF).unwrap()), false)]
    #[case::above_hangul_boundary(&String::from(char::from_u32(0xD7A4).unwrap()), false)]
    fn test_is_id_domain(#[case] s: &str, #[case] expected: bool) {
        assert_eq!(is_char_in_identifier_domain(s), expected);
    }

    #[rstest]
    #[case::hangul_ga("가", true)]
    #[case::hangul_na("나", true)]
    #[case::hangul_da("다", true)]
    #[case::hangul_hit("힣", true)]
    #[case::numeric("1", false)]
    #[case::alphabet("a", false)]
    #[case::below_hangul_boundary(&String::from(char::from_u32(0xABFF).unwrap()), false)]
    #[case::above_hangul_boundary(&String::from(char::from_u32(0xD7A4).unwrap()), false)]
    fn test_is_hangul_identifier_domain(#[case] s: &str, #[case] expected: bool) {
        assert_eq!(is_char_in_hangul_identifier_domain(s), expected);
    }
}
