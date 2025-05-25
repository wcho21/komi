use crate::lexer_tool::lex_identifier_with_init_seg_or;
use crate::{SourceScanner, TokenRes};
use komi_syntax::{Token, TokenKind as Kind};
use komi_util::{Range, Scanner};

/// Returns a token of the kind `expected_kind` if a `expected` encountered; otherwise, a token of the kind `alt_kind`.
/// The scanner stops immediately after the expected character or at the unexpected character.
pub fn expect_or(
    scanner: &mut SourceScanner,
    expected: &str,
    expected_kind: Kind,
    alt_kind: Kind,
    first_location: Range,
) -> TokenRes {
    let second_location = scanner.locate();

    match scanner.read() {
        Some(char) if char == expected => {
            scanner.advance();

            let location = Range::new(first_location.begin, second_location.end);
            Ok(Token::new(expected_kind, location))
        }
        _ => Ok(Token::new(alt_kind, first_location)),
    }
}

/// Returns a token with the kind `expected_kind` if the scanner reads the expected characters `expected`; otherwise, returns an identifier token.
pub fn expect_or_lex_identifier(
    scanner: &mut SourceScanner,
    expected: &str,
    expected_kind: Kind,
    first_char: &str,
    first_location: Range,
) -> TokenRes {
    // Stores characters to lex an identifier token if an unexpected character encountered.
    let mut init_seg = String::from(first_char);
    let mut init_seg_location = first_location;

    // Read subsequent characters and match them against the expected characters one by one.
    // Return an identifier token if unexpected character encountered.
    for expected_char in expected.chars().map(|c| String::from(c)) {
        let char = scanner.read();

        if !is_equal_str(char, &expected_char) {
            let token =
                lex_identifier_with_init_seg_or(scanner, char, init_seg.clone(), &init_seg_location.begin, || {
                    // An identifier with characters read so far.
                    Ok(Token::new(Kind::Identifier(init_seg.clone()), init_seg_location))
                })?;
            return Ok(token);
        }

        init_seg.push_str(char.unwrap());
        init_seg_location.end = scanner.locate().end;
        scanner.advance();
    }

    // All expected characters matched; return the token with the expected kind.
    let char = scanner.read();
    let token = lex_identifier_with_init_seg_or(scanner, char, init_seg, &init_seg_location.begin, || {
        Ok(Token::new(expected_kind.clone(), init_seg_location))
    })?;
    return Ok(token);
}

/// Returns true if `source` is `Some` and the value is equal to `target`.
pub fn is_equal_str(source: Option<&str>, target: &str) -> bool {
    source.is_some_and(|c| c == target)
}

// TODO: test
