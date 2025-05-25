use crate::{LexError, SourceScanner, TokenRes};
use komi_syntax::{Token, TokenKind as Kind};
use komi_util::{Range, Scanner, Spot, char_validator};

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
