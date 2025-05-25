use crate::{LexError, SourceScanner, TokenRes};
use komi_syntax::{Token, TokenKind as Kind};
use komi_util::{Range, Scanner, Spot, char_validator};

type StringRes = Result<String, LexError>;

/// Returns an identifier token if `char_read` is a valid identifier character; otherwise, returns a token produced by `alt_op`.
///
/// - `char_read`: A character just read by the scanner.
/// - `init_seg`: The initial segment of characters already read by the scanner.
/// - `init_seg_begin`: The beginning spot of the `init_seg`.
/// - `alt_op`: A closure to invoke if an identifier cannot be lexed.
pub fn lex_identifier_with_init_seg_or<F>(
    scanner: &mut SourceScanner,
    char_read: Option<&str>,
    init_seg: String,
    init_seg_begin: &Spot,
    alt_op: F,
) -> TokenRes
where
    F: Fn() -> TokenRes,
{
    match char_read {
        Some(c) if char_validator::is_in_identifier_domain(c) => {
            // Pass what the scanner just read to the identifier-lexing function below.
            let init_seg = init_seg.to_owned() + c;
            let char_end = scanner.locate().end;
            scanner.advance();

            let token = lex_identifier_with_init_seg(scanner, init_seg, Range::new(*init_seg_begin, char_end))?;
            Ok(token)
        }
        _ => alt_op(),
    }
}

/// Returns an identifier token with the initial segment `init_seg` and subsequent characters the scanner read.
/// The scanner stops at the first non-identifier character.
pub fn lex_identifier_with_init_seg(
    scanner: &mut SourceScanner,
    init_seg: String,
    init_seg_location: Range,
) -> TokenRes {
    let identifier = read_identifier_with_init_seg(scanner, init_seg.clone())?;
    let identifier_location = Range::new(init_seg_location.begin, scanner.locate().begin);

    Ok(Token::new(Kind::Identifier(identifier), identifier_location))
}

/// Returns a string of identifier characters with the initial segment `init_seg` and subsequent characters the scanner read.
/// The scanner stops at the first non-identifier character.
pub fn read_identifier_with_init_seg(scanner: &mut SourceScanner, init_seg: String) -> StringRes {
    let mut identifier = init_seg;

    // Read identifier characters one by one
    while let Some(char) = scanner.read() {
        if !char_validator::is_in_identifier_domain(char) {
            break;
        }

        identifier.push_str(char);

        scanner.advance();
    }

    Ok(identifier)
}

// TODO: test
