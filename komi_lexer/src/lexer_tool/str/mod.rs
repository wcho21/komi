use crate::lexer_tool::read_identifier_with_init_seg;
use crate::{LexError, LexErrorKind, SourceScanner, TokenRes};
use komi_syntax::{StrSegment, StrSegmentKind, Token, TokenKind as Kind};
use komi_util::{Range, Scanner, char_validator};

/// Returns a sequence of tokens in a string literal if successfully lexed, or an error otherwise.
///
/// Call this after advancing the scanner past the beginning quote `"`, with its location passed as `first_location`.
/// The scanner stops at the ending quote `"`.
pub fn lex_str(scanner: &mut SourceScanner, first_location: Range) -> TokenRes {
    let mut segments: Vec<StrSegment> = vec![];
    let mut segments_location = first_location;

    // Read each segment into `seg` and push it to `segments`.
    let mut seg = String::new();
    let mut seg_location = scanner.locate();
    loop {
        // Return error if end of source
        let first_char_location = scanner.locate();
        let Some(first_char) = scanner.read_and_advance() else {
            return Err(LexError::new(LexErrorKind::NoClosingQuoteInStr, seg_location));
        };

        // Break if end of string literal
        if first_char == "\"" {
            segments_location.end = first_char_location.end;

            push_segment_str_if_non_empty(&seg, &seg_location, &mut segments);

            break;
        }

        // Expect an escaped right brace "{{" or return error
        if first_char == "}" {
            let second_char_location = scanner.locate();
            seg_location.end = second_char_location.end;
            let Some("}") = scanner.read_and_advance() else {
                return Err(LexError::new(LexErrorKind::IllegalClosingBraceInStr, seg_location));
            };

            seg.push_str(first_char);
            continue;
        }

        // Lex interpolation or an escaped left brace "{{"
        if first_char == "{" {
            let second_char_location = scanner.locate();
            let Some(second_char) = scanner.read_and_advance() else {
                seg_location.end = second_char_location.end;
                return Err(LexError::new(LexErrorKind::NoClosingBraceInInterpolation, seg_location));
            };
            // Push a single left brace "{" if an escaped left brace "{{" encountered
            if second_char == "{" {
                seg_location.end = second_char_location.end;
                seg.push_str(first_char);
                continue;
            }
            if second_char == "}" {
                seg_location.end = second_char_location.end;
                return Err(LexError::new(LexErrorKind::NoIdentifierInInterpolation, seg_location));
            }
            if !char_validator::is_in_identifier_domain(second_char) {
                return Err(LexError::new(
                    LexErrorKind::IllegalInterpolationChar,
                    second_char_location,
                ));
            }

            // Push and reinitialize a segment, if a segment read previously
            push_segment_str_if_non_empty(&seg, &seg_location, &mut segments);

            seg = read_identifier_with_init_seg(scanner, String::from(second_char))?;
            let seg_location_end = scanner.locate().begin; // Current begin is the end of the segment
            seg_location = Range::new(second_char_location.begin, seg_location_end);

            let last_char_location = scanner.locate();
            let Some(last_char) = scanner.read_and_advance() else {
                let location = Range::new(first_char_location.begin, last_char_location.end);
                return Err(LexError::new(LexErrorKind::NoClosingBraceInInterpolation, location));
            };
            if last_char != "}" {
                return Err(LexError::new(
                    LexErrorKind::IllegalInterpolationChar,
                    last_char_location,
                ));
            }

            // Push and reinitialize a segment
            segments.push(StrSegment::new(StrSegmentKind::identifier(seg.clone()), seg_location));
            seg_location.begin = scanner.locate().begin;
            seg = String::new();

            continue;
        }

        // Push the character if not the cases above
        seg.push_str(first_char);
        seg_location.end = first_char_location.end;
    }

    let token = Token::new(Kind::Str(segments), segments_location);
    Ok(token)
}

fn push_segment_str_if_non_empty(segment: &String, segment_location: &Range, segments: &mut Vec<StrSegment>) -> () {
    if segment.len() == 0 {
        return;
    }

    segments.push(StrSegment::new(StrSegmentKind::str(segment), *segment_location));
}

// For more unit tests, see the string lexing tests in the `lib.rs` file of this crate.
#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::*;
    use komi_syntax::mktoken;
    use komi_util::str_loc;
    use rstest::rstest;

    #[rstest]
    #[case::empty(
        "\"\"",
        mktoken!(str_loc!("", "\"\""),
            Kind::Str(vec![]),
        )
    )]
    #[case::str(
        "\"a\"",
        mktoken!(str_loc!("", "\"a\""),
            Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("a"), Range::from_nums(0,1,0,2)),
            ]),
        )
    )]
    #[case::id(
        "\"{a}\"",
        mktoken!(str_loc!("", "\"{a}\""),
            Kind::Str(vec![
                StrSegment::new(StrSegmentKind::identifier("a"), Range::from_nums(0,2,0,3)),
            ]),
        )
    )]
    #[case::lbrace_escape(
        "\"{{\"",
        mktoken!(str_loc!("", "\"{{\""),
            Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("{"), Range::from_nums(0,1,0,3)),
            ]),
        )
    )]
    #[case::lbrace_escape(
        "\"}}\"",
        mktoken!(str_loc!("", "\"}}\""),
            Kind::Str(vec![
                StrSegment::new(StrSegmentKind::str("}"), Range::from_nums(0,1,0,3)),
            ]),
        )
    )]
    fn ok(#[case] source: &str, #[case] expected: Token) {
        let mut scanner = SourceScanner::new(source);
        scanner.advance();

        let token = lex_str(&mut scanner, range());

        assert_eq!(token, Ok(expected));
    }

    mod fixtures {
        use super::*;

        pub fn range() -> Range {
            Range::ORIGIN
        }
    }
}
