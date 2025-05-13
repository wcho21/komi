mod v1;

use crate::core::ExecResult;
use const_format::formatcp;

const MAGIC: &str = "komi";
const VER1: &str = "v1";
const HEADER_VER1: &str = formatcp!("{MAGIC} {VER1}");

/// Formats an execution result.
/// The formatted string always begins with the header `komi v1`.
///
/// For a successful result with a `repr` representation, returns `komi v1 ok repr`.
/// For a failed result with an `error` message, returns `komi v1 err error`.
pub fn format(res: ExecResult) -> String {
    let payload = v1::format(res);
    format!("{MAGIC} {VER1} {payload}")
}

pub fn is_ok_format(str: &str) -> bool {
    has_valid_header(str) && has_valid_ok_payload(str)
}

pub fn is_err_format(str: &str) -> bool {
    has_valid_header(str) && has_valid_err_payload(str)
}

fn has_valid_header(str: &str) -> bool {
    str.starts_with(HEADER_VER1)
}

fn has_valid_ok_payload(str: &str) -> bool {
    let base_index = HEADER_VER1.len() + 1;
    str.len() >= base_index && v1::is_ok_format(&str[base_index..])
}

fn has_valid_err_payload(str: &str) -> bool {
    let base_index = HEADER_VER1.len() + 1;
    str.len() >= base_index && v1::is_err_format(&str[base_index..])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::ExecError;
    use crate::core::err::LexError;
    use crate::util::{Range, Spot};

    const RANGE_MOCK: Range = Range::new(Spot::new(0, 0), Spot::new(0, 1));

    #[test]
    fn test_format_ok() {
        let res = Ok("some ok result".to_string());

        let formatted = format(res);

        let expected = "komi v1 ok some ok result";
        assert_eq!(formatted, expected);
    }

    #[test]
    fn test_format_err() {
        let some_lex_error = LexError::IllegalChar("bad".to_string(), RANGE_MOCK);
        let res = Err(ExecError::Lex(some_lex_error));

        let formatted = format(res);

        let expected = "komi v1 err Reason: LEX_ILLEGAL_CHAR, Cause: 'bad', Location: Range { begin: Spot { row: 0, col: 0 }, end: Spot { row: 0, col: 1 } }";
        assert_eq!(formatted, expected);
    }

    #[test]
    fn test_is_ok_format_true() {
        let str = "komi v1 ok some result";

        let ok = is_ok_format(str);

        assert_eq!(ok, true);
    }

    #[test]
    fn test_is_ok_format_false_for_bad_magic() {
        let str = "bad v1 ok some result";

        let ok = is_ok_format(str);

        assert_eq!(ok, false);
    }

    #[test]
    fn test_is_ok_format_false_for_bad_ver() {
        let str = "komi bad ok some result";

        let ok = is_ok_format(str);

        assert_eq!(ok, false);
    }

    #[test]
    fn test_is_ok_format_false_for_empty() {
        let str = "";

        let ok = is_ok_format(str);

        assert_eq!(ok, false);
    }

    #[test]
    fn test_is_ok_format_false_for_empty_payload() {
        let str = "komi v1";

        let ok = is_ok_format(str);

        assert_eq!(ok, false);
    }

    #[test]
    fn test_is_err_format_true() {
        let str = "komi v1 err some result";

        let err = is_err_format(str);

        assert_eq!(err, true);
    }

    #[test]
    fn test_is_err_format_false_for_bad_magic() {
        let str = "bad v1 err some result";

        let err = is_err_format(str);

        assert_eq!(err, false);
    }

    #[test]
    fn test_is_err_format_false_for_bad_ver() {
        let str = "komi bad err some result";

        let err = is_err_format(str);

        assert_eq!(err, false);
    }

    #[test]
    fn test_is_err_format_false_for_payload() {
        let str = "";

        let err = is_err_format(str);

        assert_eq!(err, false);
    }

    #[test]
    fn test_is_err_format_false_for_empty_payload() {
        let str = "komi v1";

        let err = is_err_format(str);

        assert_eq!(err, false);
    }
}
