use crate::core::ExecError;
use crate::core::ExecResult;

const OK_HEADER: &str = "ok";
const ERR_HEADER: &str = "err";

/// Formats an execution result.
///
/// For a successful result with a `repr` representation, returns `ok repr`.
/// For a failed result with an `error` message, returns `err error`.
pub fn format(res: ExecResult) -> String {
    match res {
        Ok(str) => format_ok(&str),
        Err(err) => format_err(err),
    }
}

pub fn is_ok_format(str: &str) -> bool {
    str.starts_with(&format!("{} ", OK_HEADER))
}

pub fn is_err_format(str: &str) -> bool {
    str.starts_with(&format!("{} ", ERR_HEADER))
}

fn format_ok(payload: &str) -> String {
    format!("{} {}", OK_HEADER, payload.to_string())
}

fn format_err(err: ExecError) -> String {
    format!("{} {}", ERR_HEADER, err.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::err::LexError;
    use crate::util::{Range, Spot};

    const RANGE_MOCK: Range = Range::new(Spot::new(0, 0), Spot::new(0, 1));

    #[test]
    fn test_format_ok() {
        let res = Ok("some result".to_string());

        let formatted = format(res);

        let expected = "ok some result";
        assert_eq!(formatted, expected);
    }

    #[test]
    fn test_format_err() {
        let some_lex_error = LexError::IllegalChar { char: "bad".to_string(), location: RANGE_MOCK };
        let res = Err(ExecError::Lex(some_lex_error));

        let formatted = format(res);

        let expected = "err Reason: LEX_ILLEGAL_CHAR, Cause: 'bad', Location: Range { begin: Spot { row: 0, col: 0 }, end: Spot { row: 0, col: 1 } }";
        assert_eq!(formatted, expected);
    }

    #[test]
    fn test_is_ok_format_true() {
        let str = "ok some result";

        let ok = is_ok_format(str);

        assert_eq!(ok, true);
    }

    #[test]
    fn test_is_ok_format_false_for_bad_header() {
        let str = "bad some result";

        let ok = is_ok_format(str);

        assert_eq!(ok, false);
    }

    #[test]
    fn test_is_ok_format_false_for_empty_result() {
        let str = "ok";

        let ok = is_ok_format(str);

        assert_eq!(ok, false);
    }

    #[test]
    fn test_is_err_format_true() {
        let str = "err some message";

        let err = is_err_format(str);

        assert_eq!(err, true);
    }

    #[test]
    fn test_is_err_format_false_for_bad_header() {
        let str = "bad some message";

        let ok = is_err_format(str);

        assert_eq!(ok, false);
    }

    #[test]
    fn test_is_err_format_false_for_empty_result() {
        let str = "err";

        let ok = is_ok_format(str);

        assert_eq!(ok, false);
    }
}
