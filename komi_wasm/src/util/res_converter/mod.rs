mod js_structs;

pub use js_structs::{JsExecError, JsExecOut};
use js_structs::{JsExecErrorCause, JsRange, JsSpot};
use komi::{ExecError, ExecOut, ExecRes};
use komi_util::Range;
use komi_util::unpacker::unpack_engine_error;

macro_rules! unpack_err {
    ($name:literal, $exec_err:ident) => {{
        let (kind, location) = unpack_engine_error($exec_err);
        (String::from($name), format!("{}", kind), location)
    }};
}

pub struct JsConverter {}

impl JsConverter {
    pub fn convert(exec_res: ExecRes) -> Result<JsExecOut, JsExecError> {
        match exec_res {
            Ok(exec_out) => Ok(Self::convert_exec_out(exec_out)),
            Err(exec_error) => Err(Self::convert_exec_error(exec_error)),
        }
    }

    fn convert_exec_out(out: ExecOut) -> JsExecOut {
        let value = out.representation;
        let stdout = out.stdout;
        JsExecOut { value, stdout }
    }

    fn convert_exec_error(err: ExecError) -> JsExecError {
        let (name, kind, location) = match &err {
            ExecError::Lex(e) => unpack_err!("LexError", e),
            ExecError::Parse(e) => unpack_err!("ParseError", e),
            ExecError::Eval(e) => unpack_err!("EvalError", e),
        };
        let cause = JsExecErrorCause { location: Self::convert_range(location) };
        JsExecError { name, message: kind, cause }
    }

    fn convert_range(range: &Range) -> JsRange {
        JsRange {
            begin: JsSpot { row: range.begin.row, col: range.begin.col },
            end: JsSpot { row: range.end.row, col: range.end.col },
        }
    }
}
