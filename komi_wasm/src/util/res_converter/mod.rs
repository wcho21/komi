use crate::JsRes;
use crate::util::js_val;
use komi::{ExecError, ExecOut, ExecOutRes};
use komi_util::unpacker::unpack_engine_error;

macro_rules! unpack_err {
    ($name:literal, $exec_err:ident) => {{
        let (kind, location) = unpack_engine_error($exec_err);
        ($name, format!("{}", kind), location)
    }};
}

pub fn convert(exec_out: &ExecOutRes) -> JsRes {
    match exec_out {
        Ok(out) => Ok(convert_out_to_js_val(out)?),
        Err(e) => Err(convert_err_to_js_val(&e)?),
    }
}

fn convert_out_to_js_val(out: &ExecOut) -> JsRes {
    let repr = &out.representation;
    let stdout = &out.stdout;

    js_val::convert_repr_and_stdout_to_js_val(&repr, &stdout)
}

fn convert_err_to_js_val(err: &ExecError) -> JsRes {
    let (name, kind, location) = match err {
        ExecError::Lex(e) => unpack_err!("LexError", e),
        ExecError::Parse(e) => unpack_err!("ParseError", e),
        ExecError::Eval(e) => unpack_err!("EvalError", e),
        _ => todo!(),
    };

    js_val::convert_str_and_location_to_js_val(name, &kind, location)
}
