use crate::core::ExecError;

pub fn fmt_ok(s: &str) -> String {
    format!("{{ \"ok\": \"{s}\" }}")
}

pub fn fmt_err(e: ExecError) -> String {
    format!("{{ \"err\": \"{e}\" }}")
}
