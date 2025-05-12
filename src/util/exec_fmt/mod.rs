use crate::core::ExecErr;

pub fn fmt_ok(s: &str) -> String {
    format!("{{ \"ok\": \"{s}\" }}")
}

pub fn fmt_err(e: ExecErr) -> String {
    format!("{{ \"err\": \"{e}\" }}")
}
