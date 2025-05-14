mod core;
mod util;

pub use core::{EMPTY_REPR, ExecError, ExecResult};

pub fn execute(source: &str) -> ExecResult {
    core::execute(source)
}
