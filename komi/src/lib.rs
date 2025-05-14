mod core;

pub use core::{ExecError, ExecResult};
pub use komi_representer::EMPTY_REPR;

pub fn execute(source: &str) -> ExecResult {
    core::execute(source)
}
