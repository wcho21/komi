mod core;

pub use core::{ExecError, ExecResult};
pub use komi_representer::EMPTY_REPR;

/// Get an execution result from a source code.
pub fn execute(source: &str) -> ExecResult {
    core::execute(source)
}
