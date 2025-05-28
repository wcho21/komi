use crate::SourceScanner;
use komi_util::Scanner;

/// Skips characters until newline characters encountered.
///
/// Call this after advance the scanner past the beginning comment character `#`.
/// The scanner stops at the first character immediately after the newline.
pub fn skip_comment(scanner: &mut SourceScanner) -> () {
    while let Some(x) = scanner.read() {
        scanner.advance();

        if let "\n" | "\r" | "\r\n" = x {
            break;
        }
    }
}
