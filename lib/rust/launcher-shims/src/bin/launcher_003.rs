// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]

use launcher_shims::wrap_launcher;



// =======================
// === EntryPoint0.0.3 ===
// =======================

/// Runs the launcher wrapper overriding the version to 0.0.3.
fn main() {
    wrap_launcher("0.0.3")
}
