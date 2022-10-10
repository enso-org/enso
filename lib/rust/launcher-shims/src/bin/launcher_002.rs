// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use launcher_shims::wrap_launcher;



// =======================
// === EntryPoint0.0.2 ===
// =======================

/// Runs the launcher wrapper overriding the version to 0.0.2.
fn main() {
    wrap_launcher("0.0.2")
}
