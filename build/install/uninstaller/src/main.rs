// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
#![feature(imported_main)]

#[cfg(windows)]
pub mod win;

#[cfg(windows)]
use win::main;

#[cfg(not(windows))]
fn main() {
    panic!("Not supported on non-Windows platforms.")
}
