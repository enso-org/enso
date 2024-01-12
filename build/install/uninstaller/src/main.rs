// === Features ===
#![feature(imported_main)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

#[cfg(windows)]
use win::main;


// ==============
// === Export ===
// ==============

#[cfg(windows)]
pub mod win;



#[cfg(not(windows))]
fn main() {
    panic!("Not supported on non-Windows platforms.")
}


#[cfg(test)]
mod tests {
    #[test]
    fn uninstaller_name_matches() {
        assert_eq!(enso_install_config::UNINSTALLER_NAME, env!("CARGO_PKG_NAME"));
    }
}
