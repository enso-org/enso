//! Generates C headers.

// === Features ===
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use std::env;
use std::fs;
use std::path::Path;



// ==============================
// === Generate C Header File ===
// ==============================

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    // We are not "supposed to" generate anything outside $OUT_DIR, but no one would be able to find
    // it in $OUT_DIR. This is a known issue: https://github.com/rust-lang/cargo/issues/5457
    let dest_dir = Path::new(&crate_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("target")
        .join("enso_parser_headers");
    let config_path = Path::new(&crate_dir).join("cbindgen.toml");
    let _ = fs::create_dir(&dest_dir);
    let dest_path = dest_dir.join("enso_parser.h");
    let _ = fs::remove_file(&dest_path);
    let wrote_header = cbindgen::Builder::new()
        .with_config(cbindgen::Config::from_file(&config_path).unwrap())
        .with_crate(crate_dir)
        .generate()
        .expect("Failed to generate bindings")
        .write_to_file(dest_path);
    assert!(wrote_header);
}
