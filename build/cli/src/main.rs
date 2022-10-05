// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
#![allow(clippy::bool_to_int_with_if)]

use enso_build::prelude::*;



fn main() -> Result {
    enso_build_cli::lib_main(Default::default())
}
