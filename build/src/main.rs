// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use enso_build::prelude::*;



fn main() -> Result {
    enso_build::cli::main::main()
}
