// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ide_ci::prelude::*;



#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    info!("Enso Formatter running in {}", ide_ci::env::current_dir()?.display());
    enso_formatter::process_path(".", enso_formatter::Action::Format).await
}
