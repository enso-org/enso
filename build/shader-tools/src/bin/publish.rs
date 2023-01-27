// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_build_shader_tools::prelude::*;

use ide_ci::prelude::setup_logging;



#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let release = enso_build_shader_tools::release_handle_from_env().await?;
    release.publish().await?;
    Ok(())
}
