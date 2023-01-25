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
    let handle = enso_build_shader_tools::repo_handle_from_env().await?;
    let release = enso_build_shader_tools::create_release(handle, "0.1.0").await?;
    ide_ci::actions::workflow::set_output(
        enso_build_shader_tools::ENSO_RELEASE_ID.as_str(),
        &release.id,
    )
    .await?;
    Ok(())
}
