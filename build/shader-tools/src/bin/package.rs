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
    let asset_name = format!("shader-tools-{TARGET_OS}-{TARGET_ARCH}.tar.gz");
    let temp_dir = tempfile::tempdir()?;
    let output_archive = temp_dir.path().join(&asset_name);
    enso_build_shader_tools::create_package(&output_archive).await?;
    release.upload_asset_file(&output_archive).await?;
    Ok(())
}
