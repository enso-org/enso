use enso_shader_tools::prelude::*;

use ide_ci::prelude::setup_logging;

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let release = enso_shader_tools::release_handle_from_env().await?;
    let asset_name = format!("shader-tools-{TARGET_OS}-{TARGET_ARCH}.tar.gz");
    let temp_dir = tempfile::tempdir()?;
    let output_archive = temp_dir.path().join(&asset_name);
    enso_shader_tools::create_package(&output_archive).await?;
    release.upload_asset_file(&output_archive).await?;
    Ok(())
}
