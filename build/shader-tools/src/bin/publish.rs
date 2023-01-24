use enso_shader_tools::prelude::*;

use ide_ci::prelude::setup_logging;

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let release = enso_shader_tools::release_handle_from_env().await?;
    release.publish().await?;
    Ok(())
}
