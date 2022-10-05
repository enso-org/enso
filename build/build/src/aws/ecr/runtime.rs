use crate::prelude::*;

use crate::paths::generated;

use ide_ci::programs::docker::BuildOptions;
use ide_ci::programs::docker::ImageId;
use ide_ci::programs::Docker;



/// Name of the repository.
pub const NAME: &str = "runtime";

pub const REGION: &str = "eu-west-1";

#[instrument(fields(%dockerfile, %engine_package_root))]
pub async fn build_runtime_image(
    dockerfile: generated::RepoRootToolsCiDocker,
    engine_package_root: generated::EnginePackage,
    tag: String,
) -> Result<ImageId> {
    ide_ci::fs::copy_to(dockerfile.docker_entrypoint_sh, &engine_package_root.bin)?;
    let mut opts = BuildOptions::new(&engine_package_root);
    opts.file = Some(dockerfile.dockerfile.to_path_buf());
    opts.tags.push(tag);
    let id = Docker.build(opts).await?;
    Ok(id)
}
