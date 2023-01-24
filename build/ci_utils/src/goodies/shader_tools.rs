use crate::prelude::*;

use crate::cache::goodie;
use crate::cache::Cache;
use crate::cache::Goodie;
use crate::env::known::PATH;
use crate::github::RepoRef;
use crate::programs::shaderc::Glslc;
use crate::programs::shaderc::SpirvOpt;
use crate::programs::spirv_cross::SpirvCross;

/// Repository where we store releases of the shader tools.
pub const SHADER_TOOLS_REPO: RepoRef = RepoRef { owner: "enso-org", name: "shader-tools" };

pub const VERSION: Version = Version::new(0, 1, 0);

pub fn asset_name(os: OS) -> String {
    // At the moment we don't have non-x64 binaries, so we can hardcode the architecture.
    let arch = Arch::X86_64;
    format!("shader-tools-{os}-{arch}.tar.gz")
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ShaderTools;

impl Goodie for ShaderTools {
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        let url = SHADER_TOOLS_REPO.url().and_then(|url_base| {
            let asset = asset_name(TARGET_OS);
            let suffix = format!("releases/download/{VERSION}/{asset}");
            url_base
                .join(&suffix)
                .with_context(|| "Failed to append suffix {suffix} to URL {url_base}")
        });
        goodie::download_try_url(url, cache)
    }

    fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
        async move {
            try {
                let _ = Glslc.lookup()?;
                let _ = SpirvCross.lookup()?;
                let _ = SpirvOpt.lookup()?;
                true
            }
        }
        .boxed()
    }

    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<crate::env::Modification>> {
        let path = package_path.join_iter(["bin"]);
        let path = crate::env::Modification::prepend_path(&PATH, path);
        Ok(vec![path])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::programs::shaderc::Glslc;

    #[tokio::test]
    #[ignore]
    async fn setup_shaderc() -> Result {
        setup_logging()?;

        assert!(Glslc.lookup().is_err());

        let cache = crate::cache::Cache::new_default().await?;

        ShaderTools.install_if_missing(&cache).await?;

        Glslc.lookup()?;


        Ok(())
    }
}
