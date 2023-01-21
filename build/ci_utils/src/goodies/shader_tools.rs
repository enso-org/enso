use crate::prelude::*;

use crate::cache::goodie;
use crate::cache::Cache;
use crate::cache::Goodie;
use crate::env::known::PATH;
use crate::programs::shaderc::Glslc;
use crate::programs::shaderc::SpirvOpt;
use crate::programs::spirv_cross::SpirvCross;

#[derive(Clone, Copy, Debug, Default)]
pub struct ShaderTools;

impl Goodie for ShaderTools {
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        let url = format!("https://github.com/enso-org/shader-tools/releases/download/0.1.0/shader-tools-{TARGET_OS}-x86_64.tar.gz");
        let url = Url::from_str(&url);
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

    // #[tokio::test]
    // #[ignore]
    // async fn download() -> Result {
    //     let temp_dir = tempfile::tempdir()?;
    //
    //     setup_logging()?;
    //     let url = download_urls()?;
    //     crate::io::download_to_dir(url, &temp_dir).await?;
    //
    //     Ok(())
    // }
}
