use crate::prelude::*;
use ide_ci::cache::Goodie;

pub fn download_urls() -> Result<Url> {
    match TARGET_OS {
        OS::Linux =>
            "https://storage.googleapis.com/shaderc/badges/build_link_linux_gcc_release.html",
        OS::MacOS =>
            "https://storage.googleapis.com/shaderc/badges/build_link_macos_clang_release.html",
        OS::Windows =>
            // "https://storage.googleapis.com/shaderc/badges/build_link_windows_vs2017_release.html",
        "https://storage.googleapis.com/shaderc/artifacts/prod/graphics_shader_compiler/shaderc/windows/continuous_release_2017/402/20230111-132259/install.zip",
        _ => bail!("Unsupported OS: {}.", TARGET_OS),
    }
    .try_into()
    .anyhow_err()
}

pub mod goodie {
    use super::*;
    use ide_ci::env::known::PATH;
    use ide_ci::ready_boxed;

    #[derive(Clone, Copy, Debug, Default)]
    pub struct Shaderc;

    impl Goodie for Shaderc {
        fn url(&self) -> BoxFuture<'static, Result<Url>> {
            ready_boxed(download_urls())
        }

        fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
            async move { Ok(crate::shaderc::programs::glslc::Glslc.lookup().is_ok()) }.boxed()
        }

        fn activation_env_changes(
            &self,
            package_path: &Path,
        ) -> Result<Vec<ide_ci::env::Modification>> {
            let path = package_path.join_iter(["install", "bin"]);
            let path = ide_ci::env::Modification::prepend_path(&PATH, path);
            Ok(vec![path])
        }
    }
}

pub mod programs {
    use super::*;

    pub mod glslc {
        use super::*;

        #[derive(Clone, Copy, Debug, Default)]
        pub struct Glslc;
        impl Program for Glslc {
            fn executable_name(&self) -> &'static str {
                "glslc"
            }
        }
    }

    pub mod spirv_opt {
        use super::*;

        #[derive(Clone, Copy, Debug, Default)]
        pub struct SpirvOpt;
        impl Program for SpirvOpt {
            fn executable_name(&self) -> &'static str {
                "spirv-opt"
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::shaderc::programs::glslc::Glslc;

    #[tokio::test]
    #[ignore]
    async fn setup_shaderc() -> Result {
        setup_logging()?;

        assert!(Glslc.lookup().is_err());

        let cache = ide_ci::cache::Cache::new_default().await?;

        let shaderc = crate::shaderc::goodie::Shaderc;
        shaderc.install_if_missing(&cache).await?;

        Glslc.lookup()?;


        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn download() -> Result {
        let temp_dir = tempfile::tempdir()?;

        setup_logging()?;
        let url = download_urls()?;
        ide_ci::io::download_to_dir(url, &temp_dir).await?;

        Ok(())
    }
}
