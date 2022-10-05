use crate::prelude::*;

use crate::cache;
use crate::env::prepend_to_path;
use crate::program::version::IsVersionPredicate;
use crate::programs::wasm_opt;
use crate::programs::wasm_opt::WasmOpt;



#[derive(Clone, Copy, Debug, Display)]
pub struct Binaryen {
    pub version: u32,
}

impl IsVersionPredicate for Binaryen {
    type Version = wasm_opt::Version;
    fn matches(&self, version: &Self::Version) -> bool {
        version.0 >= self.version
    }
}

impl Binaryen {}

impl cache::Goodie for Binaryen {
    fn url(&self) -> BoxFuture<'static, Result<Url>> {
        let version = format!("version_{}", self.version);
        async move {
            let target = match (TARGET_OS, TARGET_ARCH) {
                (OS::Windows, Arch::X86_64) => "x86_64-windows",
                (OS::Linux, Arch::X86_64) => "x86_64-linux",
                (OS::MacOS, Arch::X86_64) => "x86_64-macos",
                (OS::MacOS, Arch::AArch64) => "arm64-macos",
                (os, arch) => bail!("Not supported arch/OS combination: {arch}-{os}."),
            };
            let url = format!("https://github.com/WebAssembly/binaryen/releases/download/{version}/binaryen-{version}-{target}.tar.gz");
            url.parse2()
        }.boxed()
    }

    fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
        let this = *self;
        async move {
            WasmOpt.require_present_that(this).await?;
            Ok(true)
        }
        .boxed()
    }

    fn activate(&self, package_path: PathBuf) -> Result {
        let bin_dir = package_path.join(format!("binaryen-version_{}", self.version)).join("bin");
        crate::fs::expect_dir(&bin_dir)?;
        prepend_to_path(bin_dir)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cache;
    use crate::cache::Goodie;
    use crate::log::setup_logging;

    #[tokio::test]
    #[ignore]
    async fn install_wasm_opt() -> Result {
        setup_logging()?;
        let cache = cache::Cache::new_default().await?;
        let binaryen = Binaryen { version: 108 };
        binaryen.install_if_missing(&cache).await?;
        dbg!(WasmOpt.lookup())?;

        assert!(binaryen.is_active().await?);

        Ok(())
    }
}
