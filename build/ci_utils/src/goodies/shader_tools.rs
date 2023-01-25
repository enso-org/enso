//! Shader Tools is our collection of tools for working with shaders.
//!
//! The included programs are:
//! * [glslc](Glslc);
//! * [spirv-opt](SpirvOpt);
//! * [spirv-cross](SpirvCross).
//!
//! This module only deals with downloading and activating the tools. The code for building and
//! uploading the tools package is in the `enso-build-shader-tools` crate.

use crate::prelude::*;

use crate::cache::goodie;
use crate::cache::Cache;
use crate::cache::Goodie;
use crate::env::known::PATH;
use crate::github::RepoRef;
use crate::programs::shaderc::Glslc;
use crate::programs::shaderc::SpirvOpt;
use crate::programs::spirv_cross::SpirvCross;



// =================
// === Constants ===
// =================

/// Repository where we store releases of the shader tools.
pub const SHADER_TOOLS_REPO: RepoRef = RepoRef { owner: "enso-org", name: "shader-tools" };

/// Version of the shader tools package that we download.
pub const VERSION: Version = Version::new(0, 1, 0);


// =========================
// === Asset description ===
// =========================

pub fn asset_name(os: OS) -> String {
    // At the moment we don't have non-x64 binaries, so we can hardcode the architecture.
    let arch = Arch::X86_64;
    format!("shader-tools-{os}-{arch}.tar.gz")
}


// =========================
// === Goodie definition ===
// =========================

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
            let glslc = Glslc.lookup();
            let spirv_cross = SpirvCross.lookup();
            let spirv_opt = SpirvOpt.lookup();
            Ok(glslc.is_ok() && spirv_cross.is_ok() && spirv_opt.is_ok())
        }
        .boxed()
    }

    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<crate::env::Modification>> {
        let path = package_path.join_iter(["bin"]);
        let path = crate::env::Modification::prepend_path(&PATH, path);
        Ok(vec![path])
    }
}
