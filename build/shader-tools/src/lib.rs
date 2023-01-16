pub use ide_ci::prelude;
use ide_ci::program::command::Manipulator;

use prelude::*;

pub const SPIRV_TOOLS_URL: &str = "https://github.com/KhronosGroup/SPIRV-Cross";

pub struct CMake;
impl Program for CMake {
    fn executable_name(&self) -> &str {
        "cmake"
    }
}

/// Set the given variable in the CMake cache.
pub struct SetVariable {
    pub variable: String,
    pub value:    String,
}

impl SetVariable {
    pub fn option(name: impl Into<String>, value: bool) -> Self {
        Self { variable: name.into(), value: if value { "ON" } else { "OFF" }.into() }
    }

    pub fn filepath(name: impl Into<String>, value: impl AsRef<Path>) -> Self {
        Self { variable: name.into(), value: value.as_ref().as_str().into() }
    }

    pub fn path(name: impl Into<String>, value: impl AsRef<Path>) -> Self {
        Self { variable: name.into(), value: value.as_ref().as_str().into() }
    }
}

impl Manipulator for SetVariable {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg("-D").arg(format!("{}={}", self.variable, self.value));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ide_ci::programs::git;
    use ide_ci::programs::vs::apply_dev_environment;
    use ide_ci::programs::Git;

    #[tokio::test]
    async fn compile_spirv_cross() -> Result {
        setup_logging()?;
        if TARGET_OS == OS::Windows {
            apply_dev_environment().await?;
        }
        let path = Path::new(r"C:\temp\spirv-cross");
        let build_dir = path.join("_build");
        let install_dir = path.join("_install");
        ide_ci::fs::tokio::reset_dir(&path).await?;

        let git = Git.clone(&path, &(SPIRV_TOOLS_URL.try_into()?)).await?;

        ide_ci::fs::tokio::reset_dir(&build_dir).await?;
        CMake
            .cmd()?
            .arg(&path)
            .apply(&SetVariable::option("SPIRV_CROSS_ENABLE_TESTS", false))
            .current_dir(&build_dir)
            .run_ok()
            .await?;
        CMake
            .cmd()?
            .arg("--build")
            .arg(".")
            .arg("-j")
            .args(["--config", "Release"])
            .current_dir(&build_dir)
            .run_ok()
            .await?;

        ide_ci::fs::tokio::reset_dir(&install_dir).await?;
        CMake
            .cmd()?
            .arg("--install")
            .arg(".")
            .args(["--prefix", install_dir.as_str()])
            .current_dir(&build_dir)
            .run_ok()
            .await?;

        let archive_name = format!("spirv-cross-{}.tar.gz", TARGET_OS);
        let package =
            ide_ci::archive::create(&path.join(&archive_name), [install_dir.join("bin")]).await?;



        Ok(())
    }
}
