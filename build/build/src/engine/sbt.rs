//! This module wraps SBT commands that are provided by the Enso Engine's SBT build scripts.

use crate::prelude::*;

use ide_ci::program::command::provider::CommandProviderExt;
use ide_ci::programs::sbt;
use ide_ci::programs::Sbt;



pub fn verify_generated_package_task(package: &str, path: impl AsRef<Path>) -> String {
    format!(
        "enso/verifyGeneratedPackage {} {}",
        package,
        path.as_ref().join("THIRD-PARTY").display()
    )
}

pub trait SbtCommandProvider: CommandProvider {
    fn verify_generated_package(
        &self,
        package: &str,
        path: impl AsRef<Path>,
    ) -> BoxFuture<'static, Result> {
        self.call_arg(verify_generated_package_task(package, path))
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub repo_root:         PathBuf,
    pub system_properties: Vec<sbt::SystemProperty>,
}

impl CommandProvider for Context {
    fn command(&self) -> Result<Command> {
        let mut cmd = Sbt.cmd()?;
        cmd.current_dir(&self.repo_root);
        for property in &self.system_properties {
            cmd.args(property);
        }
        // This prevents https://github.com/sbt/sbt-assembly/issues/496
        cmd.env(ide_ci::env::known::LC_ALL, ide_ci::env::known::C_UTF8);
        // This prevents https://github.com/sbt/sbt/issues/6777#issuecomment-1613316167
        cmd.set_env(sbt::SBT_SERVER_FORCESTART, &true)?;
        // Again. Preferably there should be no sbt server spawned at all.
        cmd.apply(&sbt::ServerAutostart(false));
        Ok(cmd)
    }
}

impl SbtCommandProvider for Context {}
