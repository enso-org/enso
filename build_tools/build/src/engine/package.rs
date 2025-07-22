use crate::prelude::*;

use crate::engine::artifact::IsArtifact;
use crate::engine::sbt::SbtCommandProvider;



/// Package is a minimal artifact with some backend component.
pub trait IsPackage: IsArtifact {
    /// Get the package name that is recognized by the SBT build scripts.
    ///
    /// It can be used e.g. to verify the package by invoking `enso/verifyGeneratedPackage` task.
    fn sbt_package_name(&self) -> &str;

    /// Primary directory of the package.
    ///
    /// E.g. for the Engine package it is like
    /// `H:\NBO\enso\built-distribution\enso-engine-0.0.0-SNAPSHOT.2022-01-19-windows-amd64\enso-0.
    /// 0.0-SNAPSHOT.2022-01-19`.
    fn dir(&self) -> &Path {
        self.as_ref()
    }

    /// Invokes `enso/verifyGeneratedPackage` task on this package.
    fn verify_package_sbt(&self, sbt: &crate::engine::sbt::Context) -> BoxFuture<'static, Result> {
        let package_name = self.sbt_package_name();
        let dir = self.dir();
        sbt.verify_generated_package(package_name, dir)
    }
}

impl IsPackage for crate::paths::generated::EnginePackage {
    fn sbt_package_name(&self) -> &str {
        "engine"
    }
}

impl IsPackage for crate::paths::generated::ProjectManagerPackage {
    fn sbt_package_name(&self) -> &str {
        "project-manager"
    }
}

impl IsPackage for crate::paths::generated::LauncherPackage {
    fn sbt_package_name(&self) -> &str {
        "launcher"
    }
}
