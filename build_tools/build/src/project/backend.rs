use crate::prelude::*;

use crate::engine::BuildConfigurationFlags;
use crate::paths::TargetTriple;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;
use crate::source::WithDestination;
use crate::version::Versions;

use ide_ci::archive::is_archive_name;
use octocrab::models::repos::Asset;

#[derive(Clone)]
#[derive_where(Debug)]
pub struct BuildInput {
    pub versions: Versions,
    #[derive_where(skip)]
    pub external_runtime: Option<Arc<crate::engine::context::EnginePackageProvider>>,
}

impl BuildInput {
    pub fn prepare_context(
        &self,
        inner: Context,
        config: BuildConfigurationFlags,
    ) -> Result<crate::engine::RunContext> {
        let BuildInput { versions, external_runtime } = self;
        crate::engine::RunContext::new(
            inner,
            config,
            TargetTriple::new(versions.clone()),
            external_runtime.clone(),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Artifact {
    /// Location of the backend distribution.
    pub path: PathBuf,
    /// Versions of Engine that are bundled in this distribution.
    ///
    /// In practice, a backend bundle now contains either zero or one Enso Engine package.
    pub engine_versions: Vec<Version>,
}

impl Artifact {
    /// Latest version of Enso Engine that is bundled in this distribution.
    pub fn latest_engine_version(&self) -> Result<&Version> {
        self.engine_versions.iter().max().with_context(|| {
            format!("Backend at {} does not contain any Enso Engine packages.", self.path.display())
        })
    }
}

impl AsRef<Path> for Artifact {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

impl IsArtifact for Artifact {}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Backend {
    pub target_os: OS,
}

impl Backend {
    pub fn matches_platform(&self, name: &str) -> bool {
        // Sample name: "project-manager-bundle-2022.1.1-nightly.2022-04-16-linux-amd64.tar.gz"
        let os_matches = name.contains(self.target_os.as_str());
        let arch_matches = name.contains(TARGET_ARCH.as_str());
        os_matches && arch_matches
    }
}

impl IsTarget for Backend {
    type BuildInput = BuildInput;
    type Artifact = Artifact;

    fn artifact_name(&self) -> String {
        format!("backend-{}", self.target_os)
    }

    fn adapt_artifact(self, path: impl AsRef<Path>) -> BoxFuture<'static, Result<Self::Artifact>> {
        let path = path.as_ref().to_owned();
        async move {
            // Detect engine versions by listing directories under dist/ that are valid semver.
            let mut engine_versions = Vec::new();
            let dist_dir = path.join("dist");
            if dist_dir.exists() {
                for entry in std::fs::read_dir(&dist_dir)? {
                    let entry = entry?;
                    if entry.file_type()?.is_dir() {
                        let name = entry.file_name().to_string_lossy().to_string();
                        if let Ok(version) = Version::parse(&name) {
                            engine_versions.push(version);
                        }
                    }
                }
            }
            Ok(Artifact { path, engine_versions })
        }
        .boxed()
    }

    fn build_internal(
        &self,
        context: Context,
        job: WithDestination<Self::BuildInput>,
    ) -> BoxFuture<'static, Result<Self::Artifact>> {
        let WithDestination { inner, destination } = job;
        let target_os = self.target_os;
        let this = *self;
        let small_jdk_dir = context.repo_root.target.small_jdk.clone().to_path_buf();
        async move {
            ensure!(
                target_os == TARGET_OS,
                "Enso Engine cannot be built on '{target_os}' for target '{TARGET_OS}'.",
            );
            let config = BuildConfigurationFlags {
                build_engine_bundle: true,
                build_small_jdk: true,
                small_jdk_dir: Some(small_jdk_dir),
                ..default()
            };
            let context = inner.prepare_context(context, config)?;
            let artifacts = context.build().await?;
            let engine_bundle = artifacts.engine_bundle.context("Missing engine bundle!")?;
            ide_ci::fs::mirror_directory(&engine_bundle, &destination).await?;
            this.adapt_artifact(destination).await
        }
        .boxed()
    }

    fn matches_asset(&self, asset: &Asset) -> bool {
        // The size condition is used to discern actual artifact from its checksum.
        let name = &asset.name;
        self.matches_platform(name)
            && is_archive_name(name)
            && name.contains("engine-bundle")
            && (name.contains("bundle") || asset.size > 200_000_000)
    }
}
