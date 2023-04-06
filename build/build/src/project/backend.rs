use crate::prelude::*;

use crate::engine::BuildConfigurationFlags;
use crate::paths::pretty_print_arch;
use crate::paths::TargetTriple;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;
use crate::source::BuildTargetJob;
use crate::source::WithDestination;
use crate::version::Versions;

use derivative::Derivative;
use ide_ci::archive::is_archive_name;
use ide_ci::extensions::os::OsExt;
use octocrab::models::repos::Asset;



#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct BuildInput {
    pub versions:         Versions,
    #[derivative(Debug = "ignore")]
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

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct Artifact {
    /// Location of the Project Manager distribution.
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub path:            crate::paths::generated::ProjectManagerBundle,
    /// Versions of Engine that are bundled in this Project Manager distribution.
    ///
    /// Technically a Project Manager bundle can be shipped with arbitrary number of Enso Engine
    /// packages. However in packages we create it is almost always zero (for plain PM package) or
    /// one (for full PM bundle).
    ///
    /// Artifacts built with [`ProjectManager::build`] will have exactly one engine
    /// bundled.
    #[derivative(Debug(format_with = "ide_ci::fmt::display_list"))]
    pub engine_versions: Vec<Version>,
}

impl Artifact {
    /// Latest version of Enso Engine that is bundled in this Project Manager distribution.
    pub fn latest_engine_version(&self) -> Result<&Version> {
        self.engine_versions.iter().max().with_context(|| {
            format!(
                "Project Manager bundle at {} does not contain any Enso Engine packages.",
                self.path
            )
        })
    }
}

impl AsRef<Path> for Artifact {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

impl IsArtifact for Artifact {}

/// Retrieves a list of all Enso Engine versions that are bundled within a given Project Manager
/// distribution.
#[context("Failed to list bundled engine versions: {}", project_manager_bundle)]
pub async fn bundled_engine_versions(
    project_manager_bundle: &crate::paths::generated::ProjectManagerBundle,
) -> Result<Vec<Version>> {
    let mut ret = vec![];

    let mut dir_reader = ide_ci::fs::tokio::read_dir(&project_manager_bundle.dist).await?;
    while let Some(entry) = dir_reader.try_next().await? {
        if ide_ci::fs::tokio::metadata(&entry.path()).await?.is_dir() {
            ret.push(Version::from_str(entry.file_name().as_str())?);
        }
    }
    Ok(ret)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Backend {
    pub target_os: OS,
}

impl Backend {
    pub fn matches_platform(&self, name: &str) -> bool {
        // Sample name: "project-manager-bundle-2022.1.1-nightly.2022-04-16-linux-amd64.tar.gz"
        let os_matches = name.contains(self.target_os.as_str());
        // Arch test involves a workaround for Engine being built through Rosette on Apple Silicon.
        let arch_matches = name.contains(pretty_print_arch(TARGET_ARCH))
            || (TARGET_ARCH == Arch::AArch64 && name.contains(pretty_print_arch(Arch::X86_64)));
        os_matches && arch_matches
    }
}

impl IsTarget for Backend {
    type BuildInput = BuildInput;
    type Artifact = Artifact;

    fn artifact_name(&self) -> String {
        // Version is not part of the name intentionally. We want to refer to PM bundles as
        // artifacts without knowing their version.
        format!("project-manager-{}", self.target_os)
    }

    fn adapt_artifact(self, path: impl AsRef<Path>) -> BoxFuture<'static, Result<Self::Artifact>> {
        let exe_suffix = self.target_os.exe_suffix().to_owned();
        let path = path.as_ref().to_owned();
        let provisional_path = crate::paths::generated::ProjectManagerBundle::new_root(
            &path,
            &exe_suffix,
            "<unknown version>",
        );
        async move {
            let engine_versions = bundled_engine_versions(&provisional_path).await?;
            let path = crate::paths::generated::ProjectManagerBundle::new_root(
                &path,
                &exe_suffix,
                engine_versions
                    .last()
                    .map_or_else(|| "<unknown version>".to_string(), |v| v.to_string()),
            );
            Ok(Artifact { path, engine_versions })
        }
        .boxed()
    }

    fn build_internal(
        &self,
        context: Context,
        job: BuildTargetJob<Self>,
    ) -> BoxFuture<'static, Result<Self::Artifact>> {
        let WithDestination { inner, destination } = job;
        let target_os = self.target_os;
        let this = *self;
        async move {
            ensure!(
                target_os == TARGET_OS,
                "Enso Project Manager cannot be built on '{target_os}' for target '{TARGET_OS}'.",
            );
            let config = BuildConfigurationFlags {
                build_project_manager_bundle: true,
                generate_java_from_rust: true,
                ..default()
            };
            let context = inner.prepare_context(context, config)?;
            let artifacts = context.build().await?;
            let project_manager =
                artifacts.project_manager_bundle.context("Missing project manager bundle!")?;
            ide_ci::fs::mirror_directory(&project_manager, &destination).await?;
            this.adapt_artifact(destination).await
        }
        .boxed()
    }

    fn matches_asset(&self, asset: &Asset) -> bool {
        // The size condition is used to discern actual artifact from its checksum.
        let name = &asset.name;
        self.matches_platform(name)
            && is_archive_name(name)
            && name.contains("project-manager")
            && (name.contains("bundle") || asset.size > 200_000_000)
    }
}
