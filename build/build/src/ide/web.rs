//! Code for dealing with JS/TS components of the GUI1 and the Electron client (IDE).

use crate::prelude::*;

use crate::paths::generated;
use crate::project::gui::BuildInfo;
use crate::project::IsArtifact;
use crate::version::ENSO_VERSION;

use anyhow::Context;
use ide_ci::env::known::electron_builder::WindowsSigningCredentials;
use ide_ci::program::command::FallibleManipulator;
use ide_ci::program::command::Manipulator;
use ide_ci::programs::node::PnpmCommand;
use ide_ci::programs::Pnpm;
use sha2::Digest;
use std::process::Stdio;
use tempfile::TempDir;

// ==============
// === Export ===
// ==============

lazy_static! {
    /// Path to the file with build information that is consumed by the JS part of the IDE.
    ///
    /// The file must follow the schema of type [`BuildInfo`].
    pub static ref BUILD_INFO: PathBuf = PathBuf::from("build.json");
}

pub mod env {
    use super::*;

    use ide_ci::define_env_var;

    define_env_var! {
        ENSO_BUILD_IDE, PathBuf;
        ENSO_BUILD_PROJECT_MANAGER, PathBuf;
        ENSO_BUILD_GUI, PathBuf;
        ENSO_BUILD_ICONS, PathBuf;
        ENSO_BUILD_SIGN, bool;
        /// List of files that should be copied to the Gui.
        ENSO_BUILD_GUI_WASM_ARTIFACTS, Vec<PathBuf>;
        ENSO_BUILD_GUI_ASSETS, PathBuf;
        ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION, Version;
        ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH, PathBuf;
    }

    // === Electron Builder ===
    pub use ide_ci::env::known::electron_builder::*;


    // Cloud environment configuration
    define_env_var! {
        /// The domain where the login link should redirect, without path or trailing slash.
        ENSO_CLOUD_REDIRECT, String;

        /// The name of the backend environment, typically 'production' for production builds.
        ENSO_CLOUD_ENVIRONMENT, String;

        /// The root path for all API endpoints, without a trailing slash.
        ENSO_CLOUD_API_URL, String;

        /// The URL for the WebSocket server for chat functionality.
        ENSO_CLOUD_CHAT_URL, String;

        /// The Sentry DSN for error reporting in this environment.
        ENSO_CLOUD_SENTRY_DSN, String;

        /// Stripe's publishable key for client-side operations.
        ENSO_CLOUD_STRIPE_KEY, String;

        /// The ID of the Amplify user pool for authentication.
        ENSO_CLOUD_COGNITO_USER_POOL_ID, String;

        /// The client-side key for the Amplify user pool.
        ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID, String;

        /// The domain for Amplify requests.
        ENSO_CLOUD_COGNITO_DOMAIN, String;

        /// The AWS region for Amplify configuration, matching the domain region.
        ENSO_CLOUD_COGNITO_REGION, String;

        /// The Google Analytics tag to which Google Analytics events should be sent.
        ENSO_CLOUD_GOOGLE_ANALYTICS_TAG, String;
    }

    // GUI-specific environment variables
    define_env_var! {
        /// License key for the AG Grid library.
        VITE_ENSO_AG_GRID_LICENSE_KEY, String;
        /// The Mapbox API token for the GeoMap visualization.
        VITE_ENSO_MAPBOX_API_TOKEN, String;
    }
}

/// Name of the directory with the unpacked Electron package.
///
/// The directory is created by the `electron-builder` utility in the output directory when run
/// with the `dir` target. It is also usually created for other targets, as it is an intermediate
/// step in the packaging process.
///
/// # Panics
/// This function panics if the provided OS and architecture combination is not supported.
pub fn unpacked_dir(output_path: impl AsRef<Path>, os: OS, arch: Arch) -> PathBuf {
    let segment_name = match (os, arch) {
        (OS::Linux, Arch::X86_64) => "linux-unpacked",
        (OS::MacOS, Arch::AArch64) => "mac-arm64",
        (OS::MacOS, Arch::X86_64) => "mac",
        (OS::Windows, Arch::X86_64) => "win-unpacked",
        _ => todo!("{os}-{arch} combination is not supported"),
    };
    output_path.as_ref().join(segment_name)
}

/// Computes the SHA-256 checksum of a file and writes it to a file.
///
/// This is a Rust equivalent of the `app/ide-desktop/client/tasks/computeHashes.mjs`.
pub fn store_sha256_checksum(file: impl AsRef<Path>, checksum_file: impl AsRef<Path>) -> Result {
    let mut hasher = sha2::Sha256::new();
    let mut file = ide_ci::fs::open(&file)?;
    std::io::copy(&mut file, &mut hasher)?;
    let hash = hasher.finalize();
    ide_ci::fs::write(&checksum_file, format!("{hash:x}"))?;
    Ok(())
}

#[derive(Clone, Debug)]
pub struct IconsArtifacts(pub PathBuf);

impl FallibleManipulator for IconsArtifacts {
    fn try_applying<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) -> Result {
        command.set_env(env::ENSO_BUILD_ICONS, &self.0)?;
        Ok(())
    }
}

/// Get a relative path to the Project Manager executable in the PM bundle.
pub fn path_to_executable_in_pm_bundle(
    artifact: &generated::ProjectManagerBundle,
) -> Result<&Path> {
    artifact
        .bin
        .project_managerexe
        .strip_prefix(artifact)
        .context("Failed to generate in-bundle path to Project Manager executable.")
}

/// When secrets are not available in CI builds (e.g. when building a PR from a fork), the variables
/// are set to empty strings. This manipulator removes such variables from the environment.
#[derive(Clone, Copy, Debug)]
pub struct RemoveEmptyCscEnvVars;

impl Manipulator for RemoveEmptyCscEnvVars {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        for var in ide_ci::env::known::electron_builder::CI_CSC_SECRETS {
            match std::env::var(var) {
                Ok(value) if value.is_empty() => {
                    command.env_remove(var);
                }
                _ => {}
            }
        }
    }
}

pub fn target_os_flag(os: OS) -> Result<&'static str> {
    match os {
        OS::Windows => Ok("--win"),
        OS::Linux => Ok("--linux"),
        OS::MacOS => Ok("--mac"),
    }
}

/// Context information about Project Manager bundle that we provide to the client.
#[derive(Clone, Debug)]
pub struct ProjectManagerInfo {
    /// Latest bundled engine version, that will be used as this IDE's default.
    pub latest_bundled_engine: Version,
    /// Root of the Project Manager bundle.
    pub bundle_location:       PathBuf,
    /// Relative path from the bundle location.
    pub pm_executable:         PathBuf,
}

impl ProjectManagerInfo {
    /// Collect information about the bundle that the client will need.
    pub fn new(bundle: &crate::project::backend::Artifact) -> Result<Self> {
        let latest_bundled_engine = bundle.latest_engine_version()?.clone();
        let bundle_location = bundle.path.to_path_buf();
        let pm_executable = path_to_executable_in_pm_bundle(&bundle.path)?.to_path_buf();
        Ok(Self { latest_bundled_engine, bundle_location, pm_executable })
    }
}

impl FallibleManipulator for ProjectManagerInfo {
    fn try_applying<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) -> Result {
        command.set_env(env::ENSO_BUILD_PROJECT_MANAGER, &self.bundle_location)?;
        command.set_env(env::ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH, &self.pm_executable)?;
        command.set_env(env::ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION, &self.latest_bundled_engine)?;
        Ok(())
    }
}

#[derive(Clone)]
#[derive_where(Debug)]
pub struct IdeDesktop {
    pub build_sbt: generated::RepoRootBuildSbt,
    pub repo_root: generated::RepoRoot,
    #[derive_where(skip)]
    pub octocrab:  Octocrab,
    pub cache:     ide_ci::cache::Cache,
}

impl IdeDesktop {
    pub fn new(
        repo_root: &generated::RepoRoot,
        octocrab: Octocrab,
        cache: ide_ci::cache::Cache,
    ) -> Self {
        Self {
            build_sbt: repo_root.build_sbt.clone(),
            repo_root: repo_root.clone(),
            octocrab,
            cache,
        }
    }

    pub fn pnpm(&self) -> Result<PnpmCommand> {
        let mut command = Pnpm.cmd()?;
        command.current_dir(&self.repo_root);
        command.stdin(Stdio::null()); // nothing in that process subtree should require input
        Ok(command)
    }

    pub fn write_build_info(&self, info: &BuildInfo) -> Result {
        let path = self.repo_root.join(&*BUILD_INFO);
        path.write_as_json(info)
    }

    pub async fn build_icons(&self, output_path: impl AsRef<Path>) -> Result<IconsArtifacts> {
        self.pnpm()?
            .set_env(env::ENSO_BUILD_ICONS, output_path.as_ref())?
            .run("build:icons")
            .run_ok()
            .await?;
        Ok(IconsArtifacts(output_path.as_ref().into()))
    }

    /// Build the full Electron package, using the electron-builder.
    #[tracing::instrument(name="Preparing distribution of the IDE.", skip_all, fields(
        dest = %output_path.as_ref().display(),
        ?gui,
        ?project_manager,
        ?target_os,
        ?target,
        ?sign,
        err))]
    pub async fn dist(
        &self,
        gui: &impl IsArtifact,
        project_manager: &crate::project::backend::Artifact,
        output_path: impl AsRef<Path>,
        target_os: OS,
        target: Option<String>,
        sign: bool,
    ) -> Result {
        let output_path = output_path.as_ref();
        let electron_config = output_path.join("electron-builder.json");
        if TARGET_OS == OS::MacOS && env::CSC_KEY_PASSWORD.is_set() {
            // This means that we will be doing code signing on MacOS. This requires JDK environment
            // to be set up.
            let graalvm =
                crate::engine::deduce_graal(self.octocrab.clone(), &self.build_sbt).await?;
            graalvm.install_if_missing(&self.cache).await?;
        }


        crate::web::install(&self.repo_root).await?;
        let pm_bundle = ProjectManagerInfo::new(project_manager)?;
        let sign_artifacts = &sign;
        self.pnpm()?
            .set_env(env::ENSO_BUILD_GUI, gui.as_ref())?
            .set_env(env::ENSO_BUILD_IDE, output_path)?
            .set_env(env::ENSO_BUILD_SIGN, sign_artifacts)?
            .try_applying(&pm_bundle)?
            .run("build:ide")
            .run_ok()
            .await?;

        let icons_dist = TempDir::new()?;
        let icons_dist = icons_dist.into_path();
        let icons_build = self.build_icons(&icons_dist);
        let icons = icons_build.await?;

        let target_args = match target {
            Some(target) => vec!["--target".to_string(), target],
            None => vec![],
        };


        self.pnpm()?
            .try_applying(&icons)?
            .apply(&RemoveEmptyCscEnvVars)
            // .env("DEBUG", "electron-builder")
            .set_env(env::ENSO_BUILD_GUI, gui.as_ref())?
            .set_env(env::ENSO_BUILD_IDE, output_path)?
            .set_env(env::ENSO_BUILD_SIGN, sign_artifacts)?
            .set_env(env::ENSO_BUILD_PROJECT_MANAGER, project_manager.as_ref())?
            .set_env(enso_install_config::ENSO_BUILD_ELECTRON_BUILDER_CONFIG, &electron_config)?
            .run("dist:ide")
            .arg("--")
            .arg(target_os_flag(target_os)?)
            .args(target_args)
            .run_ok()
            .await?;

        // On Windows we build our own installer by invoking `enso_install_config::bundler::bundle`.
        if TARGET_OS == OS::Windows {
            let code_signing_certificate = WindowsSigningCredentials::new_from_env()
                .await
                .inspect_err(|e| {
                    warn!("Failed to create code signing certificate from the environment: {e:?}");
                })
                .ok();

            let ide_artifacts = crate::project::ide::Artifact::new(
                target_os,
                TARGET_ARCH,
                &ENSO_VERSION.get()?,
                output_path,
            );

            let config = enso_install_config::bundler::Config {
                electron_builder_config:  electron_config,
                unpacked_electron_bundle: unpacked_dir(output_path, target_os, TARGET_ARCH),
                repo_root:                self.repo_root.to_path_buf(),
                output_file:              ide_artifacts.image.clone(),
                intermediate_dir:         output_path.to_path_buf(),
                certificate:              code_signing_certificate,
            };
            enso_install_config::bundler::bundle(config).await?;
            store_sha256_checksum(&ide_artifacts.image, &ide_artifacts.image_checksum)?;
        }
        Ok(())
    }
}
