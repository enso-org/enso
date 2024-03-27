use crate::prelude::*;

use crate::ide::web::env::CSC_KEY_PASSWORD;
use crate::paths::generated;
use crate::project::gui::BuildInfo;
use crate::project::IsArtifact;

use anyhow::Context;
use futures_util::future::try_join;
use ide_ci::io::download_all;
use ide_ci::program::command::FallibleManipulator;
use ide_ci::programs::node::NpmCommand;
use ide_ci::programs::Npm;
use std::process::Stdio;
use tempfile::TempDir;


// ==============
// === Export ===
// ==============

pub mod dejavu_font;
pub mod enso_font;
pub mod fonts;
pub mod google_font;



lazy_static! {
    /// Path to the file with build information that is consumed by the JS part of the IDE.
    ///
    /// The file must follow the schema of type [`BuildInfo`].
    pub static ref BUILD_INFO: PathBuf = PathBuf::from("build.json");
}

pub const IDE_ASSETS_URL: &str =
    "https://github.com/enso-org/ide-assets/archive/refs/heads/main.zip";

pub const ARCHIVED_ASSET_FILE: &str = "ide-assets-main/content/assets/";

pub mod env {
    use super::*;

    use ide_ci::define_env_var;

    define_env_var! {
        ENSO_BUILD_IDE, PathBuf;
        ENSO_BUILD_PROJECT_MANAGER, PathBuf;
        ENSO_BUILD_GUI, PathBuf;
        ENSO_BUILD_ICONS, PathBuf;
        /// List of files that should be copied to the Gui.
        ENSO_BUILD_GUI_WASM_ARTIFACTS, Vec<PathBuf>;
        ENSO_BUILD_GUI_ASSETS, PathBuf;
        ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION, Version;
        ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH, PathBuf;
    }

    // === Electron Builder ===
    // Variables introduced by the Electron Builder itself.
    // See: https://www.electron.build/code-signing

    define_env_var! {
        /// The HTTPS link (or base64-encoded data, or file:// link, or local path) to certificate
        /// (*.p12 or *.pfx file). Shorthand ~/ is supported (home directory).
        WIN_CSC_LINK, String;

        /// The password to decrypt the certificate given in WIN_CSC_LINK.
        WIN_CSC_KEY_PASSWORD, String;

        /// The HTTPS link (or base64-encoded data, or file:// link, or local path) to certificate
        /// (*.p12 or *.pfx file). Shorthand ~/ is supported (home directory).
        CSC_LINK, String;

        /// The password to decrypt the certificate given in CSC_LINK.
        CSC_KEY_PASSWORD, String;

        /// The username of apple developer account.
        APPLEID, String;

        /// The app-specific password (not Apple ID password). See:
        /// https://support.apple.com/HT204397
        APPLEIDPASS, String;

        /// Apple Team ID.
        APPLETEAMID, String;

        /// `true` or `false`. Defaults to `true` — on a macOS development machine valid and
        /// appropriate identity from your keychain will be automatically used.
        CSC_IDENTITY_AUTO_DISCOVERY, bool;

        /// Path to the python2 executable, used by electron-builder on macOS to package DMG.
        PYTHON_PATH, PathBuf;

        /// Note that enabling CSC_FOR_PULL_REQUEST can pose serious security risks. Refer to the
        /// [CircleCI documentation](https://circleci.com/docs/1.0/fork-pr-builds/) for more
        /// information. If the project settings contain SSH keys, sensitive environment variables,
        /// or AWS credentials, and untrusted forks can submit pull requests to your repository, it
        /// is not recommended to enable this option.
        ///
        /// In our case we are careful to not expose any sensitive information to third-party forks,
        /// so we can safely enable this option.
        CSC_FOR_PULL_REQUEST, bool;
    }

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
    }

    // GUI-specific environment variables
    define_env_var! {
        /// License key for the AG Grid library.
        VITE_ENSO_AG_GRID_LICENSE_KEY, String;
        /// The Mapbox API token for the GeoMap visualization.
        VITE_ENSO_MAPBOX_API_TOKEN, String;
    }
}

#[derive(Clone, Debug)]
pub struct IconsArtifacts(pub PathBuf);

impl FallibleManipulator for IconsArtifacts {
    fn try_applying<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) -> Result {
        command.set_env(env::ENSO_BUILD_ICONS, &self.0)?;
        Ok(())
    }
}

/// Fill the directory under `output_path` with the assets.
pub async fn download_js_assets(output_path: impl AsRef<Path>) -> Result {
    let output = output_path.as_ref();
    let archived_asset_prefix = PathBuf::from(ARCHIVED_ASSET_FILE);
    let archive = download_all(IDE_ASSETS_URL).await?;
    let mut archive = zip::ZipArchive::new(std::io::Cursor::new(archive))?;
    ide_ci::archive::zip::extract_subtree(&mut archive, &archived_asset_prefix, output)?;
    Ok(())
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

#[derive(Clone, Copy, Debug)]
pub enum Workspaces {
    Icons,
    IdeDesktop,
    Content,
    /// The Electron client.
    Enso,
}

impl AsRef<OsStr> for Workspaces {
    fn as_ref(&self) -> &OsStr {
        match self {
            Workspaces::Icons => OsStr::new("enso-icons"),
            Workspaces::IdeDesktop => OsStr::new("enso-ide-desktop"),
            Workspaces::Content => OsStr::new("enso-content"),
            Workspaces::Enso => OsStr::new("enso"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Command {
    Build,
    Watch,
}

pub fn target_os_flag(os: OS) -> Result<&'static str> {
    match os {
        OS::Windows => Ok("--win"),
        OS::Linux => Ok("--linux"),
        OS::MacOS => Ok("--mac"),
        _ => bail!("Not supported target for Electron client: {os}."),
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

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct IdeDesktop {
    pub build_sbt: generated::RepoRootBuildSbt,
    pub repo_root: generated::RepoRoot,
    #[derivative(Debug = "ignore")]
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

    pub fn npm(&self) -> Result<NpmCommand> {
        let mut command = Npm.cmd()?;
        command.arg("--color").arg("always");
        command.arg("--yes");
        command.current_dir(&self.repo_root);
        command.stdin(Stdio::null()); // nothing in that process subtree should require input
        Ok(command)
    }

    pub fn write_build_info(&self, info: &BuildInfo) -> Result {
        let path = self.repo_root.join(&*BUILD_INFO);
        path.write_as_json(info)
    }

    pub async fn build_icons(&self, output_path: impl AsRef<Path>) -> Result<IconsArtifacts> {
        self.npm()?
            .workspace(Workspaces::Icons)
            .set_env(env::ENSO_BUILD_ICONS, output_path.as_ref())?
            .run("build")
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
        err))]
    pub async fn dist(
        &self,
        gui: &impl IsArtifact,
        project_manager: &crate::project::backend::Artifact,
        output_path: impl AsRef<Path>,
        target_os: OS,
        target: Option<String>,
    ) -> Result {
        if TARGET_OS == OS::MacOS && CSC_KEY_PASSWORD.is_set() {
            // This means that we will be doing code signing on MacOS. This requires JDK environment
            // to be set up.
            let graalvm =
                crate::engine::deduce_graal(self.octocrab.clone(), &self.build_sbt).await?;
            graalvm.install_if_missing(&self.cache).await?;
        }


        crate::web::install(&self.repo_root).await?;
        let pm_bundle = ProjectManagerInfo::new(project_manager)?;
        let client_build = self
            .npm()?
            .set_env(env::ENSO_BUILD_GUI, gui.as_ref())?
            .set_env(env::ENSO_BUILD_IDE, output_path.as_ref())?
            .try_applying(&pm_bundle)?
            .workspace(Workspaces::Enso)
            .run("build")
            .run_ok();

        let icons_dist = TempDir::new()?;
        let icons_build = self.build_icons(&icons_dist);
        let (icons, _content) = try_join(icons_build, client_build).await?;

        let python_path = if TARGET_OS == OS::MacOS && !env::PYTHON_PATH.is_set() {
            // On macOS electron-builder will fail during DMG creation if there is no python2
            // installed. It is looked for in `/usr/bin/python` which is not valid place on newer
            // MacOS versions.
            // We can work around this by setting the `PYTHON_PATH` env variable. We attempt to
            // locate `python2` in PATH which is enough to work on GitHub-hosted macOS
            // runners.
            ide_ci::program::lookup("python2")
                .inspect_err(|e| {
                    // We do not fail, as this requirement might have been lifted by the
                    // electron-builder bump. As for now, we do best effort to support both cases.
                    warn!("Failed to locate python2 in PATH: {e}");
                })
                .ok()
        } else {
            None
        };

        let target_args = match target {
            Some(target) => vec!["--target".to_string(), target],
            None => vec![],
        };

        self.npm()?
            .try_applying(&icons)?
            // .env("DEBUG", "electron-builder")
            .set_env(env::ENSO_BUILD_GUI, gui.as_ref())?
            .set_env(env::ENSO_BUILD_IDE, output_path.as_ref())?
            .set_env(env::ENSO_BUILD_PROJECT_MANAGER, project_manager.as_ref())?
            .set_env_opt(env::PYTHON_PATH, python_path.as_ref())?
            .workspace(Workspaces::Enso)
            // .args(["--loglevel", "verbose"])
            .run("dist")
            .arg("--")
            .arg(target_os_flag(target_os)?)
            .args(target_args)
            .run_ok()
            .await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn download_test() -> Result {
        let temp = TempDir::new()?;
        download_js_assets(temp.path()).await?;
        Ok(())
    }
}
