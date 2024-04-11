//! Code for dealing with JS/TS components of the GUI1 and the Electron client (IDE).

use crate::prelude::*;

use crate::paths::generated;
use crate::project::gui::BuildInfo;
use crate::project::IsArtifact;

use crate::version::ENSO_VERSION;
use anyhow::Context;
use enso_install_config::INSTALLER_NAME;
use enso_install_config::UNINSTALLER_NAME;
use ide_ci::io::download_all;
use ide_ci::program::command::FallibleManipulator;
use ide_ci::programs::cargo;
use ide_ci::programs::node::NpmCommand;
use ide_ci::programs::Cargo;
use ide_ci::programs::Npm;
use std::io::Write;
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
        let output_path = output_path.as_ref();
        let build_windows_enso_install = TARGET_OS == OS::Windows;
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
        self.npm()?
            .set_env(env::ENSO_BUILD_GUI, gui.as_ref())?
            .set_env(env::ENSO_BUILD_IDE, output_path)?
            .try_applying(&pm_bundle)?
            .workspace(Workspaces::Enso)
            .run("build")
            .run_ok()
            .await?;

        let icons_dist = TempDir::new()?;
        let icons_dist = icons_dist.into_path();
        let icons_build = self.build_icons(&icons_dist);
        let icons = icons_build.await?;

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
            .set_env(env::ENSO_BUILD_IDE, output_path)?
            .set_env(env::ENSO_BUILD_PROJECT_MANAGER, project_manager.as_ref())?
            .set_env_opt(env::PYTHON_PATH, python_path.as_ref())?
            .set_env(enso_install_config::ENSO_BUILD_ELECTRON_BUILDER_CONFIG, &electron_config)?
            .workspace(Workspaces::Enso)
            // .args(["--loglevel", "verbose"])
            .run("dist")
            .arg("--")
            .arg(target_os_flag(target_os)?)
            .args(target_args)
            .run_ok()
            .await?;

        if build_windows_enso_install {
            Cargo
                .cmd()?
                .set_env(enso_install_config::ENSO_BUILD_ELECTRON_BUILDER_CONFIG, &electron_config)?
                .current_dir(&self.repo_root)
                .apply(&cargo::Command::Build)
                .try_applying(&icons)?
                .arg("--release")
                .arg("--package")
                .arg("enso-uninstaller")
                .arg("-Z")
                .arg("unstable-options")
                .arg("--out-dir")
                .arg(output_path)
                .run_ok()
                .await?;

            let code_signing_certificate =
                WindowsSigningCredentials::new_from_env().await.inspect_err(|e| {
                    warn!("Failed to create code signing certificate from the environment: {e:?}");
                });

            let uninstaller_source = output_path.join(UNINSTALLER_NAME).with_executable_extension();
            if let Ok(certificate) = code_signing_certificate.as_ref() {
                certificate.sign(&uninstaller_source).await?;
            }

            let unpacked_dir = output_path.join("win-unpacked");
            ide_ci::fs::tokio::copy_to(&uninstaller_source, &unpacked_dir).await?;

            let archive_path = output_path.join("enso-win.tar.gz");
            ide_ci::archive::compress_directory_contents(&archive_path, &unpacked_dir).await?;
            Cargo
                .cmd()?
                .current_dir(&self.repo_root)
                .apply(&cargo::Command::Build)
                .try_applying(&icons)?
                .set_env(enso_install_config::ENSO_INSTALL_ARCHIVE_PATH, &archive_path)?
                .set_env(enso_install_config::ENSO_BUILD_ELECTRON_BUILDER_CONFIG, &electron_config)?
                .arg("--release")
                .arg("--package")
                .arg("enso-installer")
                .arg("-Z")
                .arg("unstable-options")
                .arg("--out-dir")
                .arg(output_path)
                .run_ok()
                .await?;
            let installer_path = output_path.join(INSTALLER_NAME).with_executable_extension();
            let installer_desired_filename = format!("enso-win-{}.exe", ENSO_VERSION.get()?);
            let installer_desired_path = output_path.join(&installer_desired_filename);
            ide_ci::fs::tokio::remove_file_if_exists(&installer_desired_path).await?;
            ide_ci::fs::tokio::rename(&installer_path, &installer_desired_path).await?;
            if let Ok(certificate) = code_signing_certificate.as_ref() {
                certificate.sign(&installer_desired_path).await?;
            }
            // let artifact_name = format!("enso-win-{}.exe", ENSO_VERSION.get()?);
            // if ide_ci::actions::workflow::is_in_env() {
            //     ide_ci::actions::artifacts::upload_single_file(&installer_desired_path,
            // artifact_name)         .await?;
            // }
        }

        Ok(())
    }
}

/// CSC (Code Signing Certificate) link.
#[derive(Clone, Debug)]
pub enum CscLink {
    /// Local path to the certificate file.
    FilePath(PathBuf),
    /// HTTPS link to the certificate file.
    Url(Url),
    /// The certificate file contents.
    Data(Vec<u8>),
}

impl std::str::FromStr for CscLink {
    type Err = anyhow::Error;

    #[context("Failed to parse CSC link from '{csc_link}'.")]
    fn from_str(csc_link: &str) -> Result<Self> {
        let csc_link = csc_link.trim();
        if let Some(file_path) = csc_link.strip_prefix("file://") {
            Ok(Self::FilePath(file_path.into()))
        } else if let Some(url) = csc_link.strip_prefix("https://") {
            Ok(Self::Url(url.parse()?))
        } else if csc_link.len() > 2048 {
            let contents =
                base64::decode(csc_link).context("Failed to decode base64-encoded CSC link.")?;
            Ok(Self::Data(contents))
        } else {
            Ok(Self::FilePath(csc_link.into()))
        }
    }
}

impl CscLink {
    /// Create a new certificate file from the environment variable.
    pub fn new_from_env() -> Result<Self> {
        let csc_link = env::WIN_CSC_LINK.get().or_else(|_| env::CSC_LINK.get())?;
        Self::from_str(&csc_link)
    }
}

/// CSC certificate file to be used for signing the Windows build.
#[derive(Debug)]
pub enum CodeSigningCertificate {
    /// Local certificate file.
    FilePath(PathBuf),
    /// Temporarily created certificate file.
    TempFile(tempfile::TempPath),
}

impl AsRef<Path> for CodeSigningCertificate {
    fn as_ref(&self) -> &Path {
        match self {
            Self::FilePath(path) => path.as_ref(),
            Self::TempFile(path) => path.as_ref(),
        }
    }
}

impl CodeSigningCertificate {
    /// Create a new certificate file from the given link.
    pub async fn new(link: CscLink) -> Result<Self> {
        let ret = match link {
            CscLink::FilePath(path) => Self::FilePath(path),
            CscLink::Url(url) => {
                let temp_file = tempfile::NamedTempFile::new()?.into_temp_path();
                ide_ci::io::web::download_file(url, &temp_file).await?;
                Self::TempFile(temp_file)
            }
            CscLink::Data(contents) => {
                let temp_file = tempfile::NamedTempFile::new()?;
                temp_file.as_file().write_all(&contents)?;
                // temp_file.write_all(&contents)?;
                Self::TempFile(temp_file.into_temp_path())
            }
        };
        Ok(ret)
    }

    /// Create a new certificate file from the environment variable.
    pub async fn new_from_env() -> Result<Self> {
        let csc_link = CscLink::new_from_env()?;
        Self::new(csc_link).await
    }
}

/// Data needed to sign the binaries on Windows.
#[derive(Debug)]
pub struct WindowsSigningCredentials {
    pub certificate: CodeSigningCertificate,
    pub password:    String,
}

impl WindowsSigningCredentials {
    /// Create a new certificate file from the environment variable.
    pub async fn new_from_env() -> Result<Self> {
        let certificate = CodeSigningCertificate::new_from_env().await?;
        let password = env::WIN_CSC_KEY_PASSWORD.get().or_else(|_| env::CSC_KEY_PASSWORD.get())?;
        Ok(Self { certificate, password })
    }

    /// Sign the given binary.
    pub async fn sign(&self, exe: impl AsRef<Path>) -> Result {
        ide_ci::programs::signtool::sign(exe, self.certificate.as_ref(), &self.password).await
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
