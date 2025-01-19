//! Environment variables used by the Electron Builder.

use crate::prelude::*;

use crate::define_env_var;

use base64::Engine;
use std::io::Write;



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

    /// Note that enabling CSC_FOR_PULL_REQUEST can pose serious security risks. Refer to the
    /// [CircleCI documentation](https://circleci.com/docs/1.0/fork-pr-builds/) for more
    /// information. If the project settings contain SSH keys, sensitive environment variables,
    /// or AWS credentials, and untrusted forks can submit pull requests to your repository, it
    /// is not recommended to enable this option.
    CSC_FOR_PULL_REQUEST, bool;
}

/// Environment variables set from CI-provided secrets that allow code signing.
///
/// These variables might be set to empty strings if the secrets are not available in the CI.
pub const CI_CSC_SECRETS: &[&str] = &[
    WIN_CSC_LINK.name,
    WIN_CSC_KEY_PASSWORD.name,
    CSC_LINK.name,
    CSC_KEY_PASSWORD.name,
    APPLEID.name,
    APPLEIDPASS.name,
    APPLETEAMID.name,
];

/// CSC (Code Signing Certificate) link.
///
/// This models the way Electron Builder uses to recieve the certificate file.
#[derive(Clone, Debug)]
pub enum CscLink {
    /// Local path to the certificate file.
    FilePath(PathBuf),
    /// HTTPS link to the certificate file.
    Url(Url),
    /// The certificate file contents.
    Data(Vec<u8>),
}

impl FromStr for CscLink {
    type Err = anyhow::Error;

    #[context("Failed to parse CSC link from '{csc_link}'.")]
    fn from_str(csc_link: &str) -> Result<Self> {
        let csc_link = csc_link.trim();
        if let Some(file_path) = csc_link.strip_prefix("file://") {
            Ok(Self::FilePath(file_path.into()))
        } else if let Some(url) = csc_link.strip_prefix("https://") {
            Ok(Self::Url(url.parse()?))
        } else if csc_link.len() > 2048 {
            let contents = base64::engine::general_purpose::STANDARD
                .decode(csc_link)
                .context("Failed to decode base64-encoded CSC link.")?;
            Ok(Self::Data(contents))
        } else {
            Ok(Self::FilePath(csc_link.into()))
        }
    }
}

impl CscLink {
    /// Create a new certificate file from the environment variable.
    pub fn new_from_env() -> Result<Self> {
        let csc_link = WIN_CSC_LINK.get().or_else(|_| CSC_LINK.get())?;
        // When secret is not available, we might get a variable with an empty value.
        ensure!(!csc_link.is_empty(), "CSC link is empty.");
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
                crate::io::web::download_file(url, &temp_file).await?;
                Self::TempFile(temp_file)
            }
            CscLink::Data(contents) => {
                let temp_file = tempfile::NamedTempFile::new()?;
                temp_file.as_file().write_all(&contents)?;
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
    /// Code signing certificate file.
    pub certificate: CodeSigningCertificate,
    /// Password to the certificate.
    pub password:    String,
}

impl WindowsSigningCredentials {
    /// Create a new certificate file from the environment variable.
    pub async fn new_from_env() -> Result<Self> {
        let certificate = CodeSigningCertificate::new_from_env().await?;
        let password = WIN_CSC_KEY_PASSWORD.get().or_else(|_| CSC_KEY_PASSWORD.get())?;
        Ok(Self { certificate, password })
    }

    /// Sign the given binary.
    pub async fn sign(&self, exe: impl AsRef<Path>) -> Result {
        crate::programs::signtool::sign(exe, self.certificate.as_ref(), &self.password).await
    }
}
