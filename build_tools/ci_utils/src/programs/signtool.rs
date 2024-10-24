//! Utilities for [`signtool`](SignTool) CLI tool.

use crate::prelude::*;



/// The RFC3161-compliant timestamp server used for signing.
pub const TIMESTAMP_SERVER: &str = "http://timestamp.digicert.com";

/// The hash algorithms that can be used for signing.
#[derive(Clone, Copy, Debug)]
pub enum HashAlgorithm {
    /// SHA-1 hash algorithm.
    SHA1,
    /// SHA-256 hash algorithm.
    SHA256,
}

/// Compatible with format expected by the `signtool` utility CLI.
impl AsRef<str> for HashAlgorithm {
    fn as_ref(&self) -> &str {
        match self {
            HashAlgorithm::SHA1 => "SHA1",
            HashAlgorithm::SHA256 => "SHA256",
        }
    }
}

impl AsRef<OsStr> for HashAlgorithm {
    fn as_ref(&self) -> &OsStr {
        AsRef::<str>::as_ref(self).as_ref()
    }
}

/// SignTool utility, being part of the Windows SDK.
///
/// `signtool` can be used to sign executables, verify signatures, or timestamp files.
#[derive(Clone, Copy, Debug)]
pub struct SignTool;

impl Program for SignTool {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &str {
        "signtool"
    }

    fn default_locations(&self) -> Vec<PathBuf> {
        if let Ok(sdk_dir) = locate_windows_sdk() {
            vec![sdk_dir.join("App Certification Kit")]
        } else {
            Vec::new()
        }
    }
}

/// Heuristically locate the Windows SDK.
///
/// The `signtool` utility is part of the Windows SDK, so we need to locate it to use it.
pub fn locate_windows_sdk() -> Result<PathBuf> {
    let program_files = crate::env::known::win::PROGRAMFILES_X86.get()?;
    let sdk_dir = program_files.join_iter(["Windows Kits", "10"]);
    // TODO: If we ever want anything more fancy, we should use the `InstallationFolder` key at
    //       `HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\Microsoft SDKs\Windows\v10.0`
    //       For now we don't need it, and using `winreg` crate is too much trouble cross-platform.
    if sdk_dir.exists() {
        Ok(sdk_dir)
    } else {
        bail!("Windows SDK not found!")
    }
}

/// Sign the given executable with the given certificate.
///
/// The hash algorithm used for signing is SHA-256, as SHA-1 is deprecated and not trusted.
/// The only reason not use SHA-256 would be to target OS versions older than Windows XP SP3, which
/// are not supported anyway.
pub async fn sign(
    exe: impl AsRef<Path>,
    cert: impl AsRef<Path>,
    password: impl AsRef<str>,
) -> Result {
    SignTool
        .cmd()?
        .arg("sign")
        .arg("/f")
        .arg(cert.as_ref())
        .arg("/p")
        .arg(password.as_ref())
        .arg("/fd")
        .arg(HashAlgorithm::SHA256)
        .arg("/tr")
        .arg(TIMESTAMP_SERVER)
        .arg("/td")
        .arg(HashAlgorithm::SHA256)
        .arg(exe.as_ref())
        .run_ok()
        .await
}
