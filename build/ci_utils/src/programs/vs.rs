use crate::prelude::*;

use crate::define_env_var;
use crate::env;
use crate::programs::cmd;
use crate::programs::vswhere::VsWhere;


// ==============
// === Export ===
// ==============

pub mod redist;



/// Path components from [VS installation
/// root](crate::programs::vswhere::InstanceInfo::installation_path) to the developer command file.
/// See: <https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line?view=msvc-170#developer_command_file_locations>
pub const VC_VARS_ALL_PATH: [&str; 4] = ["VC", "Auxiliary", "Build", "vcvarsall.bat"];

/// Platforms, as being host/target of MSVC toolchain.
///
/// Can be used as an argument to `vcvarsall.bat`.
#[derive(Clone, Copy, Debug, strum::Display, strum::EnumString)]
pub enum Platforms {
    X64,
    X86,
    // In theory there's also ARM64 platform but we don't support it for Windows at the moment.
}

impl Platforms {
    /// Get the native platform for the local system (host).
    pub fn local() -> Result<Self> {
        Ok(match TARGET_ARCH {
            Arch::X86_64 => Platforms::X64,
            Arch::X86 => Platforms::X86,
            _ => bail!("Unsupported target architecture: {}.", TARGET_ARCH),
        })
    }
}

impl From<Platforms> for Arch {
    fn from(platform: Platforms) -> Self {
        match platform {
            Platforms::X64 => Arch::X86_64,
            Platforms::X86 => Arch::X86,
        }
    }
}

define_env_var! {
    /// Target platform architecture. Expected to be the same as `VSCMD_ARG_TGT_ARCH`.
    Platform, Platforms;

    /// Location with MSVC CRT redistributables.
    ///
    /// E.g. `%VCINSTALLDIR%Redist\MSVC\x.y.z`.
    VCToolsRedistDir, PathBuf;

    /// The telemetry introduces undesired dependency on Power Shell.
    VSCMD_SKIP_SENDTELEMETRY, bool;
}

/// Microsoft C/C++ Optimizing compiler.
///
/// A possible component of Microsoft Visual Studio IDE, or part of the self-contained Microsoft
/// Visual C++ Build Tools.
#[derive(Clone, Copy, Debug)]
pub struct Cl;

impl Program for Cl {
    fn executable_name(&self) -> &'static str {
        "cl"
    }
}

/// Get the path to the `vcvarsall.bat` for the local VS installation.
///
/// Relies on `vswhere` to find installed VS instances.
pub async fn vc_vars_all_path() -> Result<PathBuf> {
    let msvc = VsWhere::msvc().await?;
    // See the official documentation:
    // https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line#developer_command_file_locations
    Ok(msvc.installation_path.join_iter(VC_VARS_ALL_PATH))
}

/// Capture changes in environment variables that are introduced by `vcvarsall.bat`.
///
/// The `vcvarsall.bat` script is part of Microsoft Visual C++ Build Tools. It is used to set up
/// environment variables for the MSVC compiler toolchain.
///
/// This function requires that [`vswhere`](VsWhere) is available in the `PATH`. It will be used to
/// locate installations of MSVC Build Tools / compiler.
pub async fn retrieve_dev_environment() -> Result<Vec<env::Modification>> {
    let path = vc_vars_all_path().await?;
    let platform = Platforms::local()?;

    cmd::compare_env(|command| {
        command
            .arg(path)
            .arg(platform.to_string())
            .set_env(VSCMD_SKIP_SENDTELEMETRY, &true)
            // Safety: this can only fail if `true` fails to pretty print, which is not possible.
            .unwrap()
    })
    .await
}

/// Modifies the environment of the current process, as if `vcvarsall.bat` was executed.
pub async fn apply_dev_environment() -> Result {
    let modifications = retrieve_dev_environment().await?;
    for modification in modifications {
        modification.apply()?;
    }
    Ok(())
}

/// A major Visual Studio version.
///
/// Serialization follows the VS Where `productLineVersion` format.
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum Version {
    #[serde(rename = "2017")]
    VS2017,
    #[serde(rename = "2019")]
    VS2019,
    #[serde(rename = "2022")]
    VS2022,
}
