//! Small crate with configuration definitions that can be shared both by the build script and
//! the installer/uninstaller.



pub mod prelude {
    pub use ide_ci::prelude::*;
}

pub mod bundler;
pub mod electron_builder;
pub mod payload;

use prelude::*;

use ide_ci::define_env_var;
use ide_ci::env::accessor::PathBufVariable;
use ide_ci::env::known::cargo::build::OUT_DIR;

/// The filename stem of the installer executable — and the crate name.
pub const INSTALLER_NAME: &str = "enso-installer";

/// The filename stem of the uninstaller executable — and the crate name.
pub const UNINSTALLER_NAME: &str = "enso-uninstaller";

define_env_var! {
    /// Path to the JSON file containing the Electron Builder configuration.
    ///
    /// Unlike [`ENSO_INSTALL_ELECTRON_BUILDER_CONFIG`] this is a full dump of the configuration.
    /// Provided by the enso-build to the `enso-install`'s `build.rs`.
    ENSO_BUILD_ELECTRON_BUILDER_CONFIG, PathBuf;

    /// Path to the JSON file containing the Electron Builder configuration.
    ///
    /// This file is sanitized by the `enso-install`'s `build.rs` to contain only the necessary
    /// information, as defined by the [`electron_builder::Config`] type.
    ///
    /// In general, the installer/unistaller `build.rs` should set this variable, so it can be
    /// embedded into the relevant binary.
    ENSO_INSTALL_ELECTRON_BUILDER_CONFIG, PathBuf;

    /// The path to the `tar.gz` archive containing the Enso IDE payload. Provided by the
    /// enso-build to the enso-installer's `build.rs`, which embeds it into the installer's binary.
    ENSO_INSTALL_ARCHIVE_PATH, PathBuf;

    /// The path to the JSON file containing the metadata of the Enso IDE payload.
    ///
    /// The metadata is modeled by the [`crate::payload::Metadata`] struct.
    ENSO_INSTALL_METADATA_PATH, PathBuf;
}

/// Build-time script (build.rs) that retrieves the electron-builder configuration from the
/// file designated by the [`ENSO_BUILD_ELECTRON_BUILDER_CONFIG`] environment variable.
///
/// This function is intended to be used by the installer/uninstaller's `build.rs`.
pub fn electron_builder_config_from_env() -> Result<electron_builder::Config> {
    let config_path = ENSO_BUILD_ELECTRON_BUILDER_CONFIG.get()?;
    println!("cargo:rerun-if-env-changed={ENSO_BUILD_ELECTRON_BUILDER_CONFIG}");
    println!("cargo:rerun-if-changed={}", config_path.display());
    ide_ci::fs::read_json(config_path)
}

/// Location where the sanitized electron-builder configuration is placed.
///
/// Should be used only `build.rs`-time.
///
/// Runtime installer-dependent crates should embed the file.
pub fn sanitized_electron_builder_config_path() -> Result<PathBuf> {
    Ok(OUT_DIR.get()?.join("electron-builder-config.json"))
}

/// Place electron-builder configuration under the output directory.
///
/// The file is taken from [`ENSO_BUILD_ELECTRON_BUILDER_CONFIG`], sanitized and placed under the
/// output directory. The resulting location will be exposed to the build through the environment
/// variable [`ENSO_INSTALL_ELECTRON_BUILDER_CONFIG`].
///
/// This function is intended to be used by the installer/uninstaller's `build.rs`.
///
/// Returns the parsed configuration.
pub fn sanitize_and_expose_electron_builder_config() -> Result<electron_builder::Config> {
    // We sanitize by parsing into our structure (with only the fields we need) and then dumping
    // back to JSON.
    let config = electron_builder_config_from_env();
    let out_config_path = sanitized_electron_builder_config_path()?;
    if let Ok(config) = &config {
        let json_text = serde_json::to_string_pretty(config)?;
        ide_ci::fs::write_if_different(&out_config_path, json_text)?;
    } else {
        // We write dummy. This is to avoid the build script failing if the config is not available.
        // This allows checking if the installer compiles without involving any electron-builder
        // configuration.
        ide_ci::fs::write_if_different(&out_config_path, "")?;
    }
    ide_ci::programs::cargo::build::expose_env_var(
        ENSO_INSTALL_ELECTRON_BUILDER_CONFIG,
        out_config_path.as_str(),
    );
    ENSO_INSTALL_ELECTRON_BUILDER_CONFIG.set(&out_config_path)?;
    config
}

/// The resource (RCDATA) ID of the Enso installer payload.
///
/// The payload is a `tar.gz` archive containing the Enso IDE.
pub const INSTALLER_PAYLOAD_ID: &str = "INSTALLER_PAYLOAD";

/// A constant that holds the identifier for the Enso icon.
///
/// This identifier is used to reference the icon in the installer payload.
pub const ENSO_ICON_ID: &str = "ENSO_ICON_ID";


/// Resources that can be embedded into the binary.
#[derive(Debug, Clone, Copy)]
pub enum ResourceType {
    /// Icon resource (`.ico` file).
    ///
    /// Windows will automatically use the first embedded icon as the icon for the binary.
    Icon,
    /// Arbitrary binary data.
    Binary,
}

impl Display for ResourceType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ResourceType::Icon => write!(f, "ICON"),
            ResourceType::Binary => write!(f, "RCDATA"),
        }
    }
}


/// Embeds a resource from a file.
///
/// This function is intended to be used by the installer/uninstaller's `build.rs`.
pub fn embed_resource_from_file(
    resource_id: &str,
    resource_type: ResourceType,
    resource_path: &Path,
) -> Result {
    let rc_file = OUT_DIR.get().unwrap().join(resource_id).with_extension("rc");
    println!("cargo:rerun-if-changed={}", rc_file.display());
    // We need to either replace backslashes with forward slashes or escape them, as RC file is
    // kinda-compiled. The former is easier.
    let sanitized_path = resource_path.to_str().unwrap().replace('\\', "/");
    println!("cargo:rerun-if-changed={sanitized_path}");
    let contents = format!(r#"{resource_id} {resource_type} "{sanitized_path}""#);
    ide_ci::fs::write_if_different(&rc_file, contents)?;
    embed_resource::compile(&rc_file, embed_resource::NONE);
    Ok(())
}


/// Embeds a resource using a path from an environment variable.
///
/// It should be preferred over [`embed_resource_from_file`] as it will ensure that the build script
/// is rerun when the environment variable changes.
///
/// This function is intended to be used by the installer/uninstaller's `build.rs`.
pub fn embed_resource_from_env(
    resource_id: &str,
    resource_type: ResourceType,
    env_var: &PathBufVariable,
) -> Result {
    println!("cargo:rerun-if-env-changed={env_var}");
    embed_resource_from_file(resource_id, resource_type, &env_var.get()?)
}
