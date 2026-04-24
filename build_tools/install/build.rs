//! The build script of crate that is dependency of both the installer and the uninstaller.
//!
//! Currently, it is used to include the resources (icons, manifests, version information) into the
//! installer and uninstaller binaries.

use enso_install_config::prelude::*;

#[cfg(windows)]
use enso_install_config::ENSO_ICON_ID;
#[cfg(windows)]
use enso_install_config::ResourceType;
#[cfg(windows)]
use enso_install_config::embed_resource_from_file;
use enso_install_config::sanitize_and_expose_electron_builder_config;
use ide_ci::programs::cargo::build_env::OUT_DIR;

fn main() {
    setup_logging().ok();

    // Icon path can be provided via environment variable (for Bazel builds) or falls back to
    // the default relative path (for Cargo builds).
    #[cfg(windows)]
    {
        let icon_path = std::env::var("ENSO_ICON_PATH").map(PathBuf::from).unwrap_or_else(|_| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../../app/electron-client/assets/icons/icon.ico")
        });
        // Ignore error, not compiling the icon is not a big deal, especially if we compile to check,
        // to not generate final package. (Obtaining icon is a bit of a hassle, as it is generated
        // temporarily by the enso-build.)
        if let Err(err) = embed_resource_from_file(ENSO_ICON_ID, ResourceType::Icon, &icon_path) {
            // We do not use `cargo:warning` here, as we do not want to pollute the output if the
            // icon is not available. Still, to enable debugging, we print to stderr, which is
            // captured by the cargo and stored in `target/debug/build/<pkg>/output`.
            eprintln!("Failed to embed icon: {err:?}");
        }
    }

    let config = sanitize_and_expose_electron_builder_config();
    if let Ok(config) = config {
        // Content on the rc file with the version information.
        let version_rc = format!(
            r#"
#include "winres.h"
#define IDS_VERSION_INFO 1

VS_VERSION_INFO VERSIONINFO
FILEVERSION {major},{minor},{patch},0
PRODUCTVERSION {major},{minor},{patch},0
FILEOS 0x4
FILETYPE 0x1
BEGIN
    BLOCK "StringFileInfo"
    BEGIN
        BLOCK "040904e4"
        BEGIN
            VALUE "CompanyName", "{company}"
            VALUE "FileVersion", "{version}"
            VALUE "LegalCopyright", "{copyright}"
            VALUE "ProductName", "{name}"
            VALUE "ProductVersion", "{version}"
        END
    END
    BLOCK "VarFileInfo"
    BEGIN
        VALUE "Translation", 0x409, 1252
    END
END
"#,
            major = config.extra_metadata.version.major,
            minor = config.extra_metadata.version.minor,
            patch = config.extra_metadata.version.patch,
            company = config.copyright,
            name = config.product_name,
            version = config.extra_metadata.version,
            copyright = config.copyright
        );
        let path = OUT_DIR.get().unwrap().join("version.rc");
        ide_ci::fs::write_if_different(&path, version_rc).unwrap();
        embed_resource::compile(&path, embed_resource::NONE);
    }

    // Embed the manifest file.
    // Necessary to avoid the issue with `GetWindowSubclass`. See:
    // * https://github.com/gabdube/native-windows-gui/issues/251
    // * https://github.com/microsoft/windows-rs/issues/1294
    //
    // Manifest path can be provided via environment variable (for Bazel builds) or falls back to
    // the default relative path (for Cargo builds).
    let manifest_path = std::env::var("ENSO_MANIFEST_PATH")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("enso-install.manifest"));
    assert!(manifest_path.exists(), "Manifest file does not exist: {}", manifest_path.display());
    let rc_path = OUT_DIR.get().unwrap().join("manifest.rc");
    ide_ci::fs::write_if_different(
        &rc_path,
        format!("#define RT_MANIFEST 24\n1 RT_MANIFEST \"{}\"", manifest_path.display()),
    )
    .unwrap();
    embed_resource::compile(&rc_path, embed_resource::NONE);
}
