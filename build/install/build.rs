//! This build script compiles the icon into the binary.
//!
//! Thanks to this both the installer and the uninstaller have the icon embedded (as both link to
//! this crate).
//!
//! Currently only supported on Windows, on other platforms this is effectively a no-op.

use enso_install_config::prelude::*;

use enso_install_config::embed_resource_from_file;
use enso_install_config::sanitize_and_expose_electron_builder_config;
use enso_install_config::ResourceType;
use enso_install_config::ENSO_ICON_ID;
use ide_ci::programs::cargo::build_env::OUT_DIR;



/// Build-script-time function that tries to embed the icon into the binary.
///
/// Works only on Windows, on other platforms this is effectively a no-op.
fn try_embedding_icon() -> Result {
    let config = enso_install_config::electron_builder_config_from_env()?;
    embed_resource_from_file(ENSO_ICON_ID, ResourceType::Icon, &config.win.icon)
}

fn main() {
    // Ignore error, not compiling the icon is not a big deal, especially if we compile to check, to
    // not generate final package. (Obtaining icon is a bit of a hassle, as it is generated
    // temporarily by the enso-build.)
    if let Err(err) = try_embedding_icon() {
        // We do not use `cargo:warning` here, as we do not want to pollute the output if the icon
        // is not available. Still, to enable debugging, we print to stderr, which is captured by
        // the cargo and stored in `target/debug/build/<pkg>/output`.
        eprintln!("Failed to embed icon: {err:?}");
    }

    let config = sanitize_and_expose_electron_builder_config();
    if let Ok(config) = config {
        // Content on the rc file with the version information.
        let version_rc = format!(
            r#"
#include "winres.h"
#define IDS_VERSION_INFO 1

VS_VERSION_INFO VERSIONINFO
FILEFLAGSMASK 0x3fL
FILEFLAGS 0x0L
FILEOS 0x40004L
FILETYPE 0x1L
FILESUBTYPE 0x0L
BEGIN
    BLOCK "StringFileInfo"
    BEGIN
        BLOCK "040904e4"
        BEGIN
            VALUE "CompanyName", "{company}"
            VALUE "FileVersion", "{version}"
            VALUE "ProductName", "{name}"
            VALUE "ProductVersion", "{version}"
        END
    END
END
"#,
            company = config.copyright,
            name = config.product_name,
            version = config.extra_metadata.version
        );
        let path = OUT_DIR.get().unwrap().join("version.rc");
        ide_ci::fs::write(&path, version_rc).unwrap();
        embed_resource::compile(&path, embed_resource::NONE);
    }
}
