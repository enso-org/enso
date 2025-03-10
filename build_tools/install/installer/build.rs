//! The build script of the installer.
//!
//! It is used to compile in the binary payload and metadata about it into the installer binary.
//!
//! Note that other resources (icons, manifests, version information) are already included in the
//! installer binary by the `enso-install` library crate.

use ide_ci::prelude::*;

use enso_install_config::sanitize_and_expose_electron_builder_config;
use enso_install_config::ENSO_INSTALL_ARCHIVE_PATH;
use enso_install_config::ENSO_INSTALL_METADATA_PATH;
use enso_install_config::INSTALLER_PAYLOAD_ID;
use ide_ci::env::known::cargo::build::OUT_DIR;
use ide_ci::programs::cargo;



fn main() {
    let rc_file = OUT_DIR.get().unwrap().join("archive.rc");
    cargo::build::rerun_if_file_changed(&rc_file);

    cargo::build::rerun_if_env_changed(ENSO_INSTALL_ARCHIVE_PATH);
    if let Ok(archive) = ENSO_INSTALL_ARCHIVE_PATH.get() {
        cargo::build::rerun_if_file_changed(archive.as_str());
        // We need to either replace backslashes with forward slashes or escape them, as RC file is
        // kinda-compiled. The former is easier.
        let sanitized_path = archive.as_str().replace('\\', "/");
        let contents = format!(r#"{INSTALLER_PAYLOAD_ID} RCDATA "{sanitized_path}""#);
        ide_ci::fs::write_if_different(&rc_file, contents).unwrap();
        embed_resource::compile(&rc_file, embed_resource::NONE);
    } else {
        println!("cargo:warning={ENSO_INSTALL_ARCHIVE_PATH} is not set, the installer will fail at runtime.");
    }

    cargo::build::rerun_if_env_changed(ENSO_INSTALL_METADATA_PATH);
    if !ENSO_INSTALL_METADATA_PATH.is_set() {
        println!("cargo:warning={ENSO_INSTALL_METADATA_PATH} is not set, the installer will fail at runtime.");
        let placeholder_path = OUT_DIR.get().unwrap().join("metadata.json");
        ide_ci::fs::write_if_different(&placeholder_path, "{}").unwrap();
        // Set env for the crate.
        cargo::build::expose_env_var(ENSO_INSTALL_METADATA_PATH, placeholder_path.as_str());
    }

    let _ = sanitize_and_expose_electron_builder_config();
}
