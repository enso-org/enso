use ide_ci::prelude::*;

use enso_install_config::sanitize_and_expose_electron_builder_config;
use enso_install_config::ENSO_INSTALL_ARCHIVE_PATH;
use enso_install_config::ENSO_INSTALL_METADATA_PATH;
use enso_install_config::INSTALLER_PAYLOAD_ID;
use ide_ci::env::known::cargo::build::OUT_DIR;



fn main() {
    let rc_file = OUT_DIR.get().unwrap().join("archive.rc");
    println!("cargo:rerun-if-changed={}", rc_file.display());
    println!("cargo:rerun-if-env-changed={ENSO_INSTALL_ARCHIVE_PATH}");
    if let Ok(archive) = ENSO_INSTALL_ARCHIVE_PATH.get() {
        println!("cargo:rerun-if-changed={}", archive.display());
        // We need to either replace backslashes with forward slashes or escape them, as RC file is
        // kinda-compiled. The former is easier.
        let sanitized_path = archive.as_str().replace('\\', "/");
        let contents = format!(r#"{INSTALLER_PAYLOAD_ID} RCDATA "{sanitized_path}""#);
        ide_ci::fs::write_if_different(&rc_file, contents).unwrap();
        embed_resource::compile(&rc_file, embed_resource::NONE);
    } else {
        println!("cargo:warning={ENSO_INSTALL_ARCHIVE_PATH} is not set, the installer will fail at runtime.");
    }

    println!("cargo:rerun-if-env-changed={ENSO_INSTALL_METADATA_PATH}");
    if !ENSO_INSTALL_METADATA_PATH.is_set() {
        println!("cargo:warning={ENSO_INSTALL_METADATA_PATH} is not set, the installer will fail at runtime.");
        let placeholder_path = OUT_DIR.get().unwrap().join("metadata.json");
        ide_ci::fs::write_if_different(&placeholder_path, "{}").unwrap();
        // Set env for the crate.
        ide_ci::programs::cargo::build::expose_env_var(
            ENSO_INSTALL_METADATA_PATH,
            placeholder_path.as_str(),
        );
    }

    let _ = sanitize_and_expose_electron_builder_config();
}
