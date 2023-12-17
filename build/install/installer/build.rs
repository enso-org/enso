fn main() {
    // println!("cargo:rerun-if-changed=archive.rc");

    // Path to the archive that is supposed to be embedded as RC_DATA.
    let path_to_archive = std::env::var("ENSO_INSTALL_ARCHIVE_PATH")
        .unwrap_or_else(|_| r"C:\Users\mwurb\enso-win-2023.2.1-dev.tar.gz".to_string());
    println!("cargo:rerun-if-changed={}", path_to_archive);
    let rc_file_contents = format!(r#"ARCHIVE_ID RCDATA "{path_to_archive}""#);
    std::fs::write("archive.rc", rc_file_contents).unwrap();
    embed_resource::compile_for("archive.rc", ["enso-installer"], embed_resource::NONE);
}
