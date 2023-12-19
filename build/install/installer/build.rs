fn main() {
    println!("cargo:rerun-if-changed=archive.rc");
    println!("cargo:rerun-if-env-changed=ENSO_INSTALL_ARCHIVE_PATH");
    if let Ok(archive) = std::env::var("ENSO_INSTALL_ARCHIVE_PATH") {
        // We need to either replace backslashes with forward slashes or escape them, as RC file is
        // kinda-compiled. The former is easier.
        let contents = format!(r#"ARCHIVE_ID RCDATA "{}""#, archive.replace("\\", "/"));
        std::fs::write("archive.rc", contents).unwrap();
        embed_resource::compile("archive.rc", embed_resource::NONE);
    } else {
        println!("cargo:warning=ENSO_INSTALL_ARCHIVE_PATH is not set, skipping icon embedding.");
    }
}
