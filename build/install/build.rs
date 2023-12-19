use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=icon.rc");
    println!("cargo:rerun-if-env-changed=ENSO_BUILD_ICONS");
    if let Ok(icons_path) = std::env::var("ENSO_BUILD_ICONS") {
        let icons_path = Path::new(&icons_path).join("icon.ico");
        // We need to either replace backslashes with forward slashes or escape them, as RC file is
        // kinda-compiled. The former is easier.
        let icons_path = icons_path.to_str().unwrap().replace("\\", "/");
        let contents = format!(r#"ICON_ID ICON "{icons_path}""#);
        std::fs::write("icon.rc", contents).unwrap();
        embed_resource::compile("icon.rc", embed_resource::NONE);
    } else {
        println!("cargo:warning=ENSO_BUILD_ICONS is not set, skipping icon embedding.");
    }
}
