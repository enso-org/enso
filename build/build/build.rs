// === Features ===
#![feature(exit_status_error)]

use ide_ci::prelude::*;

use ide_ci::env::expect_var;



fn main() -> Result {
    println!("cargo:rerun-if-changed=paths.yaml");
    let yaml_contents = include_bytes!("paths.yaml");
    let code = ide_ci::paths::process(yaml_contents.as_slice())?;
    let out_dir = expect_var("OUT_DIR")?.parse2::<PathBuf>()?;
    let out_path = out_dir.join("paths.rs");
    ide_ci::fs::write(&out_path, code)?;
    std::process::Command::new("rustfmt").arg(&out_path).status()?.exit_ok()?;
    Ok(())
}
