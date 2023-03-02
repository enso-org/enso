// === Features ===
#![feature(exit_status_error)]

use ide_ci::prelude::*;



fn main() -> Result {
    println!("cargo:rerun-if-changed=paths.yaml");
    let yaml_contents = include_bytes!("paths.yaml");
    let code = enso_build_macros_lib::paths::process(yaml_contents.as_slice())?;
    let out_dir = ide_ci::programs::cargo::build_env::OUT_DIR.get()?;
    let out_path = out_dir.join("paths.rs");
    ide_ci::fs::write(&out_path, code.to_string())?;
    std::process::Command::new("rustfmt").arg(&out_path).status()?.exit_ok()?;
    Ok(())
}
