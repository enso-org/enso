use ide_ci::prelude::*;

use ide_ci::programs::cargo::build::rerun_if_file_changed;



fn main() -> Result {
    rerun_if_file_changed("paths.yaml");
    let yaml_contents = include_bytes!("paths.yaml");
    let code = enso_build_macros_lib::paths::process(yaml_contents.as_slice())?;
    let out_dir = ide_ci::programs::cargo::build_env::OUT_DIR.get()?;
    let out_path = out_dir.join("paths.rs");
    ide_ci::fs::write(&out_path, code.to_string())?;
    assert!(std::process::Command::new("rustfmt").arg(&out_path).status()?.success());
    Ok(())
}
