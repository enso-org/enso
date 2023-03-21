const CONFIG_PATH: &str = "../config.yaml";

const JSON_CONFIG: &[&str] = &[
    "../../../lib/rust/ensogl/pack/js/src/runner/config.json",
    "../../../app/ide-desktop/lib/content-config/src/config.json",
];

fn main() {
    println!("cargo:rerun-if-changed={CONFIG_PATH}");
    println!("cargo:rerun-if-changed=build.rs");
    for path in JSON_CONFIG {
        println!("cargo:rerun-if-changed={path}");
    }

    config_reader::generate_config_module_from_yaml(CONFIG_PATH);
}
