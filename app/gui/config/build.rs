const CONFIG_PATH: &str = "../config.yaml";

fn main() {
    println!("cargo:rerun-if-changed={CONFIG_PATH}");
    println!("cargo:rerun-if-changed=build.rs");

    config_reader::generate_config_module_from_yaml(CONFIG_PATH);
}
