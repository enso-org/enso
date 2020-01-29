#![feature(option_result_contains)]

use std::path::PathBuf;
use std::path::Path;

const BUILD_UTILITIES_DIR : &str = "build-utilities";

/// Lists members of given Cargo.toml workspace.
fn get_workspace_members(cargo_toml_root:toml::Value) -> Vec<String> {
    let workspace = cargo_toml_root.get("workspace").expect("not a workspace");
    match &workspace["members"] {
        toml::Value::Array(list) => list.iter().map(|val| {
            match val {
                toml::Value::String(s) => s.clone(),
                _ => panic!("Workspace member is not a string"),
            }
        }).collect(),
        _ => panic!("Invalid workspace element")
    }
}

/// Parses file under given path as TOML value.
fn parse_toml(path:impl AsRef<Path>) -> toml::Value {
    let path = path.as_ref();
    let data = std::fs::read_to_string(path).unwrap();
    data.parse().unwrap()
}

/// Checks if for the given workspace member wasm-pack test should be run.
fn to_be_tested(member:&str) -> bool {
    let is_build_util = member.starts_with(BUILD_UTILITIES_DIR);
    !is_build_util && !is_proc_macro_crate(member)
}

/// Checks if given workspace member is a proc-macro crate.
fn is_proc_macro_crate(member:&str) -> bool {
    let cargo_toml_path = PathBuf::from(member).join("Cargo.toml");
    let cargo_toml_root = parse_toml(cargo_toml_path);
    get_proc_macro(cargo_toml_root).contains(&true)
}

/// Retrieve a `lib.proc-macro` field from Cargo.toml
fn get_proc_macro(cargo_toml:toml::Value) -> Option<bool> {
    cargo_toml.get("lib")?.get("proc-macro")?.as_bool()
}

/// Call wasm-pack test for each workspace member
///
/// This function reads workspace members list from `Cargo.toml` in current
/// directory, and call `wasm-pack test` each member. All script arguments
/// are passed to `wasm-pack` process.
fn main() {
    let wasm_pack_args  = std::env::args().skip(1).collect::<Vec<_>>();
    let cargo_toml_root = parse_toml("Cargo.toml");
    let all_members     = get_workspace_members(cargo_toml_root);
    let tested_members  = all_members.iter().filter(|p| to_be_tested(&p));

    for member in tested_members {
        println!("Running tests for {}:", member);
        let status = std::process::Command::new("wasm-pack")
            .arg("test")
            .arg(&member)
            .args(&wasm_pack_args)
            .status()
            .unwrap();
        if !status.success() {
            panic!("Process for {} failed!{}", member, match status.code() {
                Some(code) => format!(" Code: {}", code),
                None => String::new()
            });
        }
    }
}
