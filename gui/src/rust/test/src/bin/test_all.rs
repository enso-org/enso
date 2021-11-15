#![feature(option_result_contains)]

use std::path::Path;
use std::path::PathBuf;

/// List of workspace members that should not be tested by wasm-pack test.
/// (e.g. because they do not target wasm at all)
const PACKAGE_BLACKLIST: [&str; 3] =
    ["gui/src/rust/test", "gui/src/rust/ide/file-manager/mock-server", "lib/rust/build-utils"];

/// Attributes that denote WASM tests.
const WASM_TEST_ATTRIBUTES: [&str; 2] = ["#[wasm_bindgen_test]", "#[wasm_bindgen_test(async)]"];

/// Subdirectories in the crate directory that contain sources for the crate.
const SOURCE_SUBDIRECTORIES: [&str; 4] = ["src", "benches", "examples", "tests"];

/// Lists members of given Cargo.toml workspace.
fn get_workspace_members(cargo_toml_root: toml::Value) -> Vec<String> {
    let workspace = cargo_toml_root.get("workspace").expect("not a workspace");
    match &workspace["members"] {
        toml::Value::Array(list) => list
            .iter()
            .map(|val| match val {
                toml::Value::String(s) => s.clone(),
                _ => panic!("Workspace member is not a string"),
            })
            .collect(),
        _ => panic!("Invalid workspace element"),
    }
}

/// Parses file under given path as TOML value.
fn parse_toml(path: impl AsRef<Path>) -> toml::Value {
    let path = path.as_ref();
    let data = std::fs::read_to_string(path).unwrap();
    data.parse().unwrap()
}

///  Check if the given line of source code is an attribute denoting wasm test.
fn is_wasm_test_attribute(line: &str) -> bool {
    WASM_TEST_ATTRIBUTES.contains(&line.trim())
}

/// Check if the given workspace member contains any wasm tests in the sources.
fn has_wasm_tests(member: &str) -> bool {
    // We go over selected subdirectories only to avoid entering into sources of other crates that
    // are nested within this crate subtree.
    for subdir in SOURCE_SUBDIRECTORIES {
        let pattern = format!("{}/{}/**/*.rs", member, subdir);
        for entry in glob::glob(&pattern).unwrap() {
            let contents = std::fs::read_to_string(entry.unwrap()).unwrap();
            if contents.lines().any(is_wasm_test_attribute) {
                return true;
            }
        }
    }
    false
}

/// Checks if the given member is blacklisted from running the tests.
fn blacklisted(memeber: &str) -> bool {
    PACKAGE_BLACKLIST.contains(&memeber)
}

/// Checks if for the given workspace member wasm-pack test should be run.
fn to_be_tested(member: &str) -> bool {
    has_wasm_tests(member) && !blacklisted(member) && !is_proc_macro_crate(member)
}

/// Checks if given workspace member is a proc-macro crate.
fn is_proc_macro_crate(member: &str) -> bool {
    let cargo_toml_path = PathBuf::from(member).join("Cargo.toml");
    let cargo_toml_root = parse_toml(cargo_toml_path);
    get_proc_macro(cargo_toml_root).contains(&true)
}

/// Retrieve a `lib.proc-macro` field from Cargo.toml
fn get_proc_macro(cargo_toml: toml::Value) -> Option<bool> {
    cargo_toml.get("lib")?.get("proc-macro")?.as_bool()
}

/// Call wasm-pack test for each workspace member
///
/// This function reads workspace members list from `Cargo.toml` in current directory, and call
/// `wasm-pack test` each member. All script arguments are passed to `wasm-pack` process.
fn main() {
    let wasm_pack_args = std::env::args().skip(1).collect::<Vec<_>>();
    let cargo_toml_root = parse_toml("Cargo.toml");
    let all_members = get_workspace_members(cargo_toml_root);
    let tested_members = all_members.iter().filter(|p| to_be_tested(p));

    for member in tested_members {
        println!("Running tests for {}", member);
        let mut command = std::process::Command::new("wasm-pack");
        command.arg("test").args(&wasm_pack_args).arg(&member);
        println!("{:?}", command);
        let status = command.status().unwrap();
        if !status.success() {
            panic!("Process for {} failed!{}", member, match status.code() {
                Some(code) => format!(" Code: {}", code),
                None => String::new(),
            });
        }
    }
}
