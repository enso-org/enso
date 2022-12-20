use crate::project::*;

use anyhow::Context;
use ide_ci::programs::cargo;
use ide_ci::programs::wasm_pack;
use ide_ci::programs::WasmPack;



/// Flag that tells wasm-pack which browser should be used to run tests.
///
/// We use here Firefox rather than Chrome, because it is more stable. Chrome for some reason has
/// been randomly failing tests, both locally and on CI.
pub const BROWSER_FOR_WASM_TESTS: wasm_pack::TestFlags = wasm_pack::TestFlags::Firefox;

/// List of crates that should not be tested by wasm-pack test.
pub const PACKAGE_BLACKLIST: [&str; 1] = ["integration-test"];

/// Attributes that denote WASM tests.
pub const WASM_TEST_ATTRIBUTES: [&str; 2] = ["#[wasm_bindgen_test]", "#[wasm_bindgen_test(async)]"];

/// Subdirectories in the crate directory that contain sources for the crate.
pub const SOURCE_SUBDIRECTORIES: [&str; 4] = ["src", "benches", "examples", "tests"];

/// Lists members of given Cargo.toml workspace.
pub fn get_all_crates(repo_root: impl AsRef<Path>) -> Result<Vec<PathBuf>> {
    let pattern = repo_root.as_ref().join("**/Cargo.toml");
    let all_paths =
        glob::glob(pattern.as_str()).context(format!("Globbing {} failed", pattern.display()))?;
    let valid_paths = all_paths.filter_map(|path| match path {
        // FIXME explain unwrap
        Ok(path) => Some(path.parent().unwrap().to_owned()),
        Err(err) => {
            error!("{err}");
            None
        }
    });
    Ok(valid_paths.collect())
}

///  Check if the given line of source code is an attribute denoting wasm test.
pub fn is_wasm_test_attribute(line: &str) -> bool {
    WASM_TEST_ATTRIBUTES.contains(&line.trim())
}

/// Check if the given workspace member contains any wasm tests in the sources.
pub fn has_wasm_tests(member: &Path) -> bool {
    if let Some(member) = member.to_str() {
        // We go over selected subdirectories only to avoid entering into sources of other crates
        // that are nested within this crate subtree.
        for subdir in SOURCE_SUBDIRECTORIES {
            let pattern = format!("{}/{}/**/*.rs", member, subdir);
            for entry in glob::glob(&pattern).unwrap() {
                let contents = ide_ci::fs::read_to_string(entry.unwrap()).unwrap();
                if contents.lines().any(is_wasm_test_attribute) {
                    return true;
                }
            }
        }
        false
    } else {
        println!(
            "cargo:warning=Skipping the crate {} containing non-UTF-8 characters in its path. ",
            member.to_string_lossy()
        );
        false
    }
}

/// Parses file under given path as TOML value.
pub fn parse_toml(path: impl AsRef<Path>) -> toml::Value {
    let path = path.as_ref();
    let data = ide_ci::fs::read_to_string(path).unwrap();
    data.parse().unwrap()
}

/// Checks if the given member is blacklisted from running the tests.
pub fn blacklisted(memeber: &Path) -> bool {
    PACKAGE_BLACKLIST.iter().any(|blacklisted| memeber.ends_with(blacklisted))
}

/// Checks if given workspace member is a proc-macro crate.
pub fn is_proc_macro_crate(member: &Path) -> bool {
    let cargo_toml_path = member.join("Cargo.toml");
    let cargo_toml_root = parse_toml(cargo_toml_path);
    get_proc_macro(cargo_toml_root).contains(&true)
}

/// Retrieve a `lib.proc-macro` field from Cargo.toml
pub fn get_proc_macro(cargo_toml: toml::Value) -> Option<bool> {
    cargo_toml.get("lib")?.get("proc-macro")?.as_bool()
}

pub async fn test_all(repo_root: PathBuf) -> Result {
    // FIXME args
    //let wasm_pack_args = std::env::args().skip(1).collect::<Vec<_>>();
    let all_members = get_all_crates(&repo_root)?;

    for member in all_members {
        let member_str = member.to_string_lossy();
        if blacklisted(&member) {
            info!("Skipping blacklisted crate {member_str}");
        } else if is_proc_macro_crate(&member) {
            info!("Skipping proc-macro crate {member_str}");
        } else if has_wasm_tests(&member) {
            info!("Running tests for {member_str}");
            WasmPack
                .cmd()?
                .current_dir(&repo_root)
                .test()
                .apply(&wasm_pack::TestFlags::Headless)
                .apply(&BROWSER_FOR_WASM_TESTS)
                .env("WASM_BINDGEN_TEST_TIMEOUT", "300")
                // .args(&wasm_pack_args)
                .arg(member.strip_prefix(&repo_root).with_context(|| {
                    format!(
                        "Failed to strip prefix {} from {}. Is the test part of the repository?",
                        repo_root.display(),
                        member.display()
                    )
                })?)
                .apply(&cargo::Color::Always)
                .run_ok()
                .await?;
        } else {
            println!("No wasm tests in {}", member_str);
        }
    }
    Ok(())
}
