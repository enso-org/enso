//! This script is used to disable the `wasm-opt` optimization in the crates that can be used as
//! WASM entry points. Unfortunately, wasm-opt does not allow for disabling wasm-opt through a
//! command line flag, so we have to disable it by setting an appropriate flag in each Cargo.toml.

// === Features ===
#![feature(option_result_contains)]
#![feature(associated_type_bounds)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_build_cli::prelude::*;

use enso_build::paths::parent_cargo_toml;
use enso_build::repo::deduce_repository_path;
use ide_ci::log::setup_logging;



/// Path in the Cargo.toml file where the `wasm-opt` flag is stored.
///
/// This flag controls whether wasm-pack shall invoke wasm-opt on the generated wasm file.
const WASM_OPT_PATH: [&str; 6] =
    ["package", "metadata", "wasm-pack", "profile", "release", "wasm-opt"];

/// Piece of code that will disable wasm-opt when added to Cargo.toml.
pub fn suffix_that_disables_wasm_opt() -> String {
    let without_last = WASM_OPT_PATH[..WASM_OPT_PATH.len() - 1].join(".");
    let last = WASM_OPT_PATH.last().unwrap();
    format!(
        r#"
# Stop wasm-pack from running wasm-opt, because we run it from our build scripts in order to customize options.
[{without_last}]
{last} = false"#
    )
}

/// Check if the Rust source file under given path contains a WASM entry point.
pub fn contains_entry_point(path: impl AsRef<Path>) -> Result<bool> {
    Ok(ide_ci::fs::read_to_string(path)?.contains("#[entry_point"))
}

/// Retrieve item by repeatedly indexing.
pub fn traverse(
    item: &toml::Value,
    keys: impl IntoIterator<Item: AsRef<str>>,
) -> Option<&toml::Value> {
    keys.into_iter().try_fold(item, |item, key| item.get(key.as_ref()))
}

/// Check if the given (parsed) Cargo.toml has already disabled wasm-opt.
fn has_wasm_opt_disabled(document: &toml::Value) -> bool {
    let wasm_opt_entry = traverse(document, WASM_OPT_PATH);
    wasm_opt_entry.and_then(toml::Value::as_bool).contains(&false)
}

/// Disable wasm-opt in the Cargo.toml file.
///
/// Does nothing if wasm-opt is already disabled.
fn disable_wasm_opt_in_cargo_toml(path: impl AsRef<Path>) -> Result {
    assert!(path.as_ref().is_file());
    assert_eq!(path.as_ref().file_name().unwrap(), "Cargo.toml");
    let doc = toml::Value::from_str(&ide_ci::fs::read_to_string(&path)?)?;
    if !has_wasm_opt_disabled(&doc) {
        info!("Disabling wasm-opt in {}", path.as_ref().display());
        ide_ci::fs::append(path, suffix_that_disables_wasm_opt())?;
    } else {
        info!("wasm-opt is already disabled in {}", path.as_ref().display());
    }
    Ok(())
}

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let root = deduce_repository_path()?;
    let rs_source_glob = PathBuf::from_iter([root.as_str(), "**", "*.rs"]).display().to_string();
    info!("Searching for Rust source files in {}", rs_source_glob);
    let rs_files = glob::glob(&rs_source_glob)?.try_collect_vec()?;
    info!("Completed source discovery. Found {} files.", rs_files.len());

    let entry_points: Vec<_> = rs_files.into_iter().try_filter(|p| contains_entry_point(p))?;
    info!("{} of them are entry points.", entry_points.len());

    let cargo_tomls: BTreeSet<_> = entry_points.into_iter().try_map(parent_cargo_toml)?;
    info!("They belong to {} crates.", cargo_tomls.len());

    for cargo_toml in &cargo_tomls {
        disable_wasm_opt_in_cargo_toml(cargo_toml)?;
    }
    Ok(())
}
