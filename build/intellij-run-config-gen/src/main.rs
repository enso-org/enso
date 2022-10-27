//! Generate IntelliJ run configurations and place them in the .idea/runConfigurations directory.

// === Features ===
#![feature(exit_status_error)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(keyword_idents)]
#![deny(macro_use_extern_crate)]
#![deny(missing_abi)]
#![deny(pointer_structural_match)]
#![deny(unsafe_op_in_unsafe_fn)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(absolute_paths_not_starting_with_crate)]
#![warn(elided_lifetimes_in_paths)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_crate_dependencies)]
#![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
#![warn(variant_size_differences)]
#![warn(unreachable_pub)]

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Debug;
use std::fs;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;



// ==================
// === Processing ===
// ==================

/// A path to rust source annottated with information whether it is a main or a library main source
/// file.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct RustSourcePath {
    path: PathBuf,
}


fn process_path(path: impl AsRef<Path>) {
    let paths = discover_paths(path);
    println!("Processing {:#?}", paths);
}

/// Discover all paths containing Rust sources, recursively.
fn discover_paths(path: impl AsRef<Path>) -> Vec<PathBuf> {
    let mut vec = Vec::default();
    discover_paths_internal(&mut vec, path, false);
    vec
}

fn discover_paths_internal(vec: &mut Vec<PathBuf>, path: impl AsRef<Path>, is_main_dir: bool) {
    let path = path.as_ref();
    let md = fs::metadata(path);
    let md = md.unwrap_or_else(|_| panic!("Could get metadata of {}", path.display()));
    if md.is_dir() && path.file_name() != Some(OsStr::new("target")) {
        let dir_name = path.file_name();
        // FIXME: This should cover 'tests' folder also, but only the files that contain actual
        //        tests. Otherwise, not all attributes are allowed there.
        let is_main_dir = dir_name == Some(OsStr::new("bin")); // || dir_name == Some(OsStr::new("tests"));
        let sub_paths = fs::read_dir(path).unwrap();
        for sub_path in sub_paths {
            discover_paths_internal(vec, &sub_path.unwrap().path(), is_main_dir)
        }
    } else if md.is_file() && path.extension() == Some(OsStr::new("rs")) {
        let file_name = path.file_name().and_then(|s| s.to_str());
        let is_main_file = file_name == Some("lib.rs") || file_name == Some("main.rs");
        let is_main = is_main_file || is_main_dir;
        let content = fs::read_to_string(path).unwrap();
        let has_main_fn = content.lines().any(|line| {
            let is_comment = line.trim().starts_with("//");
            let has_main_fn = line.ends_with("fn main() {");
            !is_comment && has_main_fn
        });
        if is_main && has_main_fn {
            vec.push(path.into())
        }
    }
}

fn main() {
    process_path(".");
}
