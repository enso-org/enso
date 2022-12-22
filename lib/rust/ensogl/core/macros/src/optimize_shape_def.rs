//! A macro allowing generation of variety of things. For now it supports a few cases, however, it
//! is meant to be a hub for other extensions. See the docs in [`lib.rs`] to learn more.

use crate::prelude::*;

use paste::paste;
use std::path::Path;
use std::path::PathBuf;


const MODIFY_NAME_PREFIX: &str = "modify_";
const UPDATE_NAME_PREFIX: &str = "update_";
const SETTER_NAME_PREFIX: &str = "set_";



fn workspace_manifest_path() -> PathBuf {
    let output = std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout;
    Path::new(std::str::from_utf8(&output).unwrap().trim()).to_path_buf()
}

fn workspace_dir() -> PathBuf {
    let output = std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout;
    let cargo_path = Path::new(std::str::from_utf8(&output).unwrap().trim());
    cargo_path.parent().unwrap().to_path_buf()
}

const METADATA_VERSION: u8 = 2;

// ========================
// === Main entry point ===
// ========================
use std::env;

use cargo::core::source::Source;
use cargo::util::config::Config;

pub fn run(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input_fn = syn::parse_macro_input!(input as syn::ImplItemMethod);

    let fn_block = &input_fn.block;
    let fn_body = quote!(#fn_block).to_string();

    let mut vars = String::new();
    for (key, value) in env::vars() {
        vars += &format!("\n{key}: {value}");
    }

    let current_manifest_path =
        Path::new("/Users/wdanilo/Dev/enso/lib/rust/ensogl/example/auto-layout/Cargo.toml");
    let workspace_manifest_path = workspace_manifest_path();
    let home_dir = Path::new(&env::var("HOME").unwrap()).to_path_buf();
    let workspace_dir = workspace_dir();
    let shell = cargo::core::shell::Shell::new();
    let config = Config::new(shell, workspace_dir, home_dir);
    let mut workspace = cargo::core::Workspace::new(&current_manifest_path, &config).unwrap();

    workspace.set_ignore_lock(true);

    // let current_package = workspace.current().unwrap();
    //
    // let hashable = current_package.package_id().stable_hash(workspace.root());
    // let hash = cargo::util::short_hash(&(METADATA_VERSION, hashable));
    //
    // let compile_target =
    //     cargo::core::compiler::CompileTarget::new("wasm32-unknown-unknown").unwrap();
    //
    let mut tt = String::new();
    // // locate-project --workspace --message-format plain
    let compile_mode = cargo::core::compiler::CompileMode::Check { test: false };
    // let foo = current_package.package_id().version().to_string();
    let compile_opts = cargo::ops::CompileOptions::new(&config, compile_mode).unwrap();
    let interner = cargo::core::compiler::UnitInterner::new();
    let bcx = cargo::ops::create_bcx(&workspace, &compile_opts, &interner).unwrap();
    let mut context = cargo::core::compiler::Context::new(&bcx).unwrap();
    // context.prepare_units();
    // // let comp = cargo::core::compiler::CompilationFiles::
    //
    // for unit in &bcx.roots {
    //     //     // Collect tests and executables.
    //     // context.files();
    //     // tt += &format!("{:?}", context.files());
    // }
    let span = proc_macro::Span::call_site();
    let source = span.source_file();

    panic!("{:#?}", span);


    let output = quote! {
        // #input_fn
        // #(#out)*
    };

    let out: proc_macro::TokenStream = output.into();


    out
}
