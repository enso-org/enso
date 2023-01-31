//! Startup arguments definition.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_prelude::*;

use ensogl::system::web;



// ==============
// === Config ===
// ==============

include!(concat!(env!("OUT_DIR"), "/config.rs"));

pub use generated::*;

pub fn engine_version_requirement() -> semver::VersionReq {
    semver::VersionReq::parse(&format!("^{engine_version_supported}")).unwrap()
}



// ============
// === Args ===
// ============

ensogl::read_args! {
    application_config_url: String,
    authentication_enabled: bool,
    dark_theme: bool,
    data_gathering: bool,
    debug: bool,
    email: Option<String>,
    emit_user_timing_measurements: bool,
    enable_new_component_browser: bool,
    enable_spector:bool,
    entry: String,
    frame: bool,
    is_in_cloud: bool,
    language_server_data: Option<String>,
    language_server_rpc: Option<String>,
    loader_download_to_init_ratio: f32,
    max_before_main_entry_points_time_ms: f32,
    namespace: Option<String>,
    node_labels: bool,
    pkg_js_url: String,
    pkg_wasm_url: String,
    platform: Option<web::platform::Platform>,
    preferred_engine_version: Option<semver::Version>,
    project: Option<String>,
    project_manager: Option<String>,
    shaders_url: String,
    skip_min_version_check: bool,
    /// When profiling the application (e.g. with the `./run profile` command), this argument
    /// chooses what is profiled.
    test_workflow: Option<String>,
    theme: String,
    use_loader: bool,
}
