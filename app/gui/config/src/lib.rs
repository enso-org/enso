//! Startup arguments definition.

// === Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

// === Non-Standard Linter Configuration ===

use enso_logger::*;
use enso_prelude::*;

use enso_logger::DefaultWarningLogger as Logger;
use ensogl::system::web;



// ==============
// === Config ===
// ==============

include!(concat!(env!("OUT_DIR"), "/config.rs"));

pub use generated::*;

pub fn engine_version_requirement() -> semver::VersionReq {
    semver::VersionReq::parse(&format!("^{}", engine_version_supported)).unwrap()
}


// ============
// === Args ===
// ============

ensogl::read_args! {
    [window_app_scope_name, window_app_scope_config_name] {
        entry                  : String,
        project                : String,
        project_manager        : String,
        language_server_rpc    : String,
        language_server_data   : String,
        namespace              : String,
        platform               : web::platform::Platform,
        frame                  : bool,
        theme                  : String,
        dark_theme             : bool,
        high_contrast          : bool,
        use_loader             : bool,
        wasm_url               : String,
        wasm_glue_url          : String,
        node_labels            : bool,
        crash_report_host      : String,
        data_gathering         : bool,
        is_in_cloud            : bool,
        verbose                : bool,
        authentication_enabled : bool,
        email                  : String,
        application_config_url : String,
    }
}
