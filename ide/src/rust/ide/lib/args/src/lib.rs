//! Enso startup arguments definition.

#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_prelude::*;
use enso_logger::*;
use enso_logger::DefaultWarningLogger as Logger;
use ensogl::system::web;



// ============
// === Args ===
// ============

// Please note that the path at which the config is accessible (`enso.config`) is hardcoded below.
// This needs to be synchronised with the `src/config.yaml` configuration file. In the future, we
// could write a procedural macro, which loads the configuration and splits Rust variables from it
// during compilation time. This is not possible by using macro rules, as there is no way to plug in
// the output of `include_str!` macro to another macro input.
ensogl::read_args! {
    js::enso.config {
        entry                : String,
        project              : String,
        project_manager      : String,
        language_server_rpc  : String,
        language_server_data : String,
        platform             : web::platform::Platform,
        frame                : bool,
        dark_theme           : bool,
        high_contrast        : bool,
        use_loader           : bool,
        wasm_url             : String,
        wasm_glue_url        : String,
    }
}
