//! Startup arguments definition.

#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_logger::DefaultWarningLogger as Logger;
use enso_logger::*;
use enso_prelude::*;
use ensogl::system::web;



// ============
// === Args ===
// ============

ensogl::read_args! {
    [enso_config::window_app_scope_name,enso_config::window_app_scope_config_name] {
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
