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

use enso_json_to_struct::json_to_struct;



// ==============
// === Config ===
// ==============

include!(concat!(env!("OUT_DIR"), "/config.rs"));

pub use generated::*;

pub fn engine_version_requirement() -> semver::VersionReq {
    semver::VersionReq::parse(&format!(">={engine_version_supported}")).unwrap()
}



// ============
// === Args ===
// ============

json_to_struct!(
    "../../../../lib/rust/ensogl/pack/js/src/runner/config.json",
    "../../../../app/ide-desktop/lib/content-config/src/config.json"
);

pub fn read_args() -> Args {
    debug_span!("Reading application arguments from JS.").in_scope(|| {
        let mut args = Args::default();
        if let Ok(js_app) = ensogl::system::js::app::app() {
            for param in js_app.config().params() {
                if let Some(value) = param.value() {
                    let path = format!("{}.value", param.structural_name());
                    if let Some(err) = args.set(&path, value) {
                        error!("{}", err.display())
                    }
                }
            }
        } else {
            error!("Could not connect to JS application. Using default configuration.")
        }
        args
    })
}

lazy_static! {
    pub static ref ARGS: Args = read_args();
}
