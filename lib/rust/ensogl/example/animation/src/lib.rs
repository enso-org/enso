//! A debug scene showing the bug described in https://github.com/enso-org/ide/issues/757

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "1024"]

use ensogl_core::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::DEPRECATED_Animation;
use ensogl_text_msdf_sys::run_once_initialized;
use logger::TraceLogger as Logger;
use wasm_bindgen::prelude::*;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_animation() {
    run_once_initialized(|| {
        let app = Application::new("root");
        let logger: Logger = Logger::new("AnimationTest");
        let network = enso_frp::Network::new("test");
        let animation = DEPRECATED_Animation::<f32>::new(&network);
        animation.set_target_value(-259_830.0);

        enso_frp::extend! {network
            eval animation.value([logger](value) {
                info!(logger, "Value {value}")
            });
        }

        mem::forget(animation);
        mem::forget(network);
        mem::forget(app);
    });
}
