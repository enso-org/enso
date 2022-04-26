//! Functionality for producing debug information.

#![feature(extern_types)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_profiler as profiler;

use derivative::Derivative;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;



// ===========================
// === LifecycleController ===
// ===========================

/// Handle to an API for managing application shutdown.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct LifecycleController {
    #[derivative(Debug = "ignore")]
    #[cfg_attr(not(target_arg = "wasm32"), allow(unused))]
    api: js::lifecycle::Lifecycle,
}

impl LifecycleController {
    /// Try to obtain a handle. Will succeed if running in Electron.
    pub fn new() -> Option<LifecycleController> {
        lifecycle_controller().map(|api| Self { api })
    }

    /// Initiate application shutdown.
    pub fn quit(&self) {
        #[cfg(target_arch = "wasm32")]
        self.api.quit();
        #[cfg(not(target_arch = "wasm32"))]
        unreachable!("Instance can only be acquired under wasm32.");
    }
}



// ===========================
// === Saving profile data ===
// ===========================

/// Capture the current profile and emit it.
pub fn save_profile() {
    let already_saved = PROFILE_SAVED.swap(true, Ordering::Relaxed);
    if !already_saved {
        let log = profiler::internal::take_log();
        match profiling_data_api() {
            Some(api) => api.save_profile(&log),
            None => web_sys::console::log_1(&log.into()),
        }
    }
}

static PROFILE_SAVED: AtomicBool = AtomicBool::new(false);



// ===========
// === FFI ===
// ===========

mod js {
    pub mod lifecycle {
        use wasm_bindgen::prelude::*;

        #[wasm_bindgen]
        extern "C" {
            pub type Lifecycle;

            #[wasm_bindgen(method, js_name = quit)]
            #[allow(unsafe_code)]
            pub fn quit(this: &Lifecycle);
        }
    }

    pub mod profiling_data {
        use wasm_bindgen::prelude::*;

        #[wasm_bindgen]
        extern "C" {
            pub type ProfilingData;

            #[wasm_bindgen(method, js_name = saveProfile)]
            #[allow(unsafe_code)]
            pub fn save_profile(this: &ProfilingData, data: &str);
        }
    }
}

macro_rules! window_prop_getter {
    ($prop:expr; $fun:ident -> $ty:ty) => {
        fn $fun() -> Option<$ty> {
            use wasm_bindgen::JsCast;
            let window = web_sys::window()?;
            let prop = $prop;
            Some(js_sys::Reflect::get(&window, &prop.into()).ok()?.unchecked_into())
        }
    };
}

window_prop_getter!("enso_lifecycle"; lifecycle_controller -> js::lifecycle::Lifecycle);
window_prop_getter!("enso_profiling_data"; profiling_data_api -> js::profiling_data::ProfilingData);
