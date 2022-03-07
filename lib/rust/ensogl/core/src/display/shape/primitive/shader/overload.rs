//! GLSL 300 does not allow for overloading primitive functions. This module defines an utility
//! which scans the GLSL code and mangles all names of primitive functions. This way we can define
//! overloaded functions the same way as we did in GLSL 100.

use wasm_bindgen::prelude::*;

mod js {
    use super::*;
    #[wasm_bindgen(module = "/src/display/shape/primitive/glsl/overload.js")]
    extern "C" {
        /// Returns GLSL code which redirects mangled function names to their original primitive
        /// definitions.
        #[allow(unsafe_code)]
        pub fn builtin_redirections() -> String;

        /// Mangles the provided GLSL code to allow primitive definitions overloading.
        #[allow(unsafe_code)]
        pub fn allow_overloading(s: &str) -> String;
    }
}

#[allow(missing_docs)]
mod mock {
    pub fn builtin_redirections() -> String {
        "".into()
    }
    pub fn allow_overloading(_: &str) -> String {
        "".into()
    }
}

#[cfg(target_arch = "wasm32")]
pub use js::*;

#[cfg(not(target_arch = "wasm32"))]
pub use mock::*;
