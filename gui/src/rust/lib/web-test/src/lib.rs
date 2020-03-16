#![feature(arbitrary_self_types)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

mod system {
    pub use ensogl_system_web as web;
}

use enso_prelude as prelude;

pub use wasm_bindgen_test::wasm_bindgen_test_configure as web_configure;
pub use web_test_proc_macro::*;
pub use wasm_bindgen_test::wasm_bindgen_test;

mod bencher;
mod group;
mod container;
mod bench_container;

pub use bencher::Bencher;
pub use group::Group;
pub use container::Container;
pub use bench_container::BenchContainer;
