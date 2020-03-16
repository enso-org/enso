//! This module defines the entrypoint function for IDE.

use wasm_bindgen::prelude::*;

use ensogl::system::web;
use ide::run_ide;

/// IDE startup function.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_ide() {
    web::forward_panic_hook_to_console();
    web::set_stdout();

    // FIXME: This code is temporary. It's used to remove the loader UI.
    ensogl_core_msdf_sys::run_once_initialized(|| {
        web::get_element_by_id("loader").map(|t| {
            t.parent_node().map(|p| {
                p.remove_child(&t).unwrap()
            })
        }).ok();
        run_ide()
    });
}
