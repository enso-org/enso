#![feature(type_ascription)]
#![feature(unboxed_closures)]
#![cfg_attr(test, allow(dead_code))]
//#![warn(missing_docs)]

// Lints
#![allow(clippy::option_map_unit_fn)]

// =================================
// === Module Structure Reexport ===
// =================================

pub mod data;
pub mod dirty;
pub mod display;
pub use basegl_prelude as prelude;
pub mod backend {
    pub use basegl_backend_webgl as webgl;
}
pub mod system {
    pub use basegl_system_web as web;
}

// ============
// === Main ===
// ============

use display::world::World;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() {
    let world = World::new();
    world.add_workspace("canvas");
    world.start();
}
