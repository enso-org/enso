//! An example showing usage of GlyphSystem.

#![recursion_limit = "1024"]
// === Features ===
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::display::world::*;
use ensogl_core::prelude::*;
use ensogl_text::typeface::*;
use wasm_bindgen::prelude::*;

use ensogl_core::data::color;
use ensogl_text_msdf_sys::run_once_initialized;



/// Main example runner.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_glyph_system() {
    run_once_initialized(|| init(&World::new().displayed_in("root")));
}

fn init(world: &World) {
    let fonts = world.default_scene.extension::<font::Registry>();
    let font = fonts.load("DejaVuSans");
    let glyph_system = glyph::System::new(&world.default_scene, font);
    let height = 32.0;
    let color = color::Rgba::new(0.5, 0.0, 0.0, 1.0);
    let glyph = glyph_system.new_glyph();
    glyph.set_char('Q');
    glyph.set_color(color);
    glyph.size.set(Vector2(height, height));

    world.add_child(&glyph_system);
    world.add_child(&glyph);
    world.keep_alive_forever();
    std::mem::forget(glyph_system);
    std::mem::forget(glyph);
}
