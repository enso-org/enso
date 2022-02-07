//! An example showing usage of GlyphSystem.

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

use ensogl_core::data::color;
use ensogl_core::display::world::*;
use ensogl_core::system::web;
use ensogl_text::typeface::*;
use ensogl_text_msdf_sys::run_once_initialized;
use wasm_bindgen::prelude::*;



/// Main example runner.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_glyph_system() {
    web::forward_panic_hook_to_console();
    run_once_initialized(|| init(&World::new(&web::get_html_element_by_id("root").unwrap())));
}

fn init(world: &World) {
    let fonts = world.scene().extension::<font::Registry>();
    let font = fonts.load("DejaVuSans");
    let glyph_system = glyph::System::new(world.scene(), font);
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
