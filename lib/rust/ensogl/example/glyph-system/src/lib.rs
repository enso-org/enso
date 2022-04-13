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

const CHARS_TO_TEST: &[&str] = &["abcdqwerty", "ABCDQUERTY"];

/// Main example runner.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| init(&World::new().displayed_in("root")));
}


fn init(world: &World) {
    let fonts = world.default_scene.extension::<font::Registry>();
    let font = fonts.load("DejaVuSans");
    let glyph_system = glyph::System::new(&world.default_scene, font);
    let height = 32.0;
    let color = color::Rgba::new(0.5, 0.0, 0.0, 1.0);
    let start_pos = Vector2(-300.0, -300.0);

    for (line_ind, line) in CHARS_TO_TEST.iter().enumerate() {
        for (char_ind, char) in line.chars().enumerate() {
            let glyph = glyph_system.new_glyph();
            let bold_glyph = glyph_system.new_glyph();
            glyph.set_char(char);
            glyph.set_color(color);
            glyph.set_font_size(height);

            bold_glyph.set_char(char);
            bold_glyph.set_color(color);
            bold_glyph.set_font_size(height);
            bold_glyph.set_bold(true);

            let x = char_ind as f32 * (height + 4.0);
            let y = line_ind as f32 * (height * 2.0 + 8.0);
            let bold_y = y + height + 4.0;
            glyph.set_position_xy(start_pos + Vector2(x, y));
            bold_glyph.set_position_xy(start_pos + Vector2(x, bold_y));
            world.add_child(&glyph);
            world.add_child(&bold_glyph);
            std::mem::forget(glyph);
            std::mem::forget(bold_glyph);
        }
    }

    world.add_child(&glyph_system);
    world.keep_alive_forever();
    std::mem::forget(glyph_system);
}
