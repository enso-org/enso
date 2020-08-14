//! An example showing usage of GlyphSystem.

use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::shape::text::glyph::font;
use ensogl_core::display::shape::text::glyph::system::GlyphSystem;
use ensogl_core::display::world::*;
use ensogl_core::system::web;
use ensogl_text_msdf_sys::run_once_initialized;
use wasm_bindgen::prelude::*;



/// Main example runner.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_glyph_system() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    run_once_initialized(|| init(&World::new(&web::get_html_element_by_id("root").unwrap())));
}

fn init(world:&World) {
    let mut fonts    = font::Registry::new();
    let font         = fonts.get_or_load_embedded_font("DejaVuSans").unwrap();
    let glyph_system = GlyphSystem::new(world,font);
    let height       = 32.0;
    let color        = color::Rgba::new(0.0, 0.8, 0.0, 1.0);
    let line         = glyph_system.new_line();
    line.set_font_size(height);
    line.set_font_color(color);
    line.set_text("Follow the white rabbit ...");
    line.set_position(Vector3::new(100.0,100.0,0.0));

    world.add_child(glyph_system.sprite_system());
    world.keep_alive_forever();
    world.on_frame(move |_| {
        let &_ = &line;
    }).forget();
}
