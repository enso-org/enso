//! An example showing usage of GlyphSystem.

use ensogl::traits::*;

use ensogl::data::color;
use ensogl::display::shape::text::glyph::font::FontRegistry;
use ensogl::display::shape::text::glyph::system::GlyphSystem;
use ensogl::display::world::*;
use ensogl::system::web;
use ensogl_core_msdf_sys::run_once_initialized;
use wasm_bindgen::prelude::*;



/// Main example runner.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_glyph_system() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    run_once_initialized(|| init(&World::new(&web::get_html_element_by_id("root").unwrap())));
}

fn init(world:&World) {
    let mut fonts    = FontRegistry::new();
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
        glyph_system.sprite_system().display_object().update();
    }).forget();
}
