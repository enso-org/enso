//! An example showing usage of GlyphSystem.

use crate::display::object::DisplayObjectOps;
use crate::display::object::DisplayObject;
use crate::display::shape::text::glyph::font::FontRegistry;
use crate::display::shape::text::glyph::system::GlyphSystem;
use crate::display::world::*;

use basegl_core_msdf_sys::run_once_initialized;
use basegl_system_web::forward_panic_hook_to_console;
use basegl_system_web::set_stdout;
use nalgebra::Vector4;
use wasm_bindgen::prelude::*;



/// Main example runner.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_glyph_system() {
    forward_panic_hook_to_console();
    set_stdout();
    run_once_initialized(|| init(&WorldData::new("canvas")));
}

fn init(world: &World) {
    let mut fonts        = FontRegistry::new();
    let font_id          = fonts.load_embedded_font("DejaVuSans").unwrap();
    let mut glyph_system = GlyphSystem::new(world,font_id);
    let line_position    = Vector2::new(100.0, 100.0);
    let height           = 32.0;
    let color            = Vector4::new(0.0, 0.8, 0.0, 1.0);
    let text             = "Follow  the white rabbit...";
    let line             = glyph_system.new_line(line_position,height,text,color,&mut fonts);

    world.add_child(glyph_system.sprite_system());
    world.on_frame(move |_| {
        let &_ = &line;
        glyph_system.sprite_system().display_object().update();
    }).forget();
}
