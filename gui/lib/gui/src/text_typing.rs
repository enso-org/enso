#![allow(missing_docs)]

use wasm_bindgen::prelude::*;

use basegl::display::object::DisplayObjectOps;
use basegl::display::shape::text::glyph::font::FontRegistry;
use basegl::display::shape::text::text_field::cursor::Step::Right;
use basegl::display::shape::text::text_field::{TextField, TextFieldProperties};
use basegl::display::world::*;
use basegl::system::web;
use nalgebra::Vector2;
use nalgebra::Vector4;


#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_text_typing() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    basegl_core_msdf_sys::run_once_initialized(|| {
        let world     = &WorldData::new(&web::body());
        let mut fonts = FontRegistry::new();
        let font_id   = fonts.load_embedded_font("DejaVuSansMono").unwrap();

        let properties = TextFieldProperties {
            font_id,
            text_size  : 16.0,
            base_color : Vector4::new(0.0, 0.0, 0.0, 1.0),
            size       : Vector2::new(200.0, 200.0)
        };

        let mut text_field = TextField::new(&world,"",properties,&mut fonts);
        text_field.set_position(Vector3::new(10.0, 600.0, 0.0));
        world.add_child(&text_field);

        let now             = js_sys::Date::now();
        let animation_start = now + 3000.0;
        let start_scrolling = animation_start + 10000.0;
        let mut chars       = typed_character_list(animation_start,include_str!("../../core/src/lib.rs"));
        world.on_frame(move |_| {
            animate_text_component(&mut fonts,&mut text_field,&mut chars,start_scrolling)
        }).forget();
    });
}

struct CharToPush {
    time   : f64,
    a_char : char,
}

const ONE_CHAR_TYPING_DURATION_MS : f64 = 50.0;

fn typed_character_list(start_time:f64, text:&'static str) -> Vec<CharToPush> {
    text.char_indices().map(|(i,a_char)| {
        let time = start_time + ONE_CHAR_TYPING_DURATION_MS * i as f64;
        CharToPush {time,a_char}
    }).collect()
}

fn animate_text_component
( fonts           : &mut FontRegistry
, text_field      : &mut TextField
, typed_chars     : &mut Vec<CharToPush>
, start_scrolling : f64) {
    let now         = js_sys::Date::now();
    let to_type_now = typed_chars.drain_filter(|ch| ch.time <= now);
    for ch in to_type_now {
        let string = ch.a_char.to_string();
        text_field.edit(string.as_str(),fonts);
        text_field.navigate_cursors(Right,false,fonts);
    }
    if start_scrolling <= js_sys::Date::now() {
        text_field.scroll(Vector2::new(0.0,-0.1),fonts);
    }
    text_field.update();
}
