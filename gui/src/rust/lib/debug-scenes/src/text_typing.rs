#![allow(missing_docs)]

use wasm_bindgen::prelude::*;

use ensogl::traits::*;

use ensogl::display::shape::text::glyph::font::FontRegistry;
use ensogl::display::shape::text::text_field::TextField;
use ensogl::display::shape::text::text_field::TextFieldProperties;
use ensogl::display::world::*;
use ensogl::system::web;
use nalgebra::Vector2;
use nalgebra::Vector4;


#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_text_typing() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    ensogl_core_msdf_sys::run_once_initialized(|| {
        let world     = &World::new(&web::get_html_element_by_id("root").unwrap());
        let mut fonts = FontRegistry::new();
        let font      = fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap();

        let properties = TextFieldProperties {
            font,
            text_size  : 16.0,
            base_color : Vector4::new(0.0, 0.0, 0.0, 1.0),
            size       : Vector2::new(200.0, 200.0)
        };

        let mut text_field = TextField::new(&world,properties);
        text_field.set_position(Vector3::new(10.0, 600.0, 0.0));
        world.add_child(&text_field.display_object());

        let now             = js_sys::Date::now();
        let animation_start = now + 3000.0;
        let start_scrolling = animation_start + 10000.0;
        let mut chars       = typed_character_list(animation_start,include_str!("text_typing.rs"));
        world.on_frame(move |_| {
            animate_text_component(&mut text_field,&mut chars,start_scrolling)
        }).forget();
        world.keep_alive_forever();
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
(text_field:&mut TextField, typed_chars:&mut Vec<CharToPush>, start_scrolling:f64) {
    let now         = js_sys::Date::now();
    let to_type_now = typed_chars.drain_filter(|ch| ch.time <= now);
    for ch in to_type_now {
        let string = ch.a_char.to_string();
        text_field.write(string.as_str());
    }
    if start_scrolling <= js_sys::Date::now() {
        text_field.scroll(Vector2::new(0.0,-0.1));
    }
}
