#![allow(missing_docs)]

use wasm_bindgen::prelude::*;

use crate::display::world::World;
use crate::display::world::WorldData;
use crate::data::dirty::traits::*;

use nalgebra::Point2;
use nalgebra::Vector2;
use crate::display::shape::text::content::TextLocation;
use crate::display::shape::text::content::TextChange;
use crate::display::shape::text::TextComponentBuilder;
use crate::display::shape::text::Color;
use crate::display::shape::text::TextComponentProperties;
use crate::system::web::forward_panic_hook_to_console;
use crate::display::shape::text::cursor::Step::Right;

#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_text() {
    forward_panic_hook_to_console();
    basegl_core_msdf_sys::run_once_initialized(|| {
        let world_ref = WorldData::new("canvas");
        {
            let world: &mut WorldData = &mut world_ref.rc.borrow_mut();
            let workspace = &mut world.workspace;
            let fonts = &mut world.fonts;
            let font_id = fonts.load_embedded_font("DejaVuSansMono").unwrap();

            let mut text_component = TextComponentBuilder {
                workspace,
                fonts,
                font_id,
                text: "".to_string(),
                properties: TextComponentProperties {
                    position: Point2::new(-0.95, -0.9),
                    size: Vector2::new(1.8, 1.6),
                    text_size: 0.032,
                    color: Color { r: 0.0, g: 0.8, b: 0.0, a: 1.0 },
                }
            }.build();
            text_component.cursors.add_cursor(TextLocation { line: 0, column: 0 });
            workspace.text_components.push(text_component);
            world.workspace_dirty.set();
        }

        let now             = js_sys::Date::now();
        let animation_start = now + 3000.0;
        let start_scrolling = animation_start + 10000.0;
        let mut chars       = typed_character_list(animation_start,include_str!("../lib.rs"));
        world_ref.on_frame(move |w| {
            animate_text_component(w,&mut chars,start_scrolling)
        }).forget();
    });
}

struct CharToPush {
    time   : f64,
    a_char: char,
}

const ONE_CHAR_TYPING_DURATION_MS : f64 = 50.0;

fn typed_character_list(start_time:f64, text:&'static str) -> Vec<CharToPush> {
    text.char_indices().map(|(i,a_char)| {
        let time = start_time + ONE_CHAR_TYPING_DURATION_MS * i as f64;
        CharToPush {time,a_char}
    }).collect()
}

fn animate_text_component
( world:&World
, typed_chars:&mut Vec<CharToPush>
, start_scrolling:f64) {
    let world : &mut WorldData = &mut world.rc.borrow_mut();
    let workspace              = &mut world.workspace;
    let editor                 = workspace.text_components.first_mut().unwrap();
    let fonts                  = &mut world.fonts;
    let now                    = js_sys::Date::now();

    let to_type_now = typed_chars.drain_filter(|ch| ch.time <= now);
    for ch in to_type_now {
        let cursor = editor.cursors.cursors.first_mut().unwrap();
        let string = ch.a_char.to_string();
        let change = TextChange::insert(cursor.position, string.as_str());
        editor.content.make_change(change);
        editor.navigate_cursors(Right,false,fonts);
    }
    if start_scrolling <= js_sys::Date::now() {
        editor.scroll(Vector2::new(0.0, -0.01));
    }
    world.workspace_dirty.set();
}
