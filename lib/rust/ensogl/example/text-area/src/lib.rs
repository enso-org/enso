//! An example showing usage of Text Area.

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

use ensogl_core::prelude::*;
use ensogl_text::traits::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::system::web;
use ensogl_text::buffer;
use ensogl_text::style;
use ensogl_text::Area;
use ensogl_text_msdf::run_once_initialized;
use wasm_bindgen::JsCast;


/// Main example runner.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| {
        init(Application::new("root"));
    });
}

fn init(app: Application) {
    use ensogl_text::Range;
    use ensogl_text::UBytes;

    let area = app.new_view::<Area>();
    area.set_position_x(-100.0);
    let quote = "Et Eärello Endorenna utúlien.\nSinome maruvan ar Hildinyar tenn' Ambar-metta\n";
    let snowman = "\u{2603}";
    let zalgo = "Z̮̞̠͙͔ͅḀ̗̞͈̻̗Ḷ͙͎̯̹̞͓G̻O̭̗̮";
    let _text = quote.to_string() + snowman + zalgo;
    let text = "test".to_string();
    area.set_content("testt\n1234");
    // area.set_font("default"); // FIXME: non-monospaced fonts do not work !!!
    area.focus();
    area.hover();



    // TODO: check scrolled area
    // TODO: text width endpoints
    // TODO: check support for glyphs with multiple code points


    // area.set_cursor_at_end();
    //
    // area.set_format_option(
    //     Range::new(5.ubytes(), 7.ubytes()),
    //     style::FormatOption::Weight(style::Weight::Bold),
    // );
    // area.set_format_option(Range::new(4.ubytes(), 6.ubytes()), style::SdfWeight(0.02));
    // area.set_sdf_weight(Range::new(7.ubytes(), 15.ubytes()), style::SdfWeight(0.04));
    // area.set_sdf_weight(Range::new(24.ubytes(), 26.ubytes()), style::SdfWeight(0.02));
    // area.set_sdf_weight(Range::new(37.ubytes(), 41.ubytes()), style::SdfWeight(0.05));
    // area.set_sdf_weight(Range::new(55.ubytes(), 56.ubytes()), style::SdfWeight(0.03));
    // let quote_length = UBytes::from(quote.len());
    // let text_length = UBytes::from(text.len());
    // area.set_sdf_weight(Range::new(quote_length, text_length), style::SdfWeight(0.02));

    let scene = &app.display.default_scene;
    let navigator = Navigator::new(scene, &scene.camera());

    app.display.default_scene.add_child(&area);


    warn!("=========================");
    let range_green = buffer::Range::from(UBytes(1)..UBytes(7));
    area.set_color_bytes(range_green, color::Rgba::red());
    // area.set_color_all(color::Rgba::red());
    area.set_sdf_weight(buffer::Range::from(UBytes(1)..UBytes(3)), style::SdfWeight(0.02));


    // let text = "red green blue";
    // let colored_area = app.new_view::<Area>();
    // app.display.default_scene.add_child(&colored_area);
    // colored_area.set_font("DejaVuSans");
    // colored_area.set_position_xy(Vector2::new(200.0, 200.0));
    //
    // colored_area.set_default_color(color::Rgba::black());
    // colored_area.set_content(text);
    // let range_green = buffer::Range::from(UBytes(4)..UBytes(9));
    // colored_area.set_color_bytes(range_green, color::Rgba::green());
    // let range_blue = buffer::Range::from(UBytes(10)..UBytes(14));
    // colored_area.set_color_bytes(range_blue, color::Rgba::blue());
    // colored_area.set_default_color(color::Rgba::red());


    mem::forget(navigator);
    mem::forget(app);
    mem::forget(area);
    // mem::forget(colored_area);

    init_debug_hotkeys();
}

fn init_debug_hotkeys() {
    let closure: Closure<dyn Fn(JsValue)> = Closure::new(move |val: JsValue| {
        let event = val.unchecked_into::<web::KeyboardEvent>();
        if event.alt_key() && event.ctrl_key() {
            let key = event.code();
            warn!("{:?}", key);
            // if key == "Backquote" {
            // } else if key == "Digit0" {
            // } else if key == "Digit1" {
            // } else if key == "Digit2" {
            // } else if key == "KeyP" {
            // } else if key == "KeyQ" {
            // }
        }
    });
    let handle = web::add_event_listener_with_bool(&web::window, "keydown", closure, true);
    mem::forget(handle);
}
