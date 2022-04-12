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
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_text::style;
use ensogl_text::traits::*;
use ensogl_text::Area;
use ensogl_text_msdf_sys::run_once_initialized;



/// Main example runner.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_text_area() {
    run_once_initialized(|| {
        init(Application::new("root"));
    });
}

fn init(app: Application) {
    use ensogl_text::Bytes;
    use ensogl_text::Range;

    let area = app.new_view::<Area>();
    area.set_position_x(-100.0);
    let quote = "Et Eärello Endorenna utúlien.\nSinome maruvan ar Hildinyar tenn' Ambar-metta\n";
    let snowman = "\u{2603}";
    let zalgo = "Z̮̞̠͙͔ͅḀ̗̞͈̻̗Ḷ͙͎̯̹̞͓G̻O̭̗̮";
    let text = quote.to_string() + snowman + zalgo;
    area.set_content(text.clone() + "\n" + text.as_str());
    area.set_font("DejaVuSans");
    area.focus();
    area.hover();
    area.set_cursor_at_end();

    area.set_sdf_bold(Range::new(4.bytes(), 6.bytes()), style::SdfBold(0.02));
    area.set_sdf_bold(Range::new(7.bytes(), 15.bytes()), style::SdfBold(0.04));
    area.set_sdf_bold(Range::new(24.bytes(), 26.bytes()), style::SdfBold(0.02));
    area.set_sdf_bold(Range::new(37.bytes(), 41.bytes()), style::SdfBold(0.05));
    area.set_sdf_bold(Range::new(55.bytes(), 56.bytes()), style::SdfBold(0.03));
    let quote_length = Bytes::from(quote.len());
    let text_length = Bytes::from(text.len());
    area.set_sdf_bold(Range::new(quote_length, text_length), style::SdfBold(0.02));

    let scene = &app.display.default_scene;
    let navigator = Navigator::new(scene, &scene.camera());

    app.display.default_scene.add_child(&area);
    mem::forget(navigator);
    mem::forget(app);
    mem::forget(area);
}
