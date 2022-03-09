//! An example showing usage of Text Area.

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

use ensogl_core::application::Application;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_text::Area;
use ensogl_text_msdf_sys::run_once_initialized;
use wasm_bindgen::prelude::*;


/// Main example runner.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_text_area() {
    run_once_initialized(|| {
        init(Application::new("root"));
    });
}

fn init(app: Application) {
    let area = app.new_view::<Area>();
    area.set_position_x(-100.0);
    area.set_content(
        "Et Eärello Endorenna utúlien.\nSinome maruvan ar Hildinyar tenn' Ambar-metta",
    );
    area.focus();
    area.hover();
    area.set_cursor_at_end();

    let scene = &app.display.default_scene;
    let navigator = Navigator::new(scene, &scene.camera());

    app.display.default_scene.add_child(&area);
    let keep = Some(area);
    app.display
        .on
        .before_frame
        .add(move |_frame| {
            let _ = &keep;
        })
        .forget();
    mem::forget(navigator);
    mem::forget(app);
}
