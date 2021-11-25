//! A debug scene which shows the number and range selector.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(entry_insert)]
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
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::system::web;
use ensogl_hardcoded_theme as theme;
use ensogl_selector as selector;
use ensogl_selector::Bounds;
use ensogl_text_msdf_sys::run_once_initialized;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_slider() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
        init(&app);
        mem::forget(app);
    });
}

fn make_number_picker(app: &Application) -> Leak<selector::NumberPicker> {
    let slider = app.new_view::<selector::NumberPicker>();
    slider.frp.resize(Vector2(200.0, 50.0));
    app.display.add_child(&slider);
    Leak::new(slider)
}

fn make_range_picker(app: &Application) -> Leak<selector::NumberRangePicker> {
    let slider = app.new_view::<selector::NumberRangePicker>();
    slider.frp.resize(Vector2(400.0, 50.0));
    app.display.add_child(&slider);
    Leak::new(slider)
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let slider1 = make_number_picker(app);
    slider1.inner().frp.allow_click_selection(true);

    let slider2 = make_number_picker(app);
    slider2.inner().frp.resize(Vector2(400.0, 50.0));
    slider2.inner().frp.set_bounds.emit(Bounds::new(-100.0, 100.0));
    slider2.inner().set_position_y(50.0);
    slider2.inner().frp.use_overflow_bounds(Bounds::new(-150.0, 200.0));
    slider2.inner().frp.set_caption(Some("Value:".to_string()));

    let slider3 = make_range_picker(app);
    slider3.inner().set_position_y(-100.0);
    slider3.inner().set_track_color(color::Rgba::new(0.0, 0.80, 0.80, 1.0));

    let slider4 = make_range_picker(app);
    slider4.inner().set_position_y(-200.0);
    slider4.inner().frp.use_overflow_bounds(Bounds::new(-2.0, 3.0));
    slider4.inner().frp.set_caption(Some("Caption".to_string()));
    slider4.inner().set_track_color(color::Rgba::new(0.5, 0.70, 0.70, 1.0));
}
