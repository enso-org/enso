//! A debug scene which shows how to access and summarize runtime stats.

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
use ensogl_core::debug::stats;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::system::web;
use ensogl_hardcoded_theme as theme;
use ensogl_label::Label;
use ensogl_text_msdf_sys::run_once_initialized;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_stats() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
        init(&app);
        Leak::new(app);
    });
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let label = Label::new(app);
    app.display.add_child(&label);

    let stats_accumulator_data: stats::Accumulator = default();
    let stats_accumulator = Rc::new(RefCell::new(stats_accumulator_data));
    app.display
        .on_stats_available(f!([stats_accumulator] (stats) {
            if stats.frame_counter != 0 {
                stats_accumulator
                    .borrow_mut()
                    .add_sample(&stats);
            }
        }))
        .forget();

    let stats = app.display.scene().stats.clone();
    let mut frame_counter: usize = 0;
    app.display
        .on_frame(f_!([stats_accumulator] {
            if frame_counter % 60 == 0 {
                let stats_summary = stats_accumulator.borrow().summarize();
                let fps_summary = stats_summary.map(|s| s.fps);
                let text = iformat!(
                    "Press CTRL-OPTION-TILDE (TILDE is the key below ESC) to show Monitor panel"
                    "\n fps = " stats.fps()
                    "\n - min = " fps_summary.map_or(0.0, |s| s.min)
                    "\n - avg = " fps_summary.map_or(0.0, |s| s.avg)
                    "\n - max = " fps_summary.map_or(0.0, |s| s.max)
                );
                label.frp.set_content(text);
            }
            frame_counter += 1;
        }))
        .forget();
}
