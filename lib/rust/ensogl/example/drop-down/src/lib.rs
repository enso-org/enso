//! A debug scene which shows the Dynamic drop-down component.

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
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

mod dropdown;

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_hardcoded_theme as theme;
use ensogl_label::Label;
use ensogl_text as text;
use ensogl_text_msdf::run_once_initialized;

type EntryData = i32;

// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(app);
    theme::builtin::light::register(app);
    theme::builtin::light::enable(app);

    let world = &app.display;
    let scene = &world.default_scene;

    app.views.register::<dropdown::Dropdown<EntryData>>();
    let dropdown = setup_dropdown(app);
    world.add_child(&dropdown);

    dropdown.request_model_for_visible_entries();

    let output_text = setup_output_text(app, &dropdown);

    frp::new_network! { network
        is_single <- source::<bool>();
        is_multi <- source::<bool>();
    }

    let (single_selection, is_single) = setup_button(app, Vector2(-200.0, -100.0), "Single Select");
    let (multi_selection, is_multi) = setup_button(app, Vector2(-200.0, -130.0), "Multi Select");

    frp::new_network! { network
        single <- single_selection.constant(1);
        multi  <- multi_selection.constant(1000);
        selection_mode <- any(single,multi);
        dropdown.set_max_selected <+ selection_mode;

        eval selection_mode([is_single, is_multi](mode) {
            warn!("eval");
            is_single.emit(*mode == 1);
            is_multi.emit(*mode > 1);
        });
    }

    is_single.emit(true);
    is_multi.emit(false);

    let navigator = Navigator::new(scene, &scene.camera());
    navigator.disable_wheel_panning();

    std::mem::forget((dropdown, navigator, output_text, network));
}


fn model_for_entry(row: usize) -> dropdown::DropdownEntry<EntryData> {
    let val = row as i32 * 3 / 2 - 7;
    dropdown::DropdownEntry::<EntryData>::new(val, format!("Value {val}").into())
}

fn setup_dropdown(app: &Application) -> dropdown::Dropdown<EntryData> {
    let dropdown = app.new_view::<dropdown::Dropdown<EntryData>>();
    dropdown.set_xy(Vector2(0.0, 0.0));
    dropdown.set_number_of_entries(20);

    frp::new_network! { network
        dropdown.model_for_entry <+ dropdown.model_for_entry_needed.map(|row| (*row, model_for_entry(*row)));
    }

    std::mem::forget(network);
    dropdown
}

fn setup_output_text(app: &Application, dropdown: &dropdown::Dropdown<EntryData>) -> text::Text {
    let text = app.new_view::<text::Text>();
    app.display.add_child(&text);
    text.set_xy(Vector2(-200.0, -200.0));

    frp::new_network! { network
        text.set_content <+ dropdown.selected_values.map(|values| {
            use std::fmt::Write;
            let mut buf = String::new();
            for val in values {
                let content = val.value;
                write!(&mut buf, "{content} ").unwrap();
            }
            ImString::from(buf)
        });
    }

    std::mem::forget(network);
    text
}


mod button_bg {
    use super::*;
    ensogl_core::shape! {
        (style:Style, color_rgba: Vector4<f32>) {
            let size          = Var::canvas_size();
            let color         = Var::<color::Rgba>::from(color_rgba);
            let overlay       = Rect(size).corners_radius(5.0.px());
            let overlay       = overlay.fill(color);
            let out           = overlay;
            out.into()
        }
    }
}

fn setup_button(
    app: &Application,
    pos: Vector2,
    text: &str,
) -> (frp::Stream<()>, frp::Source<bool>) {
    let label = app.new_view::<text::Text>();
    let bg = button_bg::View::new();
    label.set_xy(pos);
    label.set_content(text.to_string());
    bg.set_xy(pos);
    let color_active = Vector4(0.6, 0.95, 0.8, 1.0);
    let color_inactive = Vector4(0.9, 1.0, 0.95, 1.0);

    app.display.add_child(&bg);
    app.display.add_child(&label);
    label.add_to_scene_layer(&app.display.default_scene.layers.label);

    frp::new_network! { network
        is_active <- source::<bool>();
        trigger <- bg.events.mouse_down.constant(());

        size <- all(&label.width, &label.height);
        eval size([bg] ((w, h)) {
            let bg_size = Vector2(w + 10.0, h + 10.0);
            bg.size.set(bg_size);
            bg.set_xy(pos + Vector2(w / 2.0, -h / 2.0));
        });
        color <- is_active.map(move |active| if *active { color_active } else { color_inactive });
        eval color((color) bg.color_rgba.set(*color));
    }

    is_active.emit(false);

    std::mem::forget((network, label, bg));
    (trigger.into(), is_active.into())
}
