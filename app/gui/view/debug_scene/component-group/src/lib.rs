//! A debug scene which shows the Component Group visual component.

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
use ensogl_core::data::color;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::frp;
use ensogl_core::Animation;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view as list_view;
use ensogl_selector as selector;
use ensogl_text_msdf_sys::run_once_initialized;
use ide_view_component_group as component_group;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[entry_point]
pub fn main() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}

fn make_number_picker(app: &Application, caption: &str) -> Leak<selector::NumberPicker> {
    let slider = app.new_view::<selector::NumberPicker>();
    slider.frp.allow_click_selection(true);
    slider.frp.resize(Vector2(400.0, 30.0));
    slider.frp.set_bounds.emit(selector::Bounds::new(0.0, 1.0));
    slider.frp.set_caption(Some(caption.to_string()));
    app.display.add_child(&slider);
    Leak::new(slider)
}



// ====================
// === Mock Entries ===
// ====================

#[derive(Clone, Debug)]
struct MockEntries {
    entries: Vec<component_group::entry::Model>,
}

impl MockEntries {
    fn new(entries: &[&str]) -> Self {
        Self { entries: entries.iter().map(|&label| label.into()).collect() }
    }

    fn get_entry(&self, i: usize) -> Option<component_group::entry::Model> {
        self.entries.get(i).cloned()
    }
}

impl list_view::entry::ModelProvider<component_group::Entry> for MockEntries {
    fn entry_count(&self) -> usize {
        self.entries.len()
    }

    fn get(&self, id: usize) -> Option<component_group::entry::Model> {
        self.get_entry(id)
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let mock_entries = MockEntries::new(&[
        "long sample entry with text overflowing the width",
        "convert",
        "table input",
        "text input",
        "number input",
        "table input",
        "data output",
        "data input",
    ]);

    let network = frp::Network::new("Component Group Debug Scene");


    // === Component groups ===

    let component_group = app.new_view::<component_group::View>();
    let dimmed_component_group = app.new_view::<component_group::View>();
    let provider = list_view::entry::AnyModelProvider::new(mock_entries);
    let group_name = "Long group name with text overflowing the width";
    component_group.set_header(group_name.to_string());
    component_group.set_entries(&provider);
    component_group.set_width(150.0);
    app.display.add_child(&component_group);
    dimmed_component_group.set_dimmed(true);
    dimmed_component_group.set_header("Input / Output".to_string());
    dimmed_component_group.set_entries(provider);
    dimmed_component_group.set_width(150.0);
    dimmed_component_group.set_position_x(-200.0);
    app.display.add_child(&dimmed_component_group);


    // === Selection ===

    let selection = list_view::selection::View::new(Logger::new("Selection"));
    selection.size.set(Vector2(150.0, list_view::entry::HEIGHT));
    selection.corner_radius.set(5.0);
    let selection_animation = Animation::<Vector2>::new(&network);
    app.display.add_child(&selection);

    frp::extend! { network
        selection_animation.target <+ component_group.selection_position_target;
        eval selection_animation.value ((pos) selection.set_position_xy(*pos));

        eval component_group.suggestion_accepted ([](id) DEBUG!("Accepted Suggestion {id}"));
        eval component_group.expression_accepted ([](id) DEBUG!("Accepted Expression {id}"));
        eval_ component_group.header_accepted ([] DEBUG!("Accepted Header"));
    }
    selection_animation.target.emit(component_group.selection_position_target.value());
    selection_animation.skip.emit(());


    // === Color sliders ===

    let red_slider = make_number_picker(app, "Red");
    red_slider.inner().set_position_y(300.0);
    red_slider.inner().set_track_color(color::Rgba::new(1.0, 0.60, 0.60, 1.0));
    let red_slider_frp = &red_slider.inner().frp;

    let green_slider = make_number_picker(app, "Green");
    green_slider.inner().set_position_y(250.0);
    green_slider.inner().set_track_color(color::Rgba::new(0.6, 1.0, 0.6, 1.0));
    let green_slider_frp = &green_slider.inner().frp;

    let blue_slider = make_number_picker(app, "Blue");
    blue_slider.inner().set_position_y(200.0);
    blue_slider.inner().set_track_color(color::Rgba::new(0.6, 0.6, 1.0, 1.0));
    let blue_slider_frp = &blue_slider.inner().frp;

    let default_color = color::Rgba(0.527, 0.554, 0.18, 1.0);
    frp::extend! { network
        init <- source_();
        red_slider_frp.set_value <+ init.constant(default_color.red);
        green_slider_frp.set_value <+ init.constant(default_color.green);
        blue_slider_frp.set_value <+ init.constant(default_color.blue);
        let red_slider_value = &red_slider_frp.value;
        let green_slider_value = &green_slider_frp.value;
        let blue_slider_value = &blue_slider_frp.value;
        color <- all_with3(red_slider_value, green_slider_value, blue_slider_value,
            |r,g,b| color::Rgba(*r, *g, *b, 1.0));
        component_group.set_color <+ color;
        dimmed_component_group.set_color <+ color;
        eval color((c) selection.color.set(c.into()));
    }
    init.emit(());


    // === Forget ===

    std::mem::forget(network);
    std::mem::forget(selection);
    std::mem::forget(component_group);
    std::mem::forget(dimmed_component_group);
}
