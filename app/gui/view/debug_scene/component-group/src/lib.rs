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
use ensogl_selector::Bounds;
use ensogl_text_msdf_sys::run_once_initialized;
use ide_view_component_group as component_group;
use list_view::entry::AnyModelProvider;
use ensogl_scroll_area::ScrollArea;
use ensogl_core::display::shape::*;

mod green_circle {
    use super::*;
    ensogl_core::define_shape_system! {
        (style:Style) {
            Circle(70.px()).fill(color::Rgba::transparent()).into()
        }
    }
}


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



// ====================
// === Mock Entries ===
// ====================

const PREPARED_ITEMS: &[&str; 8] = &[
    "long sample entry with text overflowing the width",
    "convert",
    "table input",
    "text input",
    "number input",
    "table output",
    "data output",
    "data input",
];

#[derive(Debug)]
struct MockEntries {
    entries: Vec<String>,
    count:   Cell<usize>,
}

impl MockEntries {
    fn new(count: usize) -> Rc<Self> {
        Rc::new(Self {
            entries: PREPARED_ITEMS.iter().cycle().take(count).map(ToString::to_string).collect(),
            count:   Cell::new(count),
        })
    }

    fn set_count(&self, count: usize) {
        if self.entries.len() >= count {
            self.count.set(count);
        }
    }

    fn get_entry(&self, id: list_view::entry::Id) -> Option<String> {
        self.entries.get(id).cloned()
    }
}

impl list_view::entry::ModelProvider<list_view::entry::Label> for MockEntries {
    fn entry_count(&self) -> usize {
        self.count.get()
    }

    fn get(&self, id: list_view::entry::Id) -> Option<String> {
        self.get_entry(id)
    }
}



// ========================
// === Init Application ===
// ========================


// === Helpers ====

fn create_selection() -> list_view::selection::View {
    let selection = list_view::selection::View::new(Logger::new("Selection"));
    selection.color.set(color::Rgba(0.527, 0.554, 0.18, 1.0).into());
    selection.size.set(Vector2(150.0, list_view::entry::HEIGHT));
    selection.corner_radius.set(5.0);
    selection
}

fn component_group(app: &Application) -> component_group::View {
    let component_group = app.new_view::<component_group::View>();
    let group_name = "Long group name with text overflowing the width";
    component_group.set_header(group_name.to_string());
    component_group.set_width(150.0);
    component_group.set_position_x(75.0);
    component_group.set_background_color(color::Rgba(0.927, 0.937, 0.913, 1.0));
    component_group
}

fn wide_component_group(app: &Application) -> component_group::wide::View {
    let wide_component_group = app.new_view::<component_group::wide::View>();
    wide_component_group.set_position_x(100.0);
    wide_component_group.set_width(450.0);
    wide_component_group.set_background_color(color::Rgba(0.927, 0.937, 0.913, 1.0));
    wide_component_group.set_no_items_label_text("No local variables.");
    wide_component_group
}

fn slider(app: &Application) -> selector::NumberPicker {
    let slider = app.new_view::<selector::NumberPicker>();
    app.display.add_child(&slider);
    slider.frp.resize(Vector2(400.0, 50.0));
    slider.frp.allow_click_selection(true);
    slider.frp.set_bounds(Bounds::new(0.0, 15.0));
    slider.set_position_y(250.0);
    slider.frp.set_caption(Some("Items count:".to_string()));
    slider
}


// === init ===

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let network = frp::Network::new("Component Group Debug Scene");
    let selection = create_selection();
    let selection_animation = Animation::<Vector2>::new(&network);

    let first_component_group = component_group(app);
    first_component_group.model().display_object.layer.add_exclusive(&selection);
    let second_component_group = component_group(app);
    let group_name = "Second component group";
    second_component_group.set_header(group_name.to_string());
    second_component_group.set_width(150.0);
    second_component_group.set_position_x(95.0);
    second_component_group.set_background_color(color::Rgba(0.527, 0.837, 0.713, 1.0));

    let scroll_area = ScrollArea::new(app);
    scroll_area.set_position_xy(Vector2(-400.0, 100.0));
    scroll_area.resize(Vector2(250.0, 400.0));
    scroll_area.set_content_width(250.0);
    scroll_area.set_content_height(2000.0);
    app.display.add_child(&scroll_area);
    scroll_area.content().add_child(&first_component_group);
    scroll_area.content().add_child(&second_component_group);

    let green_circle = green_circle::View::new(&app.logger);
    green_circle.size.set(Vector2(150.0, 150.0));
    green_circle.set_position_xy(Vector2(200.0, -150.0));
    scroll_area.content().add_child(&green_circle);
    std::mem::forget(green_circle);

    // === Regular Component Group ===

    frp::extend! { network
        selection_animation.target <+ first_component_group.selection_position_target;
        eval selection_animation.value ((pos) selection.set_position_xy(*pos));

        eval first_component_group.suggestion_accepted ([](id) DEBUG!("Accepted Suggestion {id}"));
        eval first_component_group.expression_accepted ([](id) DEBUG!("Accepted Expression {id}"));
        eval_ first_component_group.header_accepted ([] DEBUG!("Accepted Header"));

        eval first_component_group.size((size) first_component_group.set_position_y(- size.y / 2.0));
        _eval <- all_with(&first_component_group.size, &second_component_group.size, f!((f, s) second_component_group.set_position_y(- s.y / 2.0 - f.y - 5.0)));

        eval scroll_area.scroll_position_y((y) first_component_group.set_viewport_size(*y));
        //eval scroll_area.scroll_position_y((y) second_component_group.set_viewport_size(*y));
    }
    selection_animation.target.emit(first_component_group.selection_position_target.value());
    selection_animation.skip.emit(());
    let mock_entries = MockEntries::new(10);
    let model_provider = AnyModelProvider::from(mock_entries.clone_ref());
    first_component_group.set_entries(model_provider.clone_ref());
    second_component_group.set_entries(model_provider);

    std::mem::forget(network);
    std::mem::forget(selection);
    std::mem::forget(scroll_area);
    std::mem::forget(first_component_group);
    std::mem::forget(second_component_group);
}
