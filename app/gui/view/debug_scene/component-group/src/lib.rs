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
use ensogl_list_view::entry::GlyphHighlightedLabelModel;
use ensogl_selector as selector;
use ensogl_selector::Bounds;
use ensogl_text_msdf_sys::run_once_initialized;
use ide_view_component_group as component_group;
use ide_view_component_group::entry;
use ide_view_component_group::icon;
use ide_view_component_group::Entry;
use list_view::entry::AnyModelProvider;



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

const PREPARED_ITEMS: &[(&str, icon::Id)] = &[
    ("long sample entry with text overflowing the width", icon::Id::Star),
    ("convert", icon::Id::Convert),
    ("table input", icon::Id::DataInput),
    ("text input", icon::Id::TextInput),
    ("number input", icon::Id::NumberInput),
    ("table output", icon::Id::TableEdit),
    ("data output", icon::Id::DataOutput),
    ("data input", icon::Id::DataInput),
];

#[derive(Debug)]
struct MockEntries {
    entries: Vec<component_group::entry::Model>,
    count:   Cell<usize>,
}

impl MockEntries {
    fn new(count: usize) -> Rc<Self> {
        Rc::new(Self {
            entries: PREPARED_ITEMS
                .iter()
                .cycle()
                .take(count)
                .map(|&(label, icon)| entry::Model {
                    icon,
                    highlighted_text: GlyphHighlightedLabelModel {
                        label:       label.to_owned(),
                        highlighted: default(),
                    },
                })
                .collect(),
            count:   Cell::new(count),
        })
    }

    fn set_count(&self, count: usize) {
        if self.entries.len() >= count {
            self.count.set(count);
        }
    }

    fn get_entry(&self, id: list_view::entry::Id) -> Option<entry::Model> {
        self.entries.get(id).cloned()
    }
}

impl list_view::entry::ModelProvider<Entry> for MockEntries {
    fn entry_count(&self) -> usize {
        self.count.get()
    }

    fn get(&self, id: list_view::entry::Id) -> Option<entry::Model> {
        self.get_entry(id)
    }
}



// ========================
// === Init Application ===
// ========================


// === Helpers ====

fn create_selection() -> list_view::selection::View {
    let selection = list_view::selection::View::new(Logger::new("Selection"));
    selection.size.set(Vector2(150.0, list_view::entry::HEIGHT));
    selection.corner_radius.set(5.0);
    selection
}

fn create_component_group(app: &Application) -> component_group::View {
    let component_group = app.new_view::<component_group::View>();
    app.display.add_child(&component_group);
    component_group.set_width(150.0);
    component_group
}

fn wide_component_group(app: &Application) -> component_group::wide::View {
    let wide_component_group = app.new_view::<component_group::wide::View>();
    app.display.add_child(&wide_component_group);
    wide_component_group.set_position_x(250.0);
    wide_component_group.set_width(450.0);
    wide_component_group.set_color(color::Rgba(0.927, 0.937, 0.913, 1.0));
    wide_component_group.set_no_items_label_text("No local variables.");
    wide_component_group
}

fn items_slider(app: &Application) -> selector::NumberPicker {
    let slider = app.new_view::<selector::NumberPicker>();
    app.display.add_child(&slider);
    slider.frp.resize(Vector2(400.0, 50.0));
    slider.frp.allow_click_selection(true);
    slider.frp.set_bounds(Bounds::new(0.0, 15.0));
    slider.set_position_y(-250.0);
    slider.frp.set_caption(Some("Items count:".to_string()));
    slider
}

fn color_component_slider(app: &Application, caption: &str) -> selector::NumberPicker {
    let slider = app.new_view::<selector::NumberPicker>();
    app.display.add_child(&slider);
    slider.frp.allow_click_selection(true);
    slider.frp.resize(Vector2(400.0, 30.0));
    slider.frp.set_bounds.emit(selector::Bounds::new(0.0, 1.0));
    slider.frp.set_caption(Some(caption.to_string()));
    slider
}


// === init ===

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let items_slider = items_slider(app);
    let network = frp::Network::new("Component Group Debug Scene");
    let selection = create_selection();
    selection.color.set(color::Rgba(0.527, 0.554, 0.18, 1.0).into());
    selection.size.set(Vector2(
        150.0 + 2.0 * list_view::SHAPE_MARGIN - 8.0,
        29.0 + 2.0 * list_view::SHAPE_MARGIN,
    ));
    selection.corner_radius.set(10.0);

    let selection_animation = Animation::<Vector2>::new(&network);
    let wide_selection = create_selection();
    let wide_selection_animation = Animation::<Vector2>::new(&network);

    let component_group = create_component_group(app);
    component_group.add_child(&selection);
    let group_name = "Long group name with text overflowing the width";
    component_group.set_header(group_name.to_string());
    component_group.set_position_x(-150.0);
    let dimmed_component_group = create_component_group(app);
    dimmed_component_group.set_dimmed(true);
    dimmed_component_group.set_header("Input / Output".to_string());
    dimmed_component_group.set_position_x(-350.0);
    let wide_component_group = wide_component_group(app);
    wide_component_group.add_child(&wide_selection);


    // === Regular Component Group ===

    frp::extend! { network
        selection_animation.target <+ component_group.selection_position_target;
        eval selection_animation.value ((pos) selection.set_position_xy(*pos));

        eval component_group.suggestion_accepted ([](id) DEBUG!("Accepted Suggestion {id}"));
        eval component_group.expression_accepted ([](id) DEBUG!("Accepted Expression {id}"));
        eval_ component_group.header_accepted ([] DEBUG!("Accepted Header"));
    }
    selection_animation.target.emit(component_group.selection_position_target.value());
    selection_animation.skip.emit(());


    // === Wide Component Group ===

    wide_selection.color.set(color::Rgba(0.527, 0.554, 0.18, 1.0).into());
    frp::extend! { network
        wide_selection_animation.target <+ wide_component_group.selection_position_target;
        eval wide_selection_animation.value ((pos) wide_selection.set_position_xy(*pos));

        eval wide_component_group.suggestion_accepted ([](id) DEBUG!("[Wide] Accepted Suggestion {id}"));
        eval wide_component_group.expression_accepted ([](id) DEBUG!("[Wide] Accepted Expression {id}"));

        no_entries <- wide_component_group.entry_count.map(|count| *count == 0);
        hide_selection <- no_entries.on_true();
        show_selection <- no_entries.on_false();
        eval_ hide_selection (wide_selection.color.set(color::Rgba::transparent().into()));
        eval_ show_selection (wide_selection.color.set(color::Rgba(0.527, 0.554, 0.18, 1.0).into()));
    }
    wide_selection_animation.target.emit(wide_component_group.selection_position_target.value());
    wide_selection_animation.skip.emit(());


    // === Setup slider to change entry count ===

    let mock_entries = MockEntries::new(25);
    let model_provider = AnyModelProvider::from(mock_entries.clone_ref());
    frp::extend! { network
        int_value <- items_slider.frp.output.value.map(|v| *v as usize);
        eval int_value([component_group, dimmed_component_group, wide_component_group](i) {
            mock_entries.set_count(*i);
            component_group.set_entries(model_provider.clone_ref());
            dimmed_component_group.set_entries(model_provider.clone_ref());
            wide_component_group.set_entries(model_provider.clone_ref());
        });
    }
    items_slider.frp.set_value(10.0);
    // Select the bottom left entry at the start.
    let first_column = component_group::wide::ColumnId::new(0);
    wide_component_group.select_entry(first_column, 0);


    // === Color sliders ===

    let red_slider = color_component_slider(app, "Red");
    red_slider.set_position_y(350.0);
    red_slider.set_track_color(color::Rgba::new(1.0, 0.60, 0.60, 1.0));
    let red_slider_frp = &red_slider.frp;

    let green_slider = color_component_slider(app, "Green");
    green_slider.set_position_y(300.0);
    green_slider.set_track_color(color::Rgba::new(0.6, 1.0, 0.6, 1.0));
    let green_slider_frp = &green_slider.frp;

    let blue_slider = color_component_slider(app, "Blue");
    blue_slider.set_position_y(250.0);
    blue_slider.set_track_color(color::Rgba::new(0.6, 0.6, 1.0, 1.0));
    let blue_slider_frp = &blue_slider.frp;

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

    std::mem::forget(red_slider);
    std::mem::forget(green_slider);
    std::mem::forget(blue_slider);
    std::mem::forget(items_slider);
    std::mem::forget(network);
    std::mem::forget(selection);
    std::mem::forget(component_group);
    std::mem::forget(dimmed_component_group);
    std::mem::forget(wide_component_group);
    std::mem::forget(wide_selection);
}
