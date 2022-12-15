//! A debug scene which shows the Dynamic drop-down component.

#![recursion_limit = "1024"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_drop_down::Dropdown;
use ensogl_drop_down::DropdownValue;
use ensogl_hardcoded_theme as theme;
use ensogl_text_msdf::run_once_initialized;



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
    let navigator = Navigator::new(scene, &scene.camera());
    navigator.disable_wheel_panning();

    app.views.register::<Dropdown<EntryData>>();
    let main_dropdown = setup_main_dropdown(app);

    let multi_config_dropdown = setup_static_dropdown(app, Vector2(-200.0, 0.0), None, vec![
        SelectConfigEntry("Single select".into(), false),
        SelectConfigEntry("Multi select".into(), true),
    ]);

    let open_dropdown = setup_static_dropdown(app, Vector2(-200.0, -100.0), None, vec![
        SelectConfigEntry("Opened".into(), true),
        SelectConfigEntry("Closed".into(), false),
    ]);

    let secondary_dropdown = setup_static_dropdown(app, Vector2(100.0, 0.0), None, vec![]);

    let static_entries =
        vec!["Hello", "World", "This", "Is", "A", "Test", "Dropdown", "With", "Static", "Strings"];

    let dropdown_static1 =
        setup_static_dropdown(app, Vector2(300.0, 0.0), None, static_entries.clone());

    let dropdown_static2 = setup_static_dropdown(
        app,
        Vector2(400.0, 0.0),
        Some(Vector2(50.0, 200.0)),
        static_entries.clone(),
    );

    frp::new_network! { network
        main_dropdown.set_multiselect <+ multi_config_dropdown.single_selected_entry.unwrap().map(SelectConfigEntry::item);
        main_dropdown.set_open <+ open_dropdown.single_selected_entry.unwrap().map(SelectConfigEntry::item);

        dropdown_static1.set_selected_entries <+ dropdown_static2.selected_entries;
        dropdown_static2.set_selected_entries <+ dropdown_static1.selected_entries;

        selected_vec <- main_dropdown.selected_entries.map(|map| map.iter().cloned().collect());
        secondary_dropdown.set_all_entries <+ selected_vec;
    }

    world.add_child(&main_dropdown);
    world.add_child(&multi_config_dropdown);
    world.add_child(&open_dropdown);
    world.add_child(&secondary_dropdown);
    world.add_child(&dropdown_static1);
    world.add_child(&dropdown_static2);

    std::mem::forget((
        main_dropdown,
        multi_config_dropdown,
        open_dropdown,
        secondary_dropdown,
        dropdown_static1,
        dropdown_static2,
        navigator,
        network,
    ));
}

fn setup_main_dropdown(app: &Application) -> Dropdown<EntryData> {
    let dropdown = app.new_view::<Dropdown<EntryData>>();
    dropdown.set_xy(Vector2(0.0, 0.0));
    dropdown.set_number_of_entries(2000);

    fn entry_for_row(row: usize) -> EntryData {
        EntryData(row as i32 * 3 / 2 - 70)
    }

    let network = dropdown.network();
    frp::extend! { network
        entries <- dropdown.entries_in_range_needed.map(|range| (range.clone(), range.clone().map(entry_for_row).collect()));
        dropdown.provide_entries_at_range <+ entries;
    }

    dropdown
}

fn setup_static_dropdown<T: DropdownValue>(
    app: &Application,
    pos: Vector2,
    max_size: Option<Vector2>,
    values: Vec<T>,
) -> Dropdown<T> {
    let dropdown = app.new_view::<Dropdown<T>>();
    dropdown.set_xy(pos);
    if let Some(max_size) = max_size {
        dropdown.set_max_size(max_size);
    }
    dropdown.set_all_entries(values);
    dropdown.set_open(true);
    dropdown
}



// ========================
// === Demo entry types ===
// ========================

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct EntryData(i32);

impl DropdownValue for EntryData {
    fn label(&self) -> ImString {
        format!("{}", self.0).into()
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SelectConfigEntry<T>(ImString, T);

impl<T> DropdownValue for SelectConfigEntry<T>
where T: Debug + Clone + PartialEq + Eq + Hash + 'static
{
    fn label(&self) -> ImString {
        self.0.clone()
    }
}
impl<T> SelectConfigEntry<T>
where T: Clone
{
    fn item(&self) -> T {
        self.1.clone()
    }
}
