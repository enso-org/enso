//! A debug scene which shows the Select Component. The chosen entries are logged in console.

use crate::prelude::*;

use ensogl_core::system::web;
use ensogl_core::application::Application;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::*;
use ensogl_core::data::color;
use ensogl_text_msdf_sys::run_once_initialized;
use ensogl_gui_components::list_view;
use wasm_bindgen::prelude::*;
use ensogl_theme as theme;
use ensogl_core::display;
use ensogl_text as text;
use ensogl_gui_components::list_view::entry::Entry;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_list_view() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
        init(&app);
        mem::forget(app);
    });
}



// ====================
// === Mock Entries ===
// ====================

// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
const PADDING : f32 = 10.0;

mod icon {
    pub const SIZE : f32 = 10.0;

    use super::*;
    ensogl_core::define_shape_system! {
        (style:Style,color_rgba:Vector4) {
            let width : Var<Pixels> = "input_size.x".into();
            (Circle(&width/2.0) - Circle(width/2.75)).fill(color_rgba).into()
        }
    }
}


#[derive(Debug)]
struct MockEntry {
    display_object : display::object::Instance,
    label          : text::Area,
    icon           : icon::View,
}

impl MockEntry {
    fn new(app:&Application, text:String) -> MockEntry {
        let logger = Logger::new("MockEntry");
        let display_object = display::object::Instance::new(&logger);

        let label = text::Area::new(app);
        display_object.add_child(&label);
        label.set_position_x(PADDING);
        label.set_position_y(6.0);
        label.set_content(text);

        let icon = icon::View::new(&logger);
        display_object.add_child(&icon);
        app.display.scene().layers.label.add_exclusive(&icon);
        icon.size.set(Vector2(icon::SIZE,icon::SIZE));

        let result = MockEntry {display_object,label,icon};
        result.set_focused(false);
        result
    }
}

impl display::Object for MockEntry {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl Entry for MockEntry {
    fn set_focused(&self, selected: bool) {
        let color = if selected {color::Rgba::white()} else {color::Rgba::black()};
        self.label.set_color_all(&color);
        self.icon.color_rgba.set(color.into());
    }

    fn set_width(&self, width:f32) {
        self.icon.set_position_x(width - icon::SIZE/2.0 - PADDING);
    }
}


#[derive(Clone,Debug)]
struct MockEntries {
    logger        : Logger,
    entries_count : usize,
}

impl MockEntries {
    fn new(entries_count:usize) -> Self {
        let logger  = Logger::new("MockEntries");
        Self {logger,entries_count}
    }
}

impl list_view::entry::EntryProvider for MockEntries {
    fn entry_count(&self) -> usize { self.entries_count }

    fn get(&self, app:&Application, id:usize) -> Option<list_view::entry::AnyEntry> {
        if id >= self.entries_count {
            None
        } else {
            let entry = MockEntry::new(app,iformat!("Entry {id}"));
            Some(entry.into())
        }
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app:&Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let list_view = app.new_view::<list_view::ListView>();
    let provider  = list_view::entry::AnyEntryProvider::from(MockEntries::new(1000));
    list_view.frp.resize(Vector2(130.0,160.0));
    list_view.frp.set_entries(provider);
    list_view.set_selection_method(list_view::SelectionMethod::Hover);
    list_view.focus();
    app.display.add_child(&list_view);
    // FIXME[WD]: This should not be needed after text gets proper depth-handling.
    app.display.scene().layers.below_main.add_exclusive(&list_view);

    let logger : Logger = Logger::new("SelectDebugScene");
    let network = enso_frp::Network::new("test");
    enso_frp::extend! {network
        eval list_view.chosen_entry([logger](entry) {
            info!(logger, "Chosen entry {entry:?}")
        });
    }

    std::mem::forget(list_view);
    std::mem::forget(network);
}
