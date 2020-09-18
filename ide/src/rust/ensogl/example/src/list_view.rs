//! A debug scene which shows the Select Component. The chosen entries are logged in console.

use crate::prelude::*;

use ensogl_core::system::web;
use ensogl_core::application::Application;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::*;
use ensogl_core::data::color;
use ensogl_core::gui;
use ensogl_text_msdf_sys::run_once_initialized;
use ensogl_gui_list_view as list_view;
use logger::enabled::Logger;
use wasm_bindgen::prelude::*;
use ensogl_core::display::Scene;
use ensogl_text::buffer::data::unit::Bytes;
use ensogl_theme;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_list_view() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
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

mod icon {
    use super::*;
    ensogl_core::define_shape_system! {
        (style:Style,id:f32) {
            let width  = list_view::entry::ICON_SIZE.px();
            let height = list_view::entry::ICON_SIZE.px();
            let color  : Var<color::Rgba> = "rgba(input_id/16.0,0.0,0.0,1.0)".into();
            Rect((&width,&height)).fill(color).into()
        }
    }
}


#[derive(Clone,Debug)]
struct MockEntries {
    logger        : Logger,
    scene         : Scene,
    entries_count : usize,
}

impl MockEntries {
    fn new(app:&Application, entries_count:usize) -> Self {
        let logger  = Logger::new("MockEntries");
        let scene   = app.display.scene().clone_ref();
        Self {logger,scene,entries_count}
    }
}

impl list_view::entry::ModelProvider for MockEntries {
    fn entry_count(&self) -> usize { self.entries_count }

    fn get(&self, id:usize) -> Option<list_view::entry::Model> {
        if id >= self.entries_count {
            None
        } else {
            use list_view::entry::ICON_SIZE;
            let icon = gui::component::ShapeView::<icon::Shape>::new(&self.logger,&self.scene);
            icon.shape.sprite.size.set(Vector2(ICON_SIZE,ICON_SIZE));
            icon.shape.id.set(id as f32);
            let model = list_view::entry::Model::new(iformat!("Entry {id}")).with_icon(icon);
            if id == 10 { Some(model.highlight(std::iter::once((Bytes(1)..Bytes(3)).into()))) }
            else        { Some(model)                                                         }
        }
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app:&Application) {
    ensogl_theme::dark::setup(&app);
    ensogl_theme::light::setup(&app);

    let select                                      = app.new_view::<list_view::ListView>();
    let provider:list_view::entry::AnyModelProvider = MockEntries::new(app,1000).into();
    select.frp.resize(Vector2(100.0,160.0));
    select.frp.set_entries(provider);
    app.display.add_child(&select);

    let logger  = Logger::new("SelectDebugScene");
    let network = enso_frp::Network::new();
    enso_frp::extend! {network
        eval select.chosen_entry([logger](entry) {
            info!(logger, "Chosen entry {entry:?}")
        });
    }

    std::mem::forget(select);
    std::mem::forget(network);
}
