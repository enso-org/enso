use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_group as theme;
use ensogl_list_view as list_view;
use ensogl_text as text;



// =================
// === Constants ===
// =================



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_header_text(String),
        set_entries(list_view::entry::AnyModelProvider<list_view::entry::Label>),
        set_background_color(Rgba),
        set_size(Vector2),
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, style: &StyleWatchFrp) {
        let network = &api.network;
        let input = &api.input;
        for group in model.groups.iter() {
            frp::extend! { network
                group.set_entries <+ input.set_entries;
                group.resize <+ input.set_size;
            }
        }
    }
}



// =============
// === Model ===
// =============

/// The Model of the [`View`] component.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object: display::object::Instance,
    groups:         Rc<[list_view::ListView<list_view::entry::Label>; 3]>,
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "WideComponentGroupView"
    }

    fn new(app: &Application, logger: &Logger) -> Self {
        let display_object = display::object::Instance::new(&logger);
        let groups = Rc::new([
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
        ]);
        let mut x = -150.0;
        for group in groups.iter() {
            group.set_position_x(x);
            x += 150.0;
            display_object.add_child(group);
        }

        Model { display_object, groups }
    }
}

impl Model {}



// ============
// === View ===
// ============

/// The implementation of the visual component described in the module's documentation.
pub type View = component::ComponentView<Model, Frp>;
