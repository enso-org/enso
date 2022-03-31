
#![recursion_limit = "512"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
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



/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::scene::layer::LayerId;
use ensogl_core::display::shape::*;
use ensogl_core::DEPRECATED_Animation;
use ensogl_gui_component::component;
use ensogl_gui_component::component::Component;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view::ListView;
use ensogl_list_view::entry;
use ensogl_shadow as shadow;
use ensogl_text as text;



// ==========================
// === Shapes Definitions ===
// ==========================

// TODO



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_entries(entry::AnyModelProvider<entry::Label>),
        // TODO: or `set_size(Vector2)` ??
        resize(Vector2<f32>),
        // set_content(String),
        // set_size(Vector2)
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, _style: &StyleWatchFrp) {
        let network = &api.network;
        // let background = &model.background.events;
        frp::extend! { network
            model.entries.show_background_shadow(false);
            model.entries.set_background_corners_radius(0.0);
            model.entries.set_custom_background_color(Some(Rgba(0.0, 1.0, 0.0, 1.0)));

            model.entries.set_entries <+ api.input.set_entries;
            model.entries.resize <+ api.input.resize;
            // eval api.input.set_content((t) model.set_content(t));
            // eval api.input.set_size((size) model.set_size(*size));

            // is_hovered <- bool(&background.mouse_out, &background.mouse_over);
            // eval is_hovered((hovered) model.set_label_visible(*hovered));
        }
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object: display::object::Instance,
    header:         text::Area,
    entries:        ListView<entry::Label>,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentGroupView"
    }

    fn new(app: &Application, logger: &Logger) -> Self {
        // let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new(&logger);
        // let label = default();
        // let text = default();

        let entries = ListView::new(app);
        display_object.add_child(&entries);
        let header = text::Area::new(app);
        display_object.add_child(&header);
        // let background = background::View::new(&logger);
        // display_object.add_child(&background);
        // scene.layers.tooltip.add_exclusive(&background);

        // let app = app.clone_ref();
        // Model { app, background, label, display_object, text }
        Model { display_object, header, entries }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =================
// === Component ===
// =================

pub type View = Component<Model, Frp>;

