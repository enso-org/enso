
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



// =================
// === Constants ===
// =================

const HEADER_HEIGHT: f32 = entry::HEIGHT;



// ==========================
// === Shapes Definitions ===
// ==========================

// TODO



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_header_text(String),
        set_entries(entry::AnyModelProvider<entry::Label>),
        // TODO: or `set_size(Vector2)` ??
        resize(Vector2<f32>),
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, style: &StyleWatchFrp) {
        let network = &api.network;
        let input = &api.input;
        // FIXME: should have separate style for CGV header text size most probably
        let header_text_size = style.get_number(theme::widget::list_view::text::size);
        // let background = &model.background.events;
        frp::extend! { network

            init <- source::<()>();
            // TODO[LATER]: looks like a common pattern (also in LV); is there a helper in FRP?
            eval input.resize((size) model.resize(*size));


            // === Header ===

            header_text_size <- all(&header_text_size,&init)._0();
            model.header.set_default_text_size <+ header_text_size.map(|v| text::Size(*v));
            model.header.set_content <+ input.set_header_text;


            // === Entries ===

            model.entries.show_background_shadow(false);
            model.entries.set_background_corners_radius(0.0);
            model.entries.set_custom_background_color(Some(Rgba(0.0, 1.0, 0.0, 1.0)));

            model.entries.set_entries <+ input.set_entries;
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

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
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

impl Model {
    fn resize(&self, size: Vector2) {
        // NOTE: Currently assuming that the widget's origin is at its center (global default).
        // FIXME: what origin we want? what origin does ListView use? IMO a corner makes sense,
        // though not sure if top-left or bottom-left is better. Alternatively, center is as good
        // as anything else.
        let top_left = Vector2(-size.x / 2.0, size.y / 2.0);
        // FIXME: what's the origin of text::Area? assuming left-center
        self.header.set_position_xy(top_left + Vector2(0.0, -HEADER_HEIGHT/2.0));
        // TODO: what's the origin of ListView? assuming center
        self.entries.set_position_y(-HEADER_HEIGHT);
        self.entries.resize(size - Vector2(0.0, HEADER_HEIGHT));
    }
}



// =================
// === Component ===
// =================

pub type View = Component<Model, Frp>;

