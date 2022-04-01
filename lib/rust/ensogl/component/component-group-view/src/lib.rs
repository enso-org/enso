
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
use ensogl_list_view as list_view;
use ensogl_list_view::ListView;
use ensogl_list_view::entry;
use ensogl_shadow as shadow;
use ensogl_text as text;



// =================
// === Constants ===
// =================

const HEADER_HEIGHT: f32 = entry::HEIGHT;
const HEADER_LEFT_PADDING: f32 = entry::PADDING;



// ==========================
// === Shapes Definitions ===
// ==========================


// === Header Background ===

pub mod header_background {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,color:Vector4) {
            let sprite_width  : Var<Pixels> = "input_size.x".into();
            let sprite_height : Var<Pixels> = "input_size.y".into();
            // TODO: padding?
            let color = Var::<Rgba>::from(color);
            let shape = Rect((&sprite_width,&sprite_height)).fill(color);
            shape.into()
        }
    }
}

pub mod mcdbg {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style) {
            let sprite_width  : Var<Pixels> = "input_size.x".into();
            let sprite_height : Var<Pixels> = "input_size.y".into();
            let shape = Rect((&sprite_width,&sprite_height)).fill(Rgba(1.0,0.0,0.0,1.0));
            shape.into()
        }
    }
}


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
        model.header_background.color.set(Rgba(1.0, 1.0, 0.7, 1.0).into());
        frp::extend! { network
            // FIXME: taken from list_view::Model::padding(); this itself and calculations around it
            // look fishy
            let lv_padding = style.get_number(ensogl_hardcoded_theme::application::searcher::padding);

            init <- source::<()>();

            size_and_lv_padding <- all(&input.resize, &lv_padding);
            // TODO[LATER]: looks like a common pattern (also in LV); is there a helper in FRP?
            // eval input.resize((size) model.resize(*size));
            eval size_and_lv_padding(((size, lv_padding)) model.resize(*size, *lv_padding));


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
    display_object:    display::object::Instance,
    header:            text::Area,
    header_background: header_background::View,
    mcdbg: mcdbg::View,
    entries:           ListView<entry::Label>,
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
        let header_background = header_background::View::new(&logger);
        let mcdbg = mcdbg::View::new(&logger);
        let header = text::Area::new(app);
        display_object.add_child(&entries);
        display_object.add_child(&mcdbg);
        display_object.add_child(&header_background);
        display_object.add_child(&header);
        // let background = background::View::new(&logger);
        // display_object.add_child(&background);
        // scene.layers.tooltip.add_exclusive(&background);

        // TODO: this is based on code in list_view::entry::List::new(); is it right?
        // let label_layer = app.display.default_scene.layers.label.id();
        let label_layer = &app.display.default_scene.layers.label;
        header.add_to_scene_layer(label_layer);

        ensogl_core::shapes_order_dependencies! {
            app.display.default_scene => {
                // TODO: how to hide list_view text "below" header_background, but keep the header
                // text above header_background?
                mcdbg -> list_view::background;
                list_view::selection -> header_background;
                // header_background -> text;
                //background            -> list_view::selection;
                //list_view::background -> background;
            }
        }

        // let app = app.clone_ref();
        // Model { app, background, label, display_object, text }
        Model { display_object, header, mcdbg, header_background, entries }
    }
}

impl Model {
    fn resize(&self, size: Vector2, lv_padding: f32) {

        // NOTE: Currently assuming that the widget's origin is at its center (global default).
        // FIXME: what origin we want? what origin does ListView use? IMO a corner makes sense,
        // though not sure if top-left or bottom-left is better. Alternatively, center is as good
        // as anything else.
        let half_height = size.y / 2.0;
        let top_left = Vector2(-size.x / 2.0, half_height);
        self.mcdbg.size.set(size);
        // FIXME: what's the origin of text::Area? assuming left-center
        self.header.set_position_xy(top_left + Vector2(HEADER_LEFT_PADDING + lv_padding/2.0, -HEADER_HEIGHT/2.0));
        self.header_background.size.set(Vector2(size.x, HEADER_HEIGHT));
        self.header_background.set_position_y(half_height - HEADER_HEIGHT / 2.0);
        // TODO: what's the origin of ListView? assuming center
        self.entries.set_position_y(-HEADER_HEIGHT / 2.0);
        self.entries.resize(size - Vector2(0.0, HEADER_HEIGHT));
    }
}



// =================
// === Component ===
// =================

pub type View = Component<Model, Frp>;

