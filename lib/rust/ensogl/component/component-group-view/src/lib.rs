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
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_gui_component::component;
use ensogl_gui_component::component::Component;
use ensogl_hardcoded_theme::application::component_browser::group as theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry;
use ensogl_list_view::ListView;
use ensogl_text as text;



// =================
// === Constants ===
// =================

const HEADER_HEIGHT: f32 = entry::HEIGHT;
const HEADER_PADDING: f32 = entry::PADDING;



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
            let color = Var::<Rgba>::from(color);
            let shape = Rect((&sprite_width,&sprite_height)).fill(color);
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
        set_background_color(Rgba),
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
        let header_text_size = style.get_number(theme::name::text::size);
        frp::extend! { network
            // FIXME: taken from list_view::Model::padding(); this itself and calculations around it
            // look fishy
            let lv_padding = style.get_number(ensogl_hardcoded_theme::application::searcher::padding);

            size_and_lv_padding <- all(&input.resize, &lv_padding);
            // TODO[LATER]: looks like a common pattern (also in LV); is there a helper in FRP?
            // eval input.resize((size) model.resize(*size));
            eval size_and_lv_padding(((size, lv_padding)) model.resize(*size, *lv_padding));


            // === Header ===

            init <- source_();
            header_text_size <- all(&header_text_size,&init)._0();
            model.header.set_default_text_size <+ header_text_size.map(|v| text::Size(*v));
            model.header.set_content <+ input.set_header_text;
            eval input.set_background_color((c)
                model.header_background.color.set(c.into()));


            // === Entries ===

            model.entries.set_style_prefix(theme::entries::HERE.str);
            model.entries.show_background_shadow(false);
            model.entries.set_background_corners_radius(0.0);
            model.entries.set_custom_background_color <+ input.set_background_color.some();

            model.entries.set_entries <+ input.set_entries;
        }
        init.emit(());
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
        let display_object = display::object::Instance::new(&logger);
        let entries = ListView::new(app);
        let header_background = header_background::View::new(&logger);
        let header = text::Area::new(app);
        header.set_font("DejaVuSans-Bold");
        display_object.add_child(&entries);
        display_object.add_child(&header_background);
        display_object.add_child(&header);

        // TODO: this is based on code in list_view::entry::List::new(); is it right?
        let label_layer = &app.display.default_scene.layers.label;
        header.add_to_scene_layer(label_layer);

        ensogl_core::shapes_order_dependencies! {
            app.display.default_scene => {
                list_view::selection -> header_background;
            }
        }

        Model { display_object, header, header_background, entries }
    }
}

impl Model {
    fn resize(&self, size: Vector2, lv_padding: f32) {
        // === Header ===

        let half_height = size.y / 2.0;
        let top_left = Vector2(-size.x / 2.0, half_height);
        let header_label_height = self.header.height.value(); // TODO: pass via FRP?
        let header_padding = HEADER_PADDING + lv_padding / 2.0;
        self.header.set_position_xy(
            top_left
                + Vector2(
                    header_padding,
                    -HEADER_HEIGHT / 2.0 + header_label_height / 2.0 - lv_padding / 2.0,
                ),
        );
        self.header.set_truncation_width(size.x - 2.0 * header_padding);
        self.header_background.size.set(Vector2(size.x, HEADER_HEIGHT));
        self.header_background.set_position_y(half_height - HEADER_HEIGHT / 2.0);


        // === Entries ===

        self.entries.set_position_y(-HEADER_HEIGHT / 2.0);
        self.entries.resize(size - Vector2(0.0, HEADER_HEIGHT));
    }
}



// =================
// === Component ===
// =================

pub type View = Component<Model, Frp>;
