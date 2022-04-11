//! An EnsoGL implementation of a Component Group View.
//!
//! A Component Group View is displayed as a header (containing the name of a group of Enso
//! components) and a list of entries below it (names of the components belonging to that group).

#![recursion_limit = "512"]
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

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_gui_component::component;
use ensogl_gui_component::component::ComponentView;
use ensogl_hardcoded_theme::application::component_browser::component_group as theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry;
use ensogl_list_view::ListView;
use ensogl_text as text;



// ==========================
// === Shapes Definitions ===
// ==========================


// === Header Background ===

/// The background of the Component Group View's header.
pub mod header_background {
    use super::*;

    ensogl_core::define_shape_system! {
        above = [list_view::selection];
        (style:Style, color:Vector4) {
            let sprite_width: Var<Pixels> = "input_size.x".into();
            let sprite_height: Var<Pixels> = "input_size.y".into();
            let color = Var::<Rgba>::from(color);
            let shape = Rect((&sprite_width, &sprite_height)).fill(color);
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
        set_size(Vector2),
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, style: &StyleWatchFrp) {
        let network = &api.network;
        let input = &api.input;
        let header_text_size = style.get_number(theme::header::text::size);
        frp::extend! { network

            // === Geometry ===

            let header_geometry = HeaderGeometry::from_style(style, network);
            size_and_header_geometry <- all(&input.set_size, &header_geometry);
            eval size_and_header_geometry(((size, hdr_geom)) model.resize(*size, *hdr_geom));


            // === Header ===

            init <- source_();
            header_text_size <- all(&header_text_size, &init)._0();
            model.header.set_default_text_size <+ header_text_size.map(|v| text::Size(*v));
            model.header.set_content <+ input.set_header_text;
            eval input.set_background_color((c)
                model.header_background.color.set(c.into()));


            // === Entries ===

            model.entries.show_background_shadow(false);
            model.entries.set_background_corners_radius(0.0);
            model.entries.set_background_color <+ input.set_background_color;
            model.entries.set_entries <+ input.set_entries;
        }
        init.emit(());
    }
}



// =======================
// === Header Geometry ===
// =======================

#[derive(Debug, Copy, Clone, Default)]
struct HeaderGeometry {
    height:         f32,
    padding_left:   f32,
    padding_right:  f32,
    padding_bottom: f32,
}

impl HeaderGeometry {
    fn from_style(style: &StyleWatchFrp, network: &frp::Network) -> frp::Sampler<Self> {
        let height = style.get_number(theme::header::height);
        let padding_left = style.get_number(theme::header::padding::left);
        let padding_right = style.get_number(theme::header::padding::right);
        let padding_bottom = style.get_number(theme::header::padding::bottom);

        frp::extend! { network
            init <- source_();
            theme <- all_with5(&init,&height,&padding_left,&padding_right,&padding_bottom,
                |_,&height,&padding_left,&padding_right,&padding_bottom|
                    Self{height,padding_left,padding_right,padding_bottom});
            theme_sampler <- theme.sampler();
        }
        init.emit(());
        theme_sampler
    }
}



// =============
// === Model ===
// =============

/// The Model of the [`View`] component.
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
        let header_background = header_background::View::new(&logger);
        let header = text::Area::new(app);
        let entries = ListView::new(app);
        display_object.add_child(&header_background);
        display_object.add_child(&header);
        display_object.add_child(&entries);

        header.set_font("DejaVuSans-Bold");
        let label_layer = &app.display.default_scene.layers.label;
        header.add_to_scene_layer(label_layer);

        Model { display_object, header, header_background, entries }
    }
}

impl Model {
    fn resize(&self, size: Vector2, header_geometry: HeaderGeometry) {
        // === Header Background ===

        let header_bg_height = header_geometry.height;
        let header_bg_y = (size.y - header_bg_height) / 2.0;
        self.header_background.size.set(Vector2(size.x, header_bg_height));
        self.header_background.set_position_y(header_bg_y);


        // === Header Text ===

        let header_padding_left = header_geometry.padding_left;
        let header_text_x = -size.x / 2.0 + header_padding_left;
        let header_text_height = self.header.height.value();
        let header_padding_bottom = header_geometry.padding_bottom;
        let header_bg_bottom_y = size.y / 2.0 - header_bg_height;
        let header_text_y = header_bg_bottom_y + header_text_height + header_padding_bottom;
        self.header.set_position_xy(Vector2(header_text_x, header_text_y));
        let header_padding_right = header_geometry.padding_right;
        self.header.set_truncation_width(size.x - header_padding_left - header_padding_right);


        // === Entries ===

        self.entries.resize(size - Vector2(0.0, header_bg_height));
        self.entries.set_position_y(-header_bg_height / 2.0);
    }
}



// ============
// === View ===
// ============

/// The implementation of the visual component described in the module's documentation.
pub type View = ComponentView<Model, Frp>;
