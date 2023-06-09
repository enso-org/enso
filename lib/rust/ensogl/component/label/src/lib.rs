//! Label component. Appears as text with background.

#![recursion_limit = "512"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_hardcoded_theme::component::label as theme;
use ensogl_text as text;



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_content(String),
        set_opacity(f32)
    }
    Output {
        size (Vector2)
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, Debug)]
struct Model {
    background:     Rectangle,
    label:          text::Text,
    display_object: display::object::Instance,
    style:          StyleWatch,
}

impl Model {
    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        let background = Rectangle::new();
        background.set_corner_radius_max();

        display_object.add_child(&background);
        display_object.add_child(&label);

        let style = StyleWatch::new(&app.display.default_scene.style_sheet);
        if let Some(display::style::Data::Text(font)) = style.get(theme::text::font) {
            label.set_font(font);
        }

        let model = Model { background, label, display_object, style };
        model.set_layers(&scene.layers.tooltip, &scene.layers.tooltip_text);
        model
    }

    /// Set scene layers for background and text respectively.
    pub fn set_layers(&self, background_layer: &Layer, text_layer: &Layer) {
        // FIXME[MM/WD]: Depth sorting of labels to in front of everything else in the scene.
        //  Temporary solution. The depth management needs to allow defining relative position of
        //  the text and background and let the whole component to be set to am an arbitrary layer.
        background_layer.add(&self.background);
        self.label.add_to_scene_layer(text_layer);
    }

    /// Change the size based on the size of the contained text, returning the new size including
    /// margin.
    fn set_text_size(&self, text_size: Vector2<f32>) -> Vector2 {
        let theme_padding_outer = self.style.get_number(theme::padding_outer);
        let theme_padding_inner_x = self.style.get_number(theme::padding_inner_x);
        let theme_padding_inner_y = self.style.get_number(theme::padding_inner_y);
        let theme_text_offset = self.style.get_number(theme::text::offset);

        let margin = Vector2(theme_padding_outer, theme_padding_outer);
        let padding = Vector2(theme_padding_inner_x, theme_padding_inner_y);
        let label_size = text_size + padding * 2.0;
        let size_with_margin = label_size + margin * 2.0;
        let text_origin = Vector2(theme_text_offset - text_size.x / 2.0, text_size.y / 2.0);
        let background_origin = Vector2(theme_text_offset, 0.0) - label_size / 2.0;

        self.background.set_size(label_size);
        self.background.set_xy(background_origin);
        self.label.set_xy(text_origin);

        size_with_margin
    }

    fn set_content(&self, t: &str) {
        self.label.set_content(t);
    }

    fn set_opacity(&self, value: f32) {
        let text_color_path = theme::text;
        let text_color = self.style.get_color(text_color_path).multiply_alpha(value);
        self.label.frp.set_property(.., text_color);
        self.label.frp.set_property_default(text_color);

        let bg_color_path = theme::background;
        let bg_color = self.style.get_color(bg_color_path).multiply_alpha(value);
        self.background.color.set(bg_color.into())
    }
}



// =======================
// === Label Component ===
// =======================

#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Label {
    model:   Rc<Model>,
    pub frp: Rc<Frp>,
}

impl Label {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Rc::new(Frp::new());
        let model = Rc::new(Model::new(app));
        Label { model, frp }.init()
    }

    /// Set layers for Label's background and text respectively. This is needed because
    /// `text::Text` uses its own `add_to_scene_layer` method instead of utilizing more common
    /// [`Layer::add_exclusive`].
    pub fn set_layers(&self, background_layer: &Layer, text_layer: &Layer) {
        self.model.set_layers(background_layer, text_layer);
    }

    fn init(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;

        frp::extend! { network
            eval frp.set_content((t) model.set_content(t));

            frp.source.size <+ all_with(&model.label.width, &model.label.height,
                f!((width, height) model.set_text_size(Vector2(*width, *height))));

            eval frp.set_opacity((value) model.set_opacity(*value));
        }

        self
    }
}

impl Deref for Label {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl display::Object for Label {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
