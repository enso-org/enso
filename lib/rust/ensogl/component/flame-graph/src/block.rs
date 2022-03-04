//! A single block component that is used to build up a flame graph.

use ensogl_core::prelude::*;

use ensogl::frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::shape::*;
use ensogl_core::Animation;
use ensogl_gui_component::component;
use ensogl_gui_component::component::Component;
use ensogl_text as text;



// =======================
// === Layout Constants ===
// =======================

const TEXT_OFFSET_X: f32 = 4.0;
const TEXT_OFFSET_Y: f32 = -2.0;



// =========================
// === Shape Definition ===
// =========================

mod background {
    use super::*;
    ensogl_core::define_shape_system! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let zoom                 = Var::<f32>::from("1.0/zoom()");
            let base_color           = style.get_color("flame_graph_color");

            let shape = Rect((&width,&height));

            let border_width: Var<Pixels> = (zoom * 2.0).into();

            let right = Rect((&border_width,&height));
            let right = right.translate_x(&width/2.0);

            let left = Rect((&border_width,&height));
            let left = left.translate_x(-&width/2.0);

            let shape = shape - left;
            let shape = shape - right;
            let shape = shape.fill(base_color);

            (shape).into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_content(String),
        set_position(Vector2),
        set_size(Vector2)
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(&self, _app: &Application, model: &Model, _style: &StyleWatchFrp) {
        let network = &self.network;

        let label_opacity = Animation::<f32>::new(network);

        frp::extend! { network
           eval self.set_content((t) model.set_content(t));
           eval self.set_size((size) model.set_size(*size));

            label_opacity.target <+ model.background.events.mouse_over.constant(1.0);
            label_opacity.target <+ model.background.events.mouse_out.constant(0.0);
            eval label_opacity.value ((t) model.set_label_opacity(*t));
        }

        label_opacity.target.emit(0.0);
        model.set_label_opacity(0.0);
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    app:            Application,
    background:     background::View,
    label:          text::Area,
    display_object: display::object::Instance,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "FlameGraphBlock"
    }

    fn new(app: &Application, logger: &Logger) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new(&logger);
        let label = app.new_view::<text::Area>();
        let background = background::View::new(&logger);

        display_object.add_child(&background);
        display_object.add_child(&label);

        let app = app.clone_ref();
        let model = Model { app, background, label, display_object };
        model.set_layers(&scene.layers.tooltip, &scene.layers.tooltip_text);
        model
    }
}

impl Model {
    /// Set scene layers for background and text respectively.
    pub fn set_layers(&self, background_layer: &Layer, text_layer: &Layer) {
        // FIXME[MM/WD]: Depth sorting of labels to in front of everything else in the scene.
        //  Temporary solution. The depth management needs to allow defining relative position
        // of  the text and background and let the whole component to be set to am
        // an arbitrary layer.
        background_layer.add_exclusive(&self.background);
        self.label.add_to_scene_layer(text_layer);
    }

    fn set_size(&self, size: Vector2) {
        self.background.size.set(size);
        let text_size = self.label.height.value();
        let text_origin = Vector2(TEXT_OFFSET_X - size.x / 2.0, TEXT_OFFSET_Y + text_size / 2.0);
        self.label.set_position_xy(text_origin);
    }

    fn set_content(&self, t: &str) {
        self.label.set_content(t)
    }

    fn set_label_opacity(&self, opacity: f32) {
        let color = self.label.default_color.value();
        let color: color::Rgba = color.opaque.with_alpha(opacity);
        self.label.set_color_all.emit(color);
        self.label.set_default_color.emit(color);
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

#[allow(missing_docs)]
pub type Block = Component<Model, Frp>;
