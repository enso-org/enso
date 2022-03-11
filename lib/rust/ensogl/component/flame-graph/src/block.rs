//! A single block component that is used to build up a flame graph.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl::frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::Animation;
use ensogl_gui_component::component;
use ensogl_gui_component::component::Component;
use ensogl_text as text;



// =======================
// === Layout Constants ===
// =======================

const TEXT_OFFSET_X: f32 = 4.0;
const TEXT_OFFSET_Y: f32 = -2.0;

const EMPTY_LABEL: &str = "<No Label>";



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

            eval_  model.background.events.mouse_over(model.enable_label());
            label_opacity.target <+ model.background.events.mouse_over.constant(1.0);

            label_opacity.target <+ model.background.events.mouse_out.constant(0.0);

            eval label_opacity.value ((t) model.set_label_opacity(*t));
            eval label_opacity.value ([model](t){
                if *t <= 0.0 {
                    model.disable_label()
                }
            });
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
    label:          Rc<RefCell<Option<text::Area>>>,
    display_object: display::object::Instance,
    text:           Rc<RefCell<Option<String>>>,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "FlameGraphBlock"
    }

    fn new(app: &Application, logger: &Logger) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new(&logger);
        let label = default();
        let text = default();

        let background = background::View::new(&logger);
        display_object.add_child(&background);
        scene.layers.tooltip.add_exclusive(&background);

        let app = app.clone_ref();
        Model { app, background, label, display_object, text }
    }
}

impl Model {
    fn set_size(&self, size: Vector2) {
        self.background.size.set(size);
    }

    fn set_content(&self, t: &str) {
        self.text.set(t.to_owned());
        if let Some(label) = self.label.borrow().deref() {
            label.set_content(t.to_owned())
        }
    }

    fn set_label_opacity(&self, opacity: f32) {
        if let Some(label) = self.label.borrow().deref() {
            let color = label.default_color.value();
            let color: color::Rgba = color.opaque.with_alpha(opacity);
            label.set_color_all.emit(color);
            label.set_default_color.emit(color);
        }
    }

    fn enable_label(&self) {
        let label = self.app.new_view::<text::Area>();
        self.add_child(&label);

        let text_layer = &self.app.display.default_scene.layers.tooltip_text;
        label.add_to_scene_layer(text_layer);

        let text_size = label.height.value();
        let text_origin = Vector2(
            TEXT_OFFSET_X - self.background.size.get().x / 2.0,
            TEXT_OFFSET_Y + text_size / 2.0,
        );
        label.set_position_xy(text_origin);
        label.set_content(self.text.borrow().clone().unwrap_or_else(|| EMPTY_LABEL.to_owned()));

        self.label.set(label);
        self.set_label_opacity(0.0);
    }

    fn disable_label(&self) {
        if let Some(label) = self.label.take() {
            label.unset_parent()
        }
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
