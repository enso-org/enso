//! A single block component that is used to build up a flame graph.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl::frp;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::data::color::Lcha;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_gui_component::component;
use ensogl_gui_component::component::ComponentView;
use ensogl_text as text;



// =========================
// === Shape Definition ===
// =========================

mod background {
    use super::*;
    ensogl_core::shape! {
        alignment = center;
        (style:Style,color_rgba:Vector4<f32>) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let zoom                 = Var::<f32>::from("1.0/zoom()");
            let color = Var::<color::Rgba>::from(color_rgba);

            let shape = Rect((&width,&height));

            let border_width: Var<Pixels> = (zoom * 2.0).into();

            let right = Rect((&border_width,&height));
            let right = right.translate_x(&width/2.0);

            let left = Rect((&border_width,&height));
            let left = left.translate_x(-&width/2.0);

            let shape = shape - left;
            let shape = shape - right;
            let shape = shape.fill(color);

            (shape).into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_content(String),
        set_size(Vector2),
        set_color(Lcha)
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(
        network: &frp::Network,
        api: &Self::Private,
        app: &Application,
        model: &Model,
        _style: &StyleWatchFrp,
    ) {
        let background = &model.background.events_deprecated;
        frp::extend! { network
            eval api.input.set_content((t) model.set_content(t));
            eval api.input.set_size((size) model.set_size(*size));
            eval api.input.set_color((color) model.set_color(*color));

            tooltip_content <- api.input.set_content.sample(&background.mouse_over);
            tooltip <- tooltip_content.map(|content| tooltip::Style::set_label(content.clone()));
            app.frp.set_tooltip <+ tooltip;

            app.frp.set_tooltip <+ background.mouse_out.constant(tooltip::Style::unset_label());
        }
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    background:     background::View,
    label:          Rc<RefCell<Option<text::Text>>>,
    display_object: display::object::Instance,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "FlameGraphBlock"
    }

    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let label = default();

        let background = background::View::new();
        display_object.add_child(&background);
        scene.layers.tooltip.add(&background);

        Model { background, label, display_object }
    }
}

impl Model {
    fn set_size(&self, size: Vector2) {
        self.background.set_size(size);
    }

    fn set_content(&self, t: &str) {
        if let Some(label) = self.label.borrow().deref() {
            label.set_content(t.to_owned())
        }
    }

    fn set_color(&self, color: Lcha) {
        self.background.color_rgba.set(color::Rgba::from(color).into());
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
pub type Block = ComponentView<Model, Frp>;
