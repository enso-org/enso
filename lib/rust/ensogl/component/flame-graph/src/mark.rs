//! A single mark component that is used to build up a flame graph.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl::frp;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_gui_component::component;
use ensogl_gui_component::component::ComponentView;
use ensogl_text as text;



// =================
// === Constants ===
// =================


const MARK_WIDTH: f32 = 2.0;
const MARK_HOVER_AREA_WIDTH: f32 = 20.0;

/// Invisible dummy color to catch hover events.
const HOVER_COLOR: color::Rgba = color::Rgba::new(1.0, 0.0, 0.0, 0.000_001);

const INFINITE: f32 = 99999.0;



// =========================
// === Shape Definition ===
// =========================

mod background {
    use super::*;
    ensogl_core::shape! {
        (style:Style) {
            let width  : Var<Pixels> = MARK_WIDTH.px();
            let height : Var<Pixels> = INFINITE.px();
            let zoom                 = &Var::<f32>::from("1.0/zoom()");
            let base_color           = style.get_color("flame_graph_mark_color");

            let shape = Rect((&width*zoom,&height));
            let shape = shape.fill(base_color);

            let hover_area = shape.grow(zoom * MARK_HOVER_AREA_WIDTH.px());
            let hover_area = hover_area.fill(HOVER_COLOR);

            (shape + hover_area).into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_content(String),
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
        let background = &model.background.events;
        frp::extend! { network
            eval api.input.set_content((t) model.set_content(t));

            tooltip_content <- api.input.set_content.sample(&background.mouse_over);
            tooltip <- tooltip_content.map(|content| tooltip::Style::set_label(content.clone()));
            app.frp.set_tooltip <+ tooltip;
            app.frp.set_tooltip <+ background.mouse_out.constant(tooltip::Style::unset_label());
        }

        model.set_size(Vector2::new(MARK_WIDTH + 2.0 * MARK_HOVER_AREA_WIDTH, INFINITE));
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    app:            Application,
    background:     background::View,
    label:          Rc<RefCell<Option<text::Text>>>,
    display_object: display::object::Instance,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "FlameGraphMark"
    }

    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let label = default();

        let background = background::View::new();
        display_object.add_child(&background);
        scene.layers.tooltip.add(&background);

        let app = app.clone_ref();
        Model { app, background, label, display_object }
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
pub type Mark = ComponentView<Model, Frp>;
