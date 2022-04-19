//! A single mark component that is used to build up a flame graph.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl::frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_gui_component::component;
use ensogl_gui_component::component::ComponentView;
use ensogl_text as text;

use super::BASE_TEXT_SIZE;



// =================
// === Constants ===
// =================


const EMPTY_LABEL: &str = "<No Label>";

const MARK_WIDTH: f32 = 2.0;
const MARK_HOVER_AREA_WIDTH: f32 = 20.0;

/// Invisible dummy color to catch hover events.
const HOVER_COLOR: color::Rgba = color::Rgba::new(1.0, 0.0, 0.0, 0.000_001);

const INFINITE: f32 = 99999.0;

const TEXT_OFFSET_X: f32 = MARK_WIDTH + 8.0;



// =========================
// === Shape Definition ===
// =========================

mod background {
    use super::*;
    ensogl_core::define_shape_system! {
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
    fn init(api: &Self::Private, app: &Application, model: &Model, _style: &StyleWatchFrp) {
        let network = &api.network;
        let background = &model.background.events;

        let cursor_pos = app.cursor.frp.scene_position.clone_ref();
        frp::extend! { network
            eval api.input.set_content((t) model.set_content(t));

            is_hovered <- bool(&background.mouse_out, &background.mouse_over);
            eval is_hovered((hovered) model.set_label_visible(*hovered));

            on_mouse_over_pos <- cursor_pos.gate(&is_hovered);
            eval on_mouse_over_pos((pos) model.set_label_y(pos.y));
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
    label:          Rc<RefCell<Option<text::Area>>>,
    display_object: display::object::Instance,
    text:           Rc<RefCell<Option<String>>>,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "FlameGraphMark"
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

    fn set_label_visible(&self, visible: bool) {
        if visible {
            self.enable_label();
        } else {
            self.label.take().for_each(|label| label.unset_parent())
        }
    }

    fn enable_label(&self) {
        let label = self.app.new_view::<text::Area>();
        self.add_child(&label);

        let text_layer = &self.app.display.default_scene.layers.tooltip_text;
        label.add_to_scene_layer(text_layer);
        label.set_default_text_size(text::Size(
            BASE_TEXT_SIZE / self.app.display.default_scene.camera().zoom(),
        ));

        label.set_position_x(TEXT_OFFSET_X);
        label.set_content(self.label_text());
        self.label.set(label);
    }

    fn set_label_y(&self, y: f32) {
        if let Some(label) = self.label.deref().borrow().as_ref() {
            label.set_position_y(y)
        }
    }

    fn label_text(&self) -> String {
        self.text.borrow().clone().unwrap_or_else(|| EMPTY_LABEL.to_owned())
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
