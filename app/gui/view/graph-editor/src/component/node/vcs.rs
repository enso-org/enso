//! Functionality related to visualising the version control system status of a node.

use crate::prelude::*;
use ensogl::display::shape::*;

use crate::component::node;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;



// ==============
// === Status ===
// ==============

/// The version control system status of a node.
#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum Status {
    Unchanged,
    Added,
    Edited,
}

impl Status {
    fn get_highlight_color_from_style(self, style: &StyleWatch) -> color::Lcha {
        match self {
            Status::Unchanged =>
                style.get_color(ensogl_hardcoded_theme::graph_editor::node::vcs::unchanged).into(),
            Status::Added =>
                style.get_color(ensogl_hardcoded_theme::graph_editor::node::vcs::added).into(),
            Status::Edited =>
                style.get_color(ensogl_hardcoded_theme::graph_editor::node::vcs::edited).into(),
        }
    }
}

impl Default for Status {
    fn default() -> Self {
        Status::Unchanged
    }
}



// =======================
// === Indicator Shape ===
// =======================

/// Shape used in the status indicator. Appears as a colored border surrounding the node.
mod status_indicator_shape {
    use super::*;

    const INDICATOR_WIDTH_OUTER: f32 = 15.0;
    const INDICATOR_WIDTH_INNER: f32 = 10.0;

    ensogl::shape! {
        pointer_events = false;
        alignment = center;
        (style:Style,color_rgba:Vector4<f32>) {
            let width  = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let width  = width  - node::BACKDROP_INSET.px() * 2.0;
            let height = height - node::BACKDROP_INSET.px() * 2.0;
            let radius = node::CORNER_RADIUS.px();

            let base = Rect((&width,&height)).corners_radius(radius);
            let outer = base.grow(INDICATOR_WIDTH_OUTER.px());
            let inner = base.grow(INDICATOR_WIDTH_INNER.px());

            (outer-inner).fill(color_rgba).into()
        }
    }
}



// ==============================
// === Status Indicator Model ===
// ==============================

/// Internal data of `StatusIndicator`.
#[derive(Clone, CloneRef, Debug, display::Object)]
struct StatusIndicatorModel {
    shape:          status_indicator_shape::View,
    display_object: display::object::Instance,
}

impl StatusIndicatorModel {
    fn new() -> Self {
        let shape = status_indicator_shape::View::new();
        let display_object = display::object::Instance::new();
        display_object.add_child(&shape);
        StatusIndicatorModel { shape, display_object }
    }

    fn hide(&self) {
        self.shape.unset_parent();
    }

    fn show(&self) {
        self.display_object.add_child(&self.shape);
    }

    fn set_visibility(&self, visibility: bool) {
        if visibility {
            self.show()
        } else {
            self.hide()
        }
    }
}



// =======================
// === StatusIndicator ===
// =======================

ensogl::define_endpoints! {
    Input {
        set_status     (Option<Status>),
        set_size       (Vector2),
        set_visibility (bool),
    }
    Output {
        status (Option<Status>),
    }
}

#[derive(Clone, CloneRef, Debug, Deref, display::Object)]
#[allow(missing_docs)]
pub struct StatusIndicator {
    #[display_object]
    model:   Rc<StatusIndicatorModel>,
    #[deref]
    pub frp: Frp,
}

impl StatusIndicator {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(StatusIndicatorModel::new());
        let frp = Frp::new();
        Self { model, frp }.init_frp(app)
    }

    fn init_frp(self, app: &Application) -> Self {
        let frp = &self.frp;
        let model = &self.model;
        let network = &frp.network;
        let indicator_color = color::Animation::new(network);

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&app.display.default_scene.style_sheet);

        frp::extend! { network
            frp.source.status <+ frp.input.set_status;

            status_color <- frp.set_status.unwrap().map(f!([styles](status)
                status.get_highlight_color_from_style(&styles)
            ));
            indicator_color.target <+ status_color;

            eval indicator_color.value ((c)
                model.shape.color_rgba.set(color::Rgba::from(c).into())
            );

            eval frp.input.set_size ((size)
                model.shape.set_size(*size);
            );

            has_status <- frp.status.map(|status| status.is_some());
            visible    <- and(&frp.input.set_visibility,&has_status);
            eval visible ([model](visible) model.set_visibility(*visible));
        };

        frp.set_status.emit(None);
        frp.set_visibility.emit(true);
        self
    }
}
