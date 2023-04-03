//! FIXME[everyone] Modules should be documented.

use crate::prelude::*;
use enso_text::unit::*;
use ensogl::display::shape::*;

use crate::node::input::area;
use crate::node::input::widget;
use crate::Type;

use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;



// =================
// === Constants ===
// =================

/// The horizontal padding of ports. It affects how the port hover should extend the target text
/// boundary on both sides.
pub const PADDING_X: f32 = 4.0;



// ===================
// === Hover Shape ===
// ===================

/// Port hover shape definition.
pub mod hover {
    use super::*;
    ensogl::shape! {
        alignment = center;
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let shape  = Rect((&width,&height));
            if !area::DEBUG {
                let color = Var::<color::Rgba>::from("srgba(1.0,1.0,1.0,0.00001)");
                shape.fill(color).into()
            } else {
                let shape = shape.corners_radius(6.px());
                let color = Var::<color::Rgba>::from("srgba(1.0,0.0,0.0,0.1)");
                shape.fill(color).into()
            }
        }
    }
}



// =============
// === Shape ===
// =============

/// Port shape definition.
pub mod viz {
    use super::*;
    ensogl::shape! {
        above = [hover];
        pointer_events = false;
        alignment = center;
        (style:Style, color:Vector4) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let shape  = Rect((&width,&height)).corners_radius(&height / 2.0);
            shape.fill("srgba(input_color)").into()
        }
    }
}



// =============
// === Shape ===
// =============

/// Shapes the port is build from. It consist of the `hover_shape`, which represents a hover area of
/// a full node height, and the `viz_shape`, which is a nice, visual highlight representation.
/// Both shapes are children of the `root` display object:
///
/// ```text
///     hover_shape
///      ◄──────►
/// ╭───┬────────┬──┄
/// │   │╭──────╮│▼ viz_shape
/// │   │╰──────╯│▲ (appears after mouse_hover)
/// ╰───┴────────┴──┄
/// ```
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Shape {
    pub root:  display::object::Instance,
    pub hover: hover::View,
    pub viz:   viz::View,
}

impl Shape {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(size: Vector2, hover_height: f32) -> Self {
        let root = display::object::Instance::new();
        let hover = hover::View::new();
        let viz = viz::View::new();

        let width_padded = size.x + 2.0 * PADDING_X;
        hover.set_size((width_padded, hover_height));
        viz.set_size((width_padded, size.y));
        hover.set_x(size.x / 2.0);
        viz.set_x(size.x / 2.0);
        viz.color.set(color::Rgba::transparent().into());

        root.add_child(&hover);
        root.add_child(&viz);

        Self { root, hover, viz }
    }
}

impl display::Object for Shape {
    fn display_object(&self) -> &display::object::Instance {
        self.root.display_object()
    }
}



// =============
// === Model ===
// =============

ensogl::define_endpoints! {
    Input {
        set_optional         (bool),
        set_disabled         (bool),
        set_active           (bool),
        set_hover            (bool),
        set_connected        (bool,Option<Type>),
        set_parent_connected (bool),
        set_definition_type  (Option<Type>),
        set_usage_type       (Option<Type>),
    }

    Output {
        tp (Option<Type>),
        new_value (String),
    }
}

/// Input port model. Please note that this is not a component model. It is a `SpanTree` payload
/// model.
#[derive(Clone, Debug, Default)]
#[allow(missing_docs)]
pub struct Model {
    pub frp:         Frp,
    pub shape:       Option<Shape>,
    pub widget:      Option<widget::View>,
    pub index:       ByteDiff,
    pub local_index: ByteDiff,
    pub length:      ByteDiff,
}

impl Deref for Model {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Model {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Shape initialization. Please note that not all port models get their shapes initialized,
    /// as some are skipped. For example, given the expression `(((foo)))`, the inner parentheses
    /// will be skipped, as there is no point in making them ports. The skip algorithm is
    /// implemented as part of the port are initialization.
    #[profile(Debug)]
    pub fn init_shape(&mut self, size: Vector2, hover_height: f32) -> Shape {
        let shape = Shape::new(size, hover_height);
        self.shape = Some(shape);
        self.shape.as_ref().unwrap().clone_ref()
    }

    /// Widget initialization. Only nodes that represent function arguments or argument placeholders
    /// will have widgets created for them.
    pub fn init_widget(&mut self, app: &Application) -> widget::View {
        let widget = widget::View::new(app);
        self.widget = Some(widget.clone_ref());
        widget
    }

    /// Assign an existing widget to this port.
    pub fn use_existing_widget(&mut self, widget: widget::View) -> widget::View {
        self.widget = Some(widget.clone_ref());
        widget
    }

    /// The range of this port.
    pub fn range(&self) -> enso_text::Range<ByteDiff> {
        let start = self.index;
        let end = self.index + self.length;
        enso_text::Range::new(start, end)
    }
}
