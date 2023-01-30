//! The EnsoGL Button Component
//!
//! This crate contains an abstraction of button component, allowing creating custom buttons
//! quickly.
//!
//! # Usage
//!
//! ```text
//! // The prelude will import all structures from this crate and EnsoGL core which are needed
//! // for defining custom button.
//! use crate::prelude::*;
//!
//! // First, define our custom button shape. The shape should take two colors as a parameters:
//! // one of the icon, and one of the background. In this example we will create "close" button.
//!
//! pub mod shape {
//!     use super::*;
//!
//!     ensogl_core::shape! {
//!         (background_color:Vector4<f32>, icon_color:Vector4<f32>) {
//!             let size       = Var::canvas_size();
//!             let radius     = Min::min(size.x(),size.y()) / 2.0;
//!             let angle      = Radians::from(45.0.degrees());
//!             let bar_length = &radius * 4.0 / 3.0;
//!             let bar_width  = &bar_length / 6.5;
//!             #[allow(clippy::blacklisted_name)] // The `bar` name here is totally legit.
//!             let bar        = Rect((bar_length, &bar_width)).corners_radius(bar_width);
//!             let cross      = (bar.rotate(angle) + bar.rotate(-angle)).into();
//!             shape(background_color, icon_color, cross, radius)
//!         }
//!     }
//! }
//!
//! // The defined shape should then implement the [`ButtonShape`] trait:
//!
//! impl ButtonShape for shape::DynamicShape {
//!     fn debug_name() -> &'static str {
//!         "CloseButton"
//!     }
//!
//!     fn background_color_path(state: State) -> StaticPath {
//!         match state {
//!             State::Unconcerned => theme::normal::background_color,
//!             State::Hovered => theme::hovered::background_color,
//!             State::Pressed => theme::pressed::background_color,
//!         }
//!     }
//!
//!     fn icon_color_path(state: State) -> StaticPath {
//!         match state {
//!             State::Unconcerned => theme::normal::icon_color,
//!             State::Hovered => theme::hovered::icon_color,
//!             State::Pressed => theme::pressed::icon_color,
//!         }
//!     }
//!
//!     fn background_color(&self) -> &DynamicParam<Attribute<Vector4<f32>>> {
//!         &self.background_color
//!     }
//!
//!     fn icon_color(&self) -> &DynamicParam<Attribute<Vector4<f32>>> {
//!         &self.icon_color
//!     }
//! }
//!
//! // Finally, we can create the full component by aliasing [`View`] structure.
//!
//! pub type View = ensogl_button::View<shape::DynamicShape>;
//! ```

#![recursion_limit = "256"]
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

use crate::prelude::*;
use ensogl_core::display::shape::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::system::Shape;
use ensogl_core::display::shape::system::ShapeWithDefaultableData;
use ensogl_core::gui::component::ShapeView;



// ===============
// === Prelude ===
// ===============

/// Prelude meant to be used by the modules defining custom buttons.
pub mod prelude {
    pub use ensogl_core::prelude::*;

    pub use crate::shape::shape;
    pub use crate::ButtonShape;
    pub use crate::State;

    pub use ensogl_core::display::shape::*;
    pub use ensogl_core::display::style::StaticPath;
    pub use ensogl_core::system::gpu::shader::glsl::traits::IntoGlsl;
    pub use ensogl_core::system::gpu::Attribute;
}



// =================
// === Constants ===
// =================

/// The default button's shape size.
const DEFAULT_SIZE_XY: (f32, f32) = (12.0, 12.0);



// =============
// === State ===
// =============

/// Visual state of the button.
#[derive(Clone, Copy, Debug)]
pub enum State {
    /// Base look when button is neither hovered nor pressed.
    /// Also used when button was pressed but mouse was hovered out.
    Unconcerned,
    /// Look when button is hovered but not pressed.
    Hovered,
    /// Look when button is being pressed (held down) with mouse hovered.
    Pressed,
}

impl Default for State {
    fn default() -> Self {
        Self::Unconcerned
    }
}

/// Trait to be defined on a specific button's shape.
pub trait ButtonShape: ShapeWithDefaultableData + 'static {
    /// The human-readable name of the button, for debug purposes.
    fn debug_name() -> &'static str;

    /// Path to the color of circular button background for a specific button's state.
    fn background_color_path(state: State) -> StaticPath;

    /// Path to the color of an icon for a specific button's state.
    fn icon_color_path(state: State) -> StaticPath;

    /// Access the shader parameter for the background color.
    fn background_color(&self) -> &ProxyParam<Attribute<Vector4<f32>>>;

    /// Access the shader parameter for the icon color.
    fn icon_color(&self) -> &ProxyParam<Attribute<Vector4<f32>>>;
}


// ==============
// === Shapes ===
// ==============

/// Utilities for defining button's shape.
pub mod shape {
    use super::*;

    /// Construct shape consisting of a given icon on a circle.
    /// Icon and circle colors are RGBA colors encoded as 4-elem floating numbers vector.
    pub fn shape(
        background_color: Var<Vector4<f32>>,
        icon_color: Var<Vector4<f32>>,
        icon: AnyShape,
        radius: Var<Pixels>,
    ) -> AnyShape {
        let background_color = Var::<Rgba>::from(background_color);
        let icon_color = Var::<Rgba>::from(icon_color);
        let circle = Circle(radius).fill(background_color);
        let icon = icon.fill(icon_color);
        (circle + icon).into()
    }
}



// =============
// === Model ===
// =============

/// An internal model of the button component.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
#[allow(missing_docs)]
pub struct Model<S: Shape> {
    app:            Application,
    display_object: display::object::Instance,
    shape:          ShapeView<S>,
}

impl<Shape: ButtonShape> Model<Shape> {
    /// Construct a button's model.
    pub fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();
        let shape = ShapeView::new();
        display_object.add_child(&shape);
        Self { app, display_object, shape }
    }

    /// Set the background (i.e. the circle) color.
    pub fn set_background_color(&self, color: impl Into<Rgba>) {
        self.shape.background_color().set(color.into().into());
    }

    /// Set the icon (i.e. the shape on the circle) color.
    pub fn set_icon_color(&self, color: impl Into<Rgba>) {
        self.shape.icon_color().set(color.into().into());
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_size (Vector2),
        mouse_nearby (bool),
        click (),
    }
    Output {
        clicked (),
        is_hovered (bool),
        state(State),
        size (Vector2<f32>),
    }
}



// ============
// === View ===
// ============

/// The Button component view.
///
/// When clicked, it emits `clicked` frp event. The click requires the mouse to be both pressed and
/// released on the button. It is allowed to temporarily move mouse out of the button while holding
/// the primary mouse button pressed without interrupting the click.
///
/// The button is fully theme-aware and dynamically sized.
#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
#[allow(missing_docs)]
pub struct View<S: Shape> {
    #[deref]
    frp:   Frp,
    model: Model<S>,
    style: StyleWatchFrp,
}

impl<Shape: ButtonShape> View<Shape> {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::<Shape>::new(app);
        let network = &frp.network;
        let scene = &app.display.default_scene;
        let style = StyleWatchFrp::new(&scene.style_sheet);
        let mouse = &scene.mouse.frp;

        // Icon color initialization
        let default_icon_color_path = Shape::icon_color_path(State::Unconcerned);
        let default_icon_color = style.get_color(default_icon_color_path).value();
        let icon_color = color::Animation::new(network);
        icon_color.target(color::Lcha::from(default_icon_color));
        model.set_icon_color(default_icon_color);

        // Background color initialization
        let default_background_color_path = Shape::background_color_path(State::Unconcerned);
        let default_background_color = style.get_color(default_background_color_path).value();
        let background_color = color::Animation::new(network);
        background_color.target(color::Lcha::from(default_background_color));
        model.set_icon_color(default_background_color);

        // Style's relevant color FRP endpoints.
        let background_unconcerned_color =
            style.get_color(Shape::background_color_path(State::Unconcerned));
        let background_hovered_color =
            style.get_color(Shape::background_color_path(State::Hovered));
        let background_pressed_color =
            style.get_color(Shape::background_color_path(State::Pressed));

        let icon_unconcerned_color = style.get_color(Shape::icon_color_path(State::Unconcerned));
        let icon_hovered_color = style.get_color(Shape::icon_color_path(State::Hovered));
        let icon_pressed_color = style.get_color(Shape::icon_color_path(State::Pressed));

        model.set_background_color(background_unconcerned_color.value());
        model.set_icon_color(icon_unconcerned_color.value());
        let events = &model.shape.events;

        frp::extend! { network
            eval frp.set_size ((&size) model.shape.set_size(size););
            frp.source.size <+ frp.set_size;

            // Mouse
            frp.source.is_hovered <+ bool(&events.mouse_out,&events.mouse_over);
            pressed_on_me         <- model.shape.events.mouse_down_primary.gate(&frp.is_hovered);
            tracking_for_release  <- bool(&mouse.up_primary,&pressed_on_me);
            mouse_released_on_me  <- mouse.up_primary.gate(&frp.is_hovered);
            was_clicked           <- tracking_for_release.previous();
            frp.source.clicked    <+ mouse_released_on_me.gate(&was_clicked);
            frp.source.clicked    <+ frp.click;
            state <- all_with3(&frp.is_hovered,&frp.mouse_nearby,&tracking_for_release,
                |strict_hover,nearby_hover,clicked| {
                    match (strict_hover,nearby_hover,clicked)  {
                            (true , _    , true) => State::Pressed,
                            (true , _    , _   ) => State::Hovered,
                            (_    , true , _   ) => State::Hovered,
                            (_    , _    , true) => State::Hovered,
                            _                    => State::Unconcerned,
                        }
                    });

            frp.source.state <+ state;
            // Color animations
            background_color.target <+ all_with4(&frp.source.state,&background_unconcerned_color,
                &background_hovered_color,&background_pressed_color,
                |state,unconcerned,hovered,pressed| {
                    match state {
                        State::Hovered => hovered,
                        State::Pressed => pressed,
                        _              => unconcerned,
                    }.into()
                });

            icon_color.target <+ all_with4(&frp.source.state,&icon_unconcerned_color,
                &icon_hovered_color,&icon_pressed_color,
                |state,unconcerned,hovered,pressed| {
                    match state {
                        State::Hovered => hovered,
                        State::Pressed => pressed,
                        _              => unconcerned,
                    }.into()
                });

            eval background_color.value ((color) model.set_background_color(color));
            eval icon_color.value       ((color) model.set_icon_color(color));
        }

        let (size_x, size_y) = DEFAULT_SIZE_XY;
        frp.set_size.emit(Vector2(size_x, size_y));

        Self { frp, model, style }
    }
}

impl<S: Shape> display::Object for View<S> {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl<S: Shape> FrpNetworkProvider for View<S> {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}
