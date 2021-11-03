//! The code shared between different buttons living in the Top Buttons panel.

use prelude::*;

use enso_frp as frp;
use ensogl::application;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::data::color::Rgba;
use ensogl::display;
use ensogl::display::object::ObjectOps;
use ensogl::display::shape::*;
use ensogl::display::style;
use ensogl::display::style::data::DataMatch;
use ensogl::gui::component::ShapeView;



// ===============
// === Prelude ===
// ===============

/// Prelude meant to be used by sibling modules that provide specific button implementations.
pub mod prelude {
    pub use crate::prelude::*;

    pub use crate::window_control_buttons::common;
    pub use crate::window_control_buttons::common::shape::shape;
    pub use crate::window_control_buttons::common::ButtonShape;
    pub use crate::window_control_buttons::common::State;

    pub use ensogl::display::shape::*;
    pub use ensogl::display::style::StaticPath;
    pub use ensogl::system::gpu::shader::glsl::traits::IntoGlsl;
    pub use ensogl::system::gpu::Attribute;
}



// =================
// === Constants ===
// =================

/// Button radius to be used if theme-provided value is not available.
pub const RADIUS_FALLBACK: f32 = 12.0;



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
pub trait ButtonShape:
    CloneRef + display::object::class::Object + DynamicShapeInternals + 'static {
    /// The human readable name of the button, for debug purposes.
    fn debug_name() -> &'static str;

    /// Path to the color of circular button background for a specifc button's state.
    fn background_color_path(state: State) -> StaticPath;

    /// Path to the color of an icon for a specifc button's state.
    fn icon_color_path(state: State) -> StaticPath;

    /// Access the shader parameter for the background color.
    fn background_color(&self) -> &DynamicParam<Attribute<Vector4<f32>>>;

    /// Access the shader parameter for the icon color.
    fn icon_color(&self) -> &DynamicParam<Attribute<Vector4<f32>>>;
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

/// An internal model of Top Buttons Panel button component
#[derive(Clone, CloneRef, Debug)]
#[clone_ref(bound = "Shape:CloneRef")]
#[allow(missing_docs)]
pub struct Model<Shape> {
    app:            Application,
    logger:         DefaultTraceLogger,
    display_object: display::object::Instance,
    shape:          ShapeView<Shape>,
}

impl<Shape: ButtonShape> Model<Shape> {
    /// Construct a button's model.
    pub fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let logger = DefaultTraceLogger::new(Shape::debug_name());
        let display_object = display::object::Instance::new(&logger);
        let shape = ShapeView::new(&logger);
        display_object.add_child(&shape);
        Self { app, logger, display_object, shape }
    }

    /// Set the background (i.e. the circle) color.
    pub fn set_background_color(&self, color: impl Into<Rgba>) {
        self.shape.background_color().set(color.into().into());
    }

    /// Set the icon (i.e. the shape on the circle) color.
    pub fn set_icon_color(&self, color: impl Into<Rgba>) {
        self.shape.icon_color().set(color.into().into());
    }

    /// Retrieves circle radius value from an frp sampler event.
    fn get_radius(radius: &Option<style::data::Data>) -> f32 {
        radius.as_ref().and_then(DataMatch::number).unwrap_or(RADIUS_FALLBACK)
    }

    /// Set radius, updating the shape sizes and position.
    pub fn set_radius(&self, radius: &Option<style::data::Data>) -> Vector2<f32> {
        let radius = Self::get_radius(radius);
        let size = Self::size_for_radius(radius);
        self.shape.size().set(size);
        self.shape.set_position_x(radius);
        self.shape.set_position_y(-radius);
        size
    }

    /// Calculate the size of button for a given radius value.
    pub fn size_for_radius(radius: f32) -> Vector2<f32> {
        Vector2(radius, radius) * 2.0
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        mouse_nearby (bool),
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

/// The Control Button component view.
///
/// This is a clickable button styled after macOS, i.e. consists of an icon shape placed on top of
/// a circle. The icon is visible when button or its neighborhood (as provided by `mouse_nearby`
/// input) is hovered.
///
/// When clicked, it emits `clicked` frp event. The click requires the mouse to be both pressed and
/// released on the button. It is allowed to temporarily move mouse out of the button while holding
/// the primary mouse button pressed without interrupting the click.
///
/// The button is fully theme-aware and dynamically sized.
#[derive(Clone, CloneRef, Debug)]
#[clone_ref(bound = "Shape:CloneRef")]
#[allow(missing_docs)]
pub struct View<Shape> {
    frp:   Frp,
    model: Model<Shape>,
    style: StyleWatchFrp,
}

impl<Shape: ButtonShape> View<Shape> {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app);
        let network = &frp.network;
        let scene = app.display.scene();
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

        // Radius initialization
        let radius_frp = style.get(ensogl_theme::application::window_control_buttons::radius);

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
        let events = &model.shape.events;

        frp::extend! { network

            // Radius
            frp.source.size <+ radius_frp.map(f!((radius) model.set_radius(radius)));

            // Mouse
            frp.source.is_hovered <+ bool(&events.mouse_out,&events.mouse_over);
            pressed_on_me         <- model.shape.events.mouse_down.gate(&frp.is_hovered);
            tracking_for_release  <- bool(&mouse.up_primary,&pressed_on_me);
            mouse_released_on_me  <- mouse.up_primary.gate(&frp.is_hovered);
            was_clicked           <- tracking_for_release.previous();
            frp.source.clicked    <+ mouse_released_on_me.gate(&was_clicked);
            state <- all_with3(&frp.is_hovered,&frp.mouse_nearby,&tracking_for_release,
                |strict_hover,nearby_hover,clicked| {
                    match (strict_hover,nearby_hover,clicked)  {
                            (true , _    , true) => State::Pressed,
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

        frp.source.size.emit(model.set_radius(&radius_frp.value()));

        Self { frp, model, style }
    }
}

impl<Shape> display::Object for View<Shape> {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl<Shape> Deref for View<Shape> {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl<Shape> application::command::FrpNetworkProvider for View<Shape> {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl<Shape: ButtonShape> application::View for View<Shape> {
    fn label() -> &'static str {
        "CloseButton"
    }
    fn new(app: &Application) -> Self {
        View::new(app)
    }
    fn app(&self) -> &Application {
        &self.model.app
    }
}
