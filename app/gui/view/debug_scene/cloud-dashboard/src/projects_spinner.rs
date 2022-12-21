//! A module containing Cloud dashboard spinner component definitions (i.e., [`Frp`], [`Model`],
//! [`View`], etc.).

use ensogl::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp::stream;
use ensogl::application;
use ensogl::application::frp::API as _;
use ensogl::display;
use ensogl_core::data::color;
use ensogl_core::system::gpu::shader::glsl::traits::IntoGlsl as _;
use ensogl_hardcoded_theme as theme;
use ensogl_toggle_button as toggle_button;
use ensogl_toggle_button::ColorableShape as _;



// =================
// === Constants ===
// =================

/// RGBA alpha value for a visible (i.e., opaque icon).
const VISIBLE: f32 = 1.0;
/// RGBA alpha value for an invisible (i.e., transparent icon).
const INVISIBLE: f32 = 0.0;



// =====================
// === Project State ===
// =====================

/// Represents the current state of a [`Project`] as represented in a [`projects_spinner`] icon in
/// the [`ProjectsTable`]. Not all states of a [`Project`] are distinguished in the UI, so some of
/// them are mapped to the same icon.
///
/// [`Project`]: ::enso_cloud_view::project::Project
/// [`projects_spinner`]: crate::projects_spinner
/// [`ProjectsTable`]: crate::projects_table::View
#[derive(Clone, Copy, Debug, Default)]
pub enum State {
    /// The standard state of a [`Project`]. A [`Project`] in this state is not currently
    /// running on a VM and is not queued for execution.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    #[default]
    Closed,
    /// The state of a [`Project`] that is currently running on a VM and is available for
    /// interactive editing.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    Opened,
    /// The state of a [`Project`] transitioning from being [`Closed`] to being [`Opened`]. A
    /// [`Project`] in this state may already be already running on a VM, but not yet ready to
    /// open. It may also be queued for execution.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    /// [`Closed`]: crate::projects_spinner::State::Closed
    /// [`Opened`]: crate::projects_spinner::State::Opened
    Opening,
    /// The state of a [`Project`] transitioning from being [`Opened`] to being [`Closed`]. A
    /// [`Project`] in this state may still be already running on a VM. Or other resources (e.g.)
    /// filesystems) may still be mounted.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    /// [`Opened`]: crate::projects_spinner::State::Opened
    /// [`Closed`]: crate::projects_spinner::State::Closed
    Closing,
}


// === Main `impl` ===

impl State {
    /// Returns `true` if the [`projects_spinner::shape::Shape`] should be spinning, and `false`
    /// otherwise.
    ///
    /// [`projects_spinner::shape::Shape`]: crate::projects_spinner::shape::Shape
    fn spinner_visible(&self) -> bool {
        match self {
            State::Closed | State::Opened => false,
            State::Opening | State::Closing => true,
        }
    }

    /// Returns `true` if the [`play::Shape`] should be visible, and `false` otherwise.
    fn play_visible(&self) -> bool {
        match self {
            State::Closed => true,
            State::Opened | State::Opening | State::Closing => false,
        }
    }

    /// Returns `true` if the [`pause::Shape`] should be visible, and `false` otherwise.
    fn pause_visible(&self) -> bool {
        match self {
            State::Opened => true,
            State::Closed | State::Opening | State::Closing => false,
        }
    }
}



// =======================
// === Project Command ===
// =======================

/// Represents an action to change the current state of a [`Project`]. These are emitted when
/// the user interacts with the [`projects_spinner`] icon in the [`ProjectsTable`]. These actions
/// are used to optimistically update the UI, and are then sent to the backend to be executed.
///
/// [`Project`]: ::enso_cloud_view::project::Project
/// [`projects_spinner`]: crate::projects_spinner
/// [`ProjectsTable`]: crate::projects_table::View
#[derive(Clone, Copy, Debug)]
pub enum Command {
    /// A command to open a currently [`Closed`] or [`Closing`] [`Project`].
    ///
    /// [`Closed`]: State::Closed
    /// [`Closing`]: State::Closing
    /// [`Project`]: ::enso_cloud_view::project::Project
    Open,
    /// A command to close a currently [`Opened`] or [`Opening`] [`Project`].
    ///
    /// [`Opened`]: State::Opened
    /// [`Opening`]: State::Opening
    /// [`Project`]: ::enso_cloud_view::project::Project
    Close,
}



// =============
// === Shape ===
// =============

/// A constant representing a 90 degree angle in radians.
const RIGHT_ANGLE: f32 = std::f32::consts::PI / 2.0;
/// A constant representing a 180 degree angle in radians.
///
/// Used to make the spin of the [`projects_spinner`] aperture start at the top of the circle,
/// rather than the bottom.
///
/// [`projects_spinner`]: crate::projects_spinner
const HALF_CIRCUMFERENCE: f32 = std::f32::consts::PI;
/// A constant representing a 360 degree angle in radians.
const CIRCUMFERENCE: f32 = std::f32::consts::PI * 2.0;
/// Size of the [`projects_spinner`] icon, in pixels. This is both the height and the width of the
/// icon, as the [`projects_spinner`] is meant to be a circle, so the ratio of the sides is 1:1.
///
/// [`projects_spinner`]: crate::projects_spinner
pub const SPINNER_SIZE: f32 = 24.0;

/// Spinner icon shape definition.
///
/// The spinner is composed of a ring with an aperture, that rotates in a circle. The rotation of
/// the circle is time-dependent, and indicates that an operation is currently underway (e.g. a
/// project is being opened or closed).
mod shape {
    use super::*;

    ensogl_core::shape! {
        pointer_events = false;
        (style:Style, start_time:f32, color_rgb:Vector3<f32>, alpha: f32) {
            let width_abs = Var::<Pixels>::from("abs(input_size.x)");
            let time = Var::<f32>::from("input_time");
            let time = time - start_time;


            // === Measurements ===

            use theme::cloud_dashboard;
            let rotate_period = style.get_number(cloud_dashboard::rotate_period);
            let unit = &width_abs * 0.5;
            let outer_circle_radius = &unit * 1.0;
            let outer_circle_thickness = &unit * 0.33;
            let sampler = &time % rotate_period;
            let rotate_angle = sampler.smoothstep(0.0, rotate_period);


            // === Ring With Aperture ===

            let radius = outer_circle_radius;
            let width = outer_circle_thickness;
            let angle = &rotate_angle * CIRCUMFERENCE;
            let start_angle = Var::from(HALF_CIRCUMFERENCE);
            let rgb = color_rgb;
            let color = format!("srgba({}.x,{}.y,{}.z,{})", rgb, rgb, rgb, alpha);
            let shape = ring_with_aperture_base_shape(style, &radius,&width,&angle,&start_angle);
            let shape = shape.fill(color);

            shape.into()
        }
    }
}

/// Creates a ring shape with the given radius and ring width, in pixels, then cuts out an
/// aperture of the given `angle`, starting at the given `start_angle`.
fn ring_with_aperture_base_shape(
    style: &StyleWatch,
    radius: &Var<Pixels>,
    width: &Var<Pixels>,
    angle: &Var<f32>,
    start_angle: &Var<f32>,
) -> AnyShape {
    use theme::cloud_dashboard;
    let spinner_ring_coverage = style.get_number(cloud_dashboard::spinner_ring_coverage);
    let circumference: Var<f32> = CIRCUMFERENCE.into();
    let rotation = angle + start_angle;
    let aperture_angle = circumference * spinner_ring_coverage;

    let ring = ring_base_shape(radius, width);

    let aperture_mask = Plane().cut_angle_fast(aperture_angle).rotate(rotation);
    let shape = ring * aperture_mask;
    shape.into()
}

/// Creates a ring shape with the given radius and ring width, in pixels.
///
/// The ring is centered on the origin, and the outer edge of the ring is at the given radius.
fn ring_base_shape(radius: &Var<Pixels>, width: &Var<Pixels>) -> AnyShape {
    let circle = Circle(radius);
    let gap = Circle(radius - width);
    let ring = circle - gap;

    let shape = ring;
    shape.into()
}



// =================
// === Ring Icon ===
// =================

/// Icon for a [`Project`] state display and control button. Looks like a ring. It is used to
/// indicate that the [`Project`] is not in a transitional state. When the [`Project`] is in a
/// transitional state, the [`ring::Shape`] is replaced with a [`shape::Shape`].
///
/// [`Project`]: ::enso_cloud_view::project::Project
mod ring {
    use super::*;

    ensogl_core::shape! {
        pointer_events = false;
        (style:Style, selection:f32, start_time:f32, not_blinking:f32, color_rgb:Vector3<f32>, alpha: f32) {
            let width_abs = Var::<Pixels>::from("abs(input_size.x)");


            // === Measurements ===

            let unit = &width_abs * 0.5;
            let outer_circle_radius = &unit * 1.0;
            let outer_circle_thickness = &unit * 0.33;


            // === Ring With Aperture ===

            let radius = outer_circle_radius;
            let width = outer_circle_thickness;
            let rgb = color_rgb;
            let color = format!("srgba({}.x,{}.y,{}.z,{})", rgb, rgb, rgb, alpha.glsl());
            let shape = ring_base_shape(&radius, &width);
            let shape = shape.fill(color);

            shape.into()
        }
    }
}



// =================
// === Play Icon ===
// =================

/// Icon for the play button. Looks like a triangle pointing right.
mod play {
    use super::*;

    ensogl_core::shape! {
        pointer_events = false;
        (style:Style, color_rgba:Vector4<f32>) {
            let fill_color       = Var::<color::Rgba>::from(color_rgba);
            let width            = Var::<Pixels>::from("input_size.x");
            let right_angle      = 90.0_f32.to_radians().radians();
            let unit             = &width / 16.0;
            let triangle_width   = &unit * 8.0;
            let triangle_height  = &unit * 8.0;
            let triangle         = Triangle(triangle_width, triangle_height);
            let triangle         = triangle.rotate(right_angle);
            let play             = triangle;
            let play             = play.translate_x(&unit * 0.5);
            let icon             = play;
            let icon             = icon.fill(fill_color);
            icon.into()
        }
    }

    impl toggle_button::ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(color.into());
        }
    }
}



// ==================
// === Pause Icon ===
// ==================

/// Icon for the puse button. Looks like two vertical lines side by side.
mod pause {
    use super::*;

    ensogl_core::shape! {
        pointer_events = false;
        (style:Style, color_rgba:Vector4<f32>) {
            let fill_color   = Var::<color::Rgba>::from(color_rgba);
            let width        = Var::<Pixels>::from("input_size.x");
            let unit         = &width / 16.0;
            let line_width   = &unit * 3.0;
            let line_height  = &unit * 8.0;
            let line_rounded = Rect((&line_width,&line_height)).corners_radius(&line_width);
            let line_left    = line_rounded.translate_x(-&line_height / 4.0);
            let line_right   = &line_rounded.translate_x(&line_height / 4.0);
            let pause        = line_left + line_right;
            let icon         = pause;
            let icon         = icon.fill(fill_color);
            icon.into()
        }
    }

    impl toggle_button::ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(color.into());
        }
    }
}


// ===================
// === Hover Shape ===
// ===================

mod hover {
    use super::*;

    ensogl_core::shape! {
        above = [ring, play, pause, shape];
        pointer_events = true;
        (style: Style) {
            let shape = Plane();
            let shape = shape.fill(INVISIBLE_HOVER_COLOR);
            shape.into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
        set_size(Vector2),
        set_state(State),
    }
    Output {
        state(State),
        command(Option<Command>),
    }
}



// =============
// === Model ===
// =============

/// An internal model of the Spinner component.
#[derive(Clone, CloneRef, Debug)]
struct Model {
    display_object: display::object::Instance,
    hover:          hover::View,
    ring:           ring::View,
    spinner:        shape::View,
    play:           play::View,
    pause:          pause::View,
    state:          Rc<RefCell<State>>,
}


// === Main `impl` ===

impl Model {
    fn new(state: State) -> Self {
        let display_object = display::object::Instance::new();
        let hover = hover::View::new();
        let ring = ring::View::new();
        let spinner = shape::View::new();
        let play = play::View::new();
        let pause = pause::View::new();
        let state = Rc::new(RefCell::new(state));

        Self { display_object, hover, ring, spinner, play, pause, state }.init()
    }

    fn init(self) -> Self {
        self.init_size();
        self.init_transparency();
        self.init_hover();
        self.init_state();
        self
    }

    fn init_size(&self) {
        let size = Vector2(SPINNER_SIZE, SPINNER_SIZE);
        self.ring.size.set(size);
        self.spinner.size.set(size);
        self.hover.size.set(size);
        self.play.size.set(size);
        self.pause.size.set(size);
    }

    fn init_transparency(&self) {
        self.ring.alpha.set(VISIBLE);
        self.spinner.alpha.set(VISIBLE);
    }

    fn init_hover(&self) {
        self.display_object.add_child(&self.hover);
    }

    fn init_state(&self) {
        let state = *self.state.borrow();
        match state {
            State::Opening => self.switch_to_opening(),
            State::Opened => self.switch_to_opened(),
            State::Closing => self.switch_to_closing(),
            State::Closed => self.switch_to_closed(),
        }
    }

    fn hide_current_visual_representation(&self) {
        self.ring.unset_parent();
        self.spinner.unset_parent();
        self.play.unset_parent();
        self.pause.unset_parent();
    }

    fn set_state(&self, state: State) {
        *self.state.borrow_mut() = state;
        match state {
            State::Opening => self.switch_to_opening(),
            State::Opened => self.switch_to_opened(),
            State::Closing => self.switch_to_closing(),
            State::Closed => self.switch_to_closed(),
        }
    }

    fn switch_to_opening(&self) {
        self.hide_current_visual_representation();
        self.display_object.add_child(&self.spinner);
        self.display_object.add_child(&self.pause);
    }

    fn switch_to_opened(&self) {
        self.hide_current_visual_representation();
        self.display_object.add_child(&self.ring);
        self.display_object.add_child(&self.pause);
    }

    fn switch_to_closing(&self) {
        self.hide_current_visual_representation();
        self.display_object.add_child(&self.spinner);
        self.display_object.add_child(&self.play);
    }

    fn switch_to_closed(&self) {
        self.hide_current_visual_representation();
        self.display_object.add_child(&self.ring);
        self.display_object.add_child(&self.play);
    }

    fn toggle(&self) -> Option<Command> {
        let state = self.state.borrow();
        let command = match *state {
            State::Opened | State::Opening => Command::Close,
            State::Closed | State::Closing => Command::Open,
        };
        Some(command)
    }
}



// ============
// === View ===
// ============

/// Visual representation of an interactive progress Spinner component.
///
/// The Spinner displays an interactive (i.e., clickable) button that has an icon in the center
/// (to represent what action happens when the button is clicked) and a progress bar around the
/// spinner to represent the state and progress of the current action (e.g., 50% loaded,
/// errored, etc.).
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct View {
    #[deref]
    frp:   Frp,
    model: Model,
}


// === Main `impl` ===

impl View {
    /// Create new Spinner [`View`].
    pub fn new(state: State, app: &application::Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(state);
        Self { frp, model }.init(app)
    }

    fn init(self, app: &application::Application) -> Self {
        let frp = &self.frp;
        let network = &frp.network;

        frp::extend! { network
            init <- source_();
        }
        self.init_input_processing();
        self.init_mouse_interactions();
        self.init_state();
        let init_color_scheme = self.init_color_and_visibility(app);
        self.init_event_tracing();
        init.emit(());
        init_color_scheme.emit(());

        self
    }

    fn init_input_processing(&self) {
        let frp = &self.frp;
        let model = &self.model;
        let network = &frp.network;
        let hover = &model.hover;
        let ring = &model.ring;
        let spinner = &model.spinner;
        let play = &model.play;
        let pause = &model.pause;

        frp::extend! { network
            eval frp.set_size ((size) hover.size.set(*size));
            eval frp.set_size ((size) ring.size.set(*size));
            eval frp.set_size ((size) spinner.size.set(*size));
            eval frp.set_size ((size) play.size.set(*size));
            eval frp.set_size ((size) pause.size.set(*size));
        }
    }

    fn init_mouse_interactions(&self) {
        let frp = &self.frp;
        let model = &self.model;
        let network = &frp.network;
        let hover = &model.hover;
        let out = &frp.private().output;

        frp::extend! { network
            out.command <+ hover.events.mouse_up.map(f!((_mouse_up) model.toggle()));
            out.command <+ hover.events.mouse_down_primary.map(f!((_mouse_down_primary) model.toggle()));
        }
    }

    fn init_state(&self) {
        let frp = &self.frp;
        let model = &self.model;
        let network = &frp.network;

        frp::extend! { network
            eval frp.set_state ((state) model.set_state(*state));
        }
    }

    fn init_color_and_visibility(
        &self,
        app: &application::Application,
    ) -> stream::WeakNode<frp::SourceData> {
        let scene = &app.display.default_scene;
        let styles = StyleWatchFrp::new(&scene.style_sheet);
        let frp = &self.frp;
        let model = &self.model;
        let network = &frp.network;
        let hover = &model.hover;
        let ring = &model.ring;
        let spinner = &model.spinner;
        let play = &model.play;
        let pause = &model.pause;

        frp::extend! { network
            use theme::cloud_dashboard::button;
            let non_toggled_color       = styles.get_color(button::non_toggled);
            let toggled_color           = styles.get_color(button::toggled);
            let hovered_color           = styles.get_color(button::hovered);
            let toggled_hovered_color   = styles.get_color(button::toggled_hovered);
            init_color_scheme           <- source::<()>();

            hovered <- bool(&hover.events.mouse_out, &hover.events.mouse_over);
            pressed <- bool(&hover.events.mouse_up_primary, &hover.events.mouse_down_primary);
            color <- all_with3(&init_color_scheme, &hovered, &pressed,
                move |_cs, h, p| {
                    match (h, p) {
                        (false, false) => non_toggled_color.value(),
                        (false, true)  => toggled_color.value(),
                        (true, false)  => hovered_color.value(),
                        (true, true)   => toggled_hovered_color.value(),
                    }
                });
            color_rgb <- color.map(|c| Vector3(c.red, c.green, c.blue));
            eval color_rgb ((color) ring.color_rgb.set(*color));
            eval color_rgb ((color) spinner.color_rgb.set(*color));
            eval color ((color) play.set_color(*color));
            eval color ((color) pause.set_color(*color));

            spinner_visible <- frp.state.map(State::spinner_visible);
            ring_visible <- spinner_visible.not();

            eval spinner_visible ((visible) spinner.alpha.set(if *visible { 1.0 } else { 0.0 }));
            eval ring_visible ((visible) ring.alpha.set(if *visible { 1.0 } else { 0.0 }));
        }

        init_color_scheme
    }

    fn init_visibility(&self) {
        let frp = &self.frp;
        let model = &self.model;
        let network = &frp.network;
        let ring = &model.ring;
        let spinner = &model.spinner;

        frp::extend! { network
            spinner_visible <- frp.state.map(State::spinner_visible);
            ring_visible <- spinner_visible.not();

            eval spinner_visible ((visible) spinner.alpha.set(if *visible { VISIBLE } else { INVISIBLE }));
            eval ring_visible ((visible) ring.alpha.set(if *visible { VISIBLE } else { INVISIBLE }));
        }
    }

    fn init_event_tracing(&self) {
        let frp = &self.frp;
        let model = &self.model;
        let hover = &model.hover;
        let ring = &model.ring;
        let spinner = &model.spinner;
        let play = &model.play;
        let pause = &model.pause;
        let network = &frp.network;
        let input = &frp.input;

        frp::extend! { network
            // === State ===

            trace input.set_state;
            trace frp.set_state;
            trace frp.state;
            trace frp.command;


            // === Mouse Events ===

            trace hover.events.mouse_over;
            trace hover.events.mouse_out;
            trace hover.events.mouse_down;
            trace hover.events.mouse_up;
            trace ring.events.mouse_over;
            trace ring.events.mouse_out;
            trace ring.events.mouse_down;
            trace ring.events.mouse_up;
            trace spinner.events.mouse_over;
            trace spinner.events.mouse_out;
            trace spinner.events.mouse_down;
            trace spinner.events.mouse_up;
            trace play.events.mouse_over;
            trace play.events.mouse_over;
            trace play.events.mouse_out;
            trace play.events.mouse_down;
            trace pause.events.mouse_up;
            trace pause.events.mouse_out;
            trace pause.events.mouse_down;
            trace pause.events.mouse_up;
        }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
