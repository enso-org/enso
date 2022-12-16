//! Toggle Button implementation.

#![recursion_limit = "512"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
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

use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::system::Shape;
use ensogl_core::display::shape::system::ShapeWithDefaultableData;
use ensogl_core::gui::component::ShapeView;



// =================
// === Colorable ===
// =================

/// A shape that can have a single color.
///
/// The [`DynamicShapeInternals`] is used to allow manual creation of [`ShapeView`]. Normally, this
/// is automatically used by the [`shape!`] macro, and it's not exposed to the
/// developer.
pub trait ColorableShape: ShapeWithDefaultableData {
    /// Set the color of the shape.
    fn set_color(&self, color: color::Rgba);
}



// ===========
// === Frp ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_visibility   (bool),
        set_color_scheme (ColorScheme),
        set_size         (Vector2),
        toggle           (),
        set_state        (bool),
    }
    Output {
        state      (bool),
        visible    (bool),
        mouse_over (),
        mouse_out  (),
        is_hovered (bool),
        is_pressed (bool),
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
#[clone_ref(bound = "S: CloneRef")]
struct Model<S: Shape> {
    icon: ShapeView<S>,
}

impl<Shape: ColorableShape + 'static> Model<Shape> {
    fn new() -> Self {
        let icon = ShapeView::new();
        Self { icon }
    }
}



// ===================
// === ButtonState ===
// ===================

/// A state a button can be in.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct ButtonState {
    pub visible: bool,
    pub toggled: bool,
    pub hovered: bool,
    pub pressed: bool,
}

impl ButtonState {
    /// Constructor.
    pub fn new(visible: bool, toggled: bool, hovered: bool, pressed: bool) -> Self {
        Self { visible, toggled, hovered, pressed }
    }
}

impl Default for ButtonState {
    fn default() -> Self {
        let visible = true;
        let toggled = false;
        let hovered = false;
        let pressed = false;
        Self { visible, toggled, hovered, pressed }
    }
}



// ===================
// === ColorScheme ===
// ===================

/// Button color scheme.
#[derive(Clone, Debug, Default)]
#[allow(missing_copy_implementations)]
#[allow(missing_docs)]
pub struct ColorScheme {
    pub non_toggled:     Option<color::Lcha>,
    pub hovered:         Option<color::Lcha>,
    pub pressed:         Option<color::Lcha>,
    pub toggled:         Option<color::Lcha>,
    pub toggled_hovered: Option<color::Lcha>,
    pub toggled_pressed: Option<color::Lcha>,
}

impl ColorScheme {
    /// Query the scheme based on the button state.
    pub fn query(&self, state: ButtonState) -> color::Lcha {
        match (state.visible, state.toggled, state.hovered, state.pressed) {
            (false, _, _, _) => color::Lcha::transparent(),
            (true, false, false, false) => self.non_toggled(),
            (true, false, false, true) => self.pressed(),
            (true, false, true, false) => self.hovered(),
            (true, false, true, true) => self.pressed(),
            (true, true, false, false) => self.toggled(),
            (true, true, false, true) => self.toggled_pressed(),
            (true, true, true, false) => self.toggled_hovered(),
            (true, true, true, true) => self.toggled_pressed(),
        }
    }
}


// === Getters ===

#[allow(missing_docs)]
impl ColorScheme {
    pub fn non_toggled(&self) -> color::Lcha {
        self.non_toggled.unwrap_or_else(color::Lcha::black)
    }

    pub fn hovered(&self) -> color::Lcha {
        self.hovered.unwrap_or_else(|| self.pressed())
    }

    pub fn pressed(&self) -> color::Lcha {
        self.hovered.unwrap_or_else(|| self.toggled())
    }

    pub fn toggled(&self) -> color::Lcha {
        self.toggled.unwrap_or_else(color::Lcha::black)
    }

    pub fn toggled_hovered(&self) -> color::Lcha {
        self.toggled_hovered.unwrap_or_else(|| self.toggled())
    }

    pub fn toggled_pressed(&self) -> color::Lcha {
        self.toggled_pressed.unwrap_or_else(|| self.pressed())
    }
}



// =====================
// === Toggle Button ===
// =====================

/// A UI component that acts as a toggle which can be toggled on and of. Has a visible shape
/// that acts as button and changes color depending on the toggle state.
#[derive(CloneRef, Debug, Derivative, Deref)]
#[derivative(Clone(bound = ""))]
#[allow(missing_docs)]
pub struct ToggleButton<S: Shape> {
    #[deref]
    pub frp: Frp,
    model:   Rc<Model<S>>,
}

impl<Shape: ColorableShape + 'static> Default for ToggleButton<Shape> {
    fn default() -> Self {
        let frp = Frp::new();
        let model = Rc::new(Model::<Shape>::new());
        Self { frp, model }.init_frp()
    }
}

impl<Shape: ColorableShape + 'static> ToggleButton<Shape> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    fn init_frp(self) -> Self {
        let network = &self.frp.network;
        let frp = &self.frp;
        let model = &self.model;
        let color = color::Animation::new(network);
        let icon = &model.icon.events;

        frp::extend! { network

             // === Input Processing ===

            eval frp.set_size ((size) model.icon.set_size(*size););


            // === State ===

            toggle <- any_(frp.toggle, icon.mouse_down_primary);
            frp.source.state <+ frp.state.not().sample(&toggle);
            frp.source.state <+ frp.set_state;


            // === Mouse Interactions ===

            frp.source.mouse_over <+ icon.mouse_over;
            frp.source.mouse_out  <+ icon.mouse_out;


            // === Color ===

            invisible <- frp.set_visibility.on_false().constant(0.0);
            color.target_alpha <+ invisible;

            frp.source.visible    <+ frp.set_visibility;
            frp.source.is_hovered <+ bool(&icon.mouse_out,&icon.mouse_over);
            frp.source.is_pressed <+ bool(&icon.mouse_up_primary,&icon.mouse_down_primary);

            button_state <- all_with4(&frp.visible,&frp.state,&frp.is_hovered,&frp.is_pressed,
                |a,b,c,d| ButtonState::new(*a,*b,*c,*d));

            color_target <- all_with(&frp.set_color_scheme,&button_state,
                |colors,state| colors.query(*state));

            color.target <+ color_target;
            eval color.value ((color) model.icon.set_color(color.into()));
        }

        frp.set_state.emit(false);
        color.target_alpha.emit(0.0);
        self
    }

    /// Return the underlying shape view. Note that some parameters like size and color will be
    /// overwritten regularly by internals of the `ToggleButton` mechanics.
    pub fn view(&self) -> ShapeView<Shape> {
        self.model.icon.clone_ref()
    }
}

impl<S: Shape> display::Object for ToggleButton<S> {
    fn display_object(&self) -> &display::object::Instance {
        self.model.icon.display_object()
    }
}
