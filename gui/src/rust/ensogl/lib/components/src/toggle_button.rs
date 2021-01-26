//! Toggle Button implementation.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::shape::primitive::system;
use ensogl_core::display;
use ensogl_core::gui::component::ShapeView;



// =================
// === Colorable ===
// =================

/// A shape that can have a single color.
// TODO implement a derive like macro for this trait that can be used for shape creation.
pub trait ColorableShape : system::Shape {
    /// Set the color of the shape.
    fn set_color(&self, color:color::Rgba);
}



// ===========
// === Frp ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_visibility   (bool),
        set_color_scheme (ColorScheme),
        set_size         (Vector2),
    }
    Output {
        state      (bool),
        mouse_over (),
        mouse_out  (),
    }
}



// =============
// === Model ===
// =============

#[derive(Clone,Debug)]
struct Model<Shape:system::Shape> {
    icon : ShapeView<Shape>,
}

impl<Shape:ColorableShape+'static> Model<Shape> {
    fn new(app:&Application) -> Self {
        let logger = Logger::new("ToggleButton");
        let icon   = ShapeView::new(&logger, app.display.scene());
        Self{icon}
    }
}



// ===================
// === ButtonState ===
// ===================

/// A state a button can be in.
#[derive(Clone,Copy,Debug)]
#[allow(missing_docs)]
pub struct ButtonState {
    pub visible : bool,
    pub toggled : bool,
    pub hovered : bool,
    pub pressed : bool,
}

impl ButtonState {
    /// Constructor.
    pub fn new(visible:bool, toggled:bool, hovered:bool, pressed:bool) -> Self {
        Self {visible,toggled,hovered,pressed}
    }
}

impl Default for ButtonState {
    fn default() -> Self {
        let visible = true;
        let toggled = false;
        let hovered = false;
        let pressed = false;
        Self {visible,toggled,hovered,pressed}
    }
}



// ===================
// === ColorScheme ===
// ===================

/// Button color scheme.
#[derive(Clone,Debug,Default)]
#[allow(missing_copy_implementations)]
#[allow(missing_docs)]
pub struct ColorScheme {
    pub non_toggled     : Option<color::Lcha>,
    pub hovered         : Option<color::Lcha>,
    pub pressed         : Option<color::Lcha>,
    pub toggled         : Option<color::Lcha>,
    pub toggled_hovered : Option<color::Lcha>,
    pub toggled_pressed : Option<color::Lcha>,
}

impl ColorScheme {
    /// Query the scheme based on the button state.
    pub fn query(&self, state:ButtonState) -> color::Lcha {
        match (state.visible, state.toggled, state.hovered, state.pressed) {
            ( false , _    , _    , _     ) => color::Lcha::transparent(),
            ( true  , false, false, false ) => self.non_toggled(),
            ( true  , false, false, true  ) => self.pressed(),
            ( true  , false, true , false ) => self.hovered(),
            ( true  , false, true , true  ) => self.pressed(),
            ( true  , true , false, false ) => self.toggled(),
            ( true  , true , false, true  ) => self.toggled_pressed(),
            ( true  , true , true , false ) => self.toggled_hovered(),
            ( true  , true , true , true  ) => self.toggled_pressed(),
        }
    }
}


// === Getters ===

#[allow(missing_docs)]
impl ColorScheme {
    pub fn non_toggled (&self) -> color::Lcha {
        self.non_toggled.unwrap_or_else(color::Lcha::black)
    }

    pub fn hovered (&self) -> color::Lcha {
        self.hovered.unwrap_or_else(||self.pressed())
    }

    pub fn pressed (&self) -> color::Lcha {
        self.hovered.unwrap_or_else(||self.toggled())
    }

    pub fn toggled (&self) -> color::Lcha {
        self.toggled.unwrap_or_else(color::Lcha::black)
    }

    pub fn toggled_hovered (&self) -> color::Lcha {
        self.toggled_hovered.unwrap_or_else(||self.toggled())
    }

    pub fn toggled_pressed (&self) -> color::Lcha {
        self.toggled_pressed.unwrap_or_else(||self.pressed())
    }
}



// =====================
// === Toggle Button ===
// =====================

/// A UI component that acts as a toggle which can be toggled on and of. Has a visible shape
/// that acts as button and changes color depending on the toggle state.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct ToggleButton<Shape:system::Shape> {
    pub frp : Frp,
    model   : Rc<Model<Shape>>,
}

impl<Shape:system::Shape> Deref for ToggleButton<Shape> {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl<Shape:ColorableShape+'static> ToggleButton<Shape>{
    /// Constructor.
    pub fn new(app:&Application) -> Self {
        let frp   = Frp::new();
        let model = Rc::new(Model::<Shape>::new(app));
        Self {frp,model}.init_frp()
    }

    fn init_frp(self) -> Self {
        let network = &self.frp.network;
        let frp     = &self.frp;
        let model   = &self.model;
        let color   = color::Animation::new(network);
        let icon    = &model.icon.events;

        frp::extend! { network

             // === Input Processing ===

            eval frp.set_size ((size) {
                model.icon.shape.sprites().iter().for_each(|sprite| sprite.size.set(*size))
            });


            // === Mouse Interactions ===

            frp.source.mouse_over <+ icon.mouse_over;
            frp.source.mouse_out  <+ icon.mouse_out;
            frp.source.state      <+ icon.mouse_down.toggle();


            // === Color ===

            invisible <- frp.set_visibility.on_false().constant(0.0);
            color.target_alpha <+ invisible;

            visible    <- frp.set_visibility.gate(&frp.set_visibility);
            is_hovered <- bool(&icon.mouse_out,&icon.mouse_over);
            is_pressed <- bool(&icon.mouse_up,&icon.mouse_down);

            button_state <- all_with4(&visible,&frp.state,&is_hovered,&is_pressed,
                |a,b,c,d| ButtonState::new(*a,*b,*c,*d));

            color_target <- all_with(&frp.set_color_scheme,&button_state,
                |colors,state| colors.query(*state));

            color.target <+ color_target;
            eval color.value ((color) model.icon.shape.set_color(color.into()));
        }

        color.target_alpha.emit(0.0);
        self
    }

    /// Return the underlying shape view. Note that some parameters like size and color will be
    /// overwritten regularly by internals of the `ToggleButton` mechanics.
    pub fn view(&self) -> ShapeView<Shape> {
        self.model.icon.clone_ref()
    }
}

impl<T:ColorableShape> display::Object for ToggleButton<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.icon.display_object()
    }
}
