//! Toggle Button

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatch;
use ensogl_core::display::shape::primitive::system;
use ensogl_core::display::shape::style_watch;
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
        set_visibility (bool),
        set_base_color (style_watch::ColorSource),
        set_size       (Vector2),
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
        Self {frp,model}.init_frp(app)
    }

    fn init_frp(self, app:&Application) -> Self {
        let network = &self.frp.network;
        let frp     = &self.frp;
        let model   = &self.model;
        let style   = StyleWatch::new(&app.display.scene().style_sheet);
        let color   = color::DEPRECARTED_Animation::new();
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

            invisible <- frp.set_visibility.gate_not(&frp.set_visibility);
            eval_ invisible (color.set_target_alpha(0.0));

            visible    <- frp.set_visibility.gate(&frp.set_visibility);
            is_hovered <- bool(&icon.mouse_out,&icon.mouse_over);

            button_state <- all3(&visible,&is_hovered,&frp.state);
            state_change <- all(&frp.set_base_color, &button_state);
            eval state_change ([color,style]((source,(visible,hovered,state))) {
                let source = source.clone();
                match(*visible,*hovered,*state) {
                    (false,_,_)        => color.set_target_alpha(0.0),
                    (true,true,_)      => color.set_target(style.get_color(source)),
                    (true,false,true)  => color.set_target(style.get_color(source)),
                    (true,false,false) => color.set_target(style.get_color_dim(source)),
                }
            });

            eval color.value ((color) model.icon.shape.set_color(color.into()));
        }

        color.alpha.set_value(0.0);
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
