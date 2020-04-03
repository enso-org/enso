//! A module with all TextField's FRP definitions with binding them to js events.

pub mod mouse;
pub mod keyboard;

use crate::prelude::*;

use crate::control::io::mouse::MouseFrpCallbackHandles;
use crate::display::shape::text::text_field::frp::keyboard::TextFieldKeyboardFrp;
use crate::display::shape::text::text_field::frp::mouse::TextFieldMouseFrp;
use crate::display::shape::text::text_field::WeakTextField;
use crate::display::world::World;



// ====================
// === TextFieldFrp ===
// ====================

/// A structure holding all of the FRP definitions for TextField, which are bound to appropriate
/// js events.
// TODO[ao]: the frp should be bound to global event system (provided by e.g. display object).
#[derive(Debug)]
pub struct TextFieldFrp {
    /// Keyboard FRP definitions.
    pub keyboard: TextFieldKeyboardFrp,
    /// Mouse FRP definitions.
    pub mouse: TextFieldMouseFrp,
    mouse_binding    : MouseFrpCallbackHandles,
}

impl TextFieldFrp {
    /// Create FRP definitions which will do their actions on given `text_field`, and are bound
    /// to JS events.
    pub fn new(world:&World, text_field:WeakTextField) -> Self {
        let keyboard         = TextFieldKeyboardFrp::new(text_field.clone_ref());
        let mouse            = TextFieldMouseFrp::new(text_field,&keyboard);
        let mouse_binding    = mouse.bind_frp_to_mouse(world);
        TextFieldFrp {keyboard,mouse,mouse_binding}
    }
}
