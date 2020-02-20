//! A module with all TextField's FRP definitions with binding them to js events.

pub mod mouse;
pub mod keyboard;

use crate::prelude::*;

use crate::control::io::mouse::MouseFrpCallbackHandles;
use crate::display::shape::text::text_field::frp::keyboard::TextFieldKeyboardFrp;
use crate::display::shape::text::text_field::frp::mouse::TextFieldMouseFrp;
use crate::display::shape::text::text_field::TextFieldData;
use crate::display::world::World;
use crate::system::web::text_input::KeyboardBinding;



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
    keyboard_binding : KeyboardBinding,
    mouse_binding    : MouseFrpCallbackHandles,
}

impl TextFieldFrp {
    /// Create FRP definitions which will do their actions on given `text_field`, and are bound
    /// to JS events.
    pub fn new(world:&World, text_field_ptr:Weak<RefCell<TextFieldData>>) -> Self {
        let keyboard         = TextFieldKeyboardFrp::new(text_field_ptr.clone());
        let mouse            = TextFieldMouseFrp::new(text_field_ptr,&keyboard);
        let keyboard_binding = keyboard.bind_frp_to_js_text_input_actions();
        let mouse_binding    = mouse.bind_frp_to_mouse(world);
        TextFieldFrp {keyboard,mouse,keyboard_binding,mouse_binding}
    }
}
