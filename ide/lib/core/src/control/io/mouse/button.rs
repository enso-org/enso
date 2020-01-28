//! This module defines bindings to mouse buttons.

use crate::prelude::*;


// ===================
// === MouseButton ===
// ===================

/// An enumeration representing the mouse buttons. Please note that we do not name the buttons
/// left, right, and middle, as this assumes we use a mouse for right-hand people.
///
/// JS supports up to 5 mouse buttons currently:
/// https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button
#[derive(Debug,Clone,Copy)]
#[allow(missing_docs)]
pub enum Button {Button0,Button1,Button2,Button3,Button4}
pub use Button::*;

#[allow(non_upper_case_globals)]
mod button_aliases {
    use super::*;
    const PrimaryButton   : Button = Button0;
    const MiddleButton    : Button = Button1;
    const SecondaryButton : Button = Button2;
}
pub use button_aliases::*;

impl Button {
    /// Construct a button from the code point. Panics if the code is bigger than 4 or smaller
    /// than 0.
    pub fn from_code(code:i16) -> Self {
        match code {
            0 => Self::Button0,
            1 => Self::Button1,
            2 => Self::Button2,
            3 => Self::Button3,
            4 => Self::Button4,
            _ => panic!("Invalid button code"),
        }
    }
}
