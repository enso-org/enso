//! Mouse FRP bindings.

use crate::prelude::*;

use crate as frp;
use crate::data::bitfield::BitField;
use crate::data::bitfield::BitField32;

use nalgebra::Vector2;



// ==============
// === Button ===
// ==============

/// An enumeration representing the mouse buttons. Please note that we do not name the buttons
/// left, right, and middle, as this assumes we use a mouse for right-hand people.
///
/// JS supports up to 5 mouse buttons currently:
/// https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button
/// https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Button {
    #[default]
    Button0,
    Button1,
    Button2,
    Button3,
    Button4,
}
pub use Button::*;

#[allow(non_upper_case_globals, missing_docs)]
mod button_aliases {
    use super::*;
    pub const PrimaryButton: Button = Button0;
    pub const MiddleButton: Button = Button1;
    pub const SecondaryButton: Button = Button2;
}
pub use button_aliases::*;

impl Button {
    /// Construct a button from the provided code point. In case the code is unrecognized, `None`
    /// will be returned.
    pub fn try_from_code(code: i32) -> Option<Self> {
        match code {
            0 => Some(Self::Button0),
            1 => Some(Self::Button1),
            2 => Some(Self::Button2),
            3 => Some(Self::Button3),
            4 => Some(Self::Button4),
            _ => None,
        }
    }

    /// Construct a button from the provided code point. In case the code is unrecognized, the
    /// default button will be returned.
    pub fn from_code(code: i32) -> Self {
        Self::try_from_code(code).unwrap_or_else(|| {
            let invalid_msg = "The provided mouse button code is invalid";
            let revert_msg = "Reverting to the default button.";
            warn!("{invalid_msg} ({code}). {revert_msg}");
            default()
        })
    }

    /// The code point of the button.
    pub fn code(self) -> usize {
        match self {
            Button0 => 0,
            Button1 => 1,
            Button2 => 2,
            Button3 => 3,
            Button4 => 4,
        }
    }

    /// Simple, user-friendly name of a key. Used in shortcut manager.
    pub fn simple_name(self) -> String {
        format!("mouse-button-{}", self.code())
    }
}



// ==================
// === ButtonMask ===
// ==================

/// The button bitmask (each bit represents one button). Used for matching button combinations.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[allow(missing_docs)]
pub struct ButtonMask {
    pub bits: BitField32,
}

impl ButtonMask {
    /// Creates ButtonMask from Vec<Key>.
    pub fn from_vec(buttons: Vec<Button>) -> Self {
        buttons.iter().collect()
    }

    /// Check if button bit is on.
    pub fn contains(self, button: Button) -> bool {
        self.bits.get_bit(button.code())
    }

    /// Set the `button` bit with the new state.
    pub fn set(&mut self, button: Button, state: bool) {
        self.bits.set_bit(button.code(), state);
    }

    /// Clone the mask and set the `button` bit with the new state.
    pub fn with_set(mut self, button: Button, state: bool) -> Self {
        self.set(button, state);
        self
    }
}

impl<'a> FromIterator<&'a Button> for ButtonMask {
    fn from_iter<T: IntoIterator<Item = &'a Button>>(buttons: T) -> Self {
        let mut mask = ButtonMask::default();
        for button in buttons {
            mask.set(*button, true)
        }
        mask
    }
}

impl From<&[Button]> for ButtonMask {
    fn from(t: &[Button]) -> Self {
        ButtonMask::from_iter(t)
    }
}
impl From<&[Button; 0]> for ButtonMask {
    fn from(t: &[Button; 0]) -> Self {
        ButtonMask::from_iter(t)
    }
}
impl From<&[Button; 1]> for ButtonMask {
    fn from(t: &[Button; 1]) -> Self {
        ButtonMask::from_iter(t)
    }
}
impl From<&[Button; 2]> for ButtonMask {
    fn from(t: &[Button; 2]) -> Self {
        ButtonMask::from_iter(t)
    }
}
impl From<&[Button; 3]> for ButtonMask {
    fn from(t: &[Button; 3]) -> Self {
        ButtonMask::from_iter(t)
    }
}
impl From<&[Button; 4]> for ButtonMask {
    fn from(t: &[Button; 4]) -> Self {
        ButtonMask::from_iter(t)
    }
}
impl From<&[Button; 5]> for ButtonMask {
    fn from(t: &[Button; 5]) -> Self {
        ButtonMask::from_iter(t)
    }
}
impl From<&ButtonMask> for ButtonMask {
    fn from(t: &ButtonMask) -> Self {
        *t
    }
}



// =============
// === Mouse ===
// =============

/// Mouse FRP bindings.
///
/// # Deprecated
/// This API is deprecated. Instead, use the display object's event API. For example, to get an FRP
/// endpoint for mouse event, you can use the [`crate::display::Object::on_event`] function.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
#[allow(non_camel_case_types)]
pub struct Mouse_DEPRECATED {
    pub network:              frp::Network,
    pub up:                   frp::Source<Button>,
    pub down:                 frp::Source<Button>,
    pub wheel:                frp::Source,
    pub up_0:                 frp::Stream,
    pub up_1:                 frp::Stream,
    pub up_2:                 frp::Stream,
    pub up_3:                 frp::Stream,
    pub up_4:                 frp::Stream,
    pub up_primary:           frp::Stream,
    pub up_middle:            frp::Stream,
    pub up_secondary:         frp::Stream,
    pub down_0:               frp::Stream,
    pub down_1:               frp::Stream,
    pub down_2:               frp::Stream,
    pub down_3:               frp::Stream,
    pub down_4:               frp::Stream,
    pub down_primary:         frp::Stream,
    pub down_middle:          frp::Stream,
    pub down_secondary:       frp::Stream,
    pub is_up_0:              frp::Stream<bool>,
    pub is_up_1:              frp::Stream<bool>,
    pub is_up_2:              frp::Stream<bool>,
    pub is_up_3:              frp::Stream<bool>,
    pub is_up_4:              frp::Stream<bool>,
    pub is_up_primary:        frp::Stream<bool>,
    pub is_up_middle:         frp::Stream<bool>,
    pub is_up_secondary:      frp::Stream<bool>,
    pub is_down_0:            frp::Stream<bool>,
    pub is_down_1:            frp::Stream<bool>,
    pub is_down_2:            frp::Stream<bool>,
    pub is_down_3:            frp::Stream<bool>,
    pub is_down_4:            frp::Stream<bool>,
    pub is_down_primary:      frp::Stream<bool>,
    pub is_down_middle:       frp::Stream<bool>,
    pub is_down_secondary:    frp::Stream<bool>,
    pub position:             frp::Source<Vector2<f32>>,
    pub position_top_left:    frp::Source<Vector2<f32>>,
    pub position_bottom_left: frp::Source<Vector2<f32>>,
    pub prev_position:        frp::Stream<Vector2<f32>>,
    pub translation:          frp::Stream<Vector2<f32>>,
    pub distance:             frp::Stream<f32>,
    pub ever_moved:           frp::Stream<bool>,
    pub button_mask:          frp::Stream<ButtonMask>,
    pub prev_button_mask:     frp::Stream<ButtonMask>,
}

impl Mouse_DEPRECATED {
    /// Smart accessor for `up_X` field.
    pub fn up(&self, button: Button) -> &frp::Stream {
        match button {
            Button0 => &self.up_0,
            Button1 => &self.up_1,
            Button2 => &self.up_2,
            Button3 => &self.up_3,
            Button4 => &self.up_4,
        }
    }

    /// Smart accessor for `down_X` field.
    pub fn down(&self, button: Button) -> &frp::Stream {
        match button {
            Button0 => &self.down_0,
            Button1 => &self.down_1,
            Button2 => &self.down_2,
            Button3 => &self.down_3,
            Button4 => &self.down_4,
        }
    }

    /// Smart accessor for `is_up_X` field.
    pub fn is_up(&self, button: Button) -> &frp::Stream<bool> {
        match button {
            Button0 => &self.is_up_0,
            Button1 => &self.is_up_1,
            Button2 => &self.is_up_2,
            Button3 => &self.is_up_3,
            Button4 => &self.is_up_4,
        }
    }

    /// Smart accessor for `is_down_X` field.
    pub fn is_down(&self, button: Button) -> &frp::Stream<bool> {
        match button {
            Button0 => &self.is_down_0,
            Button1 => &self.is_down_1,
            Button2 => &self.is_down_2,
            Button3 => &self.is_down_3,
            Button4 => &self.is_down_4,
        }
    }
}

impl Default for Mouse_DEPRECATED {
    fn default() -> Self {
        frp::new_network! { network
            up            <- source();
            down          <- source();
            wheel         <- source();
            position      <- source();
            position_top_left <- source();
            position_bottom_left <- source();
            prev_position <- position.previous();
            translation   <- position.map2(&prev_position,|t,s|t-s);
            distance      <- translation.map(|t:&Vector2<f32>|t.norm());
            ever_moved    <- position.constant(true);

            up_0_check    <- up.map(|t|*t==Button0);
            up_1_check    <- up.map(|t|*t==Button1);
            up_2_check    <- up.map(|t|*t==Button2);
            up_3_check    <- up.map(|t|*t==Button3);
            up_4_check    <- up.map(|t|*t==Button4);

            down_0_check  <- down.map(|t|*t==Button0);
            down_1_check  <- down.map(|t|*t==Button1);
            down_2_check  <- down.map(|t|*t==Button2);
            down_3_check  <- down.map(|t|*t==Button3);
            down_4_check  <- down.map(|t|*t==Button4);

            up_0          <- up.gate(&up_0_check).constant(());
            up_1          <- up.gate(&up_1_check).constant(());
            up_2          <- up.gate(&up_2_check).constant(());
            up_3          <- up.gate(&up_3_check).constant(());
            up_4          <- up.gate(&up_4_check).constant(());
            let up_primary   = up_0.clone_ref();
            let up_middle    = up_1.clone_ref();
            let up_secondary = up_2.clone_ref();

            down_0        <- down.gate(&down_0_check).constant(());
            down_1        <- down.gate(&down_1_check).constant(());
            down_2        <- down.gate(&down_2_check).constant(());
            down_3        <- down.gate(&down_3_check).constant(());
            down_4        <- down.gate(&down_4_check).constant(());
            let down_primary   = down_0.clone_ref();
            let down_middle    = down_1.clone_ref();
            let down_secondary = down_2.clone_ref();

            is_down_0     <- bool(&up_0,&down_0);
            is_down_1     <- bool(&up_1,&down_1);
            is_down_2     <- bool(&up_2,&down_2);
            is_down_3     <- bool(&up_3,&down_3);
            is_down_4     <- bool(&up_4,&down_4);
            let is_down_primary   = is_down_0.clone_ref();
            let is_down_middle    = is_down_1.clone_ref();
            let is_down_secondary = is_down_2.clone_ref();

            is_up_0       <- is_down_0.map(|t|!t);
            is_up_1       <- is_down_1.map(|t|!t);
            is_up_2       <- is_down_2.map(|t|!t);
            is_up_3       <- is_down_3.map(|t|!t);
            is_up_4       <- is_down_4.map(|t|!t);
            let is_up_primary   = is_up_0.clone_ref();
            let is_up_middle    = is_up_1.clone_ref();
            let is_up_secondary = is_up_2.clone_ref();

            button_mask   <- any_mut::<ButtonMask>();
            button_mask   <+ down . map2(&button_mask,|button,mask| mask.with_set(*button,true));
            button_mask   <+ up   . map2(&button_mask,|button,mask| mask.with_set(*button,false));

            prev_button_mask <- button_mask.previous();
        };
        let button_mask = button_mask.into();
        Self {
            network,
            up,
            down,
            wheel,
            up_0,
            up_1,
            up_2,
            up_3,
            up_4,
            up_primary,
            up_middle,
            up_secondary,
            down_0,
            down_1,
            down_2,
            down_3,
            down_4,
            down_primary,
            down_middle,
            down_secondary,
            is_up_0,
            is_up_1,
            is_up_2,
            is_up_3,
            is_up_4,
            is_up_primary,
            is_up_middle,
            is_up_secondary,
            is_down_0,
            is_down_1,
            is_down_2,
            is_down_3,
            is_down_4,
            is_down_primary,
            is_down_middle,
            is_down_secondary,
            position,
            position_top_left,
            position_bottom_left,
            prev_position,
            translation,
            distance,
            ever_moved,
            button_mask,
            prev_button_mask,
        }
    }
}

impl Mouse_DEPRECATED {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}
