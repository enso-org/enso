//! Abstractions for objects that can interact with the pointer (in most cases this is a mouse).

use crate::prelude::*;

use crate::control::io::mouse;
use crate::display::symbol;

use enso_frp as frp;


// =================
// === Constants ===
// =================

const ID_ENCODING_OVERFLOW_ERR: u32 =
    include!("../shape/primitive/glsl/error_codes/id_encoding_overflow.txt");



// =====================
// === PointerTarget ===
// =====================

/// Abstraction for objects that can interact with a mouse.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct PointerTarget {
    network:           frp::Network,
    /// Mouse button was pressed while the pointer was hovering this object.
    pub mouse_down:    frp::Source<mouse::Button>,
    /// Mouse button was released while the pointer was hovering this object.
    pub mouse_up:      frp::Source<mouse::Button>,
    /// Mouse button that was earlier pressed on this object was just released. The mouse pointer
    /// does not have to hover this object anymore.
    pub mouse_release: frp::Source<mouse::Button>,
    /// Mouse pointer entered the object shape.
    pub mouse_over:    frp::Source,
    /// Mouse pointer exited the object shape.
    pub mouse_out:     frp::Source,
    /// The mouse target was dropped.
    pub on_drop:       frp::Source,
}

impl PointerTarget {
    /// Constructor.
    pub fn new() -> Self {
        frp::new_network! { network
            on_drop       <- source_();
            mouse_down    <- source();
            mouse_up      <- source();
            mouse_release <- source();
            mouse_over    <- source_();
            mouse_out     <- source_();

            is_mouse_over <- bool(&mouse_out,&mouse_over);
            out_on_drop   <- on_drop.gate(&is_mouse_over);
            eval_ out_on_drop (mouse_out.emit(()));
        }
        Self { network, mouse_down, mouse_up, mouse_release, mouse_over, mouse_out, on_drop }
    }
}

impl Default for PointerTarget {
    fn default() -> Self {
        Self::new()
    }
}



// =======================
// === PointerTargetId ===
// =======================

/// Pointer target ID, a unique ID for an object pointed by the mouse.
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
#[allow(missing_docs)]
pub enum PointerTargetId {
    Background,
    Symbol { id: symbol::GlobalInstanceId },
}

impl PointerTargetId {
    /// Decode the [`PointerTargetId`] from an RGBA value. If alpha is set to 0, the result will be
    /// background. In case alpha is 255, the result will be decoded based on the first 3 bytes,
    /// which allows for storing up to 16 581 375 unique IDs.
    ///
    /// Please see the [`fragment_runner.glsl`] file to see the encoding implementation and learn
    /// more about the possible overflow behavior.
    pub fn decode_from_rgba(v: Vector4<u32>) -> Result<Self, DecodeError> {
        let alpha = v.w;
        match alpha {
            0 => Ok(Self::Background),
            255 => {
                let raw_id = Self::decode_raw(v.x, v.y, v.z);
                let id = symbol::GlobalInstanceId::new(raw_id);
                Ok(Self::Symbol { id })
            }
            _ => {
                let err = if alpha == ID_ENCODING_OVERFLOW_ERR {
                    DecodeError::Overflow
                } else {
                    DecodeError::WrongAlpha(alpha)
                };
                Err(err)
            }
        }
    }

    fn decode_raw(r: u32, g: u32, b: u32) -> u32 {
        (b << 16) + (g << 8) + r
    }

    /// Check whether this id points to the background.
    pub fn is_background(self) -> bool {
        self == Self::Background
    }

    /// Check whether this id points to a symbol.
    pub fn is_symbol(self) -> bool {
        !self.is_background()
    }
}

impl Default for PointerTargetId {
    fn default() -> Self {
        Self::Background
    }
}

impl From<symbol::GlobalInstanceId> for PointerTargetId {
    fn from(id: symbol::GlobalInstanceId) -> Self {
        Self::Symbol { id }
    }
}

impl From<&symbol::GlobalInstanceId> for PointerTargetId {
    fn from(id: &symbol::GlobalInstanceId) -> Self {
        Self::from(*id)
    }
}


// === Errors ===

/// [`PointerTargetId`] decoding error. See the docs of [`PointerTargetId::decode_from_rgba`] to
/// learn more.
#[derive(Copy, Clone, Debug, Fail)]
#[allow(missing_docs)]
pub enum DecodeError {
    WrongAlpha(u32),
    Overflow,
}

impl Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WrongAlpha(alpha) => {
                let err1 = "Failed to decode mouse target.";
                let err2 = "The alpha channel should be either 0 or 255, got";
                write!(f, "{} {} {}.", err1, err2, alpha)
            }
            Self::Overflow => {
                write!(f, "ID overflow error, too many objects on the scene.")
            }
        }
    }
}
