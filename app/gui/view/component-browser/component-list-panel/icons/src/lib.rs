// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use prelude::*;


// ==============
// === Export ===
// ==============

pub use ensogl_core::display::shape::compound::from_cache::recolorized as any;



mod prelude {
    pub use ensogl_core::application::traits::*;
    pub use ensogl_core::display::shape::*;
    pub use ensogl_core::prelude::*;
}

pub mod common_part;
pub mod component_icons;
mod define_macro;



// =================
// === Constants ===
// =================

/// The width and height of all icons.
pub const SIZE: f32 = 16.0;

/// This constant exists for development purposes only, and is published for debug scene.
/// Due to a rendering error, shapes appear too big when the camera is zoomed in very closely.
/// (Documented here: https://github.com/enso-org/ide/issues/1698)
/// In the user interface, this is not a big problem, since icon are usually shown at lower zoom
/// levels. But it is a problem during development of icon when it becomes necessary to inspect
/// them closely. In those situations, one can apply `.shrink(0.35)` to shapes to compensate for the
/// bug and make them appear at the correct size while the camera is zoomed in. But that work-around
/// will make them appear too thin on the default zoom level.
///
/// To make it easy to turn this shrinking on and off before and after working on icon, we define
/// the constant `SHRINK_AMOUNT` and apply `.shrink(SHRINK_AMOUNT.px())` to all icon. In every
/// commit, `SHRINK_AMOUNT` should be set to 0.0 to make icon look best in the user interface. But
/// during work on the icon, it can temporarily be set to 0.35.
pub const SHRINK_AMOUNT: f32 = 0.0;



// ==============
// === Errors ===
// ==============

/// Error occuring when we try parse string being an invalid icon name to icon Id.
#[derive(Clone, Debug, Fail)]
#[fail(display = "Unknown icon '{}'.", name)]
pub struct UnknownIcon {
    /// The copied icon name from parsed string.
    pub name: String,
}
