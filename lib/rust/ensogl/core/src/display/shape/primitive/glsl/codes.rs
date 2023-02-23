//! GLSL codes, numbers which are used to identify errors and display modes. These are shared
//! between Rust and GLSL code.

use crate::prelude::*;

use inflector::Inflector;



// =============
// === Codes ===
// =============

/// The error code used for ID encoding errors.
pub const ID_ENCODING_OVERFLOW_ERROR: u32 = 100;

/// Enum describing possible GLSL display modes.
#[derive(Clone, Copy, Debug, num_enum::IntoPrimitive, num_enum::TryFromPrimitive)]
#[repr(u32)]
#[allow(missing_docs)]
pub enum DisplayModes {
    Normal,
    DebugSpriteUv,
    DebugSpriteOverview,
    DebugSpriteGrid,
    DebugSdf,
    DebugShapeAaSpan,
    DebugInstanceId,
    CachedShapesTexture,
}

// This is not derived, as then [`num_enum::TryFromPrimitive`] will return the default value for
// not matched numbers.
impl Default for DisplayModes {
    fn default() -> Self {
        Self::Normal
    }
}

impl DisplayModes {
    /// The numeric representation of the code.
    pub const fn value(self) -> u32 {
        self as u32
    }

    /// Conversion from the numeric representation of the code to the code variant.
    pub fn from_value(number: u32) -> Option<Self> {
        Self::try_from(number).ok()
    }

    /// Name of the code.
    pub fn name(self) -> String {
        format!("display_mode_{self:?}").to_snake_case()
    }

    /// Check if the display mode allows mouse interactions in GUI. The display modes that do not
    /// allow it use mouse for debug purposes (like changing object hue on click).
    pub fn allow_mouse_events(self) -> bool {
        match self {
            DisplayModes::Normal => true,
            DisplayModes::CachedShapesTexture => false,
            DisplayModes::DebugSpriteUv => true,
            DisplayModes::DebugSpriteOverview => false,
            DisplayModes::DebugSpriteGrid => false,
            DisplayModes::DebugSdf => true,
            DisplayModes::DebugShapeAaSpan => true,
            DisplayModes::DebugInstanceId => false,
        }
    }

    /// All registered codes.
    pub fn all() -> Vec<Self> {
        let mut codes = vec![];
        for i in 0.. {
            if let Some(code) = Self::from_value(i) {
                codes.push(code);
            } else {
                break;
            }
        }
        codes
    }
}
