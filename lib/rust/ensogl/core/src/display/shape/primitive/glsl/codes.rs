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
pub enum DisplayModes {
    Normal,
    DebugSpriteUv,
    DebugSpriteOverview,
    DebugSpriteGrid,
    DebugSdf,
    DebugShapeAaSpan,
    DebugInstanceId,
}

impl DisplayModes {
    /// The numeric representation of the code.
    pub const fn value(&self) -> u32 {
        *self as u32
    }

    /// Conversion from the numeric representation of the code to the code variant.
    pub fn from_value(number: u32) -> Option<Self> {
        Self::try_from(number).ok()
    }

    /// Name of the code.
    pub fn name(&self) -> String {
        format!("display_mode_{:?}", self).to_snake_case()
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
