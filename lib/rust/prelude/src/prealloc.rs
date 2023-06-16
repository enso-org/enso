//! Preallocated memory configuration.
//!
//! Some containers can be configured to preallocate memory for their elements. This module defines
//! the available preallocation modes.



// =============
// === Modes ===
// =============

/// The preallocation mode available as a runtime value.
#[derive(Copy, Clone, Debug, Default)]
pub enum Mode {
    #[default]
    Disabled,
    Default,
    Zeroed,
}

/// Do not preallocate the memory.
#[derive(Copy, Clone, Debug, Default)]
pub struct Disabled;

/// Preallocate the memory with values initialized to their defaults.
#[derive(Copy, Clone, Debug, Default)]
pub struct Default;

/// Preallocate zero-initialized memory.
///
/// In this mode, the container items must be [`Zeroable`] and it has to be enforced by the
/// container implementation bounds.
#[derive(Copy, Clone, Debug, Default)]
pub struct Zeroed;


// === Conversions ===

impl From<Disabled> for Mode {
    fn from(_: Disabled) -> Self {
        Self::Disabled
    }
}

impl From<Default> for Mode {
    fn from(_: Default) -> Self {
        Self::Default
    }
}

impl From<Zeroed> for Mode {
    fn from(_: Zeroed) -> Self {
        Self::Zeroed
    }
}
