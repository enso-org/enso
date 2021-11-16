//! Provides the `Mode` enum to represent the graph editors view mode.

use crate::prelude::*;



// =================
// === View Mode ===
// =================

/// Represents the current view mode of the graph editor. In profiling mode, most colors are removed
/// from the interface and each node displays some profiling information, using color to represent
/// the running time.
#[derive(Debug, Copy, Clone, CloneRef, PartialEq, Eq)]
pub enum Mode {
    /// The standard mode of the graph editor. Edges are colored to reflect type information and no
    /// profiling information is visible.
    Normal,

    /// The graph editor's profiling mode. Color is used sparingly. Every node shows profiling
    /// information and uses color to represent its running time.
    Profiling,
}

impl Default for Mode {
    fn default() -> Self {
        Mode::Normal
    }
}

impl Mode {
    /// Returns true if this is the normal mode.
    pub fn is_normal(self) -> bool {
        matches!(self, Mode::Normal)
    }

    /// Returns true if this is the profiling mode.
    pub fn is_profiling(self) -> bool {
        matches!(self, Mode::Profiling)
    }

    /// Maps `Normal` to `Profiling` and `Profiling` to `Normal`.
    pub fn switch(self) -> Mode {
        match self {
            Mode::Normal => Mode::Profiling,
            Mode::Profiling => Mode::Normal,
        }
    }
}
