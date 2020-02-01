//! Module containing utilities for specifying layout.

pub mod alignment;

/// Commonly used types.
pub mod types {
    use super::*;
    pub use alignment::Alignment;
    pub use alignment::HorizontalAlignment;
    pub use alignment::VerticalAlignment;
}
