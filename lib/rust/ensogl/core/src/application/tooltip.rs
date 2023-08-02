//! This module contains the `Style` a tooltip, which is a structure that can be updates partially
//! and holds information about how to render a tooltip and its content.
//!
//! See the [define_style] for more information.

use enso_prelude::*;



// =================
// === Placement ===
// =================

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
/// Indicates the placement of the tooltip relative to the base position location.
pub enum Placement {
    Top,
    Bottom,
    Left,
    Right,
}

impl Default for Placement {
    fn default() -> Self {
        Placement::Top
    }
}



// =============
// === Style ===
// =============

use crate::define_style;
use crate::gui::style::StyleValue;
use enso_prelude::default;

define_style! {
    text      : Option<String>,
    placement : Placement
}

impl Style {
    /// Create a `TooltipUpdate` that sets the label of the tooltip.
    pub fn set_label(text: impl Str) -> Self {
        let text = Some(StyleValue::new(Some(text.into())));
        Self { text, ..default() }
    }
    /// Create a `TooltipUpdate` that unsets the label of the tooltip.
    pub fn unset_label() -> Self {
        let text = Some(StyleValue::new(None));
        Self { text, ..default() }
    }

    /// Indicate whether the `Style` has content to display.
    pub fn has_content(&self) -> bool {
        self.content().is_some()
    }

    /// Create a `TooltipUpdate` that sets the placement of the tooltip.
    pub fn set_placement(placement: Placement) -> Self {
        let placement = Some(StyleValue::new(placement));
        Self { placement, ..default() }
    }

    /// Sets the placement of the tooltip.
    pub fn with_placement(mut self, placement: Placement) -> Self {
        self.placement = Some(StyleValue::new(placement));
        self
    }

    /// Label content of the tooltip.
    pub fn content(&self) -> Option<&str> {
        if let Some(style_value) = self.text.as_ref() {
            if let Some(inner) = style_value.value.as_ref() {
                return inner.as_ref().map(|s| s.as_str());
            }
        }
        None
    }

    /// Label placement of the tooltip.
    pub fn placement(&self) -> Option<Placement> {
        self.placement.as_ref().and_then(|style_value| style_value.value)
    }
}
