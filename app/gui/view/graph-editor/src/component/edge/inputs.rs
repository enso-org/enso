use super::coords::*;
use crate::prelude::*;

use ensogl::data::color;



// ==============
// === Inputs ===
// ==============

/// The inputs to the edge state computation. These values are all set orthogonally, so that the
/// order of events that set different properties doesn't affect the outcome.
#[derive(Debug, Default)]
pub(super) struct Inputs {
    /// The width and height of the node that originates the edge. The edge may begin anywhere
    /// around the bottom half of the node.
    pub source_size:     Cell<Vector2>,
    /// The coordinates of the node input the edge connects to. The edge enters the node from
    /// above.
    pub target_position: Cell<ParentCoords>,
    /// Whether the edge is connected to a node input.
    pub target_attached: Cell<bool>,
    /// Whether the edge is connected to a node output.
    pub source_attached: Cell<bool>,
    pub color:           Cell<color::Rgba>,
    /// The location of the mouse over the edge.
    pub hover_position:  Cell<Option<SceneCoords>>,
    pub disabled:        Cell<bool>,
    /// Reset the hover position at next redraw.
    pub clear_focus:     Cell<bool>,
}

impl Inputs {
    /// Set the color of the edge.
    pub(super) fn set_color(&self, color: color::Lcha) {
        // We must never use alpha in edges, as it will show artifacts with overlapping sub-parts.
        let color: color::Lcha = color.opaque.into();
        let color_rgba = color::Rgba::from(color);
        self.color.set(color_rgba);
    }

    pub(super) fn set_source_size(&self, size: Vector2) {
        self.source_size.set(size);
    }

    pub(super) fn set_disabled(&self, disabled: bool) {
        self.disabled.set(disabled);
    }

    pub(super) fn set_target_position(&self, position: ParentCoords) {
        self.target_position.set(position);
    }

    pub(super) fn set_target_attached(&self, attached: bool) {
        self.target_attached.set(attached);
        self.clear_focus.set(true);
    }

    pub(super) fn set_source_attached(&self, attached: bool) {
        self.source_attached.set(attached);
        self.clear_focus.set(true);
    }

    pub(super) fn set_mouse_position(&self, pos: SceneCoords) {
        self.hover_position.set(Some(pos));
    }
}
