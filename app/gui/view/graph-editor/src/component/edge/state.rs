use crate::prelude::*;
use ensogl::data::color;

use super::layout::Corner;
use super::layout::EdgeSplit;
use super::layout::Oriented;



// =========================
// === State information ===
// =========================

/// The complete computed state of an edge, containing all information needed to render it.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct State {
    /// The layout.
    pub layout:            Layout,
    /// The color scheme.
    pub colors:            Colors,
    /// Whether hover events are enabled.
    pub is_hoverable:      IsHoverable,
    /// What part, if any, is focused.
    pub focus_split:       FocusSplit,
    /// Whether the target end is attached.
    pub target_attachment: TargetAttachment,
}

/// An edge's layout.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct Layout {
    /// Offset from the edge's parent of the target node's edge attachment point.
    pub target_offset:   Vector2,
    /// Points where the corners composing the edge meet. Computed from the target offset.
    pub junction_points: Vec<Vector2>,
    /// The corners composing the edge. Computed from the junction points.
    pub corners:         Vec<Oriented<Corner>>,
}

/// An edge's color scheme.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct Colors {
    /// The color of the part of the edge nearer the source.
    pub source_color: color::Rgba,
    /// The color of the part of the edge nearer the target.
    pub target_color: color::Rgba,
}

/// Whether an edge responds to hover events.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct IsHoverable {
    /// Whether the edge responds to hover events.
    pub is_hoverable: bool,
}

/// Information about how an edge may be divided into two parts that would be colored differently.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct FocusSplit {
    /// What part of the edge is focused.
    pub focus_split: Option<EdgeSplit>,
}

/// Whether an edge is attached at the target end.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct TargetAttachment {
    /// Whether the target end is attached to a node's input port.
    pub target_attached: bool,
}



// =====================
// === State changes ===
// =====================

/// References to all the parts of a [`State`], along with information about whether the values have
/// changed.
#[derive(Debug, Copy, Clone)]
pub(super) struct StateUpdate<'a, 'b, 'c, 'd, 'e> {
    pub layout:            Update<&'a Layout>,
    pub colors:            Update<&'b Colors>,
    pub is_hoverable:      Update<&'c IsHoverable>,
    pub focus_split:       Update<&'d FocusSplit>,
    pub target_attachment: Update<&'e TargetAttachment>,
}

/// A value, along with information about whether it has changed.
#[derive(Debug, Copy, Clone)]
pub(super) struct Update<T> {
    value:   T,
    changed: bool,
}

impl State {
    pub(super) fn compare(&self, other: &Option<Self>) -> StateUpdate {
        macro_rules! compare {
            ($field:ident) => {
                Update {
                    value:   &self.$field,
                    changed: other.as_ref().map_or(true, |value| value.$field != self.$field),
                }
            };
        }
        StateUpdate {
            layout:            compare!(layout),
            colors:            compare!(colors),
            is_hoverable:      compare!(is_hoverable),
            focus_split:       compare!(focus_split),
            target_attachment: compare!(target_attachment),
        }
    }
}

impl<T> Update<T> {
    /// Apply the given function to the value if the value has changed; otherwise, return `None`.
    pub(super) fn changed<U>(self, f: impl FnOnce(T) -> U) -> Option<U> {
        match self.changed {
            true => Some(f(self.value)),
            false => None,
        }
    }
}

/// Return the cross product of the inputs.
pub(super) fn any<'a, 'b, A, B>(a: Update<&'a A>, b: Update<&'b B>) -> Update<(&'a A, &'b B)> {
    let value = (a.value, b.value);
    let changed = a.changed | b.changed;
    Update { value, changed }
}

/// Return the cross product of the inputs.
pub(super) fn any3<'a, 'b, 'c, A, B, C>(
    a: Update<&'a A>,
    b: Update<&'b B>,
    c: Update<&'c C>,
) -> Update<(&'a A, &'b B, &'c C)> {
    let value = (a.value, b.value, c.value);
    let changed = a.changed | b.changed | c.changed;
    Update { value, changed }
}

/// Return the cross product of the inputs.
#[allow(unused)]
pub(super) fn any4<'a, 'b, 'c, 'd, A, B, C, D>(
    a: Update<&'a A>,
    b: Update<&'b B>,
    c: Update<&'c C>,
    d: Update<&'d D>,
) -> Update<(&'a A, &'b B, &'c C, &'d D)> {
    let value = (a.value, b.value, c.value, d.value);
    let changed = a.changed | b.changed | c.changed | d.changed;
    Update { value, changed }
}
