use crate::prelude::*;

use super::layout::Corner;
use super::layout::EdgeSplit;
use super::layout::Oriented;
use super::layout::TargetAttachment;

use ensogl::data::color;



// =========================
// === State information ===
// =========================

/// The complete computed state of an edge, containing all information needed to render it.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct State {
    /// The layout.
    pub layout:      Layout,
    /// The color scheme.
    pub colors:      Colors,
    /// Whether the edge is attached to nodes at both ends.
    pub is_attached: IsAttached,
    /// What part, if any, is focused.
    pub focus_split: FocusSplit,
}

/// An edge's layout.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct Layout {
    /// The corners composing the main part of the edge.
    pub corners:           Vec<Oriented<Corner>>,
    /// The center of the backward-edge arrow.
    pub arrow:             Option<Vector2>,
    /// The target-attachment end.
    pub target_attachment: Option<TargetAttachment>,
    /// The size of the source node.
    pub source_size:       Vector2,
}

/// An edge's color scheme.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct Colors {
    /// The color of the part of the edge nearer the source.
    pub source_color: color::Rgba,
    /// The color of the part of the edge nearer the target.
    pub target_color: color::Rgba,
}

/// Whether an edge is attached at both ends.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct IsAttached {
    /// Whether the edge is attached at both ends.
    pub is_attached: bool,
}

/// Information about how an edge may be divided into two parts that would be colored differently.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct FocusSplit {
    /// What part of the edge is focused.
    pub focus_split: Option<EdgeSplit>,
}



// =====================
// === State changes ===
// =====================

/// References to all the parts of a [`State`], along with information about whether the values have
/// changed.
#[derive(Debug, Copy, Clone)]
pub(super) struct StateUpdate<'a, 'b, 'c, 'd> {
    pub layout:      Update<&'a Layout>,
    pub colors:      Update<&'b Colors>,
    pub is_attached: Update<&'c IsAttached>,
    pub focus_split: Update<&'d FocusSplit>,
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
            layout:      compare!(layout),
            colors:      compare!(colors),
            is_attached: compare!(is_attached),
            focus_split: compare!(focus_split),
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

/// Return the product of the inputs.
#[allow(unused)]
pub(super) fn any<'a, 'b, A, B>(a: Update<&'a A>, b: Update<&'b B>) -> Update<(&'a A, &'b B)> {
    let value = (a.value, b.value);
    let changed = a.changed | b.changed;
    Update { value, changed }
}

/// Return the product of the inputs.
#[allow(unused)]
pub(super) fn any3<'a, 'b, 'c, A, B, C>(
    a: Update<&'a A>,
    b: Update<&'b B>,
    c: Update<&'c C>,
) -> Update<(&'a A, &'b B, &'c C)> {
    let value = (a.value, b.value, c.value);
    let changed = a.changed | b.changed | c.changed;
    Update { value, changed }
}

/// Return the product of the inputs.
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
