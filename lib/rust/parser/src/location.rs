use crate::prelude::*;
use crate::Bytes;



// ===================
// === Info & With ===
// ===================

/// A location information. See [`With`] to learn more about the available fields.
pub type Info = With<()>;

/// A location information with an element [`T`]. The struct derefers to the element.
///
/// Please note that the left offset information is stored in two fields, [`left_visible_offset`]
/// and [`left_offset`]. The first one stores the offset visible on the screen in a "spaces" metric.
/// For example, for the tab char, the visible offset will be counted as 4 spaces. The latter can
/// differ depending on which space character is used. See the following link to learn more:
/// https://en.wikipedia.org/wiki/Whitespace_character.
#[derive(Clone, Copy, Deref, PartialEq, Eq)]
pub struct With<T> {
    #[deref]
    pub elem:                T,
    pub left_visible_offset: usize,
    pub left_offset:         Bytes,
    pub start:               Bytes,
    pub len:                 Bytes,
}
