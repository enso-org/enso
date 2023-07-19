//! A module with useful structures related to the content of
//! [grid inside the Component List Panel](crate::View).

use crate::prelude::*;

use ensogl_core::data::color;



// ===========================
// === Content Identifiers ===
// ===========================

/// A Group identifier.
pub type GroupId = usize;
/// An Entry identifier.
pub type EntryId = usize;



// ============
// === Info ===
// ============


// === Group ===

/// An information about group being laid in one of the Component List Panel columns.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Group {
    /// The group identifier.
    pub id:    GroupId,
    /// The group color defined by library's author.
    pub color: Option<color::Rgb>,
}


// === Info ===

/// A information about Grid content allowing to compute groups' layout.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Info {
    pub entry_count: usize,
    pub groups:      Vec<Group>,
    pub is_filtered: bool,
}
