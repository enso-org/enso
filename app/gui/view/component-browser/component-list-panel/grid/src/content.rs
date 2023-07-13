//! A module with useful structures related to the content of
//! [grid inside the Component List Panel](crate::View).

use crate::prelude::*;

use ensogl_core::data::color;



// ===========================
// === Content Identifiers ===
// ===========================

/// A Group identifier.
pub type GroupId = usize;
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
#[derive(Clone, Debug, Default)]
pub struct Info {
    pub entry_count:               usize,
    /// List of groups to be arranged in columns. Does not contain Local Scope Group.
    pub groups:                    Vec<Group>,
    pub is_filtered:               bool,
    /// `true` if this is a particular module's content (in opposite to default view displaying
    /// many modules and Virtual Groups).
    pub displaying_module_content: bool,
}
