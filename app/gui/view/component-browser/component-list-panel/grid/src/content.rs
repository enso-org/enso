//! A module with useful structures related to the content of
//! [grid inside the Component List Panel](crate::View).

use crate::prelude::*;

use ensogl_core::data::color;



// ===========================
// === Content Identifiers ===
// ===========================

// === SectionId ===

/// A Component Groups List Section identifier.
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
#[repr(usize)]
pub enum SectionId {
    /// The "Popular Tools" section.
    #[default]
    Popular,
    /// The "Local Scope" section.
    LocalScope,
    /// A namespace section defined by its position in the list of namespaces.
    Namespace(usize),
}



// === GroupId ===

/// A Group identifier. If `section` is [`SectionId::LocalScope`], the `index` should be 0, as that
/// section has always only one group.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct GroupId {
    pub section: SectionId,
    pub index:   usize,
}

impl GroupId {
    /// Get id of the only group in "Local Scope" section.
    pub fn local_scope_group() -> Self {
        GroupId { section: SectionId::LocalScope, index: default() }
    }
}


// === EntryInGroup ===

/// An index of entry in some specific group.
pub type EntryInGroup = usize;


// === ElementInGroup ===

/// An identifier of element inside a concrete group: entry (by entry index) or header.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash)]
pub enum ElementInGroup {
    /// A group's header.
    #[default]
    Header,
    /// A group's normal entry with index.
    Entry(EntryInGroup),
}


// === ElementId ===

/// An identifier of some group's element in Component Browser.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct ElementId {
    pub group:   GroupId,
    pub element: ElementInGroup,
}

impl ElementId {
    /// Convert to GroupEntryId if the element is an entry
    pub fn as_entry_id(self) -> Option<GroupEntryId> {
        let Self { group, element } = self;
        match element {
            ElementInGroup::Entry(entry) => Some(GroupEntryId { group, entry }),
            _ => None,
        }
    }

    /// Convert to GroupId if the element is a header.
    pub fn as_header(self) -> Option<GroupId> {
        matches!(self.element, ElementInGroup::Header).as_some(self.group)
    }
}

impl From<GroupEntryId> for ElementId {
    fn from(GroupEntryId { group, entry }: GroupEntryId) -> Self {
        Self { group, element: ElementInGroup::Entry(entry) }
    }
}

impl From<GroupId> for ElementId {
    fn from(group: GroupId) -> Self {
        Self { group, element: ElementInGroup::Header }
    }
}


// === GroupEntryId ===

/// An identifier of Component Entry in Component List.
///
/// The component is identified by its group id and its number on the component list.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct GroupEntryId {
    pub group: GroupId,
    pub entry: EntryInGroup,
}



// ============
// === Info ===
// ============


// === Group ===

/// An information about group being laid in one of the Component List Panel columns.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Group {
    /// The group identifier.
    pub id:              GroupId,
    /// Height of group in rows, not counting the header as it will be visible in Component List
    /// Panel. If the entries are not filtered, should be equal to [`original_height`]. Otherwise
    /// the filtered out entries will not be counted.
    pub height:          usize,
    /// Height in rows the groups would have if no filtering is applied. Used to layout groups
    /// in columns.
    pub original_height: usize,
    /// The group color defined by library's author.
    pub color:           Option<color::Rgb>,
}


// === Info ===

/// A information about Grid content allowing to compute groups' layout.
#[derive(Clone, Debug, Default)]
pub struct Info {
    /// List of groups to be arranged in columns. Does not contain Local Scope Group.
    pub groups:                  Vec<Group>,
    /// A number of entries in Local Scope section.
    pub local_scope_entry_count: usize,
    /// The number of namespace sections that are available.
    pub namespace_section_count: usize,
}
