use crate::prelude::*;

use ensogl_core::data::color;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;

/// A Component Groups List Section identifier.
#[derive(Copy, Clone, Debug, Default, Eq, Hash, IntoPrimitive, PartialEq, TryFromPrimitive)]
#[repr(usize)]
pub enum SectionId {
    /// The "Popular Tools" section.
    #[default]
    Popular    = 1,
    /// The "Local Scope" section.
    LocalScope = 2,
    /// The "Sub-Modules" section.
    SubModules = 0,
}

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

pub type EntryInGroup = usize;

/// An identifier of element inside a concrete group: entry (by entry index) or header.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash)]
pub enum ElementInGroup {
    /// A group's header.
    #[default]
    Header,
    /// A group's normal entry with index.
    Entry(EntryInGroup),
}

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

    pub fn header_group(self) -> Option<GroupId> {
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

/// An identifier of Component Entry in Component List.
///
/// The component is identified by its group id and its number on the component list.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct GroupEntryId {
    pub group: GroupId,
    pub entry: EntryInGroup,
}


/// A information about group needed to compute the Component Panel List layout.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Group {
    /// The group identifier.
    pub id:              GroupId,
    /// Height of group in rows, not counting the header nor filtered-out entries.
    pub height:          usize,
    /// Height of group in rows if no entry is filtered out, not counting the header.
    pub original_height: usize,
    pub color:           Option<color::Rgb>,
}


#[derive(Clone, Debug, Default)]
pub struct Info {
    pub groups:           Vec<Group>,
    pub local_scope_size: usize,
}
