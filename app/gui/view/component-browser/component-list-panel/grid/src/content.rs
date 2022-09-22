use crate::prelude::*;
use ensogl_core::data::color;

/// A Component Groups List Section identifier.
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub enum SectionId {
    /// The "Popular Tools" section.
    #[default]
    Popular,
    /// The "Local Scope" section.
    LocalScope,
    /// The "Sub-Modules" section.
    SubModules,
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
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub enum ElementInGroup {
    /// A group's header.
    #[default]
    Header,
    /// A group's normal entry with index.
    Entry(EntryInGroup),
}

/// An identifier of some group's element in Component Browser.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct ElementId {
    pub group:   GroupId,
    pub element: ElementInGroup,
}

/// An identifier of Component Entry in Component List.
///
/// The component is identified by its group id and its number on the component list.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct GroupEntryId {
    pub group:    GroupId,
    pub entry_id: EntryInGroup,
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
    pub color:           color::Rgba,
}


#[derive(Clone, Debug, Default)]
pub struct Info {
    groups:           Vec<Group>,
    local_scope_size: usize,
}
