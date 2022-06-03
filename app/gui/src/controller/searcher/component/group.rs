//! A module containing definition of [`component::Group`], the list of those and related
//! structures.

use crate::prelude::*;

use crate::controller::searcher::component;
use crate::controller::searcher::component::Component;
use crate::model::execution_context;
use crate::model::suggestion_database;

use ensogl::data::color;



// ============
// === Data ===
// ============

/// The [`Group`] fields, which are shared and available by [`AsRef`] and [`Deref`].
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Data {
    pub name:         ImString,
    pub color:        Option<color::Rgb>,
    /// A component corresponding to this group, e.g. the module of whose content the group
    /// contains.
    pub component_id: Option<component::Id>,
    pub entries:      RefCell<Vec<Component>>,
    /// A flag indicating that the group should be displayed in the Component Browser. It may be
    /// hidden in some scenarios, e.g. when all items are filtered out.
    pub visible:      Cell<bool>,
}

impl Data {
    fn new_empty_visible(name: impl Into<ImString>, component_id: Option<component::Id>) -> Self {
        Data {
            name: name.into(),
            color: None,
            component_id,
            entries: default(),
            visible: Cell::new(true),
        }
    }
}



// =============
// === Group ===
// =============

/// A group of [`Component`]s.
#[derive(Clone, CloneRef, Debug)]
pub struct Group {
    data: Rc<Data>,
}

impl Deref for Group {
    type Target = Data;
    fn deref(&self) -> &Self::Target {
        &*self.data
    }
}

impl Group {
    /// Create empty group referring to some module component.
    pub fn from_entry(component_id: component::Id, entry: &suggestion_database::Entry) -> Self {
        let name: String = if entry.module.is_top_module() {
            (&entry.module).into()
        } else {
            entry.module.name().into()
        };
        Self { data: Rc::new(Data::new_empty_visible(name, Some(component_id))) }
    }

    /// Construct from [`execution_context::ComponentGroup`]. The group components are looked up in
    /// the suggestion database by their full qualified name and skipped if not found. Returns
    /// [`None`] if none of the components were found in the suggestion database.
    /// Returns a [`Group`] with
    /// [`Component`]s found in the suggestion database 
    /// Only the [`Component`]s for which 
    /// Looks up the qualified names of the
    /// group components in the suggestion database to retrieve full [`Component`] data skipping . Components
    /// without 
    pub fn from_execution_context_component_group(
        group: execution_context::ComponentGroup,
        suggestion_db: &model::SuggestionDatabase,
    ) -> Self {
        let components = group.components.iter().filter_map(|qualified_name| {
            let (id, suggestion) = suggestion_db.lookup_by_qualified_name(qualified_name)?;
            Some(Component { id: Immutable(id), suggestion, match_info: default() })
        });
        let entries = RefCell::new(components.collect());
        let group_data = Data {
            name: group.name,
            color: group.color,
            component_id: None,
            visible: Cell::new(true),
            entries,
        };
        Group { data: Rc::new(group_data) }
    }

    /// Update the group sorting according to the current filtering pattern.
    pub fn update_sorting(&self, pattern: impl AsRef<str>) {
        let mut entries = self.entries.borrow_mut();
        // The `sort_by_key` method is not suitable here, because the closure it takes
        // cannot return reference nor [`Ref`], and we don't want to copy anything here.
        if pattern.as_ref().is_empty() {
            entries.sort_by(|a, b| {
                a.can_be_entered().cmp(&b.can_be_entered()).then_with(|| a.label().cmp(b.label()))
            });
        } else {
            entries.sort_by(|a, b| a.match_info.borrow().cmp(&*b.match_info.borrow()).reverse());
        }
        let visible = !entries.iter().all(|c| c.is_filtered_out());
        self.visible.set(visible);
    }
}



// ========================
// === AlphabeticalList ===
// ========================

/// An immutable [`Group`] list, keeping the groups in alphabetical order.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct AlphabeticalList {
    groups: Rc<Vec<Group>>,
}

impl Deref for AlphabeticalList {
    type Target = [Group];
    fn deref(&self) -> &Self::Target {
        self.groups.as_slice()
    }
}

impl AsRef<[Group]> for AlphabeticalList {
    fn as_ref(&self) -> &[Group] {
        self.groups.as_slice()
    }
}


// === AlphabeticalListBuilder ===


/// The builder of [`AlphabeticalList`]. The groups will be sorted in [`Self::build`] method.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, AsRef, Deref, AsMut, DerefMut)]
pub struct AlphabeticalListBuilder {
    pub groups: Vec<Group>,
}

impl AlphabeticalListBuilder {
    /// Sort the groups and create an [`AlphabeticalList`].
    pub fn build(mut self) -> AlphabeticalList {
        // The `sort_unstable_by_key` method is not suitable here, because the closure it takes
        // cannot return reference, and we don't want to copy strings here.
        self.groups.sort_unstable_by(|a, b| a.name.cmp(&b.name));
        AlphabeticalList { groups: Rc::new(self.groups) }
    }
}



// ============
// === List ===
// ============

/// An immutable [`Group`] list, keeping the groups in the order provided in the constructor.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct List {
    groups: Rc<Vec<Group>>,
}

impl List {
    /// Constructor.
    pub fn new(groups: Vec<Group>) -> Self {
        Self { groups: Rc::new(groups) }
    }
}

impl Deref for List {
    type Target = [Group];
    fn deref(&self) -> &Self::Target {
        self.groups.as_slice()
    }
}

impl AsRef<[Group]> for List {
    fn as_ref(&self) -> &[Group] {
        self.groups.as_slice()
    }
}
