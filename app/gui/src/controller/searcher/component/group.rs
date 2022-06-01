//! A module containing definition of [`component::Group`], the list of those and related
//! structures.

use crate::prelude::*;

use crate::controller::searcher::component;
use crate::controller::searcher::component::Component;
use crate::model::suggestion_database;



// ============
// === Data ===
// ============

/// The [`Group`] fields, which are shared and available by [`AsRef`] and [`Deref`].
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Data {
    pub name:         ImString,
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
        Data { name: name.into(), component_id, entries: default(), visible: Cell::new(true) }
    }
}



// =============
// === Group ===
// =============

/// A group of [`Component`]s.
#[derive(Clone, CloneRef, Debug, AsRef, Deref)]
pub struct Group {
    data: Rc<Data>,
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

    /// Check if group should be displayed in Component Browser.
    pub fn check_visibility(&self) {
        let entries = self.entries.borrow();
        let filtered_out = entries.iter().filter(|c| c.is_filtered_out());
        let visible = filtered_out.count() < entries.len();
        self.visible.set(visible);
    }
}



// ============
// === List ===
// ============

/// An immutable [`Group`] list, keeping the groups in alphabetical order.
#[derive(Clone, CloneRef, Debug, Default, AsRef, Deref)]
pub struct List {
    groups: Rc<Vec<Group>>,
}


// === ListBuilder ===


/// The builder of [`List`]. The groups will be sorted in [`Self::build`] method.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, AsRef, Deref, AsMut, DerefMut)]
pub struct ListBuilder {
    pub groups: Vec<Group>,
}

impl ListBuilder {
    /// Sort the groups and create a [`List`].
    pub fn build(mut self) -> List {
        // The `sort_unstable_by_key` method is not suitable here, because the closure it takes
        // cannot return reference, and we don't want to copy strings here.
        self.groups.sort_unstable_by(|a, b| a.name.cmp(&b.name));
        List { groups: Rc::new(self.groups) }
    }
}
