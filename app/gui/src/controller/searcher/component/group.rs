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
#[derive(Clone, Debug, Default)]
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
    fn from_name_and_id(name: impl Into<ImString>, component_id: Option<component::Id>) -> Self {
        Data {
            name: name.into(),
            color: None,
            component_id,
            entries: default(),
            visible: default(),
        }
    }
}



// =============
// === Group ===
// =============

/// A group of [`Component`]s.
#[derive(Clone, CloneRef, Debug, Default)]
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
    /// Create a named empty group referring to module with specified component ID.
    pub fn from_name_and_id(
        name: impl Into<ImString>,
        component_id: Option<component::Id>,
    ) -> Self {
        Self { data: Rc::new(Data::from_name_and_id(name, component_id)) }
    }

    /// Create empty group referring to some module component.
    pub fn from_entry(component_id: component::Id, entry: &suggestion_database::Entry) -> Self {
        let name: String = if entry.module.is_top_module() {
            (&entry.module).into()
        } else {
            entry.module.name().into()
        };
        Self::from_name_and_id(name, Some(component_id))
    }

    /// Construct from [`execution_context::ComponentGroup`] components looked up in the suggestion
    /// database by their full qualified name. Returns a group containing only the successfully
    /// looked up components, or [`None`] if none of the components were found in the suggestion
    /// database.
    pub fn from_execution_context_component_group(
        group: &execution_context::ComponentGroup,
        suggestion_db: &model::SuggestionDatabase,
    ) -> Option<Self> {
        let lookup_component = |qualified_name| {
            let (id, suggestion) = suggestion_db.lookup_by_qualified_name(qualified_name)?;
            Some(Component::new(id, suggestion))
        };
        let components = &group.components;
        let looked_up_components = components.iter().filter_map(lookup_component).collect_vec();
        let any_components_found_in_db = !looked_up_components.is_empty();
        any_components_found_in_db.then(|| {
            let group_data = Data {
                name:         group.name.clone(),
                color:        group.color,
                component_id: None,
                visible:      Cell::new(true),
                entries:      RefCell::new(looked_up_components),
            };
            Group { data: Rc::new(group_data) }
        })
    }

    /// Update the group sorting according to the `order` and call [`update_visibility`].
    pub fn update_sorting_and_visibility(&self, order: component::Order) {
        // The `sort_by_key` method is not suitable here, because the closure it takes
        // cannot return reference nor [`Ref`], and we don't want to copy anything here.
        self.entries.borrow_mut().sort_by(|a, b| order.compare(a, b));
        self.update_visibility();
    }

    /// Sets the [`visible`] flag to [`true`] if at least one of the group's entries is not
    /// filtered out. Sets the flag to [`false`] otherwise.
    pub fn update_visibility(&self) {
        let visible = !self.entries.borrow().iter().all(|c| c.is_filtered_out());
        self.visible.set(visible);
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

impl FromIterator<Group> for List {
    fn from_iter<T: IntoIterator<Item = Group>>(iter: T) -> Self {
        Self::new(iter.into_iter().collect())
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



// ========================
// === AlphabeticalList ===
// ========================

/// An immutable [`Group`] list, keeping the groups in alphabetical order.
#[derive(Clone, CloneRef, Debug, Default, AsRef, Deref)]
pub struct AlphabeticalList {
    groups: List,
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
        AlphabeticalList { groups: List::new(self.groups) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::controller::searcher::component::tests::mock_suggestion_db;
    use std::assert_matches::assert_matches;

    /// Test whether [`Group::from_execution_context_component_group`] correctly looks up
    /// components in the suggestion database.
    #[test]
    fn lookup_component_groups_in_suggestion_database() {
        let logger = Logger::new("tests::lookup_component_groups_in_suggestion_database");
        let suggestion_db = Rc::new(mock_suggestion_db(logger));

        // Prepare a mock group containing fully qualified component names in non-alphabetical
        // order. Some of the names correspond to entries present in the suggestion database,
        // some do not.
        let ec_group = execution_context::ComponentGroup {
            name:       "Test Group 1".into(),
            color:      color::Rgb::from_css_hex("#aabbcc"),
            components: vec![
                "test.Test.TopModule1.fun2".into(),
                "test.Test.TopModule1.SubModule2.SubModule3.fun6".into(),
                "test.Test.NonExistantModule.fun6".into(),
                "test.Test.TopModule1.fun1".into(),
                "test.Test.TopModule1.nonexistantfun".into(),
            ],
        };

        // Construct a components group with entries looked up in the suggestion database.
        let group = Group::from_execution_context_component_group(&ec_group, &suggestion_db);

        // Verify the contents of the components group.
        let group = group.unwrap();
        assert_eq!(group.name, ImString::new("Test Group 1"));
        let color = group.color.unwrap();
        assert_eq!((color.red * 255.0) as u8, 0xaa);
        assert_eq!((color.green * 255.0) as u8, 0xbb);
        assert_eq!((color.blue * 255.0) as u8, 0xcc);
        let entry_ids_and_names = group
            .entries
            .borrow()
            .iter()
            .map(|e| (*e.id, e.suggestion.name.to_string()))
            .collect_vec();
        let expected_ids_and_names =
            vec![(6, "fun2".to_string()), (10, "fun6".to_string()), (5, "fun1".to_string())];
        assert_eq!(entry_ids_and_names, expected_ids_and_names);
    }

    // Test constructing a component group from an [`execution_context::ComponentGroup`] containing
    // only names not found in the suggestion database.
    #[test]
    fn constructing_component_group_from_names_not_found_in_db() {
        let logger = Logger::new("tests::constructing_component_group_from_names_not_found_in_db");
        let suggestion_db = Rc::new(mock_suggestion_db(logger));
        let ec_group = execution_context::ComponentGroup {
            name:       "Input".into(),
            color:      None,
            components: vec!["NAME.NOT.FOUND.IN.DB".into()],
        };
        let group = Group::from_execution_context_component_group(&ec_group, &suggestion_db);
        assert_matches!(group, None);
    }
}
