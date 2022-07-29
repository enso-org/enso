//! A module containing definition of [`Component`] and its [`List`]
//!
//! Component is a language entity displayed in the Component Browser.

use crate::prelude::*;

use crate::model::suggestion_database;

use convert_case::Case;
use convert_case::Casing;


// ==============
// === Export ===
// ==============

pub mod builder;
pub mod group;

pub use group::Group;



// ====================
// === Type Aliases ===
// ====================

/// A component identifier.
pub type Id = suggestion_database::entry::Id;
/// Information how the component matches the filtering pattern.
pub type MatchInfo = controller::searcher::action::MatchInfo;



// =============
// === Order ===
// =============

/// Defines supported sorting orders for [`Component`]s. Used by
/// [`Group::update_sorting_and_visibility`].
#[derive(Copy, Clone, Debug)]
pub enum Order {
    /// The same order of components as when the group was built.
    /// Will use the [`Group::initial_entries_order`] field.
    Initial,
    /// Order non-modules by name, followed by modules (also by name).
    ByNameNonModulesThenModules,
    /// Order [`Component`]s by [`Component::match_info`] score. The matching entries will go
    /// first, and the _lesser_ score will take precedence. That is due to way of displaying
    /// components in component browser - the lower (with greater indices) entries are more
    /// handy.
    ByMatch,
}



// =================
// === Component ===
// =================

/// A structure describing a language entity which may be picked in the Component Browser panel to
/// create a new node or change existing one.
///
/// The components are usually stored in [`List`], which may be filtered; the single component keeps
/// then information how it matches the current filtering pattern.
///
/// The component corresponds to some Suggestion Database Entry, and the entry will be used to
/// properly insert code into the program.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Component {
    pub id:         Immutable<Id>,
    pub suggestion: Rc<suggestion_database::Entry>,
    pub match_info: Rc<RefCell<MatchInfo>>,
}

impl Component {
    /// Construct a new component.
    ///
    /// The matching info will be filled for an empty pattern.
    pub fn new(id: Id, suggestion: Rc<suggestion_database::Entry>) -> Self {
        Self { id: Immutable(id), suggestion, match_info: default() }
    }

    /// The label which should be displayed in the Component Browser.
    pub fn label(&self) -> String {
        self.to_string()
    }

    /// Checks if component is filtered out.
    pub fn is_filtered_out(&self) -> bool {
        matches!(*self.match_info.borrow(), MatchInfo::DoesNotMatch)
    }

    /// Checks if the component can be entered in Component Browser.
    ///
    /// Currently, only modules can be entered, and then the Browser should display content and
    /// submodules of the entered module.
    pub fn can_be_entered(&self) -> bool {
        self.suggestion.kind == suggestion_database::entry::Kind::Module
    }

    /// Update matching info.
    ///
    /// It should be called each time the filtering pattern changes.
    pub fn update_matching_info(&self, pattern: impl Str) {
        let label = self.label();
        let matches = fuzzly::matches(&label, pattern.as_ref());
        let subsequence = matches.and_option_from(|| {
            let metric = fuzzly::metric::default();
            fuzzly::find_best_subsequence(label, pattern, metric)
        });
        *self.match_info.borrow_mut() = match subsequence {
            Some(subsequence) => MatchInfo::Matches { subsequence },
            None => MatchInfo::DoesNotMatch,
        };
    }
}

impl Display for Component {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let self_type_not_here =
            self.suggestion.self_type.as_ref().filter(|t| *t != &self.suggestion.module);
        if let Some(self_type) = self_type_not_here {
            let self_name = self_type.name.from_case(Case::Snake).to_case(Case::Title);
            let name = self.suggestion.name.from_case(Case::Snake).to_case(Case::Lower);
            write!(f, "{} {}", self_name, name)
        } else {
            write!(f, "{}", self.suggestion.name.from_case(Case::Snake).to_case(Case::Lower))
        }
    }
}



// ============
// === List ===
// ============

// === ModuleGroups ===

/// The Component groups associated with a module: the group containing the module's content, and
/// groups with direct submodules' content.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct ModuleGroups {
    pub content:    Group,
    pub submodules: group::AlphabeticalList,
}


// === List ===

/// The Component List.
///
/// The List is used to provide information to Component Browser Panel View: thus the components
/// are arranged in appropriate groups, and the groups are ordered accordingly. You may check the
/// [design doc](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#overview)
/// for information how the Component Browser works.
///
/// The components and their structure are immutable, only the filtering may change in created List.
/// If there is need to change/extend the component list, a new one should be created using
/// [`builder::List`].
#[derive(Clone, CloneRef, Debug, Default)]
pub struct List {
    all_components:        Rc<Vec<Component>>,
    top_modules:           group::AlphabeticalList,
    top_modules_flattened: group::AlphabeticalList,
    module_groups:         Rc<HashMap<Id, ModuleGroups>>,
    filtered:              Rc<Cell<bool>>,
    /// Components to display in the "Local Scope" section of the [Component
    /// Browser](crate::controller::Searcher).
    pub local_scope:       Group,
    /// Groups of components to display in the "Favorites Data Science Tools" section of the
    /// [Component Browser](crate::controller::Searcher).
    pub favorites:         group::List,
}

impl List {
    /// Return the list of top modules, which should be displayed in Component Browser.
    ///
    /// If the list is filtered, all top modules will be flattened.
    pub fn top_modules(&self) -> &group::AlphabeticalList {
        if self.filtered.get() {
            &self.top_modules_flattened
        } else {
            &self.top_modules
        }
    }

    /// Get the list of given component submodules. Returns [`None`] if given component is not
    /// a module.
    pub fn submodules_of(&self, component: Id) -> Option<&group::AlphabeticalList> {
        self.module_groups.get(&component).map(|mg| &mg.submodules)
    }

    /// Get the content of given module component. Returns [`None`] if given component is not a
    /// module.
    pub fn get_module_content(&self, component: Id) -> Option<&Group> {
        self.module_groups.get(&component).map(|mg| &mg.content)
    }

    /// Update matching info in all components according to the new filtering pattern.
    pub fn update_filtering(&self, pattern: impl AsRef<str>) {
        let pattern = pattern.as_ref();
        for component in &*self.all_components {
            component.update_matching_info(pattern)
        }
        let pattern_not_empty = !pattern.is_empty();
        let submodules_order =
            if pattern_not_empty { Order::ByMatch } else { Order::ByNameNonModulesThenModules };
        let favorites_order = if pattern_not_empty { Order::ByMatch } else { Order::Initial };
        for group in self.all_groups_not_in_favorites() {
            group.update_sorting(submodules_order);
        }
        for group in self.favorites.iter() {
            group.update_sorting(favorites_order);
        }
        self.filtered.set(pattern_not_empty);
    }

    /// All groups from [`List`] without the groups found in [`List::favorites`].
    fn all_groups_not_in_favorites(&self) -> impl Iterator<Item = &Group> {
        let normal = self.module_groups.values().map(|mg| &mg.content);
        let flattened = self.top_modules_flattened.iter();
        normal.chain(flattened).chain(std::iter::once(&self.local_scope))
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    use crate::model::suggestion_database::entry::Kind;

    use double_representation::module;
    use engine_protocol::language_server;


    // === Helpers ===

    pub fn mock_module(name: &str) -> model::suggestion_database::Entry {
        let ls_entry = language_server::SuggestionEntry::Module {
            module:                 name.to_owned(),
            documentation:          default(),
            documentation_html:     default(),
            documentation_sections: default(),
            reexport:               default(),
        };
        model::suggestion_database::Entry::from_ls_entry(ls_entry).unwrap()
    }

    pub fn mock_function(
        module: &module::QualifiedName,
        name: &str,
    ) -> model::suggestion_database::Entry {
        model::suggestion_database::Entry {
            kind:               Kind::Function,
            module:             module.clone(),
            name:               name.to_owned(),
            arguments:          vec![],
            return_type:        "Standard.Builtin.Integer".to_string(),
            documentation_html: None,
            self_type:          None,
            scope:              model::suggestion_database::entry::Scope::Everywhere,
            icon_name:          None,
        }
    }

    pub fn mock_suggestion_db(logger: impl AnyLogger) -> model::SuggestionDatabase {
        let top_module_1 = mock_module("test.Test.TopModule1");
        let top_module_2 = mock_module("test.Test.TopModule2");
        let sub_module_1 = mock_module("test.Test.TopModule1.SubModule1");
        let sub_module_2 = mock_module("test.Test.TopModule1.SubModule2");
        let sub_module_3 = mock_module("test.Test.TopModule1.SubModule2.SubModule3");
        let fun1 = mock_function(&top_module_1.module, "fun1");
        let fun2 = mock_function(&top_module_1.module, "fun2");
        let fun3 = mock_function(&top_module_2.module, "fun3");
        let fun4 = mock_function(&sub_module_1.module, "fun4");
        let fun5 = mock_function(&sub_module_2.module, "fun5");
        let fun6 = mock_function(&sub_module_3.module, "fun6");
        let all_entries = [
            top_module_1,
            top_module_2,
            sub_module_1,
            sub_module_2,
            sub_module_3,
            fun1,
            fun2,
            fun3,
            fun4,
            fun5,
            fun6,
        ];

        let suggestion_db = model::SuggestionDatabase::new_empty(logger);
        for (id, entry) in all_entries.into_iter().enumerate() {
            suggestion_db.put_entry(id, entry)
        }
        suggestion_db
    }

    fn mock_favorites(
        db: &model::SuggestionDatabase,
        component_ids: &[Id],
    ) -> Vec<crate::model::execution_context::ComponentGroup> {
        let db_entries = component_ids.iter().map(|id| db.lookup(*id).unwrap());
        let group = crate::model::execution_context::ComponentGroup {
            name:       "Test Group 1".into(),
            color:      None,
            components: db_entries.into_iter().map(|e| e.qualified_name()).collect(),
        };
        vec![group]
    }


    // === Filtering Component List ===

    /// Assert IDs and order of all entries in the group which have their [`Component::match_info`]
    /// set to [`MatchInfo::Matches`]. Additionally, verify the [`Group::visible`] field is
    /// [`true`] iff no IDs are expected.
    fn assert_ids_of_matches_entries(group: &Group, expected_ids: &[Id]) {
        let ids_of_matches = group
            .entries
            .borrow()
            .iter()
            .take_while(|c| matches!(*c.match_info.borrow(), MatchInfo::Matches { .. }))
            .map(|c| *c.id)
            .collect_vec();
        assert_eq!(ids_of_matches, expected_ids);
    }

    #[test]
    fn filtering_component_list() {
        let logger = Logger::new("test::update_list_after_filtering_pattern_change");
        let top_module = mock_module("test.Test.TopModule");
        let sub_module = mock_module("test.Test.TopModule.SubModule");
        let fun1 = mock_function(&top_module.module, "fun1");
        let funx2 = mock_function(&sub_module.module, "funx1");
        let all_entries = [&top_module, &sub_module, &fun1, &funx2];
        let suggestion_db = model::SuggestionDatabase::new_empty(logger);
        for (id, entry) in all_entries.into_iter().enumerate() {
            suggestion_db.put_entry(id, entry.clone())
        }
        let favorites = mock_favorites(&suggestion_db, &[3, 2]);
        let mut builder = builder::List::new().with_local_scope_module_id(0);
        builder.set_grouping_and_order_of_favorites(&suggestion_db, &favorites);
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, 0..4);
        let list = builder.build();

        list.update_filtering("fu");
        let match_infos = list.top_modules()[0]
            .entries
            .borrow()
            .iter()
            .map(|c| c.match_info.borrow().clone())
            .collect_vec();
        DEBUG!("{match_infos:?}");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[2, 3]);
        assert_ids_of_matches_entries(&list.favorites[0], &[3, 2]);
        assert_ids_of_matches_entries(&list.local_scope, &[2]);

        list.update_filtering("x");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[3]);
        assert_ids_of_matches_entries(&list.favorites[0], &[3]);
        assert_ids_of_matches_entries(&list.local_scope, &[]);

        list.update_filtering("Sub");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[1]);
        assert_ids_of_matches_entries(&list.favorites[0], &[]);
        assert_ids_of_matches_entries(&list.local_scope, &[]);

        list.update_filtering("y");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[]);
        assert_ids_of_matches_entries(&list.favorites[0], &[]);
        assert_ids_of_matches_entries(&list.local_scope, &[]);

        list.update_filtering("");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[2, 1]);
        assert_ids_of_matches_entries(&list.favorites[0], &[3, 2]);
        assert_ids_of_matches_entries(&list.local_scope, &[2]);
    }


    // === Component List modules tree ===

    #[test]
    fn component_list_modules_tree() {
        // Create a components list with sample data.
        let logger = Logger::new("test::component_list_modules_tree");
        let suggestion_db = mock_suggestion_db(logger);
        let mut builder = builder::List::new().with_local_scope_module_id(0);
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, 0..11);
        let list = builder.build();

        // Verify that we can read all top-level modules from the component list.
        let expected_top_modules_ids = vec![Some(0), Some(1)];
        let top_modules_ids = list.top_modules().iter().map(|m| m.component_id).collect_vec();
        assert_eq!(top_modules_ids, expected_top_modules_ids);

        // Verify that we can read content and direct submodules of a second-level submodule
        // ("test.Test.TopModule1.SubModule2").
        let content = list.get_module_content(3).unwrap();
        let expected_content_ids = vec![9, 4];
        let content_ids = content.entries.borrow().iter().map(|entry| *entry.id).collect_vec();
        assert_eq!(content_ids, expected_content_ids);
        let direct_submodules = list.submodules_of(3).unwrap();
        let expected_direct_submodules_ids = vec![Some(4)];
        let direct_submodules_ids = direct_submodules.iter().map(|m| m.component_id).collect_vec();
        assert_eq!(direct_submodules_ids, expected_direct_submodules_ids);

        // Verify that we can read content of a third-level submodule
        // ("test.Test.TopModule1.SubModule1.SubSubModule").
        let content = list.get_module_content(4).unwrap();
        let expected_content_ids = vec![10];
        let content_ids = content.entries.borrow().iter().map(|entry| *entry.id).collect_vec();
        assert_eq!(content_ids, expected_content_ids);
    }
}
