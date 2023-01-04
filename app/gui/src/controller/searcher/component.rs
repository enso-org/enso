//! A module containing definition of [`Component`] and its [`List`]
//!
//! Component is a language entity displayed in the Component Browser.

use crate::prelude::*;

use crate::model::suggestion_database;

use convert_case::Case;
use convert_case::Casing;
use double_representation::name::QualifiedName;


// ==============
// === Export ===
// ==============

pub mod builder;
pub mod group;
pub mod hardcoded;

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



// ============
// === Data ===
// ============

/// Contains detailed data of a [`Component`]. The storage of the details differs depending on
/// where the data originates from (either from the [`suggestion_database`] or from a
/// [`hardcoded::Snippet`]).
#[derive(Clone, CloneRef, Debug)]
pub enum Data {
    /// A component from the [`suggestion_database`]. When this component is picked in the
    /// Component Browser, the code returned by [`suggestion_database::Entry::code_to_insert`] will
    /// be inserted into the program.
    FromDatabase {
        /// The ID of the component in the [`suggestion_database`].
        id:    Immutable<Id>,
        /// The component's entry in the [`suggestion_database`].
        entry: Rc<suggestion_database::Entry>,
    },
    /// A virtual component containing a hardcoded snippet of code. When this component is picked
    /// in the Component Browser, the [`Snippet::code`] will be inserted into the program.
    Virtual {
        /// A hardcoded snippet of code.
        snippet: Rc<hardcoded::Snippet>,
    },
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
/// See the documentation of the [`Data`] variants for information on what will happen when the
/// component is picked in the Component Browser panel.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Component {
    pub data:       Data,
    pub match_info: Rc<RefCell<MatchInfo>>,
}

impl Component {
    /// Construct a new component from a [`suggestion_database`] entry.
    ///
    /// The matching info will be filled for an empty pattern.
    pub fn new_from_database_entry(id: Id, entry: Rc<suggestion_database::Entry>) -> Self {
        let data = Data::FromDatabase { id: Immutable(id), entry };
        Self { data, match_info: default() }
    }

    /// The label which should be displayed in the Component Browser.
    pub fn label(&self) -> String {
        self.to_string()
    }

    /// The name of the component.
    pub fn name(&self) -> &str {
        match &self.data {
            Data::FromDatabase { entry, .. } => entry.name.as_str(),
            Data::Virtual { snippet } => snippet.name,
        }
    }

    /// The [`Id`] of the component in the [`suggestion_database`], or `None` if not applicable.
    pub fn id(&self) -> Option<Id> {
        match self.data {
            Data::FromDatabase { id, .. } => Some(*id),
            Data::Virtual { .. } => None,
        }
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
        use suggestion_database::entry::Kind as EntryKind;
        matches!(&self.data, Data::FromDatabase { entry, .. } if entry.kind == EntryKind::Module)
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

impl From<Rc<hardcoded::Snippet>> for Component {
    fn from(snippet: Rc<hardcoded::Snippet>) -> Self {
        Self { data: Data::Virtual { snippet }, match_info: default() }
    }
}

impl Display for Component {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.data {
            Data::FromDatabase { entry, .. } => {
                let entry_name = entry.name.from_case(Case::Snake).to_case(Case::Lower);
                let self_type_ref = entry.self_type.as_ref();
                let self_type_not_here = self_type_ref.filter(|t| *t != &entry.defined_in);
                if let Some(self_type) = self_type_not_here {
                    let self_name = self_type.name().from_case(Case::Snake).to_case(Case::Title);
                    write!(f, "{} {}", self_name, entry_name)
                } else {
                    write!(f, "{}", entry_name)
                }
            }
            Data::Virtual { snippet } => write!(f, "{}", snippet.name),
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
    pub qualified_name: Rc<QualifiedName>,
    pub content:        Group,
    pub submodules:     group::AlphabeticalList,
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
    all_components:             Rc<Vec<Component>>,
    top_modules:                Rc<Vec<group::AlphabeticalList>>,
    top_modules_flattened:      Rc<Vec<group::AlphabeticalList>>,
    section_names:              Rc<Vec<ImString>>,
    module_groups:              Rc<HashMap<Id, ModuleGroups>>,
    filtered:                   Rc<Cell<bool>>,
    /// Components to display in the "Local Scope" section of the [Component
    /// Browser](crate::controller::Searcher).
    pub local_scope:            Group,
    /// Groups of components to display in the "Favorites Data Science Tools" section of the
    /// [Component Browser](crate::controller::Searcher).
    pub favorites:              group::List,
}

impl List {
    /// Return the list of top modules, which should be displayed in Component Browser.
    ///
    /// If the list is filtered, all top modules will be flattened.
    pub fn top_modules(&self) -> &[group::AlphabeticalList] {
        if self.filtered.get() {
            &self.top_modules_flattened
        } else {
            &self.top_modules
        }
    }

    /// Return the list of filtered top modules and their contents.
    pub fn top_modules_flattened(&self) -> &[group::AlphabeticalList] {
        &self.top_modules_flattened
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

    /// Get the qualified name of the module. Returns [`None`] if given component is not a module.
    pub fn module_qualified_name(&self, component: Id) -> Option<Rc<QualifiedName>> {
        self.module_groups.get(&component).map(|mg| mg.qualified_name.clone_ref())
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
        let flattened = self.top_modules_flattened.iter().flat_map(|group| group.iter());
        normal.chain(flattened).chain(std::iter::once(&self.local_scope))
    }

    /// Get a vector of section names for the sections of the top modules.
    pub fn section_names(&self) -> &[ImString] {
        &self.section_names
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    use crate::mock_suggestion_database;
    use double_representation::name::project;

    pub fn mock_suggestion_db() -> model::SuggestionDatabase {
        mock_suggestion_database! {
            test.Test {
                mod TopModule1 {
                    fn fun1() -> Standard.Base.Any;

                    mod SubModule1 {
                        fn fun4() -> Standard.Base.Any;
                    }
                    mod SubModule2 {
                        fn fun5 -> Standard.Base.Any;
                        mod SubModule3 {
                            fn fun6 -> Standard.Base.Any;
                        }
                    }

                    fn fun2() -> Standard.Base.Any;
                }
                mod TopModule2 {
                    fn fun3() -> Standard.Base.Any;
                }
            }
        }
    }

    fn mock_favorites(
        db: &model::SuggestionDatabase,
        component_ids: &[Id],
    ) -> Vec<crate::model::execution_context::ComponentGroup> {
        let db_entries = component_ids.iter().map(|id| db.lookup(*id).unwrap());
        let group = crate::model::execution_context::ComponentGroup {
            project:    project::QualifiedName::standard_base_library(),
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
            .map(|c| c.id().unwrap())
            .collect_vec();
        assert_eq!(ids_of_matches, expected_ids);
    }

    #[test]
    fn filtering_component_list() {
        let suggestion_db = mock_suggestion_database! {
            test.Test {
                mod TopModule {
                    fn fun1() -> Standard.Base.Any;

                    mod SubModule {
                        fn funx2() -> Standard.Base.Any;
                    }
                }
            }
        };
        let favorites = mock_favorites(&suggestion_db, &[4, 2]);
        let mut builder = builder::List::new().with_local_scope_module_id(1);
        builder.set_grouping_and_order_of_favorites(&suggestion_db, &favorites);
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, 0..=4);
        let list = builder.build();

        list.update_filtering("fu");
        let match_infos = list.top_modules()[0]
            .entries
            .borrow()
            .iter()
            .map(|c| c.match_info.borrow().clone())
            .collect_vec();
        DEBUG!("{match_infos:?}");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[2, 4]);
        assert_ids_of_matches_entries(&list.favorites[0], &[4, 2]);
        assert_ids_of_matches_entries(&list.local_scope, &[2]);

        list.update_filtering("x");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[4]);
        assert_ids_of_matches_entries(&list.favorites[0], &[4]);
        assert_ids_of_matches_entries(&list.local_scope, &[]);

        list.update_filtering("Sub");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[3]);
        assert_ids_of_matches_entries(&list.favorites[0], &[]);
        assert_ids_of_matches_entries(&list.local_scope, &[]);

        list.update_filtering("y");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[]);
        assert_ids_of_matches_entries(&list.favorites[0], &[]);
        assert_ids_of_matches_entries(&list.local_scope, &[]);

        list.update_filtering("");
        assert_ids_of_matches_entries(&list.top_modules()[0], &[2, 3]);
        assert_ids_of_matches_entries(&list.favorites[0], &[4, 2]);
        assert_ids_of_matches_entries(&list.local_scope, &[2]);
    }


    // === Component List modules tree ===

    #[test]
    fn component_list_modules_tree() {
        // Create a components list with sample data.
        let suggestion_db = mock_suggestion_db();
        let mut builder = builder::List::new().with_local_scope_module_id(0);
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, 0..=11);
        let list = builder.build();

        // Verify that we can read all top-level modules from the component list.
        let expected_top_modules_ids = vec![Some(1), Some(10)];
        let top_modules_ids = list.top_modules().iter().map(|m| m.component_id).collect_vec();
        assert_eq!(top_modules_ids, expected_top_modules_ids);

        // Verify that we can read content and direct submodules of a second-level submodule
        // ("test.Test.TopModule1.SubModule2").
        let content = list.get_module_content(5).unwrap();
        let expected_content_ids = vec![6, 7];
        let content_ids = content.entries.borrow().iter().map(|e| e.id().unwrap()).collect_vec();
        assert_eq!(content_ids, expected_content_ids);
        let direct_submodules = list.submodules_of(5).unwrap();
        let expected_direct_submodules_ids = vec![Some(7)];
        let direct_submodules_ids = direct_submodules.iter().map(|m| m.component_id).collect_vec();
        assert_eq!(direct_submodules_ids, expected_direct_submodules_ids);

        // Verify that we can read content of a third-level submodule
        // ("test.Test.TopModule1.SubModule1.SubSubModule").
        let content = list.get_module_content(7).unwrap();
        let expected_content_ids = vec![8];
        let content_ids = content.entries.borrow().iter().map(|e| e.id().unwrap()).collect_vec();
        assert_eq!(content_ids, expected_content_ids);
    }
}
