//! A module with entities used for building proper [`component::List`].
//!
//! The [`List`] type builds a [`component::List`] with contents sorted as described below:
//!  - [`component::Group`]s are sorted alphabetically by name;
//!  - [`Component`]s in each [`component::Group`] are ordered: non-modules sorted alphabetically,
//!    followed by modules sorted alphabetically;
//!  - [`Component`]s and [`component::Group`]s in [`component::List::favorites`] keep the grouping
//!    and order set with [`List::set_grouping_and_order_of_favorites`].
//!
//! When using the methods of the [`List`] type to build a [`component::List`]:
//!  - The components and groups are sorted once.
//!  - The [`component::List::favorites`] will contain:
//!    - components with IDs that were passed both to [`List::set_grouping_and_order_of_favorites`]
//!      and to [`List::extend_list_and_allow_favorites_with_ids`],
//!    - virtual components inserted with [`List::insert_virtual_components_in_favorites_group`].
//!  - Empty component groups are allowed in favorites. (This simplifies distributing groups of
//!    favorites over columns in [Component Browser](crate::controller::Searcher) consistently.
//!    That's because for the same input to [`List::set_grouping_and_order_of_favorites`], the same
//!    number of groups will be always present in favorites.)

use crate::prelude::*;

use crate::controller::searcher::component;
use crate::controller::searcher::component::Component;
use crate::model::execution_context;
use crate::model::suggestion_database;

use double_representation::name::project;
use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use double_representation::name::QualifiedNameTemplate;



// =================
// === Constants ===
// =================

/// The `local` namespace, within which modules local to the project are located.
const LOCAL_NAMESPACE: &str = "local";



// ====================
// === ModuleGroups ===
// ====================

/// Module Groups Builder.
///
/// The builder allow extending the groups with module content, and add new submodules.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct ModuleGroups {
    pub qualified_name:    QualifiedName,
    pub content:           component::Group,
    /// The flattened content contains the content of a module and all its submodules. Is set to
    /// `Some` only when such flattened content is needed (decided during construction).
    ///
    /// For example when the module is a top module, so need its flattened content to fill the
    /// `top_module_flattened` field of [`component::List`].
    pub flattened_content: Option<component::Group>,
    pub submodules:        component::group::AlphabeticalListBuilder,
    pub is_top_module:     bool,
}

impl ModuleGroups {
    /// Construct the builder without content nor submodules.
    ///
    /// The existence of flattened content is decided during construction.
    ///
    /// Returns [`FallibleResult::Err`] if entry's qualified name can't be converted into a module
    /// name.
    pub fn new(
        component_id: component::Id,
        entry: &suggestion_database::Entry,
    ) -> FallibleResult<Self> {
        let is_top_module = entry.kind == suggestion_database::entry::Kind::Module
            && entry.defined_in.is_top_element();
        let qualified_name = entry.qualified_name();
        let mk_group = || component::Group::from_entry(component_id, entry);
        Ok(Self {
            qualified_name,
            content: mk_group(),
            flattened_content: is_top_module.as_some_from(mk_group),
            submodules: default(),
            is_top_module,
        })
    }

    /// Build [`component::ModuleGroups`] structure with appropriately sorted submodules.
    pub fn build(self) -> component::ModuleGroups {
        component::ModuleGroups {
            qualified_name: Rc::new(self.qualified_name),
            content:        self.content,
            submodules:     self.submodules.build(),
        }
    }
}



// ================
// === Sections ===
// ================

/// Construct a component section from a list of module groups.
fn new_section(name: ImString, modules: &[&ModuleGroups]) -> component::Section {
    let modules_iter = modules.iter();
    let mut module_builder = component::group::AlphabeticalListBuilder::default();
    module_builder.extend(modules_iter.clone().map(|g| g.content.clone_ref()));
    let modules = module_builder.build();
    let mut flattened_module_builder = component::group::AlphabeticalListBuilder::default();
    flattened_module_builder.extend(modules_iter.filter_map(|g| g.flattened_content.clone()));
    let modules_flattened = flattened_module_builder.build();
    component::Section { modules, modules_flattened, name }
}

/// The sections builder collects module groups by namespace in alphabetical order. It builds a
/// list of component sections.
struct Sections<'a> {
    modules: BTreeMap<ImString, Vec<&'a ModuleGroups>>,
}

impl<'a> Sections<'a> {
    /// Collect module groups by namespace in alphabetical order.
    fn new(groups: Vec<&'a ModuleGroups>) -> Self {
        let mut modules: BTreeMap<_, Vec<&ModuleGroups>> = BTreeMap::new();
        for group in groups {
            let namespace = group.qualified_name.project().namespace.clone();
            modules.entry(namespace).or_insert_with(default).push(group);
        }
        Sections { modules }
    }

    /// Build a list of component sections from the collected module groups.
    fn build(&self) -> Vec<component::Section> {
        self.modules.iter().map(|(name, modules)| new_section(name.clone(), modules)).collect()
    }

    /// Get a map from section names to section indices.
    fn section_indices(&self) -> HashMap<ImString, usize> {
        self.modules.keys().enumerate().map(|(pos, name)| (name.clone(), pos)).collect()
    }
}



// ============
// === List ===
// ============

/// A [`component::List`] builder. See the documentation of the module for more details.
#[derive(Clone, Debug, Default)]
pub struct List {
    all_components:                  Vec<Component>,
    module_groups:                   HashMap<component::Id, ModuleGroups>,
    local_scope:                     component::Group,
    grouping_and_order_of_favorites: component::group::List,
    /// IDs of [`Component`]s allowed in [`component::List::favorites`] if they are also present in
    /// [`grouping_and_order_of_favorites`].
    allowed_favorites:               HashSet<component::Id>,
    keep_private_components:         bool,
}

impl List {
    /// Construct List builder without content.
    pub fn new() -> Self {
        default()
    }

    /// Construct List builder without content, do not remove private components when extending
    /// list.
    pub fn new_with_private_components() -> Self {
        List { keep_private_components: true, ..default() }
    }

    /// Return [`List`] with a new [`local_scope`] with its [`Group::component_id`] field set to
    /// `module_id`. When the [`extend_list_and_allow_favorites_with_ids`] method is called on the
    /// returned object, components passed to the method which have their parent module ID equal
    /// to `module_id` will be cloned into [`component::List::local_scope`].
    pub fn with_local_scope_module_id(self, module_id: component::Id) -> Self {
        use crate::controller::searcher::component::Group;
        const LOCAL_SCOPE_GROUP_NAME: &str = "Local Scope";
        let id = Some(module_id);
        let local_scope = Group::from_name_and_project_and_id(LOCAL_SCOPE_GROUP_NAME, None, id);
        Self { local_scope, ..self }
    }

    /// Extend the list with new entries looked up by ID in suggestion database. Allow those
    /// entries to be present in [`component::List::favorites`]. See the module documentation for
    /// more details.
    pub fn extend_list_and_allow_favorites_with_ids(
        &mut self,
        db: &model::SuggestionDatabase,
        entry_ids: impl IntoIterator<Item = component::Id>,
    ) {
        use suggestion_database::entry::Kind;
        let local_scope_id = self.local_scope.component_id;
        let id_and_looked_up_entry = |id| Some((id, db.lookup(id).ok()?));
        let ids_and_entries = entry_ids.into_iter().filter_map(id_and_looked_up_entry);
        let keep_private_components = self.keep_private_components;
        for (id, entry) in ids_and_entries {
            self.allowed_favorites.insert(id);
            let component = Component::new_from_database_entry(id, entry.clone_ref());
            let mut component_inserted_somewhere = false;
            if let Some(parent_module) = entry.parent_module() {
                if let Some(parent_group) = self.lookup_module_group(db, parent_module.clone_ref())
                {
                    let parent_id = parent_group.content.component_id;
                    let in_local_scope = parent_id == local_scope_id && local_scope_id.is_some();
                    let namespace = &parent_group.qualified_name.project().namespace;
                    let in_local_namespace = namespace == LOCAL_NAMESPACE;
                    let keep_private_component =
                        in_local_scope || in_local_namespace || keep_private_components;
                    if !component.is_private() || keep_private_component {
                        parent_group.content.entries.borrow_mut().push(component.clone_ref());
                        component_inserted_somewhere = true;
                        let not_module = entry.kind != Kind::Module;
                        if in_local_scope && not_module {
                            self.local_scope.entries.borrow_mut().push(component.clone_ref());
                        }
                    }
                }
                if let Some(top_group) = self.lookup_module_group(db, top_module(&parent_module)) {
                    if let Some(flatten_group) = &mut top_group.flattened_content {
                        let project = flatten_group.project.as_ref();
                        let in_local_namespace =
                            project.map(|name| name.namespace == LOCAL_NAMESPACE).unwrap_or(false);
                        let keep_private_component = in_local_namespace || keep_private_components;
                        if !component.is_private() || keep_private_component {
                            flatten_group.entries.borrow_mut().push(component.clone_ref());
                            component_inserted_somewhere = true;
                        }
                    }
                }
            }
            if component_inserted_somewhere {
                self.all_components.push(component);
            }
        }
    }

    /// Set the grouping and order of [`Components`] in [`component::List::favorites`]. Skips
    /// components not present in the suggestion database and skips empty groups.
    pub fn set_grouping_and_order_of_favorites<'a>(
        &mut self,
        db: &model::SuggestionDatabase,
        component_groups: impl IntoIterator<Item = &'a execution_context::ComponentGroup>,
    ) {
        self.grouping_and_order_of_favorites = component_groups
            .into_iter()
            .filter_map(|g| component::Group::from_execution_context_component_group(g, db))
            .collect();
    }

    fn take_grouping_and_order_of_favorites_as_vec(&mut self) -> Vec<component::Group> {
        std::mem::take(&mut self.grouping_and_order_of_favorites).into_iter().collect_vec()
    }

    /// Insert virtual components at the beginning of a favorites group with given name defined in
    /// given project. If a group with that name and project does not exist, it is created. The
    /// virtual components are created from the given snippets.
    pub fn insert_virtual_components_in_favorites_group(
        &mut self,
        group_name: &str,
        project: project::QualifiedName,
        snippets: impl IntoIterator<Item = Rc<component::hardcoded::Snippet>>,
    ) {
        use component::Group;
        let mut favorites_grouping = self.take_grouping_and_order_of_favorites_as_vec();
        let name_and_project_match =
            |g: &&mut Group| g.name == group_name && g.project.as_ref() == Some(&project);
        let group_with_matching_name = favorites_grouping.iter_mut().find(name_and_project_match);
        if let Some(group) = group_with_matching_name {
            group.insert_entries(&snippets.into_iter().map(Into::into).collect_vec());
        } else {
            let group = Group::from_name_and_project_and_snippets(group_name, project, snippets);
            favorites_grouping.insert(0, group);
        }
        self.grouping_and_order_of_favorites = component::group::List::new(favorites_grouping);
    }

    fn lookup_module_group(
        &mut self,
        db: &model::SuggestionDatabase,
        module: QualifiedNameRef,
    ) -> Option<&mut ModuleGroups> {
        let (module_id, db_entry) = db.lookup_by_qualified_name(&module).ok()?;

        // Note: My heart is bleeding at this point, but because of lifetime checker limitations
        // we must do it in this suboptimal way.
        //
        // See https://users.rust-lang.org/t/returning-a-mutable-reference-extends-its-lifetime/28643
        // for example of similar problem.
        if self.module_groups.contains_key(&module_id) {
            self.module_groups.get_mut(&module_id)
        } else {
            let groups = ModuleGroups::new(module_id, &db_entry).ok()?;
            if let Some(module) = module.parent() {
                if let Some(parent_groups) = self.lookup_module_group(db, module) {
                    parent_groups.submodules.push(groups.content.clone_ref())
                }
            }
            Some(self.module_groups.entry(module_id).or_insert(groups))
        }
    }

    /// Build the list as described in the module's documentation.
    pub fn build(mut self) -> component::List {
        let components_order = component::Order::ByNameNonModulesThenModules;
        for group in self.module_groups.values() {
            group.content.update_match_info_and_sorting(components_order);
            if let Some(flattened) = &group.flattened_content {
                flattened.update_match_info_and_sorting(components_order);
            }
        }
        self.local_scope.update_match_info_and_sorting(components_order);
        let favorites = self.build_favorites_and_add_to_all_components();
        let top_module_groups = self.module_groups.values().filter(|g| g.is_top_module).collect();
        let section_list_builder = Sections::new(top_module_groups);

        component::List {
            all_components: Rc::new(self.all_components),
            top_module_sections: Rc::new(section_list_builder.build()),
            top_module_section_indices: Rc::new(section_list_builder.section_indices()),
            module_groups: Rc::new(
                self.module_groups.into_iter().map(|(id, group)| (id, group.build())).collect(),
            ),
            local_scope: self.local_scope,
            filtered: default(),
            favorites,
        }
    }

    fn build_favorites_and_add_to_all_components(&mut self) -> component::group::List {
        let mut favorites_groups = self.take_grouping_and_order_of_favorites_as_vec();
        for group in favorites_groups.iter_mut() {
            group.retain_entries(|e| match e.data {
                component::Data::FromDatabase { id, .. } => self.allowed_favorites.contains(&id),
                component::Data::Virtual { .. } => true,
            });
            self.all_components.extend(group.entries.borrow().iter().cloned());
        }
        component::group::List::new(favorites_groups)
    }
}

fn top_module<Segments: AsRef<[ImString]>>(
    name: &QualifiedNameTemplate<Segments>,
) -> QualifiedNameRef {
    let top_module_end = name.path().len().min(1);
    name.sub_path(0..top_module_end)
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::controller::searcher::component::tests::mock_suggestion_db;

    use double_representation::name::project;


    #[derive(Clone, Debug, Eq, PartialEq)]
    struct ComparableGroupData<'a> {
        name:         &'a str,
        component_id: Option<component::Id>,
        entries:      Vec<component::Id>,
    }

    impl<'a> From<&'a component::Group> for ComparableGroupData<'a> {
        fn from(component: &'a component::Group) -> Self {
            Self {
                name:         component.name.as_str(),
                component_id: component.component_id,
                entries:      component.entries.borrow().iter().map(|e| e.id().unwrap()).collect(),
            }
        }
    }

    #[test]
    fn building_component_list() {
        let suggestion_db = mock_suggestion_db();
        let mut builder = List::new().with_local_scope_module_id(1);
        let first_part = (0..3).chain(6..12);
        let second_part = 3..6;
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, first_part);
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, second_part);
        let list = builder.build();

        let top_modules: Vec<ComparableGroupData> = list
            .top_module_sections
            .iter()
            .flat_map(|section| section.modules.iter())
            .map(Into::into)
            .collect();
        let expected = vec![
            ComparableGroupData {
                name:         "Test.TopModule1",
                component_id: Some(1),
                entries:      vec![2, 9, 3, 5],
            },
            ComparableGroupData {
                name:         "Test.TopModule2",
                component_id: Some(10),
                entries:      vec![11],
            },
        ];
        assert_eq!(top_modules, expected);

        let flattened_top_modules: Vec<ComparableGroupData> = list
            .top_module_sections
            .iter()
            .flat_map(|section| section.modules_flattened.iter())
            .map(Into::into)
            .collect();
        let expected = vec![
            ComparableGroupData {
                name:         "Test.TopModule1",
                component_id: Some(1),
                entries:      vec![2, 9, 4, 6, 8, 3, 5, 7],
            },
            ComparableGroupData {
                name:         "Test.TopModule2",
                component_id: Some(10),
                entries:      vec![11],
            },
        ];
        assert_eq!(flattened_top_modules, expected);

        let module_groups: BTreeMap<component::Id, ComparableGroupData> = list
            .module_groups
            .iter()
            .map(|(id, m_groups)| (*id, (&m_groups.content).into()))
            .collect();
        let expected: BTreeMap<component::Id, ComparableGroupData> = [
            (0, ComparableGroupData {
                name:         "Test.Main",
                component_id: Some(0),
                entries:      vec![1, 10],
            }),
            (1, ComparableGroupData {
                name:         "Test.TopModule1",
                component_id: Some(1),
                entries:      vec![2, 9, 3, 5],
            }),
            (3, ComparableGroupData {
                name:         "SubModule1",
                component_id: Some(3),
                entries:      vec![4],
            }),
            (5, ComparableGroupData {
                name:         "SubModule2",
                component_id: Some(5),
                entries:      vec![6, 7],
            }),
            (7, ComparableGroupData {
                name:         "SubModule3",
                component_id: Some(7),
                entries:      vec![8],
            }),
            (10, ComparableGroupData {
                name:         "Test.TopModule2",
                component_id: Some(10),
                entries:      vec![11],
            }),
        ]
        .into_iter()
        .collect();
        assert_eq!(module_groups, expected);

        let module_subgroups: BTreeMap<component::Id, Vec<&str>> = list
            .module_groups
            .iter()
            .map(|(id, m_group)| {
                (*id, m_group.submodules.iter().map(|sg| sg.name.as_str()).collect())
            })
            .collect();
        let expected: BTreeMap<component::Id, Vec<&str>> = [
            (0, vec!["Test.TopModule1", "Test.TopModule2"]),
            (1, vec!["SubModule1", "SubModule2"]),
            (3, vec![]),
            (5, vec!["SubModule3"]),
            (7, vec![]),
            (10, vec![]),
        ]
        .into_iter()
        .collect();
        assert_eq!(module_subgroups, expected);

        let local_scope_entries = &list.local_scope.entries;
        let component_id = |c: &Component| c.id().unwrap();
        let local_scope_ids = local_scope_entries.borrow().iter().map(component_id).collect_vec();
        let expected_ids = vec![2, 9];
        assert_eq!(local_scope_ids, expected_ids);
    }

    /// Test building a component list with non-empty favorites. Verify that the favorites are
    /// processed as described in the docs of the [`List::build`] method.
    #[test]
    fn building_component_list_with_favorites() {
        let db = mock_suggestion_db();
        let mut builder = List::new();
        let qn_of_db_entry_0 = db.lookup(0).unwrap().qualified_name();
        let qn_of_db_entry_1 = db.lookup(1).unwrap().qualified_name();
        let qn_of_db_entry_3 = db.lookup(3).unwrap().qualified_name();
        let qn_not_in_db = QualifiedName::from_text("test.Test.NameNotInSuggestionDb").unwrap();
        assert!(db.lookup_by_qualified_name(&qn_not_in_db).is_err());
        let groups = [
            execution_context::ComponentGroup {
                project:    project::QualifiedName::standard_base_library(),
                name:       "Group 1".into(),
                color:      None,
                components: vec![
                    qn_of_db_entry_0.clone(),
                    qn_of_db_entry_3.clone(),
                    qn_not_in_db.clone(),
                    qn_of_db_entry_3.clone(),
                    qn_not_in_db.clone(),
                    qn_of_db_entry_1,
                    qn_of_db_entry_0,
                ],
            },
            execution_context::ComponentGroup {
                project:    project::QualifiedName::standard_base_library(),
                name:       "Group 2".into(),
                color:      None,
                components: vec![
                    qn_of_db_entry_3.clone(),
                    qn_not_in_db.clone(),
                    qn_of_db_entry_3,
                    qn_not_in_db,
                ],
            },
        ];
        builder.set_grouping_and_order_of_favorites(&db, &groups);
        builder.extend_list_and_allow_favorites_with_ids(&db, [0, 1, 2].into_iter());
        let list = builder.build();
        let favorites: Vec<ComparableGroupData> = list.favorites.iter().map(Into::into).collect();
        let expected = vec![
            ComparableGroupData {
                name:         "Group 1",
                component_id: None,
                entries:      vec![0, 1, 0],
            },
            ComparableGroupData {
                name:         "Group 2",
                component_id: None,
                entries:      vec![],
            },
        ];
        assert_eq!(favorites, expected);
    }

    fn check_names_and_order_of_group_entries(group: &component::Group, expected_names: &[&str]) {
        let entries = group.entries.borrow();
        let entry_names = entries.iter().map(|c| c.name()).collect_vec();
        assert_eq!(&entry_names, expected_names);
    }

    /// Test building a component list with a virtual component. The virtual component will be
    /// inserted into an existing favorites group.
    #[test]
    fn building_component_list_with_virtual_component_in_existing_favorites_group() {
        let db = mock_suggestion_db();
        let mut builder = List::new();
        let qn_of_db_entry_1 = db.lookup(1).unwrap().qualified_name();
        let project = project::QualifiedName::standard_base_library();
        const GROUP_NAME: &str = "Group";
        let groups = [execution_context::ComponentGroup {
            project:    project.clone(),
            name:       GROUP_NAME.into(),
            color:      None,
            components: vec![qn_of_db_entry_1.clone()],
        }];
        builder.set_grouping_and_order_of_favorites(&db, &groups);
        let snippet = component::hardcoded::Snippet { name: "test snippet".into(), ..default() };
        let snippet_iter = std::iter::once(Rc::new(snippet));
        builder.insert_virtual_components_in_favorites_group(GROUP_NAME, project, snippet_iter);
        builder.extend_list_and_allow_favorites_with_ids(&db, std::iter::once(1));
        let list = builder.build();
        let favorites = list.favorites;
        assert_eq!(favorites.len(), 1, "Expected one group of favorites, got: {favorites:?}.");
        let expected_entry_names = ["test snippet", qn_of_db_entry_1.name()];
        check_names_and_order_of_group_entries(&favorites[0], &expected_entry_names);
    }

    /// Test building a component list with a virtual component. The virtual component will be
    /// inserted into a new favorites group.
    #[test]
    fn building_component_list_with_virtual_component_in_new_favorites_group() {
        let db = mock_suggestion_db();
        let mut builder = List::new();
        let qn_of_db_entry_1 = db.lookup(1).unwrap().qualified_name();
        let project = project::QualifiedName::standard_base_library();
        const GROUP_1_NAME: &str = "Group 1";
        let groups = [execution_context::ComponentGroup {
            project:    project.clone(),
            name:       GROUP_1_NAME.into(),
            color:      None,
            components: vec![qn_of_db_entry_1.clone()],
        }];
        builder.set_grouping_and_order_of_favorites(&db, &groups);
        let snippet = component::hardcoded::Snippet { name: "test snippet".into(), ..default() };
        let snippet_iter = std::iter::once(Rc::new(snippet));
        const GROUP_2_NAME: &str = "Group 2";
        builder.insert_virtual_components_in_favorites_group(GROUP_2_NAME, project, snippet_iter);
        builder.extend_list_and_allow_favorites_with_ids(&db, std::iter::once(1));
        let list = builder.build();
        let favorites = list.favorites;
        assert_eq!(favorites.len(), 2, "Expected two groups of favorites, got: {favorites:?}.");
        let group_at_0 = &favorites[0];
        assert_eq!(group_at_0.name, "Group 2");
        check_names_and_order_of_group_entries(group_at_0, &["test snippet"]);
        let group_at_1 = &favorites[1];
        assert_eq!(group_at_1.name, "Group 1");
        check_names_and_order_of_group_entries(group_at_1, &[qn_of_db_entry_1.name()]);
    }

    /// Test building a component list with a private component. The private component will be
    /// excluded from the list.
    #[test]
    fn building_component_list_with_private_component() {
        let private_doc_section = enso_suggestion_database::doc_section!(@ Private, "");
        let suggestion_db = enso_suggestion_database::mock_suggestion_database! {
            test.Test {
                mod LocalModule {
                    fn local_fun1() -> Standard.Base.Any;
                    #[with_doc_section(private_doc_section.clone())]
                    fn local_private_fun2() -> Standard.Base.Any;
                }
                mod TopModule {
                    fn fun3() -> Standard.Base.Any;
                    #[with_doc_section(private_doc_section.clone())]
                    fn private_fun4() -> Standard.Base.Any;
                }
            }
        };
        let mut builder = List::new().with_local_scope_module_id(1);
        // ID's for `test.Test`, `TopModule` and 3 function ID's in `suggestion_db`.
        let entry_ids = 0..9;
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, entry_ids);
        let list = builder.build();
        let component_names: Vec<_> = list
            .all_components
            .iter()
            .filter_map(|component| match &component.data {
                component::Data::FromDatabase { entry, .. } => Some(&entry.name),
                _ => None,
            })
            .collect();
        let expected = vec!["LocalModule", "local_fun1", "local_private_fun2", "TopModule", "fun3"];
        assert_eq!(component_names, expected);
    }
}
