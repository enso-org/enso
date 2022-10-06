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

use double_representation::module;
use double_representation::project;



// ====================
// === ModuleGroups ===
// ====================

/// Module Groups Builder.
///
/// The builder allow extending the groups with module content, and add new submodules.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct ModuleGroups {
    pub qualified_name:    module::QualifiedName,
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
        let is_top_module = entry.module.is_top_module();
        let qualified_name = entry.qualified_name();
        let qualified_name = module::QualifiedName::from_all_segments(qualified_name.into_iter())?;
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
}

impl List {
    /// Construct List builder without content.
    pub fn new() -> Self {
        default()
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
        for (id, entry) in ids_and_entries {
            self.allowed_favorites.insert(id);
            let component = Component::new_from_database_entry(id, entry.clone_ref());
            let mut component_inserted_somewhere = false;
            if let Some(parent_module) = entry.parent_module() {
                if let Some(parent_group) = self.lookup_module_group(db, &parent_module) {
                    parent_group.content.entries.borrow_mut().push(component.clone_ref());
                    component_inserted_somewhere = true;
                    let parent_id = parent_group.content.component_id;
                    let in_local_scope = parent_id == local_scope_id && local_scope_id.is_some();
                    let not_module = entry.kind != Kind::Module;
                    if in_local_scope && not_module {
                        self.local_scope.entries.borrow_mut().push(component.clone_ref());
                    }
                }
                if let Some(top_group) = self.lookup_module_group(db, &parent_module.top_module()) {
                    if let Some(flatten_group) = &mut top_group.flattened_content {
                        flatten_group.entries.borrow_mut().push(component.clone_ref());
                        component_inserted_somewhere = true;
                    }
                }
            } else {
                // Entry has no parent module, so either it belongs to the main module of the
                // project, or it is a main module itself.
                if !entry.is_main_module() {
                    let project_name = entry.module.project_name.clone();
                    let main_module = module::QualifiedName::new_main(project_name);
                    if let Some(main_group) = self.lookup_module_group(db, &main_module) {
                        main_group.content.entries.borrow_mut().push(component.clone_ref());
                        component_inserted_somewhere = true;
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
        module: &module::QualifiedName,
    ) -> Option<&mut ModuleGroups> {
        let (module_id, db_entry) = db.lookup_by_qualified_name(module)?;

        // Note: My heart is bleeding at this point, but because of lifetime checker limitations
        // we must do it in this suboptimal way.
        //
        // See https://users.rust-lang.org/t/returning-a-mutable-reference-extends-its-lifetime/28643
        // for example of similar problem.
        if self.module_groups.contains_key(&module_id) {
            self.module_groups.get_mut(&module_id)
        } else {
            let groups = ModuleGroups::new(module_id, &*db_entry).ok()?;
            if let Some(module) = module.parent_module() {
                if let Some(parent_groups) = self.lookup_module_group(db, &module) {
                    parent_groups.submodules.push(groups.content.clone_ref())
                }
            } else {
                // Module has no parent, so it is a top-level module that can be added as a
                // submodule of the main module of the project.
                let main_module = module::QualifiedName::new_main(module.project_name.clone());
                if main_module != *module {
                    if let Some(main_groups) = self.lookup_module_group(db, &main_module) {
                        main_groups.submodules.push(groups.content.clone_ref());
                    }
                }
            }
            Some(self.module_groups.entry(module_id).or_insert(groups))
        }
    }

    /// Build the list as described in the module's documentation.
    pub fn build(mut self) -> component::List {
        let components_order = component::Order::ByNameNonModulesThenModules;
        for group in self.module_groups.values() {
            group.content.update_sorting(components_order);
            if let Some(flattened) = &group.flattened_content {
                flattened.update_sorting(components_order);
            }
        }
        self.local_scope.update_sorting(components_order);
        let top_modules_iter = self.module_groups.values().filter(|g| g.is_top_module);
        let mut top_mdl_bld = component::group::AlphabeticalListBuilder::default();
        top_mdl_bld.extend(top_modules_iter.clone().map(|g| g.content.clone_ref()));
        let mut top_mdl_flat_bld = component::group::AlphabeticalListBuilder::default();
        top_mdl_flat_bld.extend(top_modules_iter.filter_map(|g| g.flattened_content.clone()));
        let favorites = self.build_favorites_and_add_to_all_components();
        component::List {
            all_components: Rc::new(self.all_components),
            top_modules: top_mdl_bld.build(),
            top_modules_flattened: top_mdl_flat_bld.build(),
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



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::controller::searcher::component::tests::mock_suggestion_db;

    use double_representation::project;


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
        let mut builder = List::new().with_local_scope_module_id(0);
        let first_part = (0..3).chain(6..11);
        let second_part = 3..6;
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, first_part);
        builder.extend_list_and_allow_favorites_with_ids(&suggestion_db, second_part);
        let list = builder.build();

        let top_modules: Vec<ComparableGroupData> =
            list.top_modules.iter().map(Into::into).collect();
        let expected = vec![
            ComparableGroupData {
                name:         "Test.TopModule1",
                component_id: Some(0),
                entries:      vec![5, 6, 2, 3],
            },
            ComparableGroupData {
                name:         "Test.TopModule2",
                component_id: Some(1),
                entries:      vec![7],
            },
        ];
        assert_eq!(top_modules, expected);

        let flattened_top_modules: Vec<ComparableGroupData> =
            list.top_modules_flattened.iter().map(Into::into).collect();
        let expected = vec![
            ComparableGroupData {
                name:         "Test.TopModule1",
                component_id: Some(0),
                entries:      vec![5, 6, 8, 9, 10, 2, 3, 4],
            },
            ComparableGroupData {
                name:         "Test.TopModule2",
                component_id: Some(1),
                entries:      vec![7],
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
                name:         "Test.TopModule1",
                component_id: Some(0),
                entries:      vec![5, 6, 2, 3],
            }),
            (1, ComparableGroupData {
                name:         "Test.TopModule2",
                component_id: Some(1),
                entries:      vec![7],
            }),
            (2, ComparableGroupData {
                name:         "SubModule1",
                component_id: Some(2),
                entries:      vec![8],
            }),
            (3, ComparableGroupData {
                name:         "SubModule2",
                component_id: Some(3),
                entries:      vec![9, 4],
            }),
            (4, ComparableGroupData {
                name:         "SubModule3",
                component_id: Some(4),
                entries:      vec![10],
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
            (0, vec!["SubModule1", "SubModule2"]),
            (1, vec![]),
            (2, vec![]),
            (3, vec!["SubModule3"]),
            (4, vec![]),
        ]
        .into_iter()
        .collect();
        assert_eq!(module_subgroups, expected);

        let local_scope_entries = &list.local_scope.entries;
        let component_id = |c: &Component| c.id().unwrap();
        let local_scope_ids = local_scope_entries.borrow().iter().map(component_id).collect_vec();
        let expected_ids = vec![5, 6];
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
        const QN_NOT_IN_DB: &str = "test.Test.NameNotInSuggestionDb";
        assert_eq!(db.lookup_by_qualified_name_str(QN_NOT_IN_DB), None);
        let groups = [
            execution_context::ComponentGroup {
                project:    project::QualifiedName::standard_base_library(),
                name:       "Group 1".into(),
                color:      None,
                components: vec![
                    qn_of_db_entry_0.clone(),
                    qn_of_db_entry_3.clone(),
                    QN_NOT_IN_DB.into(),
                    qn_of_db_entry_3.clone(),
                    QN_NOT_IN_DB.into(),
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
                    QN_NOT_IN_DB.into(),
                    qn_of_db_entry_3,
                    QN_NOT_IN_DB.into(),
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
        let qn_of_db_entry_0 = db.lookup(0).unwrap().qualified_name();
        let project = project::QualifiedName::standard_base_library();
        const GROUP_NAME: &str = "Group";
        let groups = [execution_context::ComponentGroup {
            project:    project.clone(),
            name:       GROUP_NAME.into(),
            color:      None,
            components: vec![qn_of_db_entry_0],
        }];
        builder.set_grouping_and_order_of_favorites(&db, &groups);
        let snippet = component::hardcoded::Snippet { name: "test snippet", ..default() };
        let snippet_iter = std::iter::once(Rc::new(snippet));
        builder.insert_virtual_components_in_favorites_group(GROUP_NAME, project, snippet_iter);
        builder.extend_list_and_allow_favorites_with_ids(&db, std::iter::once(0));
        let list = builder.build();
        let favorites = list.favorites;
        assert_eq!(favorites.len(), 1, "Expected one group of favorites, got: {:?}.", favorites);
        let expected_entry_names = ["test snippet", "TopModule1"];
        check_names_and_order_of_group_entries(&favorites[0], &expected_entry_names);
    }

    /// Test building a component list with a virtual component. The virtual component will be
    /// inserted into a new favorites group.
    #[test]
    fn building_component_list_with_virtual_component_in_new_favorites_group() {
        let db = mock_suggestion_db();
        let mut builder = List::new();
        let qn_of_db_entry_0 = db.lookup(0).unwrap().qualified_name();
        let project = project::QualifiedName::standard_base_library();
        const GROUP_1_NAME: &str = "Group 1";
        let groups = [execution_context::ComponentGroup {
            project:    project.clone(),
            name:       GROUP_1_NAME.into(),
            color:      None,
            components: vec![qn_of_db_entry_0],
        }];
        builder.set_grouping_and_order_of_favorites(&db, &groups);
        let snippet = component::hardcoded::Snippet { name: "test snippet", ..default() };
        let snippet_iter = std::iter::once(Rc::new(snippet));
        const GROUP_2_NAME: &str = "Group 2";
        builder.insert_virtual_components_in_favorites_group(GROUP_2_NAME, project, snippet_iter);
        builder.extend_list_and_allow_favorites_with_ids(&db, std::iter::once(0));
        let list = builder.build();
        let favorites = list.favorites;
        assert_eq!(favorites.len(), 2, "Expected two groups of favorites, got: {:?}.", favorites);
        let group_at_0 = &favorites[0];
        assert_eq!(group_at_0.name, "Group 2");
        check_names_and_order_of_group_entries(group_at_0, &["test snippet"]);
        let group_at_1 = &favorites[1];
        assert_eq!(group_at_1.name, "Group 1");
        check_names_and_order_of_group_entries(group_at_1, &["TopModule1"]);
    }
}
