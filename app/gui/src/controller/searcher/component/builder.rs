//! A module with entities used for building proper [`component::List`].

use crate::prelude::*;

use crate::controller::searcher::component;
use crate::controller::searcher::component::Component;
use crate::model::execution_context;
use crate::model::suggestion_database;

use double_representation::module;



// ====================
// === ModuleGroups ===
// ====================

/// Module Groups Builder.
///
/// The builder allow extending the groups with module content, and add new submodules.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct ModuleGroups {
    pub content:           component::Group,
    /// The flattened content contains the content af module and all its submodules. Is set to
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
    pub fn new(component_id: component::Id, entry: &suggestion_database::Entry) -> Self {
        let is_top_module = entry.module.is_top_module();
        let mk_group = || component::Group::from_entry(component_id, entry);
        Self {
            content: mk_group(),
            flattened_content: is_top_module.as_some_from(mk_group),
            submodules: default(),
            is_top_module,
        }
    }

    /// Build [`component::ModuleGroups`] structure with appropriately sorted submodules.
    pub fn build(self) -> component::ModuleGroups {
        component::ModuleGroups { content: self.content, submodules: self.submodules.build() }
    }
}



// ============
// === List ===
// ============

/// A [`component::List`] builder.
///
/// The builder allow extending the list with new entries, and build a list with properly sorted
/// groups.
#[derive(Clone, Debug, Default)]
pub struct List {
    all_components: Vec<Component>,
    module_groups:  HashMap<component::Id, ModuleGroups>,
    favorites:      component::group::List,
}

impl List {
    /// Construct List builder without content.
    pub fn new() -> Self {
        default()
    }

    /// Extend the list with new entries looked up by ID in suggestion database.
    pub fn extend(
        &mut self,
        db: &model::SuggestionDatabase,
        entries: impl IntoIterator<Item = component::Id>,
    ) {
        let lookup_component_by_id = |id| Some(Component::new(id, db.lookup(id).ok()?));
        let components = entries.into_iter().filter_map(lookup_component_by_id);
        for component in components {
            let mut component_inserted_somewhere = false;
            if let Some(parent_module) = component.suggestion.parent_module() {
                if let Some(parent_group) = self.lookup_module_group(db, &parent_module) {
                    parent_group.content.entries.borrow_mut().push(component.clone_ref());
                    component_inserted_somewhere = true;
                }
                if let Some(top_group) = self.lookup_module_group(db, &parent_module.top_module()) {
                    if let Some(flatten_group) = &mut top_group.flattened_content {
                        flatten_group.entries.borrow_mut().push(component.clone_ref());
                        component_inserted_somewhere = true;
                    }
                }
            }
            if component_inserted_somewhere {
                self.all_components.push(component);
            }
        }
    }

    /// Set the favorites in the list. Components are looked up by ID in the suggestion database.
    pub fn set_favorites<'a>(
        &mut self,
        db: &model::SuggestionDatabase,
        component_groups: impl IntoIterator<Item = &'a execution_context::ComponentGroup>,
    ) {
        self.favorites = component_groups
            .into_iter()
            .filter_map(|g| component::Group::from_execution_context_component_group(g, db))
            .collect();
        for group in &*self.favorites {
            self.all_components.extend(group.entries.borrow().iter().cloned());
        }
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
            let groups = ModuleGroups::new(module_id, &*db_entry);
            if let Some(module) = module.parent_module() {
                if let Some(parent_groups) = self.lookup_module_group(db, &module) {
                    parent_groups.submodules.push(groups.content.clone_ref())
                }
            }
            Some(self.module_groups.entry(module_id).or_insert(groups))
        }
    }

    /// Build the list, sorting all group lists and groups' contents appropriately. (Does not sort
    /// the [`component::List::favorites`].)
    pub fn build(self) -> component::List {
        for group in self.module_groups.values() {
            group.content.update_sorting_and_visibility("");
            if let Some(flattened) = &group.flattened_content {
                flattened.update_sorting_and_visibility("");
            }
        }
        let top_modules_iter = self.module_groups.values().filter(|g| g.is_top_module);
        let mut top_mdl_bld = component::group::AlphabeticalListBuilder::default();
        top_mdl_bld.extend(top_modules_iter.clone().map(|g| g.content.clone_ref()));
        let mut top_mdl_flat_bld = component::group::AlphabeticalListBuilder::default();
        top_mdl_flat_bld.extend(top_modules_iter.filter_map(|g| g.flattened_content.clone()));
        component::List {
            all_components:        Rc::new(self.all_components),
            top_modules:           top_mdl_bld.build(),
            top_modules_flattened: top_mdl_flat_bld.build(),
            module_groups:         Rc::new(
                self.module_groups.into_iter().map(|(id, group)| (id, group.build())).collect(),
            ),
            filtered:              default(),
            favorites:             self.favorites,
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::controller::searcher::component::tests::mock_suggestion_db;


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
                entries:      component.entries.borrow().iter().map(|e| *e.id).collect(),
            }
        }
    }

    #[test]
    fn building_component_list() {
        let logger = Logger::new("tests::module_groups_in_component_list");
        let suggestion_db = Rc::new(mock_suggestion_db(logger));
        let mut builder = List::new();
        let first_part = (0..3).chain(6..11);
        let second_part = 3..6;
        builder.extend(&suggestion_db, first_part);
        builder.extend(&suggestion_db, second_part);
        let list = builder.build();

        let top_modules: Vec<ComparableGroupData> =
            list.top_modules.iter().map(Into::into).collect();
        let expected = vec![
            ComparableGroupData {
                name:         "test.Test.TopModule1",
                component_id: Some(0),
                entries:      vec![5, 6, 2, 3],
            },
            ComparableGroupData {
                name:         "test.Test.TopModule2",
                component_id: Some(1),
                entries:      vec![7],
            },
        ];
        assert_eq!(top_modules, expected);

        let flattened_top_modules: Vec<ComparableGroupData> =
            list.top_modules_flattened.iter().map(Into::into).collect();
        let expected = vec![
            ComparableGroupData {
                name:         "test.Test.TopModule1",
                component_id: Some(0),
                entries:      vec![5, 6, 8, 9, 10, 2, 3, 4],
            },
            ComparableGroupData {
                name:         "test.Test.TopModule2",
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
                name:         "test.Test.TopModule1",
                component_id: Some(0),
                entries:      vec![5, 6, 2, 3],
            }),
            (1, ComparableGroupData {
                name:         "test.Test.TopModule2",
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
    }
}
