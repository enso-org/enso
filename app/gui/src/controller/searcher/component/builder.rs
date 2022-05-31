use crate::prelude::*;


use crate::controller::searcher::component;
use crate::controller::searcher::component::Component;
use crate::model::suggestion_database;

use double_representation::module;



// ====================
// === ModuleGroups ===
// ====================

#[derive(Clone, Debug)]
pub struct ModuleGroups {
    pub group:           component::Group,
    pub flattened_group: Option<component::Group>,
    pub subgroups:       component::group::ListBuilder,
    pub is_top_module:   bool,
}

impl ModuleGroups {
    fn new(component_id: component::Id, entry: &suggestion_database::Entry) -> Self {
        Self {
            group:           component::Group::from_entry(component_id, entry),
            flattened_group: entry
                .module
                .is_top_module()
                .as_some_from(|| component::Group::from_entry(component_id, entry)),
            subgroups:       default(),
            is_top_module:   entry.module.is_top_module(),
        }
    }

    fn build(self) -> component::ModuleGroups {
        component::ModuleGroups { group: self.group, subgroups: self.subgroups.build() }
    }
}



// ============
// === List ===
// ============

#[derive(Clone, Debug)]
pub struct List {
    pub suggestion_db:  Rc<model::SuggestionDatabase>,
    pub all_components: Vec<Component>,
    pub module_groups:  HashMap<component::Id, ModuleGroups>,
}

impl List {
    pub fn new(suggestion_db: Rc<model::SuggestionDatabase>) -> Self {
        Self { suggestion_db, all_components: default(), module_groups: default() }
    }

    pub fn extend(&mut self, entries: impl IntoIterator<Item = component::Id>) {
        let suggestion_db = self.suggestion_db.clone_ref();
        let components = entries.into_iter().filter_map(|id| {
            Some(Component {
                id:         Immutable(id),
                suggestion: suggestion_db.lookup(id).ok()?,
                match_info: default(),
            })
        });
        for component in components {
            DEBUG!("Putting component {component.id:?}");
            let mut component_inserted_somewhere = false;
            if let Some(parent_module) = component.suggestion.parent_module() {
                DEBUG!("Found parent module: {parent_module:?}");
                if let Some(parent_group) = self.lookup_module_group(&parent_module) {
                    parent_group.group.entries.borrow_mut().push(component.clone_ref());
                    component_inserted_somewhere = true;
                }
                if let Some(top_group) = self.lookup_module_group(&parent_module.top_module()) {
                    if let Some(flatten_group) = &mut top_group.flattened_group {
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

    fn lookup_module_group(&mut self, module: &module::QualifiedName) -> Option<&mut ModuleGroups> {
        let (module_id, db_entry) = self.suggestion_db.lookup_by_qualified_name(module)?;

        // Note: My heart is bleeding at this point, but because of lifetime checker limitations
        // we must do it in this suboptimal way.
        //
        // See https://users.rust-lang.org/t/returning-a-mutable-reference-extends-its-lifetime/28643
        // for example of similar problem.
        if self.module_groups.contains_key(&module_id) {
            self.module_groups.get_mut(&module_id)
        } else {
            DEBUG!("Creating new ModuleGroupsBuilder for {module} ({module_id})");
            let groups = ModuleGroups::new(module_id, &*db_entry);
            if let Some(module) = module.parent_module() {
                DEBUG!("The parent module is {module}");
                if let Some(parent_groups) = self.lookup_module_group(&module) {
                    DEBUG!("Found Parent Groups");
                    parent_groups.subgroups.push(groups.group.clone_ref())
                }
            }
            Some(self.module_groups.entry(module_id).or_insert(groups))
        }
    }

    fn build(self) -> component::List {
        let top_modules_iter = self.module_groups.values().filter(|g| g.is_top_module);
        let mut top_mdl_bld = component::group::ListBuilder::default();
        top_mdl_bld.extend(top_modules_iter.clone().map(|g| g.group.clone_ref()));
        let mut top_mdl_flat_bld = component::group::ListBuilder::default();
        top_mdl_flat_bld.extend(top_modules_iter.filter_map(|g| g.flattened_group.clone()));
        component::List {
            logger:                Logger::new("searcher::component::List"),
            all_components:        self.all_components,
            top_modules:           top_mdl_bld.build(),
            top_modules_flattened: top_mdl_flat_bld.build(),
            module_groups:         self
                .module_groups
                .into_iter()
                .map(|(id, group)| (id, group.build()))
                .collect(),
            filtered:              default(),
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::suggestion_database::entry::Kind;
    use crate::model::suggestion_database::entry::QualifiedName;
    use engine_protocol::language_server;

    use crate::test::mock;


    fn mock_module(name: &str) -> model::suggestion_database::Entry {
        let ls_entry = language_server::SuggestionEntry::Module {
            module:             name.to_owned(),
            documentation:      default(),
            documentation_html: default(),
            reexport:           default(),
        };
        model::suggestion_database::Entry::from_ls_entry(ls_entry).unwrap()
    }

    fn mock_function(
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
        }
    }

    fn mock_suggestion_db(logger: impl AnyLogger) -> model::SuggestionDatabase {
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
        let mut builder = List::new(suggestion_db);
        let first_part = (0..3).chain(6..11);
        let second_part = 3..6;
        builder.extend(first_part);
        builder.extend(second_part);
        let list = builder.build();

        let top_modules: Vec<ComparableGroupData> =
            list.top_modules.iter().map(Into::into).collect();
        let expected = vec![
            ComparableGroupData {
                name:         "test.Test.TopModule1",
                component_id: Some(0),
                entries:      vec![2, 6, 3, 5],
            },
            ComparableGroupData {
                name:         "test.Test.TopModule2",
                component_id: Some(1),
                entries:      vec![7],
            },
        ];
        assert_eq!(top_modules, expected);

        let module_groups: BTreeMap<component::Id, ComparableGroupData> = list
            .module_groups
            .iter()
            .map(|(id, m_groups)| (*id, (&m_groups.group).into()))
            .collect();
        let expected: BTreeMap<component::Id, ComparableGroupData> = [
            (0, ComparableGroupData {
                name:         "test.Test.TopModule1",
                component_id: Some(0),
                entries:      vec![2, 6, 3, 5],
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
                (*id, m_group.subgroups.iter().map(|sg| sg.name.as_str()).collect())
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
