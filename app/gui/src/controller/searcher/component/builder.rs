use crate::controller::searcher::component;
use crate::controller::searcher::component::hardcoded;
use crate::controller::searcher::component::Component;
use crate::model::execution_context;
use crate::model::suggestion_database;
use crate::prelude::*;
use double_representation::name::project;
use double_representation::name::project::STANDARD_NAMESPACE;
use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use enso_suggestion_database::entry;
use enso_suggestion_database::SuggestionDatabase;



// ===============================
// === Component Ordering Keys ===
// ===============================

// === InGroupComponentOrderingKey ===

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum InGroupComponentOrderingKey<'a> {
    FromDatabase { module: QualifiedNameRef<'a>, index: suggestion_database::entry::Id },
    Virtual { name: &'a str },
}

impl<'a> InGroupComponentOrderingKey<'a> {
    fn of(component: &'a Component) -> Self {
        match &component.suggestion {
            component::Suggestion::FromDatabase { entry, id } =>
                Self::FromDatabase { module: entry.defined_in.as_ref(), index: *id },
            component::Suggestion::Virtual { snippet } => Self::Virtual { name: &snippet.name },
        }
    }
}

// === ComponentOrderingKey ===

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum ComponentOrderingKey<'a> {
    InGroup { group_index: usize, key: InGroupComponentOrderingKey<'a> },
    ModuleContent { key: InGroupComponentOrderingKey<'a> },
    Module { non_standard: bool, module: QualifiedNameRef<'a> },
}

impl<'a> ComponentOrderingKey<'a> {
    fn of(component: &'a Component) -> Self {
        match component.group_id {
            Some(group_index) =>
                Self::InGroup { group_index, key: InGroupComponentOrderingKey::of(component) },
            None => match &component.suggestion {
                component::Suggestion::FromDatabase { entry, .. }
                    if entry.kind == entry::Kind::Module =>
                    Self::Module {
                        non_standard: entry.defined_in.project().namespace != STANDARD_NAMESPACE,
                        module:       entry.defined_in.as_ref(),
                    },
                _ => Self::ModuleContent { key: InGroupComponentOrderingKey::of(component) },
            },
        }
    }
}



// ===============
// === Builder ===
// ===============

#[derive(Clone, Debug)]
struct EntryInGroup {
    entry:       Rc<suggestion_database::Entry>,
    group_index: usize,
}

#[derive(Clone, Debug)]
pub struct Builder<'a> {
    db:                &'a SuggestionDatabase,
    this_type:         Option<QualifiedName>,
    inside_module:     Option<QualifiedName>,
    built_list:        component::List,
    groups_of_entries: HashMap<suggestion_database::entry::Id, EntryInGroup>,
}

impl<'a> Builder<'a> {
    pub fn new_empty(db: &'a SuggestionDatabase) -> Self {
        Self {
            db,
            this_type: default(),
            inside_module: default(),
            built_list: default(),
            groups_of_entries: default(),
        }
    }

    pub fn new_static(
        db: &'a SuggestionDatabase,
        groups: Vec<execution_context::ComponentGroup>,
    ) -> Self {
        let components_in_groups = groups
            .iter()
            .enumerate()
            .map(|(group_index, group_data)| {
                group_data.components.iter().filter_map(move |component_qn| {
                    db.lookup_by_qualified_name(component_qn)
                        .handle_err(|err| {
                            warn!("Cannot put entry {component_qn} to component groups. {err}")
                        })
                        .map(|(id, entry)| (id, EntryInGroup { entry, group_index }))
                })
            })
            .flatten();
        let groups = groups.iter().map(|group_data| component::Group {
            project: group_data.project.clone_ref(),
            name:    group_data.name.clone_ref(),
            color:   group_data.color,
        });
        Self {
            db,
            this_type: None,
            inside_module: None,
            built_list: component::List { groups: groups.collect(), ..default() },
            groups_of_entries: components_in_groups.collect(),
        }
    }

    pub fn new_with_this_type(db: &'a SuggestionDatabase, this_type: &str) -> Self {
        Self {
            db,
            this_type: QualifiedName::from_text(this_type).handle_err(|err| warn!("Cannot create component list for type {this_type}: {err} Will display all components.")),
            inside_module: None,
            built_list: default(),
            groups_of_entries: default(),
        }
    }

    pub fn new_inside_module(
        db: &'a SuggestionDatabase,
        module_id: suggestion_database::entry::Id,
    ) -> Self {
        Self {
            db,
            this_type: None,
            inside_module: db
                .lookup(module_id)
                .map(|module_entry| module_entry.defined_in.clone())
                .handle_err(|err| warn!("Cannot create component list inside module: {err} Will display all components.")),
            built_list: default(),
            groups_of_entries: default(),
        }
    }

    pub fn add_components_from_db(
        &mut self,
        entry_ids: impl IntoIterator<Item = suggestion_database::entry::Id>,
    ) {
        for id in entry_ids {
            self.add_component_from_db(id).log_err("Could not add entries to Component List.");
        }
    }

    pub fn add_component_from_db(&mut self, id: suggestion_database::entry::Id) -> FallibleResult {
        let in_group = self.groups_of_entries.get(&id);
        let entry =
            in_group.map_or_else(|| self.db.lookup(id), |group| Ok(group.entry.clone_ref()))?;
        // let matches_this_type = entry.self_type == self.this_type;
        let (is_shown, is_shown_when_filtering) = match &self.inside_module {
            Some(module) if entry.kind == suggestion_database::entry::Kind::Module => {
                let parent_module = entry.defined_in.parent();
                (
                    parent_module.contains(module),
                    parent_module.map_or(false, |parent| parent.is_descendant_of(module.as_ref())),
                )
            }
            Some(module) =>
                (&entry.defined_in == module, entry.defined_in.is_descendant_of(module.as_ref())),
            None if self.this_type.is_some() => (true, true),
            None => (
                (entry.kind == suggestion_database::entry::Kind::Module
                    && entry.defined_in.is_top_element())
                    || in_group.is_some(),
                true,
            ),
        };
        let component =
            Component::new_from_database_entry(id, entry, in_group.map(|e| e.group_index));
        if is_shown {
            self.built_list.components.push(component.clone());
        }
        if is_shown_when_filtering {
            self.built_list.filterable_components.push(component);
        }
        Ok(())
    }

    pub fn add_virtual_entries_to_group(
        &mut self,
        group_name: &str,
        project: project::QualifiedName,
        snippets: impl IntoIterator<Item = Rc<hardcoded::Snippet>>,
    ) {
        let groups = &mut self.built_list.groups;
        let group_index =
            groups.iter().position(|grp| grp.name == group_name).unwrap_or_else(|| {
                groups.push(component::Group { project, name: group_name.into(), color: None });
                groups.len() - 1
            });
        for snippet in snippets {
            let component = Component {
                suggestion: component::Suggestion::Virtual { snippet },
                group_id:   Some(group_index),
                match_info: Default::default(),
            };
            self.built_list.components.push(component.clone());
            self.built_list.filterable_components.push(component);
        }
    }

    pub fn build(mut self) -> component::List {
        self.built_list
            .components
            .sort_by(|lhs, rhs| ComponentOrderingKey::of(lhs).cmp(&ComponentOrderingKey::of(rhs)));
        self.built_list
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::controller::searcher::component::tests::check_components;
    use crate::controller::searcher::component::tests::check_groups;
    use enso_suggestion_database::mock_suggestion_database;
    use ide_view::component_browser::component_list_panel::icon;

    fn mock_database() -> SuggestionDatabase {
        mock_suggestion_database! {
            test.Test {
                mod TopModule1 {
                    fn fun2() -> Standard.Base.Any;

                    mod SubModule1 {
                        fn fun4() -> Standard.Base.Any;
                    }
                    mod SubModule2 {
                        fn fun5 -> Standard.Base.Any;
                        mod SubModule3 {
                            fn fun6 -> Standard.Base.Any;
                        }
                    }

                    fn fun1() -> Standard.Base.Any;
                }
                mod TopModule2 {
                    fn fun0() -> Standard.Base.Any;
                }
            }
        }
    }

    fn mock_groups() -> Vec<execution_context::ComponentGroup> {
        let project = project::QualifiedName::from_text("test.Test").unwrap();
        vec![
            execution_context::ComponentGroup {
                project:    project.clone(),
                name:       "First Group".into(),
                color:      None,
                components: vec![
                    "test.Test.TopModule2.fun0".try_into().unwrap(),
                    "test.Test.TopModule1.fun1".try_into().unwrap(),
                    "test.Test.TopModule1.fun2".try_into().unwrap(),
                ],
            },
            execution_context::ComponentGroup {
                project,
                name: "Second Group".into(),
                color: None,
                components: vec!["test.Test.TopModule1.SubModule2.fun5".try_into().unwrap()],
            },
        ]
    }

    fn check_filterable_components(list: &component::List, mut expected: Vec<&str>) {
        expected.sort();
        let components = list
            .filterable_components
            .iter()
            .map(|component| component.label())
            .sorted()
            .collect_vec();
        assert_eq!(components, expected);
    }

    #[test]
    fn building_main_list() {
        let database = mock_database();
        let groups = mock_groups();
        let mut builder = Builder::new_static(&database, groups);

        let empty = builder.clone().build();
        assert_eq!(empty.len(), 0);
        assert_eq!(empty.groups.len(), 2);

        builder.add_components_from_db(database.keys());
        let list = builder.build();

        check_components(&list, vec![
            "TopModule1.fun2",
            "TopModule1.fun1",
            "TopModule2.fun0",
            "SubModule2.fun5",
            "test.Test.TopModule1",
            "test.Test.TopModule2",
        ]);
        assert_eq!(list.filterable_components.len(), database.keys().len());

        assert_eq!(list.groups.len(), 2);
        check_groups(&list, vec![Some(0), Some(0), Some(0), Some(1), None, None]);
    }

    #[test]
    fn building_module_content_list() {
        let database = mock_database();
        let (module_id, _) = database
            .lookup_by_qualified_name(&QualifiedName::from_text("test.Test.TopModule1").unwrap())
            .unwrap();
        let mut builder = Builder::new_inside_module(&database, module_id);

        let empty = builder.clone().build();
        assert_eq!(empty.len(), 0);

        builder.add_components_from_db(database.keys());
        let list = builder.build();

        check_components(&list, vec![
            "TopModule1.fun2",
            "TopModule1.fun1",
            "SubModule1",
            "SubModule2",
        ]);
        check_filterable_components(&list, vec![
            "TopModule1.fun1",
            "TopModule1.fun2",
            "SubModule1",
            "SubModule1.fun4",
            "SubModule2",
            "SubModule2.fun5",
            "SubModule3",
            "SubModule3.fun6",
        ]);
    }



    #[test]
    fn adding_virtual_entries_to_group() {
        let project = project::QualifiedName::from_text("test.Test").unwrap();
        let database = mock_database();
        let groups = mock_groups();
        let snippets = vec![
            Rc::new(hardcoded::Snippet::new("test1", "3 + 3", icon::Id::NumberInput)),
            Rc::new(hardcoded::Snippet::new("test2", "4 + 4", icon::Id::NumberInput)),
        ];

        // Adding to an empty builder.
        let mut builder = Builder::new_empty(&database);
        builder.add_virtual_entries_to_group("First Group", project.clone_ref(), snippets.clone());
        let list = builder.build();
        check_components(&list, vec!["test1", "test2"]);
        check_filterable_components(&list, vec!["test1", "test2"]);

        let case = |group_name: &str,
                    expected_components: Vec<&str>,
                    expected_group: Vec<Option<usize>>| {
            let mut builder = Builder::new_static(&database, groups.clone());
            builder.add_virtual_entries_to_group(group_name, project.clone_ref(), snippets.clone());
            builder.add_components_from_db(database.keys());
            let list = builder.build();
            check_components(&list, expected_components.clone());
            check_groups(&list, expected_group.clone());

            let mut builder = Builder::new_static(&database, groups.clone());
            builder.add_components_from_db(database.keys());
            builder.add_virtual_entries_to_group(group_name, project.clone_ref(), snippets.clone());
            let list = builder.build();
            check_components(&list, expected_components.clone());
            check_groups(&list, expected_group.clone());
        };

        // Adding to existing group.
        case(
            "First Group",
            vec![
                "TopModule1.fun2",
                "TopModule1.fun1",
                "TopModule2.fun0",
                "test1",
                "test2",
                "SubModule2.fun5",
                "test.Test.TopModule1",
                "test.Test.TopModule2",
            ],
            iter::repeat(Some(0))
                .take(5)
                .chain(iter::once(Some(1)))
                .chain(iter::repeat(None).take(2))
                .collect(),
        );
        // Adding to non-exiting group.
        case(
            "Another Group",
            vec![
                "TopModule1.fun2",
                "TopModule1.fun1",
                "TopModule2.fun0",
                "SubModule2.fun5",
                "test1",
                "test2",
                "test.Test.TopModule1",
                "test.Test.TopModule2",
            ],
            iter::repeat(Some(0))
                .take(3)
                .chain(iter::once(Some(1)))
                .chain(iter::repeat(Some(2)).take(2))
                .chain(iter::repeat(None).take(2))
                .collect(),
        );
    }
}
