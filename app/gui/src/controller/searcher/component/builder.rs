//! A module containing logic for constructing [`component::List`]. The main structure is
//! [`Builder`].

use crate::prelude::*;

use crate::controller::searcher::component;
use crate::controller::searcher::component::hardcoded;
use crate::controller::searcher::component::Component;
use crate::model::execution_context;
use crate::model::execution_context::GroupQualifiedName;
use crate::model::suggestion_database;

use double_representation::name::project::STANDARD_NAMESPACE;
use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use enso_doc_parser::DocSection;
use enso_doc_parser::Tag;
use enso_suggestion_database::SuggestionDatabase;



// ===============================
// === Component Ordering Keys ===
// ===============================

// === InGroupComponentOrderingKey ===

/// The ordering key inside single group or module. See [`ComponentOrderingKey`].
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum InGroupComponentOrderingKey<'a> {
    FromDatabase { module: QualifiedNameRef<'a>, label: &'a str },
    Virtual { name: &'a str },
}

impl<'a> InGroupComponentOrderingKey<'a> {
    fn of(component: &'a Component) -> Self {
        match &component.suggestion {
            component::Suggestion::FromDatabase { entry, .. } =>
                Self::FromDatabase { module: entry.defined_in.as_ref(), label: &component.label },
            component::Suggestion::Virtual { snippet } => Self::Virtual { name: &snippet.name },
        }
    }
}

// === ComponentOrderingKey ===

/// The ordering key for the main view of components.
///
/// Used in `sort_by_key` kind of methods, it allows expressing groups order in much simpler way
/// than chains of `cmp` calls. The `Ord` and `PartialOrd` are derived from the variants/fields
/// order.
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ComponentOrderingKey<'a> {
    InGroup { group_index: usize, key: InGroupComponentOrderingKey<'a> },
    ModuleContent { key: InGroupComponentOrderingKey<'a> },
    Module { non_standard: bool, module: QualifiedNameRef<'a> },
}

impl<'a> ComponentOrderingKey<'a> {
    fn of(component: &'a Component) -> Self {
        use suggestion_database::entry::Kind;
        match component.group_id {
            Some(group_index) =>
                Self::InGroup { group_index, key: InGroupComponentOrderingKey::of(component) },
            None => match &component.suggestion {
                component::Suggestion::FromDatabase { entry, .. } if entry.kind == Kind::Module =>
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

/// [`component::List`] builder.
///
/// It should be feed with list of suggestion database Ids received from the engine and potential
/// "virtual" entries being just a hardcoded code snippets. Then, the list may be build using
/// [`build`](Self::build`) method.
///
/// There are three modes of building list. Each mode my filter out some entries, and specifies
/// what components will be displayed without filtering and in what order.
/// * [The base one](Self::new_static), is meant to be used for searcher opened without source node.
///   When not filtered, the component groups will be displayed first, and then only the top modules
///   or every imported library - to browse other content, user needs to enter one of those modules.
/// * [The content of a specific module](Self::new_inside_module). Only component defined in the
///   module or one of its submodules will be in the list. Without filtering, only the modules
///   content and direct submodules will be displayed.
/// * [For for a specific "self" argument type](Self::new_with_this_type). All passed components
///   will be displayed, as the filtering out non-matching types is currently done on the Engine
///   side.
///
/// The order of components, when not filtered, is specified by [`ComponentOrderingKey`].
#[derive(Clone, Debug)]
pub struct Builder<'a> {
    db:                 &'a SuggestionDatabase,
    this_type:          Option<QualifiedName>,
    inside_module:      Option<QualifiedName>,
    built_list:         component::List,
    /// A mapping from entry id to group index and the cached suggestion database entry.
    entry_to_group_map: HashMap<suggestion_database::entry::Id, EntryInGroup>,
    group_name_to_id:   HashMap<GroupQualifiedName, usize>,
}

impl<'a> Builder<'a> {
    /// Create new builder without any groups.
    pub fn new_empty(db: &'a SuggestionDatabase) -> Self {
        Self {
            db,
            this_type: default(),
            inside_module: default(),
            built_list: default(),
            entry_to_group_map: default(),
            group_name_to_id: default(),
        }
    }

    /// Create builder for base view (without self type and not inside any module).
    pub fn new(db: &'a SuggestionDatabase, groups: &[execution_context::ComponentGroup]) -> Self {
        let group_name_to_id =
            groups.iter().enumerate().map(|(index, data)| (data.name.clone_ref(), index)).collect();
        let entry_to_group_entries = groups.iter().enumerate().flat_map(|(index, data)| {
            Self::group_data_to_map_entries(db, index, data, &group_name_to_id)
        });
        let groups = groups.iter().map(|group_data| component::Group {
            name:  group_data.name.clone_ref(),
            color: group_data.color,
        });

        Self {
            db,
            this_type: None,
            inside_module: None,
            built_list: component::List { groups: groups.collect(), ..default() },
            entry_to_group_map: entry_to_group_entries.collect(),
            group_name_to_id,
        }
    }

    fn group_data_to_map_entries<'b>(
        db: &'b SuggestionDatabase,
        group_index: usize,
        group_data: &'b execution_context::ComponentGroup,
        group_name_to_id: &'b HashMap<GroupQualifiedName, usize>,
    ) -> impl Iterator<Item = (suggestion_database::entry::Id, EntryInGroup)> + 'b {
        group_data.components.iter().filter_map(move |component_qn| {
            let (id, entry) = db.lookup_by_qualified_name(component_qn).handle_err(|err| {
                warn!("Cannot put entry {component_qn} to component groups. {err}")
            })?;
            // The group name from documentation tags has precedence.
            let group_from_tag = group_index_from_entry_tag(&entry, group_name_to_id);
            Some((id, EntryInGroup { entry, group_index: group_from_tag.unwrap_or(group_index) }))
        })
    }

    /// Create builder for view with self type.
    pub fn new_with_this_type(
        db: &'a SuggestionDatabase,
        groups: &[execution_context::ComponentGroup],
        this_type: &str,
    ) -> Self {
        Self {
            this_type: QualifiedName::from_text(this_type).handle_err(|err| warn!("Cannot create component list for type {this_type}: {err} Will display all components.")),
            ..Self::new(db, groups)
        }
    }

    /// Create builder for a specific module content.
    pub fn new_inside_module(
        db: &'a SuggestionDatabase,
        groups: &[execution_context::ComponentGroup],
        module_id: suggestion_database::entry::Id,
    ) -> Self {
        let module_entry = db.lookup(module_id);
        let module_qn = module_entry.map(|entry| entry.defined_in.clone()).handle_err(|err| {
            warn!("Cannot create component list inside module: {err} Will display all components.")
        });
        Self { inside_module: module_qn, ..Self::new(db, groups) }
    }

    /// Return the built list.
    pub fn build(mut self) -> component::List {
        self.built_list
            .displayed_by_default
            .sort_by(|lhs, rhs| ComponentOrderingKey::of(lhs).cmp(&ComponentOrderingKey::of(rhs)));
        self.built_list
            .components
            .sort_by(|lhs, rhs| ComponentOrderingKey::of(lhs).cmp(&ComponentOrderingKey::of(rhs)));
        self.built_list
    }
}

// === Adding Components to the List ===

#[derive(Copy, Clone, Debug)]
enum WhenDisplayed {
    Always,
    Never,
    OnlyWhenFiltering,
}

impl WhenDisplayed {
    fn in_base_mode(entry: &suggestion_database::Entry, inside_group: bool) -> Self {
        let is_top_module = entry.kind == suggestion_database::entry::Kind::Module
            && entry.defined_in.is_top_element();
        if is_top_module || inside_group {
            Self::Always
        } else {
            Self::OnlyWhenFiltering
        }
    }

    fn inside_module(entry: &suggestion_database::Entry, module: QualifiedNameRef) -> Self {
        let entry_module = if entry.kind == suggestion_database::entry::Kind::Module {
            entry.defined_in.parent()
        } else {
            Some(entry.defined_in.as_ref())
        };
        match entry_module {
            Some(entry_module) if entry_module == module => Self::Always,
            Some(entry_module) if entry_module.is_descendant_of(module) => Self::OnlyWhenFiltering,
            _ => Self::Never,
        }
    }

    fn with_self_type() -> Self {
        // Currently, the engine does the filtering.
        Self::Always
    }

    fn consider_tags(self, entry: &suggestion_database::Entry) -> Self {
        let is_private_tag =
            |doc: &DocSection| matches!(doc, DocSection::Tag { tag: Tag::Private, .. });
        let is_private = entry.documentation.iter().any(is_private_tag);
        if is_private {
            Self::Never
        } else {
            self
        }
    }
}

impl<'a> Builder<'a> {
    /// Add Suggestion Database entries as components to the list.
    ///
    /// Not all components may be actually added, as it depends on the mode. See
    /// [structure's docs](Builder) for details.
    pub fn add_components_from_db(
        &mut self,
        entry_ids: impl IntoIterator<Item = suggestion_database::entry::Id>,
    ) {
        for id in entry_ids {
            self.add_component_from_db(id).log_err("Could not add entries to Component List.");
        }
    }

    /// Add a single Suggestion Database entry as components to the list.
    ///
    /// It may not be actually added, depending on the mode. See [structure's docs](Builder) for
    /// details.
    pub fn add_component_from_db(&mut self, id: suggestion_database::entry::Id) -> FallibleResult {
        // If the entry was specified as in some group, we already retrieved it from suggestion
        // database.
        let cached_in_group = self.entry_to_group_map.get(&id);
        let entry = cached_in_group
            .map_or_else(|| self.db.lookup(id), |entry| Ok(entry.entry.clone_ref()))?;
        let group_id = cached_in_group
            .map(|entry| entry.group_index)
            .or_else(|| self.group_index_from_entry_tag(&entry));
        let when_displayed = match &self.inside_module {
            Some(module) => WhenDisplayed::inside_module(&entry, module.as_ref()),
            None if self.this_type.is_some() => WhenDisplayed::with_self_type(),
            None => WhenDisplayed::in_base_mode(&entry, group_id.is_some()),
        };
        let when_displayed = when_displayed.consider_tags(&entry);
        let label = match entry.kind {
            suggestion_database::entry::Kind::Module if self.inside_module.is_none() =>
                format!("{}", entry.defined_in).into(),
            _ => match entry.self_type.as_ref() {
                Some(self_type) if self.this_type.is_none() =>
                    format!("{}.{}", self_type.alias_name(), entry.name).into(),
                _ => entry.name.to_im_string(),
            },
        };
        let component = Component::new_from_database_entry(id, entry, group_id, label);
        if matches!(when_displayed, WhenDisplayed::Always) {
            self.built_list.displayed_by_default.push(component.clone());
        }
        if matches!(when_displayed, WhenDisplayed::Always | WhenDisplayed::OnlyWhenFiltering) {
            self.built_list.components.push(component);
        }
        Ok(())
    }

    fn group_index_from_entry_tag(&self, entry: &suggestion_database::Entry) -> Option<usize> {
        group_index_from_entry_tag(entry, &self.group_name_to_id)
    }

    /// Add virtual entries to a specific group.
    ///
    /// If the groups was not specified in constructor, it will be added.
    pub fn add_virtual_entries_to_group(
        &mut self,
        group_name: GroupQualifiedName,
        snippets: impl IntoIterator<Item = Rc<hardcoded::Snippet>>,
    ) {
        let groups = &mut self.built_list.groups;
        let existing_group_index = groups.iter().position(|grp| grp.name == group_name);
        let group_index = existing_group_index.unwrap_or_else(|| {
            groups.push(component::Group { name: group_name, color: None });
            groups.len() - 1
        });
        for snippet in snippets {
            let component = Component::new_virtual(snippet, group_index);
            self.built_list.displayed_by_default.push(component.clone());
            self.built_list.components.push(component);
        }
    }
}

fn group_index_from_entry_tag(
    entry: &suggestion_database::Entry,
    group_name_to_id: &HashMap<GroupQualifiedName, usize>,
) -> Option<usize> {
    entry.group_name.as_ref().and_then(|name| {
        // If the group name in tag is not fully qualified, we assume a group defined in the
        // same project where entry is defined.
        let qn_name =
            GroupQualifiedName::try_from(name.as_str()).unwrap_or_else(|_| GroupQualifiedName {
                project: entry.defined_in.project().clone_ref(),
                name:    name.into(),
            });
        group_name_to_id.get(&qn_name).copied()
    })
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::controller::searcher::component::tests::check_displayed_components;
    use crate::controller::searcher::component::tests::check_groups;

    use double_representation::name::project;
    use enso_suggestion_database::doc_section;
    use enso_suggestion_database::mock_suggestion_database;
    use ide_view::component_browser::component_list_panel::icon;

    fn mock_database() -> SuggestionDatabase {
        mock_suggestion_database! {
            test.Test {
                mod TopModule1 {
                    #[in_group("First Group")]
                    fn fun2() -> Standard.Base.Any;

                    #[with_doc_section(doc_section!(@ Private, ""))]
                    fn private() -> Standard.Base.Any;

                    mod SubModule1 {
                        fn fun4() -> Standard.Base.Any;
                    }
                    mod SubModule2 {
                        #[in_group("Second Group")]
                        fn fun5 -> Standard.Base.Any;
                        mod SubModule3 {
                            fn fun6 -> Standard.Base.Any;
                        }
                    }

                    #[in_group("First Group")]
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
                name:       GroupQualifiedName::new(project.clone_ref(), "First Group"),
                color:      None,
                components: vec![
                    "test.Test.TopModule2.fun0".try_into().unwrap(),
                    // Should be overwritten by tag.
                    "test.Test.TopModule1.SubModule2.fun5".try_into().unwrap(),
                ],
            },
            execution_context::ComponentGroup {
                name:       GroupQualifiedName::new(project, "Second Group"),
                color:      None,
                components: vec![],
            },
        ]
    }

    fn check_filterable_components(list: &component::List, mut expected: Vec<&str>) {
        expected.sort();
        let components = list
            .components
            .iter()
            .map(|component| component.matched_label().text)
            .sorted()
            .collect_vec();
        assert_eq!(components, expected);
    }

    #[test]
    fn building_main_list() {
        let database = mock_database();
        let groups = mock_groups();
        let mut builder = Builder::new(&database, &groups);

        let empty = builder.clone().build();
        assert_eq!(empty.components.len(), 0);
        assert_eq!(empty.displayed_by_default.len(), 0);
        assert_eq!(empty.groups.len(), 2);

        builder.add_components_from_db(database.keys());
        let list = builder.build();

        check_displayed_components(&list, vec![
            "TopModule1.fun1",
            "TopModule1.fun2",
            "TopModule2.fun0",
            "SubModule2.fun5",
            "test.Test.TopModule1",
            "test.Test.TopModule2",
        ]);
        // We subtract a single private component.
        assert_eq!(list.components.len(), database.keys().len() - 1);

        assert_eq!(list.groups.len(), 2);
        check_groups(&list, vec![Some(0), Some(0), Some(0), Some(1), None, None]);
    }

    #[test]
    fn building_module_content_list() {
        let database = mock_database();
        let groups = mock_groups();
        let (module_id, _) = database
            .lookup_by_qualified_name(&QualifiedName::from_text("test.Test.TopModule1").unwrap())
            .unwrap();
        let mut builder = Builder::new_inside_module(&database, &groups, module_id);

        let empty = builder.clone().build();
        assert_eq!(empty.components.len(), 0);
        assert_eq!(empty.displayed_by_default.len(), 0);

        builder.add_components_from_db(database.keys());
        let list = builder.build();

        check_displayed_components(&list, vec![
            "TopModule1.fun1",
            "TopModule1.fun2",
            "SubModule1",
            "SubModule2",
        ]);
        check_groups(&list, vec![Some(0), Some(0), None, None]);
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
    fn building_main_module_content_list() {
        let database = mock_database();
        let groups = mock_groups();
        let (module_id, _) = database
            .lookup_by_qualified_name(&QualifiedName::from_text("test.Test").unwrap())
            .unwrap();
        let mut builder = Builder::new_inside_module(&database, &groups, module_id);

        builder.add_components_from_db(database.keys());
        let list = builder.build();

        check_displayed_components(&list, vec!["TopModule1", "TopModule2"]);
        check_groups(&list, vec![None, None]);
        check_filterable_components(&list, vec![
            "TopModule1",
            "TopModule1.fun1",
            "TopModule1.fun2",
            "SubModule1",
            "SubModule1.fun4",
            "SubModule2",
            "SubModule2.fun5",
            "SubModule3",
            "SubModule3.fun6",
            "TopModule2",
            "TopModule2.fun0",
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
        builder.add_virtual_entries_to_group(
            GroupQualifiedName::new(project.clone_ref(), "First Group"),
            snippets.clone(),
        );
        let list = builder.build();
        check_displayed_components(&list, vec!["test1", "test2"]);
        check_filterable_components(&list, vec!["test1", "test2"]);

        let case = |group_name: &str,
                    expected_components: Vec<&str>,
                    expected_group: Vec<Option<usize>>| {
            let mut builder = Builder::new(&database, &groups);
            let group_name = GroupQualifiedName::new(project.clone_ref(), group_name);
            builder.add_virtual_entries_to_group(group_name.clone_ref(), snippets.clone());
            builder.add_components_from_db(database.keys());
            let list = builder.build();
            check_displayed_components(&list, expected_components.clone());
            check_groups(&list, expected_group.clone());

            let mut builder = Builder::new(&database, &groups);
            builder.add_components_from_db(database.keys());
            builder.add_virtual_entries_to_group(group_name, snippets.clone());
            let list = builder.build();
            check_displayed_components(&list, expected_components);
            check_groups(&list, expected_group);
        };

        // Adding to existing group.
        case(
            "First Group",
            vec![
                "TopModule1.fun1",
                "TopModule1.fun2",
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
                "TopModule1.fun1",
                "TopModule1.fun2",
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
