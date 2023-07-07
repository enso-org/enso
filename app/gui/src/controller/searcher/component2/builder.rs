use crate::controller::searcher::component::hardcoded;
use crate::controller::searcher::component2;
use crate::controller::searcher::component2::Component;
use crate::model::execution_context;
use crate::model::suggestion_database;
use crate::prelude::*;
use double_representation::name::project;
use double_representation::name::project::STANDARD_NAMESPACE;
use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use enso_suggestion_database::entry;
use enso_suggestion_database::SuggestionDatabase;

#[derive(Clone, Debug)]
struct EntryInGroup {
    entry:       Rc<suggestion_database::Entry>,
    group_index: usize,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum InGroupComponentOrderingKey<'a> {
    FromDatabase { module: QualifiedNameRef<'a>, name: &'a str },
    Virtual { name: &'a str },
}

impl<'a> InGroupComponentOrderingKey<'a> {
    fn of(component: &'a Component) -> Self {
        match &component.data {
            component2::Data::FromDatabase { entry, .. } =>
                Self::FromDatabase { module: entry.defined_in.as_ref(), name: &entry.name },
            component2::Data::Virtual { snippet } => Self::Virtual { name: &snippet.name },
        }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum ComponentOrderingKey<'a> {
    InGroup { group_index: usize, key: InGroupComponentOrderingKey<'a> },
    ModuleContent { module: QualifiedNameRef<'a>, id: suggestion_database::entry::Id },
    Module { non_standard: bool, module: QualifiedNameRef<'a> },
    Other { label: &'a str },
}

impl<'a> ComponentOrderingKey<'a> {
    fn of(component: &'a Component) -> Self {
        match component.group_id {
            Some(group_index) =>
                Self::InGroup { group_index, key: InGroupComponentOrderingKey::of(component) },
            None => match &component.data {
                component2::Data::FromDatabase { entry, .. }
                    if entry.kind == entry::Kind::Module =>
                    Self::Module {
                        non_standard: entry.defined_in.project().namespace != STANDARD_NAMESPACE,
                        module:       entry.defined_in.as_ref(),
                    },
                component2::Data::FromDatabase { entry, id } =>
                    Self::ModuleContent { module: entry.defined_in.as_ref(), id: *id },
                component2::Data::Virtual { snippet } => Self::Other { label: &snippet.name },
            },
        }
    }
}


pub struct Builder<'a> {
    db:                &'a SuggestionDatabase,
    this_type:         Option<QualifiedName>,
    inside_module:     Option<QualifiedName>,
    built_list:        component2::List,
    groups_of_entries: HashMap<suggestion_database::entry::Id, EntryInGroup>,
}

impl<'a> Builder<'a> {
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
        let groups = groups.iter().map(|group_data| component2::Group {
            name:  group_data.name.clone_ref(),
            color: group_data.color,
        });
        Self {
            db,
            this_type: None,
            inside_module: None,
            built_list: component2::List { groups: groups.collect(), ..default() },
            groups_of_entries: components_in_groups.collect(),
        }
    }

    pub fn new_with_this_type(db: &'a SuggestionDatabase, this_type: QualifiedName) -> Self {
        Self {
            db,
            this_type: Some(this_type),
            inside_module: None,
            built_list: default(),
            groups_of_entries: default(),
        }
    }

    pub fn new_inside_module(db: &'a SuggestionDatabase, module: QualifiedName) -> Self {
        Self {
            db,
            this_type: None,
            inside_module: Some(module),
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
        let matches_this_type = entry.self_type == self.this_type;
        let (is_shown, is_shown_when_filtering) = match &self.inside_module {
            Some(module) =>
                (&entry.defined_in == module, entry.defined_in.is_descendant_of(module.as_ref())),
            None if self.this_type.is_some() => (true, true),
            None => (entry.defined_in.is_top_element() || in_group.is_some(), true),
        };
        let component =
            Component::new_from_database_entry(id, entry, in_group.map(|e| e.group_index));
        if matches_this_type && is_shown {
            self.built_list.components.push(component.clone());
        }
        if matches_this_type && is_shown_when_filtering {
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
                groups.push(component2::Group { name: group_name.into(), color: None });
                groups.len()
            });
        for snippet in snippets {
            let component = Component {
                data:       component2::Data::Virtual { snippet },
                group_id:   Some(group_index),
                match_info: Default::default(),
            };
            self.built_list.components.push(component.clone());
            self.built_list.filterable_components.push(component);
        }
    }

    pub fn build(mut self) -> component2::List {
        self.built_list
            .components
            .sort_by(|lhs, rhs| ComponentOrderingKey::of(lhs).cmp(&ComponentOrderingKey::of(rhs)));
        self.built_list
    }
}
