pub mod group;

pub use group::Group;

use crate::model::module;
use crate::model::suggestion_database;
use crate::model::suggestion_database::entry::Kind;
use crate::model::SuggestionDatabase;
use crate::prelude::*;
use engine_protocol::language_server::SuggestionId;
use js_sys::Atomics::sub;
use std::collections::hash_map::Entry;

/// Information how the list entry matches the filtering pattern.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum MatchInfo {
    DoesNotMatch,
    Matches { subsequence: fuzzly::Subsequence },
}

impl Default for MatchInfo {
    fn default() -> Self {
        MatchInfo::Matches { subsequence: default() }
    }
}

#[derive(Clone, CloneRef, Debug)]
pub struct Component {
    pub suggestion_id: Immutable<suggestion_database::entry::Id>,
    pub suggestion:    Rc<suggestion_database::Entry>,
    pub match_info:    Rc<RefCell<MatchInfo>>,
}

pub struct ModuleGroups {
    pub group:           Group,
    pub flattened_group: Option<Group>,
    pub subgroups:       group::List,
}

pub struct List {
    pub logger:                Logger,
    // pub suggestion_db:             Rc<model::SuggestionDatabase>,
    pub all_entries:           Vec<Component>,
    pub top_modules:           group::List,
    pub top_modules_flattened: group::List,
    pub module_groups:         HashMap<suggestion_database::entry::Id, ModuleGroups>,
}

impl List {
    pub fn iter_top_modules(&self) -> impl Iterator<Item = Group> + '_ {
        self.top_modules.iter().filter(|group| group.visible).cloned()
    }
}

#[derive(Clone, Debug)]
pub struct ModuleGroupsBuilder {
    pub group:           Group,
    pub flattened_group: Option<Group>,
    pub subgroups:       group::ListBuilder,
}

impl ModuleGroupsBuilder {
    fn new(module: &suggestion_database::Entry) -> Self {
        Self {
            group:           Group::from_entry(module),
            flattened_group: module
                .module
                .is_top_module()
                .as_some_from(|| Group::from_entry(module)),
            subgroups:       default(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ListBuilder {
    pub suggestion_db:  Rc<model::SuggestionDatabase>,
    pub all_components: Vec<Component>,
    pub module_groups:  HashMap<suggestion_database::entry::Id, ModuleGroupsBuilder>,
}

impl ListBuilder {
    pub fn extend(&mut self, entries: impl IntoIterator<Item = suggestion_database::entry::Id>) {
        let suggestion_db = self.suggestion_db.clone_ref();
        let components = entries.into_iter().filter_map(|id| {
            Some(Component {
                suggestion_id: Immutable(id),
                suggestion:    suggestion_db.lookup(id).ok()?,
                match_info:    default(),
            })
        });
        for component in components {
            let mut component_inserted_somewhere = false;
            if let Some(parent_module) = component.suggestion.qualified_name().parent_module() {
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

    fn lookup_module_group(
        &mut self,
        module: &module::QualifiedName,
    ) -> Option<&mut ModuleGroupsBuilder> {
        let module_id: suggestion_database::entry::Id = todo!("call to nonexistent function");
        let (is_new, groups) = match self.module_groups.entry(module_id) {
            Entry::Occupied(mut entry) => (false, entry.get_mut()),
            Entry::Vacant(entry) => {
                let db_entry = self.suggestion_db.lookup(module_id).ok()?;
                let groups = ModuleGroupsBuilder::new(&*db_entry);
                (true, entry.insert(groups))
            }
        };
        let module_to_add_subgroup = is_new.and_option_from(|| module.parent_module());
        if let Some(module) = module_to_add_subgroup {
            if let Some(parent_groups) = self.lookup_module_group(&module) {
                parent_groups.subgroups.push(groups.group.clone_ref())
            }
        }
        Some(groups)
    }
}
