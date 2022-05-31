pub mod group;

pub use group::Group;

use crate::model::execution_context;
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

impl Component {
    pub fn label(&self) -> &str {
        &self.suggestion.name
    }

    pub fn is_filtered_out(&self) -> bool {
        matches!(*self.match_info.borrow(), MatchInfo::DoesNotMatch)
    }

    pub fn can_be_entered(&self) -> bool {
        self.suggestion.kind == suggestion_database::entry::Kind::Module
    }

    pub fn update_matching_info(&self, pattern: impl Str) {
        let label = self.label();
        let matches = fuzzly::matches(label, pattern.as_ref());
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

#[derive(Clone, CloneRef, Debug)]
pub struct ModuleGroups {
    pub group:     Group,
    pub subgroups: group::List,
}

#[derive(Clone, Debug)]
pub struct List {
    pub logger:                Logger,
    pub all_components:        Vec<Component>,
    pub top_modules:           group::List,
    pub top_modules_flattened: group::List,
    pub module_groups:         HashMap<suggestion_database::entry::Id, ModuleGroups>,
    pub favorites:             group::List,
    pub filtered:              Cell<bool>,
}

impl List {
    pub fn top_modules(&self) -> &group::List {
        if self.filtered.get() {
            &self.top_modules_flattened
        } else {
            &self.top_modules
        }
    }

    pub fn submodules_of(&self, component: &Component) -> Option<&group::List> {
        self.module_groups.get(&*component.suggestion_id).map(|mg| &mg.subgroups)
    }

    pub fn update_filtering(&self, pattern: impl Str) {
        for component in &self.all_components {
            component.update_matching_info(pattern.as_ref())
        }
        for group in self.all_groups() {
            group.check_visibility();
        }
        self.filtered.set(!pattern.as_ref().is_empty());
    }

    fn all_groups(&self) -> impl Iterator<Item = &Group> {
        let normal = self.module_groups.values().map(|mg| &mg.group);
        let flattened = self.top_modules_flattened.iter();
        normal.chain(flattened)
    }
}

#[derive(Clone, Debug)]
pub struct ModuleGroupsBuilder {
    pub group:           Group,
    pub flattened_group: Option<Group>,
    pub subgroups:       group::ListBuilder,
    pub is_top_module:   bool,
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
            is_top_module:   module.module.is_top_module(),
        }
    }

    fn build(self) -> ModuleGroups {
        ModuleGroups { group: self.group, subgroups: self.subgroups.build() }
    }
}

#[derive(Clone, Debug)]
pub struct ListBuilder {
    pub suggestion_db:  Rc<model::SuggestionDatabase>,
    pub all_components: Vec<Component>,
    pub module_groups:  HashMap<suggestion_database::entry::Id, ModuleGroupsBuilder>,
    pub favorites:      Vec<Group>,
}

impl ListBuilder {
    pub fn new(suggestion_db: Rc<model::SuggestionDatabase>) -> Self {
        Self {
            suggestion_db,
            all_components: default(),
            module_groups: default(),
            favorites: default(),
        }
    }

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
            DEBUG!("Putting component {component.suggestion_id:?}");
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

    pub fn add_favorites(
        &mut self,
        groups: impl IntoIterator<Item = execution_context::ComponentGroup>,
    ) {
        let suggestion_db = &self.suggestion_db;
        let favorites = groups
            .into_iter()
            .map(|g| Group::from_execution_context_component_group(g, suggestion_db));
        self.favorites.extend(favorites);
    }

    fn lookup_module_group(
        &mut self,
        module: &module::QualifiedName,
    ) -> Option<&mut ModuleGroupsBuilder> {
        let (module_id, db_entry) = self.suggestion_db.lookup_by_qualified_name(module)?;

        // Note: My heart is bleeding at this point, but because of lifetime checker limitations
        // we must do it in this suboptimal way.
        //
        // See https://users.rust-lang.org/t/returning-a-mutable-reference-extends-its-lifetime/28643
        // for example of similar problem.
        if self.module_groups.contains_key(&module_id) {
            self.module_groups.get_mut(&module_id)
        } else {
            let groups = ModuleGroupsBuilder::new(&*db_entry);
            if let Some(module) = module.parent_module() {
                if let Some(parent_groups) = self.lookup_module_group(&module) {
                    parent_groups.subgroups.push(groups.group.clone_ref())
                }
            }
            Some(self.module_groups.entry(module_id).or_insert(groups))
        }
    }

    fn build(self) -> List {
        let top_modules_iter = self.module_groups.values().filter(|g| g.is_top_module);
        let mut top_mdl_bld = group::ListBuilder::default();
        top_mdl_bld.extend(top_modules_iter.clone().map(|g| g.group.clone_ref()));
        let mut top_mdl_flat_bld = group::ListBuilder::default();
        top_mdl_flat_bld.extend(top_modules_iter.filter_map(|g| g.flattened_group.clone()));
        List {
            logger:                Logger::new("searcher::component::List"),
            all_components:        self.all_components,
            top_modules:           top_mdl_bld.build(),
            top_modules_flattened: top_mdl_flat_bld.build(),
            module_groups:         self
                .module_groups
                .into_iter()
                .map(|(id, group)| (id, group.build()))
                .collect(),
            favorites:             group::List::new_unsorted(self.favorites),
            filtered:              default(),
        }
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::suggestion_database::entry::QualifiedName;
    use engine_protocol::language_server;
    use ensogl::data::color;

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
        let sub_module_3 = mock_module("test.Test.TopModule2.SubModule3");
        let fun1 = mock_function(&top_module_1.module, "fun1");
        let fun2 = mock_function(&top_module_1.module, "fun2");
        let fun3 = mock_function(&sub_module_1.module, "fun3");
        let fun4 = mock_function(&sub_module_2.module, "fun4");
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
        ];

        let suggestion_db = model::SuggestionDatabase::new_empty(logger);
        for (id, entry) in all_entries.into_iter().enumerate() {
            suggestion_db.put_entry(id, entry)
        }
        suggestion_db
    }

    #[test]
    fn favorites_in_component_list() {
        let logger = Logger::new("tests::favorites_in_component_list");
        let suggestion_db = Rc::new(mock_suggestion_db(logger));
        let mut builder = ListBuilder::new(suggestion_db);
        builder.add_favorites([
            execution_context::ComponentGroup {
                name:       "Test Group 1".into(),
                color:      color::Rgb::from_css_hex("#aabbcc"),
                components: vec![
                    "Standard.Base.System.File.new".into(),
                    "local.Unnamed_10.Main.main".into(),
                ],
            },
            execution_context::ComponentGroup {
                name:       "Input".into(),
                color:      None,
                components: vec!["Standard.Base.System.File.new".into()],
            },
        ]);
        let list = builder.build();
    }

    #[test]
    fn module_groups_in_component_list() {
        let logger = Logger::new("tests::module_groups_in_component_list");
        let suggestion_db = Rc::new(mock_suggestion_db(logger));
        let mut builder = ListBuilder::new(suggestion_db);
        let first_part = (0..3).chain(6..9);
        let second_part = 3..6;
        builder.extend(first_part);
        builder.extend(second_part);
        let list = builder.build();
        let top_modules = list.top_modules();
        assert_eq!(top_modules.len(), 2);
        assert_eq!(top_modules[0].name, "test.Test.TopModule1");
        assert_eq!(top_modules[1].name, "test.Test.TopModule2");

        assert_eq!(top_modules[0].entries.borrow()[0].suggestion.name, "foo1");
    }
}
