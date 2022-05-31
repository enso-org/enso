pub mod builder;
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

pub type Id = suggestion_database::entry::Id;

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
    pub id:         Immutable<Id>,
    pub suggestion: Rc<suggestion_database::Entry>,
    pub match_info: Rc<RefCell<MatchInfo>>,
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
    pub module_groups:         HashMap<Id, ModuleGroups>,
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

    pub fn submodules_of(&self, component: Id) -> Option<&group::List> {
        self.module_groups.get(&component).map(|mg| &mg.subgroups)
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
