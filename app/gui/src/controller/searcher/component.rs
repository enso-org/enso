use crate::model::suggestion_database;
use crate::prelude::*;

/// Information how the list entry matches the filtering pattern.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum MatchInfo {
    DoesNotMatch,
    Matches { subsequence: fuzzly::Subsequence },
}

pub struct Component {
    pub suggestion_entry: Rc<suggestion_database::Entry>,
    pub match_info:       Rc<RefCell<MatchInfo>>,
}

pub struct Group {
    pub name:             ImString,
    pub entries:          RefCell<Vec<Component>>,
    pub matching_entries: usize,
}

pub struct List {
    pub all_entries: Vec<Component>,
    pub modules:     BTreeMap<suggestion_database::entry::Id, Group>,
    pub top_modules: BTreeMap<suggestion_database::entry::Id, Group>,
}

impl List {
    pub fn iter_modules(&self) -> impl Iterator<Item = Group> {}
}
