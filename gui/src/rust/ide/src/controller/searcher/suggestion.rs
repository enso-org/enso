//! All structures related to the suggestion list provided by SearcherController.

use crate::prelude::*;


// ===================
// === Suggestion ===
// ===================

/// Suggestion for input completion: possible functions, arguments, etc.
pub type Completion = Rc<model::suggestion_database::Entry>;

/// A single suggestion on the Searcher suggestion list.
#[derive(Clone,CloneRef,Debug,Eq,PartialEq)]
pub enum Suggestion {
    /// Suggestion for input completion: possible functions, arguments, etc.
    Completion(Completion)
    // In future, other suggestion types will be added (like suggestions of actions, etc.).
}

impl Suggestion {
    /// The suggestion caption (suggested function name, or action name, etc.).
    pub fn caption(&self) -> String {
        match self {
            Self::Completion(completion) => completion.code_to_insert(None)
        }
    }
}



// ==================
// === List Entry ===
// ==================

/// Information how the Suggestion list entry matches the filtering pattern.
#[allow(missing_docs)]
#[derive(Clone,Debug,PartialEq)]
pub enum MatchInfo {
    DoesNotMatch,
    Matches {subsequence:fuzzly::Subsequence}
}

/// The single suggestion list entry.
#[allow(missing_docs)]
#[derive(Clone,Debug)]
pub struct ListEntry {
    pub match_info : MatchInfo,
    pub suggestion : Suggestion,
}

impl ListEntry {
    /// Update the current match info according to the new filtering pattern.
    pub fn update_matching_info(&mut self, pattern:impl Str) {
        let matches     = fuzzly::matches(self.suggestion.caption(),pattern.as_ref());
        let subsequence = matches.and_option_from(|| {
            let metric = fuzzly::metric::default();
            fuzzly::find_best_subsequence(self.suggestion.caption(),pattern,metric)
        });
        self.match_info = match subsequence {
            Some(subsequence) => MatchInfo::Matches {subsequence},
            None              => MatchInfo::DoesNotMatch,
        };
    }

    /// Compare how two entries matches filtering pattern.
    ///
    /// The "greater" entry matches better.
    pub fn compare_match_scores(&self, rhs:&ListEntry) -> std::cmp::Ordering {
        use MatchInfo::*;
        use std::cmp::Ordering::*;
        match (&self.match_info,&rhs.match_info) {
            (DoesNotMatch             ,DoesNotMatch             ) => Equal,
            (DoesNotMatch             ,Matches {..}             ) => Less,
            (Matches {..}             ,DoesNotMatch             ) => Greater,
            (Matches {subsequence:lhs},Matches {subsequence:rhs}) => lhs.compare_scores(rhs),
        }
    }
}

impl From<Suggestion> for ListEntry {
    fn from(suggestion:Suggestion) -> Self {
        let subsequence = default();
        let match_info  = MatchInfo::Matches {subsequence};
        ListEntry {match_info,suggestion}
    }
}



// ============
// === List ===
// ============

/// Suggestion list.
///
/// This structure should be notified about filtering changes. using `update_filtering` function.
#[derive(Clone,Debug)]
pub struct List {
    entries  : RefCell<Vec<ListEntry>>,
    matching : CloneCell<Range<usize>>,
}

impl Default for List {
    fn default() -> Self {
        List {
            entries  : default(),
            matching : CloneCell::new(0..0),
        }
    }
}

impl List {
    /// Create new empty list.
    pub fn new() -> Self {
        default()
    }

    /// Create list from suggestions.
    ///
    /// The list will assume that the filtering pattern is an empty string.
    pub fn from_suggestions(suggestions:impl IntoIterator<Item=Suggestion>) -> Self {
        let entries = suggestions.into_iter().map(ListEntry::from).collect_vec();
        let len     = entries.len();
        Self {
            entries  : RefCell::new(entries),
            matching : CloneCell::new(0..len),
        }
    }

    /// Update the list filtering.
    ///
    /// The "matching score" of each entry is recalculated against the given pattern and the entries
    /// are re-ordered, so the best matches will go first.
    pub fn update_filtering(&self, pattern:impl Str) {
        let mut entries_mut = self.entries.borrow_mut();
        for entry in entries_mut.iter_mut() {
            entry.update_matching_info(pattern.as_ref());
        }
        entries_mut.sort_by(|l,r| l.compare_match_scores(r).reverse());
        let not_matching       = |e:&&ListEntry| e.match_info == MatchInfo::DoesNotMatch;
        let first_not_matching = entries_mut.iter().find_position(not_matching);
        let matches_end        = first_not_matching.map_or(entries_mut.len(), |(id,_)| id);
        self.matching.set(0..matches_end);
    }

    /// Length of the suggestion list.
    pub fn len(&self) -> usize { self.entries.borrow().len() }

    /// Number of currently matching entries.
    pub fn matching_count(&self) -> usize { self.matching.get().end }

    /// Get the suggestion list entry
    pub fn get_cloned(&self, index:usize) -> Option<ListEntry> {
        self.entries.borrow().get(index).cloned()
    }

    /// Check if list is empty.
    pub fn is_empty(&self) -> bool { self.entries.borrow().is_empty() }

    /// Iterate over suggestion entries.
    pub fn iter<'a>(&'a self) -> impl Iterator<Item=ListEntry> + 'a {
        let existing_ids = (0..self.len()).take_while(move |id| *id < self.len());
        existing_ids.filter_map(move |id| self.entries.borrow().get(id).cloned())
    }

    /// Extend the list with new suggestions.
    ///
    /// The new suggestions will be put at end, regardless the current filtering. This function
    /// is meant to be a part of list's initialization.
    pub fn extend<T:IntoIterator<Item=Suggestion>>(&self, iter: T) {
        self.entries.borrow_mut().extend(iter.into_iter().map(ListEntry::from));
        self.matching.set(0..self.len())
    }

    /// Convert to the suggestion vector.
    ///
    /// Used for testing.
    pub fn to_suggestion_vec(&self) -> Vec<Suggestion> {
        self.entries.borrow().iter().map(|entry| entry.suggestion.clone_ref()).collect()
    }
}

impl<IntoIter> From<IntoIter> for List
where IntoIter : IntoIterator<Item=Suggestion> {
    fn from(suggestions:IntoIter) -> Self {
        Self::from_suggestions(suggestions)
    }
}
