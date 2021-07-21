//! All structures related to the suggestion list provided by SearcherController.
use crate::prelude::*;



// ==============
// === Action ===
// ==============

/// Suggestion for code completion: possible functions, arguments, etc.
pub type Suggestion = Rc<model::suggestion_database::Entry>;

/// Action of adding example code.
pub type Example = Rc<model::suggestion_database::Example>;

/// A variants of project management actions. See also [`Action`].
#[allow(missing_docs)]
#[derive(Clone,CloneRef,Debug,Eq,PartialEq)]
pub enum ProjectManagement {
    CreateNewProject,
    OpenProject {
        id   : Immutable<Uuid>,
        name : ImString,
    }
}

/// A single action on the Searcher list. See also `controller::searcher::Searcher` docs.
#[derive(Clone,CloneRef,Debug,Eq,PartialEq)]
pub enum Action {
    /// Add to the searcher input a suggested code and commit editing (new node is inserted or
    /// existing is modified). This action can be also used to complete searcher input without
    /// committing (see `Searcher::use_as_suggestion` method).
    Suggestion(Suggestion),
    /// Add to the current module a new function with example code, and a new node in
    /// current scene calling that function.
    Example(Example),
    /// The project management operation: creating or opening, projects.
    ProjectManagement(ProjectManagement),
    // In the future, other action types will be added (like module/method management, etc.).
}

impl Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Suggestion(completion) => if let Some(self_type) = completion.self_type.as_ref() {
                let should_put_project_name = self_type.name == constants::PROJECTS_MAIN_MODULE
                    && self_type.module_segments.is_empty();
                let self_type_name = if should_put_project_name {
                    self_type.project_name.project.as_ref()
                } else { &self_type.name };
                write!(f,"{}.{}",self_type_name,completion.name)
            } else {
                write!(f, "{}", completion.name.clone())
            }
            Self::Example(example) => write!(f,"Example: {}", example.name),
            Self::ProjectManagement(action) => match action {
                ProjectManagement::CreateNewProject => write!(f,"New Project"),
                ProjectManagement::OpenProject {name,..} => write!(f,"{}", name),
            }
        }
    }
}



// ==================
// === List Entry ===
// ==================

/// Information how the list entry matches the filtering pattern.
#[allow(missing_docs)]
#[derive(Clone,Debug,PartialEq)]
pub enum MatchInfo {
    DoesNotMatch,
    Matches {subsequence:fuzzly::Subsequence}
}

/// The single list entry.
#[allow(missing_docs)]
#[derive(Clone,Debug)]
pub struct ListEntry {
    pub match_info : MatchInfo,
    pub action     : Action,
}

impl ListEntry {
    /// Update the current match info according to the new filtering pattern.
    pub fn update_matching_info(&mut self, pattern:impl Str) {
        let matches     = fuzzly::matches(self.action.to_string(),pattern.as_ref());
        let subsequence = matches.and_option_from(|| {
            let metric = fuzzly::metric::default();
            fuzzly::find_best_subsequence(self.action.to_string(),pattern,metric)
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

impl From<Action> for ListEntry {
    fn from(action:Action) -> Self {
        let subsequence = default();
        let match_info  = MatchInfo::Matches {subsequence};
        ListEntry {match_info,action}
    }
}



// ============
// === List ===
// ============

/// Action list.
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

    /// Create list from actions.
    ///
    /// The list will assume that the filtering pattern is an empty string.
    pub fn from_actions(actions:impl IntoIterator<Item=Action>) -> Self {
        let entries = actions.into_iter().map(ListEntry::from).collect_vec();
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

    /// Length of the actions list.
    pub fn len(&self) -> usize { self.entries.borrow().len() }

    /// Number of currently matching entries.
    pub fn matching_count(&self) -> usize { self.matching.get().end }

    /// Get the action list entry
    pub fn get_cloned(&self, index:usize) -> Option<ListEntry> {
        self.entries.borrow().get(index).cloned()
    }

    /// Check if list is empty.
    pub fn is_empty(&self) -> bool { self.entries.borrow().is_empty() }

    /// Iterate over action entries.
    pub fn iter(&self) -> impl Iterator<Item=ListEntry> + '_ {
        let existing_ids = (0..self.len()).take_while(move |id| *id < self.len());
        existing_ids.filter_map(move |id| self.entries.borrow().get(id).cloned())
    }

    /// Extend the list with new actions.
    ///
    /// The new actions will be put at end, regardless the current filtering. This function
    /// is meant to be a part of list's initialization.
    pub fn extend<T:IntoIterator<Item=Action>>(&self, iter: T) {
        self.entries.borrow_mut().extend(iter.into_iter().map(ListEntry::from));
        self.matching.set(0..self.len())
    }

    /// Convert to the action vector.
    ///
    /// Used for testing.
    pub fn to_action_vec(&self) -> Vec<Action> {
        self.entries.borrow().iter().map(|entry| entry.action.clone_ref()).collect()
    }
}

impl<IntoIter> From<IntoIter> for List
where IntoIter : IntoIterator<Item=Action> {
    fn from(actions:IntoIter) -> Self {
        Self::from_actions(actions)
    }
}
