//! All structures related to the suggestion list provided by SearcherController.

use crate::prelude::*;

use crate::model::module::MethodId;
use crate::model::suggestion_database::entry::CodeToInsert;

use double_representation::module;


// ==============
// === Export ===
// ==============

pub mod hardcoded;



// ==============
// === Action ===
// ==============

#[derive(Clone, CloneRef, Debug, Eq, PartialEq)]
/// Suggestion for code completion: possible functions, arguments, etc.
pub enum Suggestion {
    /// The suggestion from Suggestion Database received from the Engine.
    FromDatabase(Rc<model::suggestion_database::Entry>),
    /// The one of the hard-coded suggestion.
    Hardcoded(Rc<hardcoded::Suggestion>),
}

impl Suggestion {
    /// Return the code to be inserted in searcher input upon picking suggestion.
    pub fn code_to_insert(
        &self,
        current_module: Option<&module::QualifiedName>,
        generate_this: bool,
    ) -> CodeToInsert {
        match self {
            Suggestion::FromDatabase(s) => s.code_to_insert(current_module, generate_this),
            Suggestion::Hardcoded(s) => CodeToInsert {
                code:    s.code.to_owned(),
                imports: s.imports.iter().cloned().collect(),
            },
        }
    }

    /// Return the expected arguments to be added after picking the suggestion.
    pub fn argument_types(&self) -> Vec<String> {
        match self {
            Suggestion::FromDatabase(suggestion) =>
                suggestion.arguments.iter().map(|a| a.repr_type.clone()).collect(),
            Suggestion::Hardcoded(suggestion) =>
                suggestion.argument_types.iter().map(|t| t.into()).collect(),
        }
    }

    /// Return the documentation assigned to the suggestion.
    pub fn documentation_html(&self) -> Option<&str> {
        match self {
            Suggestion::FromDatabase(s) => s.documentation_html.as_ref().map(AsRef::<str>::as_ref),
            Suggestion::Hardcoded(s) => s.documentation_html,
        }
    }

    /// The Id of the method called by a suggestion, or [`None`] if the suggestion is not a method
    /// call.
    pub fn method_id(&self) -> Option<MethodId> {
        match self {
            Suggestion::FromDatabase(s) => s.method_id(),
            Suggestion::Hardcoded(s) => s.method_id.clone(),
        }
    }
}

/// Action of adding example code.
pub type Example = Rc<model::suggestion_database::Example>;

/// A variants of project management actions. See also [`Action`].
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, Eq, PartialEq)]
pub enum ProjectManagement {
    CreateNewProject,
    OpenProject { id: Immutable<Uuid>, name: ImString },
}

/// A single action on the Searcher list. See also `controller::searcher::Searcher` docs.
#[derive(Clone, CloneRef, Debug, Eq, PartialEq)]
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

impl Action {
    /// Get the name of the icon associated with given action.
    pub fn icon(&self) -> ImString {
        use Suggestion::*;
        match self {
            Self::Suggestion(Hardcoded(s)) => s.icon.clone_ref(),
            _ => hardcoded::ICONS.with(|ics| ics.default.clone_ref()),
        }
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Suggestion(Suggestion::FromDatabase(suggestion)) =>
                if let Some(self_type) = suggestion.self_type.as_ref() {
                    let should_put_project_name = self_type.name
                        == ast::constants::PROJECTS_MAIN_MODULE
                        && self_type.module_segments.is_empty();
                    let self_type_name = if should_put_project_name {
                        self_type.project_name.project.as_ref()
                    } else {
                        &self_type.name
                    };
                    write!(f, "{}.{}", self_type_name, suggestion.name)
                } else {
                    write!(f, "{}", suggestion.name.clone())
                },
            Self::Suggestion(Suggestion::Hardcoded(suggestion)) =>
                Display::fmt(&suggestion.name, f),
            Self::Example(example) => write!(f, "Example: {}", example.name),
            Self::ProjectManagement(ProjectManagement::CreateNewProject) =>
                write!(f, "New Project"),
            Self::ProjectManagement(ProjectManagement::OpenProject { name, .. }) =>
                Display::fmt(name, f),
        }
    }
}



// ==================
// === Categories ===
// ==================

/// The Category Identifier: the index on the category list in [`List`].
pub type CategoryId = usize;

/// The Root category. See also [`Category`].
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct RootCategory {
    pub name: Cow<'static, str>,
    pub icon: ImString,
}

/// The category of suggestions.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Subcategory {
    pub name:   Cow<'static, str>,
    pub icon:   ImString,
    /// The id of the root category this category belongs to.
    pub parent: CategoryId,
}



// ==================
// === List Entry ===
// ==================

/// Information how the list entry matches the filtering pattern.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum MatchInfo {
    DoesNotMatch,
    Matches { subsequence: fuzzly::Subsequence },
}

impl Default for MatchInfo {
    fn default() -> Self {
        Self::Matches { subsequence: default() }
    }
}

impl Ord for MatchInfo {
    /// Compare Match infos: the better matches are greater. The scores are compared using the full
    /// ordering as described in [`fuzzly::Subsequence::compare_scores`].
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        use MatchInfo::*;
        match (&self, &rhs) {
            (DoesNotMatch, DoesNotMatch) => Equal,
            (DoesNotMatch, Matches { .. }) => Less,
            (Matches { .. }, DoesNotMatch) => Greater,
            (Matches { subsequence: lhs }, Matches { subsequence: rhs }) => lhs.compare_scores(rhs),
        }
    }
}

impl PartialOrd for MatchInfo {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl PartialEq for MatchInfo {
    fn eq(&self, rhs: &Self) -> bool {
        self.cmp(rhs) == std::cmp::Ordering::Equal
    }
}

impl Eq for MatchInfo {}


/// The single list entry.
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ListEntry {
    pub category:   CategoryId,
    pub match_info: MatchInfo,
    pub action:     Action,
}

impl ListEntry {
    /// Update the current match info according to the new filtering pattern.
    pub fn update_matching_info(&mut self, pattern: impl Str) {
        let matches = fuzzly::matches(self.action.to_string(), pattern.as_ref());
        let subsequence = matches.and_option_from(|| {
            let metric = fuzzly::metric::default();
            fuzzly::find_best_subsequence(self.action.to_string(), pattern, metric)
        });
        self.match_info = match subsequence {
            Some(subsequence) => MatchInfo::Matches { subsequence },
            None => MatchInfo::DoesNotMatch,
        };
    }

    /// Returns true if the entry matches current filtering pattern.
    pub fn matches(&self) -> bool {
        matches!(self.match_info, MatchInfo::Matches { .. })
    }

    /// The ordering on the action list: first, are the matched entries are gathered on the top of
    /// the list, then sorted by categories, and those of same category are ordered by match score
    /// (the best matches are first).
    #[profile(Debug)]
    pub fn ordering_on_list(&self, rhs: &Self) -> std::cmp::Ordering {
        self.matches()
            .cmp(&rhs.matches())
            .reverse()
            .then_with(|| self.category.cmp(&rhs.category))
            .then_with(|| self.match_info.cmp(&rhs.match_info).reverse())
    }
}



// ============
// === List ===
// ============

/// Action list.
///
/// The structure contains also information about all the categories the actions belongs to. It
/// also supports updating list when filtering pattern changes (See [`List::update_filtering`]
/// function).
#[derive(Clone, Debug)]
pub struct List {
    /// List of root categories. It should be immutable in constructed list, as categories refers
    /// to the indexes in this vector.
    root_categories: Vec<RootCategory>,
    /// List of subcategories. It should be immutable in constructed list, as actions refers
    /// to the indexes in this vector. It also have to be sorted by parent category id: this is
    /// ensured by [`ListBuilder`].
    subcategories:   Vec<Subcategory>,
    /// The all entries in actions list. It should be kept ordered as defined by
    /// [`ListEntry::ordering_on_list`] function.
    entries:         RefCell<Vec<ListEntry>>,
    /// The range of matching entries. Should be kept up-to-date after each `entries` change.
    matching:        CloneCell<Range<usize>>,
}

impl Default for List {
    fn default() -> Self {
        List {
            root_categories: default(),
            subcategories:   default(),
            entries:         default(),
            matching:        CloneCell::new(0..0),
        }
    }
}

impl List {
    /// Create new empty list.
    pub fn new() -> Self {
        default()
    }

    /// Update the list filtering.
    ///
    /// The "matching score" of each entry is recalculated against the given pattern and the entries
    /// are re-ordered, so the best matches will go first.
    #[profile(Debug)]
    pub fn update_filtering(&self, pattern: impl Str) {
        {
            let mut entries_mut = self.entries.borrow_mut();
            for entry in entries_mut.iter_mut() {
                entry.update_matching_info(pattern.as_ref());
            }
        }
        self.update_sorting();
    }

    /// Length of the actions list.
    pub fn len(&self) -> usize {
        self.entries.borrow().len()
    }

    /// Number of currently matching entries.
    pub fn matching_count(&self) -> usize {
        self.matching.get().end
    }

    /// Get the action list entry
    pub fn get_cloned(&self, index: usize) -> Option<ListEntry> {
        self.entries.borrow().get(index).cloned()
    }

    /// Check if list is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.borrow().is_empty()
    }

    /// Iterate over root categories.
    pub fn root_categories(&self) -> impl Iterator<Item = (CategoryId, &RootCategory)> {
        self.root_categories.iter().enumerate()
    }

    /// Iterate over all subcategories of given root category.
    pub fn subcategories_of(
        &self,
        id: CategoryId,
    ) -> impl Iterator<Item = (CategoryId, &Subcategory)> {
        let start = self.subcategories.partition_point(|cat| cat.parent < id);
        let end = self.subcategories.partition_point(|cat| cat.parent <= id);
        (start..end).zip(self.subcategories[start..end].iter())
    }

    /// Iterate over all actions of given subcategory.
    pub fn actions_of(&self, id: CategoryId) -> impl Iterator<Item = (usize, ListEntry)> + '_ {
        let range = {
            let actions = self.entries.borrow();
            let start = actions.partition_point(|entry| entry.category < id);
            let end = actions.partition_point(|entry| entry.category <= id);
            start..end
        };
        self.actions_range(range)
    }

    /// Iterate over action entries.
    pub fn actions(&self) -> impl Iterator<Item = (usize, ListEntry)> + '_ {
        self.actions_range(0..self.len())
    }

    fn actions_range(&self, range: Range<usize>) -> impl Iterator<Item = (usize, ListEntry)> + '_ {
        let existing_ids = range.take_while(move |id| *id < self.len());
        existing_ids.filter_map(move |id| self.entries.borrow().get(id).cloned().map(|e| (id, e)))
    }

    /// Convert to the action vector.
    ///
    /// Used for testing.
    pub fn to_action_vec(&self) -> Vec<Action> {
        self.entries.borrow().iter().map(|entry| entry.action.clone_ref()).collect()
    }

    #[profile(Debug)]
    fn update_sorting(&self) {
        let mut entries_mut = self.entries.borrow_mut();
        entries_mut.sort_by(|l, r| l.ordering_on_list(r));
        let first_not_matching = entries_mut.partition_point(|e| e.matches());
        self.matching.set(0..first_not_matching);
    }
}



// ===================
// === ListBuilder ===
// ===================

/// The List Builder.
///
/// As the [`List`] itself keeps its categories immutable, the builder is used to extend the list.
/// It ensures, that the all indexes will remain intact and in a proper order (see also fields docs
/// in [`List`] - therefore the sub-builders are designed such way, that intermixing subcategories
/// of different root-categories is impossible.
#[derive(Debug, Default)]
pub struct ListBuilder {
    built_list: List,
}

/// The builder of a single root category.
///
/// It allows to add sub-categories to the category.
#[derive(Debug)]
pub struct RootCategoryBuilder<'a> {
    list_builder:     &'a mut ListBuilder,
    root_category_id: CategoryId,
}

/// The builder of single category.
///
/// It allows to add entries to the category. The built list will have no filtering applied.
#[derive(Debug)]
pub struct CategoryBuilder<'a> {
    list_builder: &'a mut ListBuilder,
    category_id:  CategoryId,
}

impl ListBuilder {
    /// Add the new root category with a given name. The returned builder should be used to add
    /// sub-categories to it.
    pub fn add_root_category(
        &mut self,
        name: impl Into<Cow<'static, str>>,
        icon: ImString,
    ) -> RootCategoryBuilder {
        let name = name.into();
        let root_category_id = self.built_list.root_categories.len();
        self.built_list.root_categories.push(RootCategory { name, icon });
        RootCategoryBuilder { list_builder: self, root_category_id }
    }

    /// Consumes self returning built list.
    pub fn build(self) -> List {
        self.built_list.update_sorting();
        self.built_list
    }
}

impl<'a> RootCategoryBuilder<'a> {
    /// Add the category with a given name to the root category. The returned builder should be
    /// used to add actions to the newly created category.
    pub fn add_category(
        &mut self,
        name: impl Into<Cow<'static, str>>,
        icon: ImString,
    ) -> CategoryBuilder {
        let name = name.into();
        let parent = self.root_category_id;
        let category_id = self.list_builder.built_list.subcategories.len();
        self.list_builder.built_list.subcategories.push(Subcategory { name, icon, parent });
        CategoryBuilder { list_builder: self.list_builder, category_id }
    }
}

impl<'a> CategoryBuilder<'a> {
    /// Add an action to the category.
    pub fn add_action(&self, action: Action) {
        self.extend(std::iter::once(action))
    }

    /// Add many actions to the category.
    pub fn extend<T: IntoIterator<Item = Action>>(&self, iter: T) {
        let built_list = &self.list_builder.built_list;
        let category = self.category_id;
        built_list.entries.borrow_mut().extend(iter.into_iter().map(|action| {
            let match_info = MatchInfo::Matches { subsequence: default() };
            ListEntry { category, match_info, action }
        }));
    }
}


// === ListWithSearchResultBuilder ===

/// The Actions list builder which adds a special "All search result" category: it will contain
/// all entries (regardless of their categories) and is designed to be displayed upon filtering.
#[derive(Debug, Default)]
pub struct ListWithSearchResultBuilder {
    internal:               ListBuilder,
    search_result_category: CategoryId,
}

impl ListWithSearchResultBuilder {
    /// Constructor.
    pub fn new() -> Self {
        let icon = hardcoded::ICONS.with(|icons| icons.search_result.clone_ref());
        let mut internal = ListBuilder::default();
        let mut search_result_root =
            internal.add_root_category("All Search Result", icon.clone_ref());
        let search_result_category = search_result_root.add_category("All Search Result", icon);
        let search_result_category = search_result_category.category_id;
        Self { internal, search_result_category }
    }

    /// Add the new root category with a given name. The returned builder should be used to add
    /// sub-categories to it.
    pub fn add_root_category(
        &mut self,
        name: impl Into<Cow<'static, str>>,
        icon: ImString,
    ) -> RootCategoryBuilder {
        self.internal.add_root_category(name, icon)
    }

    /// Consumes self returning built list.
    pub fn build(self) -> List {
        let search_results = self.make_searcher_result_entries();
        // The entries will be sorted, so it is no problem in pushing search results at the end.
        self.internal.built_list.entries.borrow_mut().extend(search_results);
        self.internal.build()
    }

    fn make_searcher_result_entries(&self) -> Vec<ListEntry> {
        self.internal
            .built_list
            .entries
            .borrow()
            .iter()
            .map(|entry| {
                let match_info = MatchInfo::Matches { subsequence: default() };
                let action = entry.action.clone_ref();
                let category = self.search_result_category;
                ListEntry { category, match_info, action }
            })
            .collect()
    }
}
