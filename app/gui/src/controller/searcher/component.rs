//! A module containing definition of [`Component`] and its [`List`]
//!
//! Component is a language entity displayed in the Component Browser.

use crate::prelude::*;

use crate::controller::graph::RequiredImport;
use crate::controller::searcher::search;
use crate::controller::searcher::search::search;
use crate::controller::searcher::Filter;
use crate::model::execution_context::GroupQualifiedName;

use enso_doc_parser::DocSection;
use enso_doc_parser::Tag;
use enso_suggestion_database::entry;
use enso_suggestion_database::Entry;
use ensogl::data::color;
use std::cmp;
use superslice::Ext;


// ==============
// === Export ===
// ==============

pub mod builder;
pub mod hardcoded;

pub use builder::Builder;



// =============
// === Group ===
// =============

/// Group properties.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Group {
    pub name:  GroupQualifiedName,
    /// Color as defined in project's `package.yaml` file.
    pub color: Option<color::Rgb>,
}



// =================
// === Component ===
// =================

// === MatchInfo ===

/// Information how the list entry matches the filtering pattern.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum MatchInfo {
    DoesNotMatch,
    Matches { subsequence: search::Subsequence, alias: Option<usize> },
}


// === Suggestion ===

/// Code suggestion.
///
/// It's a part of [`Component`], containing the data required for generating code suggested by
/// given component.
#[derive(Clone, Debug, PartialEq)]
pub enum Suggestion {
    /// A component from the [`suggestion_database`]. When this component is picked in the
    /// Component Browser, the code returned by [`Entry::code_to_insert`] will
    /// be inserted into the program.
    FromDatabase {
        /// The ID of the component in the [`suggestion_database`].
        id:    entry::Id,
        /// The component's entry in the [`suggestion_database`].
        entry: Rc<Entry>,
    },
    /// A virtual component containing a hardcoded snippet of code. When this component is picked
    /// in the Component Browser, the [`Snippet::code`] will be inserted into the program.
    Virtual {
        /// A hardcoded snippet of code.
        snippet: Rc<hardcoded::Snippet>,
    },
}

impl Suggestion {
    /// The name of the suggested component.
    pub fn name(&self) -> &str {
        match self {
            Self::FromDatabase { entry, .. }
                if entry.kind == entry::Kind::Module && entry.defined_in.is_main_module() =>
                entry.defined_in.alias_name().as_str(),
            Self::FromDatabase { entry, .. } => entry.name.as_str(),
            Self::Virtual { snippet } => snippet.name.as_str(),
        }
    }

    /// The import required by this suggestion.
    pub fn required_import(&self) -> Option<RequiredImport> {
        match self {
            Self::FromDatabase { entry, .. } => Some(RequiredImport::Entry(entry.clone_ref())),
            Self::Virtual { .. } => None,
        }
    }
}


// === Component ===

/// A single component entry to be displayed in the Component Browser.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Component {
    pub suggestion: Suggestion,
    /// A group id, being an index of `group` field of [`List`] structure.
    pub group_id:   Option<usize>,
    /// The string representation that will be used during matching. This is the name, prefixed
    /// with a module and type if needed.
    label:          ImString,
    /// Aliases for the component; used during matching.
    aliases:        Rc<[ImString]>,
    /// Results of matching this component against the current filter, if any.
    match_info:     Option<MatchInfo>,
}

impl Component {
    /// Construct a new component from a [`suggestion_database`] entry.
    pub fn new_from_database_entry(
        id: entry::Id,
        entry: Rc<Entry>,
        group_id: Option<usize>,
    ) -> Self {
        let label = match entry.kind {
            entry::Kind::Module
                if entry.defined_in.is_main_module() || entry.defined_in.is_top_element() =>
                format!("{}", entry.defined_in).into(),
            _ => match entry.self_type.as_ref() {
                Some(self_type) => format!("{}.{}", self_type.alias_name(), entry.name).into(),
                None => entry.name.to_im_string(),
            },
        };
        let aliases =
            entry.aliases().map(|alias| format!("{alias} ({label})").into()).collect_vec().into();
        let data = Suggestion::FromDatabase { id, entry };
        Self { suggestion: data, label, aliases, group_id, match_info: default() }
    }

    /// Construct a new component without any associated [`suggestion_database`] entry.
    pub fn new_virtual(snippet: Rc<hardcoded::Snippet>, group_index: usize) -> Self {
        Self {
            label:      snippet.name.clone(),
            aliases:    Rc::new([]),
            suggestion: Suggestion::Virtual { snippet },
            group_id:   Some(group_index),
            match_info: Default::default(),
        }
    }

    /// The label as should be displayed in the Component Browser. Formatting and highlighting is
    /// determined by the current filter pattern.
    pub fn matched_label(&self) -> HighlightedString {
        let text = match &self.match_info {
            Some(MatchInfo::Matches { alias: Some(i), .. }) =>
                self.aliases.get(*i).cloned().unwrap_or_default(),
            _ => self.label.clone(),
        };
        let highlights = match &self.match_info {
            Some(MatchInfo::Matches { subsequence, .. }) =>
                subsequence.match_indexes.byte_ranges(&text).collect(),
            _ => default(),
        };
        HighlightedString { text, highlights }
    }

    /// The [ID](entry::Id) of the component in the [`suggestion_database`], or
    /// `None` if not applicable.
    pub fn id(&self) -> Option<entry::Id> {
        match &self.suggestion {
            Suggestion::FromDatabase { id, .. } => Some(*id),
            Suggestion::Virtual { .. } => None,
        }
    }

    /// Checks if component is filtered out.
    pub fn is_filtered_out(&self) -> bool {
        matches!(self.match_info, Some(MatchInfo::DoesNotMatch))
    }

    /// Checks if the component can be entered in Component Browser.
    ///
    /// Currently, only modules can be entered, and then the Browser should display content and
    /// submodules of the entered module.
    pub fn can_be_entered(&self) -> bool {
        use entry::Kind as EntryKind;
        matches!(&self.suggestion, Suggestion::FromDatabase { entry, .. } if entry.kind == EntryKind::Module)
    }

    /// Update matching info.
    ///
    /// It should be called each time the filtering pattern changes.
    pub fn update_matching_info(&mut self, filter: Filter) {
        let filter_enabled = filter.context.is_some() || !filter.pattern.is_empty();
        let excluded_by_context = filter
            .context
            .map(|context| match &self.suggestion {
                Suggestion::FromDatabase { entry, .. } =>
                    !entry.qualified_name().to_string().contains(context.as_str()),
                Suggestion::Virtual { .. } => true,
            })
            .unwrap_or_default();
        let match_info = filter_enabled.then(|| {
            if !excluded_by_context {
                self.match_info_for_pattern(&filter.pattern)
            } else {
                MatchInfo::DoesNotMatch
            }
        });
        self.match_info = match_info;
    }

    fn match_info_for_pattern(&self, pattern: &str) -> MatchInfo {
        // Match the input pattern to the component label.
        let label_match = search(&self.label, pattern, search::TargetInfo { is_alias: false })
            .map(|subsequence| MatchInfo::Matches { subsequence, alias: None });

        // Match the input pattern to an entry's aliases and select the best alias match.
        let alias_matches = self.aliases.iter().enumerate().filter_map(|(i, label_with_alias)| {
            let (alias, _) = label_with_alias.split_once(' ').unwrap_or_default();
            search(alias, pattern, search::TargetInfo { is_alias: true })
                .map(|subsequence| MatchInfo::Matches { subsequence, alias: Some(i) })
        });

        // Select the best match of the available matches.
        let best_match_info = label_match.into_iter().chain(alias_matches).max();
        best_match_info.unwrap_or(MatchInfo::DoesNotMatch)
    }

    /// Check whether the component contains the "PRIVATE" tag.
    pub fn is_private(&self) -> bool {
        match &self.suggestion {
            Suggestion::FromDatabase { entry, .. } => entry
                .documentation
                .iter()
                .any(|doc| matches!(doc, DocSection::Tag { tag: Tag::Private, .. })),
            _ => false,
        }
    }
}


// === Highlighted strings ===

/// A string with some ranges identified for highlighting.
#[derive(Debug)]
pub struct HighlightedString {
    /// The string.
    pub text:       ImString,
    /// The ranges of characters in the string that should be highlighted.
    pub highlights: Vec<enso_text::Range<enso_text::Byte>>,
}



// ============
// === List ===
// ============

/// The Component List.
///
/// The list is created using [`Builder`] for a specific Component Browser input context. Then the
/// filtering may be applied with [`Self::update_filtering`] method. [`Self::displayed`] returns the
/// list of components which ought to be displayed with current filtering.
///
/// Please note, that even without filtering [`Self::displayed`] may not return all components,
/// depending on the mode the list was built: see [`Builder`] docs for details.
#[derive(Clone, Debug, Default)]
pub struct List {
    pub(crate) filtered_in:          Option<RangeTo<usize>>,
    pub(crate) components:           Vec<Component>,
    pub(crate) displayed_by_default: Vec<Component>,
    pub(crate) groups:               Vec<Group>,
}

impl List {
    /// Return a slice of the currently displayed component.
    ///
    /// The filtering applied with [`Self::update_filtering`] method will be taken into account.
    pub fn displayed(&self) -> &[Component] {
        if let Some(range) = self.filtered_in {
            &self.components[range]
        } else {
            &self.displayed_by_default
        }
    }

    /// Get description of all component groups.
    pub fn groups(&self) -> &[Group] {
        &self.groups
    }
    /// Returns true if the list is currently filtered.
    pub fn is_filtered(&self) -> bool {
        self.filtered_in.is_some()
    }

    /// Update list filtering.
    ///
    /// If the filtering pattern is not empty, the components will be sorted by match score (best
    /// match first), and [`Self::displayed`] will return only matched entries. Otherwise
    /// [`Self::displayed`] will return a "default" view, which depend on the context - see
    /// [structure docs](List) for details.
    pub fn update_filtering(&mut self, filter: Filter) {
        if filter.pattern.trim().is_empty() {
            self.filtered_in = None;
        } else {
            for component in &mut self.components {
                component.update_matching_info(filter.clone_ref());
            }
            self.components
                .sort_by(|lhs, rhs| Self::entry_match_ordering(&lhs.match_info, &rhs.match_info));
            let first_non_matching =
                self.components.lower_bound_by_key(&true, |entry| entry.is_filtered_out());
            self.filtered_in = Some(..first_non_matching);
        }
    }

    /// Return the entry match ordering when sorting by match. See [`component::Order::ByMatch`].
    fn entry_match_ordering(lhs: &Option<MatchInfo>, rhs: &Option<MatchInfo>) -> cmp::Ordering {
        lhs.cmp(rhs).reverse()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    use double_representation::name::QualifiedName;
    use enso_suggestion_database::mock_suggestion_database;

    pub fn check_displayed_components(list: &List, expected: Vec<&str>) {
        let components =
            list.displayed().iter().map(|component| component.matched_label().text).collect_vec();
        assert_eq!(components, expected);
    }

    pub fn check_groups(list: &List, expected: Vec<Option<usize>>) {
        let groups = list.displayed().iter().map(|component| component.group_id).collect_vec();
        assert_eq!(groups, expected);
    }

    #[test]
    fn filtering() {
        let db = mock_suggestion_database! {
            local.New_Project_1 {
                fn main() -> Standard.Base.Any;
            }
            test.Test {
                mod TopModule1 {
                    fn foo() -> Standard.Base.Any;
                    fn bar() -> Standard.Base.Any;

                    mod SubModule1 {
                        fn bazz() -> Standard.Base.Any;
                    }
                }
            }
        };

        let mut builder = Builder::new_empty(&db);
        builder.add_components_from_db(db.keys());
        let mut list = builder.build();

        let module_name = Rc::new(QualifiedName::from_text("local.New_Project_1").unwrap());
        let make_filter = |pat: &str| Filter {
            pattern:     pat.into(),
            context:     None,
            module_name: module_name.clone_ref(),
        };
        check_displayed_components(&list, vec!["test.Test.TopModule1"]);
        list.update_filtering(make_filter("main"));
        check_displayed_components(&list, vec!["New_Project_1.main"]);
        list.update_filtering(make_filter("fo"));
        check_displayed_components(&list, vec!["TopModule1.foo"]);
        list.update_filtering(make_filter("ba"));
        check_displayed_components(&list, vec!["TopModule1.bar", "SubModule1.bazz"]);
        list.update_filtering(make_filter(""));
        check_displayed_components(&list, vec!["test.Test.TopModule1"]);
    }
}
