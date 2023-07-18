use crate::prelude::*;

use convert_case::Casing;
use enso_doc_parser::DocSection;
use enso_doc_parser::Tag;
use ensogl::data::color;
use ordered_float::OrderedFloat;
use std::cmp;
use superslice::Ext;

use crate::controller::searcher::Filter;
use crate::model::suggestion_database;

pub mod builder;
pub mod hardcoded;

use crate::controller::graph::RequiredImport;
pub use builder::Builder;
use double_representation::name::project;
use enso_suggestion_database::entry;


// =================
// === Constants ===
// =================

/// A factor to multiply a component's alias match score by. It is intended to reduce the importance
/// of alias matches compared to label matches.
const ALIAS_MATCH_ATTENUATION_FACTOR: f32 = 0.75;



// =================
// === MatchInfo ===
// =================

/// Which part of the component browser entry was best matched to the searcher input.
#[derive(Clone, Debug, Default)]
pub enum MatchKind {
    /// The entry's label to be displayed in the component browser was matched.
    #[default]
    Label,
    /// The entry's name from the code was matched.
    Name,
    /// An alias of the entry was matched, contains the specific alias that was matched.
    Alias(ImString),
}

/// Information how the list entry matches the filtering pattern.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum MatchInfo {
    DoesNotMatch,
    Matches { subsequence: fuzzly::Subsequence, kind: MatchKind },
}

impl Default for MatchInfo {
    fn default() -> Self {
        Self::Matches { subsequence: default(), kind: default() }
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
            (Matches { subsequence: lhs, .. }, Matches { subsequence: rhs, .. }) =>
                OrderedFloat(lhs.score).cmp(&OrderedFloat(rhs.score)),
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



// ==================
// === Suggestion ===
// ==================

#[derive(Clone, Debug, PartialEq)]
pub enum Suggestion {
    /// A component from the [`suggestion_database`]. When this component is picked in the
    /// Component Browser, the code returned by [`suggestion_database::Entry::code_to_insert`] will
    /// be inserted into the program.
    FromDatabase {
        /// The ID of the component in the [`suggestion_database`].
        id:    suggestion_database::entry::Id,
        /// The component's entry in the [`suggestion_database`].
        entry: Rc<suggestion_database::Entry>,
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

    pub fn required_import(&self) -> Option<RequiredImport> {
        match self {
            Self::FromDatabase { entry, .. } => Some(RequiredImport::Entry(entry.clone_ref())),
            Self::Virtual { .. } => None,
        }
    }
}



// =============
// === Group ===
// =============

#[derive(Clone, Debug, Default)]
pub struct Group {
    pub project: project::QualifiedName,
    pub name:    ImString,
    pub color:   Option<color::Rgb>,
}



// =================
// === Component ===
// =================

#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Component {
    pub suggestion: Suggestion,
    pub group_id:   Option<usize>,
    pub match_info: MatchInfo,
}

impl Component {
    /// Construct a new component from a [`suggestion_database`] entry.
    ///
    /// The matching info will be filled for an empty pattern.
    pub fn new_from_database_entry(
        id: suggestion_database::entry::Id,
        entry: Rc<suggestion_database::Entry>,
        group_id: Option<usize>,
    ) -> Self {
        let data = Suggestion::FromDatabase { id, entry };
        Self { suggestion: data, group_id, match_info: default() }
    }

    /// The label which should be displayed in the Component Browser.
    pub fn label(&self) -> String {
        match &self.match_info {
            MatchInfo::Matches { kind: MatchKind::Alias(alias), .. } => {
                format!("{alias} ({self})")
            }
            _ => self.to_string(),
        }
    }

    /// The name of the component.
    pub fn name(&self) -> &str {
        self.suggestion.name()
    }

    /// The [ID](suggestion_database::entry::Id) of the component in the [`suggestion_database`], or
    /// `None` if not applicable.
    pub fn id(&self) -> Option<suggestion_database::entry::Id> {
        match &self.suggestion {
            Suggestion::FromDatabase { id, .. } => Some(*id),
            Suggestion::Virtual { .. } => None,
        }
    }

    /// Checks if component is filtered out.
    pub fn is_filtered_out(&self) -> bool {
        matches!(self.match_info, MatchInfo::DoesNotMatch)
    }

    /// Checks if the component can be entered in Component Browser.
    ///
    /// Currently, only modules can be entered, and then the Browser should display content and
    /// submodules of the entered module.
    pub fn can_be_entered(&self) -> bool {
        use suggestion_database::entry::Kind as EntryKind;
        matches!(&self.suggestion, Suggestion::FromDatabase { entry, .. } if entry.kind == EntryKind::Module)
    }

    /// Update matching info.
    ///
    /// It should be called each time the filtering pattern changes.
    pub fn update_matching_info(&mut self, filter: Filter) {
        // Match the input pattern to the component label.
        let label = self.to_string();
        let label_matches = fuzzly::matches(&label, filter.pattern.as_str());
        let label_subsequence = label_matches.and_option_from(|| {
            let metric = fuzzly::metric::default();
            fuzzly::find_best_subsequence(label, filter.pattern.as_str(), metric)
        });
        let label_match_info = label_subsequence
            .map(|subsequence| MatchInfo::Matches { subsequence, kind: MatchKind::Label });

        // Match the input pattern to the component name.
        let name = self.name();
        let name_matches = fuzzly::matches(name, filter.pattern.as_str());
        let name_subsequence = name_matches.and_option_from(|| {
            let metric = fuzzly::metric::default();
            fuzzly::find_best_subsequence(name, filter.pattern.as_str(), metric)
        });
        let name_match_info = name_subsequence.map(|subsequence| {
            let subsequence = fuzzly::Subsequence { indices: Vec::new(), ..subsequence };
            MatchInfo::Matches { subsequence, kind: MatchKind::Name }
        });

        // Match the input pattern to an entry's aliases and select the best alias match.
        let alias_matches = self.aliases().filter_map(|alias| {
            if fuzzly::matches(alias, filter.pattern.as_str()) {
                let metric = fuzzly::metric::default();
                let subsequence =
                    fuzzly::find_best_subsequence(alias, filter.pattern.as_str(), metric);
                subsequence.map(|subsequence| (subsequence, alias))
            } else {
                None
            }
        });
        let alias_match = alias_matches.max_by_key(|(m, _)| OrderedFloat(m.score));
        let alias_match_info = alias_match.map(|(subsequence, alias)| {
            let subsequence = fuzzly::Subsequence {
                score: subsequence.score * ALIAS_MATCH_ATTENUATION_FACTOR,
                ..subsequence
            };
            MatchInfo::Matches { subsequence, kind: MatchKind::Alias(alias.to_im_string()) }
        });

        // Select the best match of the available label-, code- and alias matches.
        let match_info_iter = [alias_match_info, name_match_info, label_match_info].into_iter();
        let best_match_info = match_info_iter.flatten().max_by(|lhs, rhs| lhs.cmp(rhs));
        self.match_info = best_match_info.unwrap_or(MatchInfo::DoesNotMatch);

        // Filter out components with FQN not matching the context.
        if let Some(context) = filter.context {
            if let Suggestion::FromDatabase { entry, .. } = &self.suggestion {
                if !entry.qualified_name().to_string().contains(context.as_str()) {
                    self.match_info = MatchInfo::DoesNotMatch;
                }
            } else {
                // Remove virtual entries if the context is present.
                self.match_info = MatchInfo::DoesNotMatch;
            }
        }
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

    /// Return an iterator over the component's aliases from the "ALIAS" tags in the entry's
    /// documentation.
    pub fn aliases(&self) -> impl Iterator<Item = &str> {
        let aliases = match &self.suggestion {
            Suggestion::FromDatabase { entry, .. } => {
                let aliases = entry.documentation.iter().filter_map(|doc| match doc {
                    DocSection::Tag { tag: Tag::Alias, body } =>
                        Some(body.as_str().split(',').map(|s| s.trim())),
                    _ => None,
                });
                Some(aliases.flatten())
            }
            _ => None,
        };
        aliases.into_iter().flatten()
    }
}

impl Display for Component {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use suggestion_database::entry::Kind;
        match &self.suggestion {
            Suggestion::FromDatabase { entry, .. } => match entry.kind {
                Kind::Module
                    if entry.defined_in.is_main_module() || entry.defined_in.is_top_element() =>
                    write!(f, "{}", entry.defined_in),
                _ => match entry.self_type.as_ref() {
                    Some(self_type) => write!(f, "{}.{}", self_type.alias_name(), entry.name),
                    None => write!(f, "{}", entry.name),
                },
            },
            Suggestion::Virtual { snippet } => write!(f, "{}", snippet.name),
        }
    }
}



// ============
// === List ===
// ============

#[derive(Clone, Debug, Default)]
pub struct List {
    pub(crate) filtered_in:           Option<RangeTo<usize>>,
    pub(crate) components:            Vec<Component>,
    pub(crate) filterable_components: Vec<Component>,
    pub(crate) groups:                Vec<Group>,
}

impl List {
    pub fn is_filtering(&self) -> bool {
        self.filtered_in.is_some()
    }

    pub fn update_filtering(&mut self, filter: Filter) {
        if filter.pattern.trim().is_empty() {
            self.filtered_in = None;
        } else {
            for component in &mut self.filterable_components {
                component.update_matching_info(filter.clone_ref());
            }
            self.filterable_components
                .sort_by(|lhs, rhs| Self::entry_match_ordering(&lhs.match_info, &rhs.match_info));
            let first_non_matching = self
                .filterable_components
                .lower_bound_by_key(&true, |entry| entry.is_filtered_out());
            self.filtered_in = Some(..first_non_matching);
        }
    }

    /// Return the entry match ordering when sorting by match. See [`component::Order::ByMatch`].
    fn entry_match_ordering(lhs: &MatchInfo, rhs: &MatchInfo) -> cmp::Ordering {
        lhs.cmp(rhs).reverse()
    }

    pub fn components(&self) -> &[Component] {
        if let Some(range) = self.filtered_in {
            &self.filterable_components[range]
        } else {
            &self.components
        }
    }

    pub fn get(&self, index: usize) -> Option<&Component> {
        self.components().get(index)
    }

    pub fn len(&self) -> usize {
        self.components().len()
    }

    pub fn is_empty(&self) -> bool {
        self.components().is_empty()
    }

    pub fn groups(&self) -> &[Group] {
        &self.groups
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::model::execution_context;
    use double_representation::name::QualifiedName;
    use enso_suggestion_database::mock_suggestion_database;

    pub fn check_components(list: &List, expected: Vec<&str>) {
        let components = list.components().iter().map(|component| component.label()).collect_vec();
        assert_eq!(components, expected);
    }

    pub fn check_groups(list: &List, expected: Vec<Option<usize>>) {
        let groups = list.components().iter().map(|component| component.group_id).collect_vec();
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
        check_components(&list, vec!["test.Test.TopModule1"]);
        list.update_filtering(make_filter("main"));
        check_components(&list, vec!["New_Project_1.main"]);
        list.update_filtering(make_filter("fo"));
        check_components(&list, vec!["TopModule1.foo"]);
        list.update_filtering(make_filter("ba"));
        check_components(&list, vec!["TopModule1.bar", "SubModule1.bazz"]);
        list.update_filtering(make_filter(""));
        check_components(&list, vec!["test.Test.TopModule1"]);
    }
}
