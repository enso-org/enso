use crate::controller::searcher::action::MatchKind;
use crate::controller::searcher::component::hardcoded;
use crate::controller::searcher::Filter;
use crate::prelude::*;
use convert_case::Case;
use convert_case::Casing;
use double_representation::name::QualifiedName;
use enso_doc_parser::DocSection;
use enso_doc_parser::Tag;
use ensogl::data::color;
use ordered_float::OrderedFloat;
use std::cmp;
use superslice::Ext;

use crate::model::suggestion_database;

pub mod builder;



// =================
// === Constants ===
// =================

/// A "matching" score assigned to the entries which does not match the current pattern entirely.
///
/// **Note**: If some entries matches, but their score are equal or below this value, they will be
/// filtered out as well!
pub const NOT_MATCHING_SCORE: f32 = 0.0;

/// A factor to multiply a component's alias match score by. It is intended to reduce the importance
/// of alias matches compared to label matches.
const ALIAS_MATCH_ATTENUATION_FACTOR: f32 = 0.75;



// ====================
// === Type Aliases ===
// ====================

/// Information how the component matches the filtering pattern.
pub type MatchInfo = controller::searcher::action::MatchInfo;

#[derive(Clone, Debug)]
pub enum Data {
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

#[derive(Clone, Debug, Default)]
pub struct Group {
    name:  ImString,
    color: Option<color::Rgb>,
}

#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Component {
    data:       Data,
    group_id:   Option<usize>,
    match_info: MatchInfo,
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
        let data = Data::FromDatabase { id, entry };
        Self { data, group_id, match_info: default() }
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
        match &self.data {
            Data::FromDatabase { entry, .. } => entry.name.as_str(),
            Data::Virtual { snippet } => snippet.name.as_str(),
        }
    }

    /// The [ID](suggestion_database::entry::Id) of the component in the [`suggestion_database`], or
    /// `None` if not applicable.
    pub fn id(&self) -> Option<suggestion_database::entry::Id> {
        match &self.data {
            Data::FromDatabase { id, .. } => Some(*id),
            Data::Virtual { .. } => None,
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
        matches!(&self.data, Data::FromDatabase { entry, .. } if entry.kind == EntryKind::Module)
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
            if let Data::FromDatabase { entry, .. } = &self.data {
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
        match &self.data {
            Data::FromDatabase { entry, .. } => entry
                .documentation
                .iter()
                .any(|doc| matches!(doc, DocSection::Tag { tag: Tag::Private, .. })),
            _ => false,
        }
    }

    /// Return an iterator over the component's aliases from the "ALIAS" tags in the entry's
    /// documentation.
    pub fn aliases(&self) -> impl Iterator<Item = &str> {
        let aliases = match &self.data {
            Data::FromDatabase { entry, .. } => {
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
        match &self.data {
            Data::FromDatabase { entry, .. } => {
                let entry_name = entry.name.from_case(Case::Snake).to_case(Case::Lower);
                let self_type_ref = entry.self_type.as_ref();
                let self_type_not_here = self_type_ref.filter(|t| *t != &entry.defined_in);
                if let Some(self_type) = self_type_not_here {
                    let self_name = self_type.name().from_case(Case::Snake).to_case(Case::Title);
                    write!(f, "{entry_name} ({self_name})")
                } else {
                    write!(f, "{entry_name}")
                }
            }
            Data::Virtual { snippet } => write!(f, "{}", snippet.name),
        }
    }
}


#[derive(Clone, Debug, Default)]
pub struct List {
    pub(crate) filtered_in:           Option<RangeTo<usize>>,
    pub(crate) components:            Vec<Component>,
    pub(crate) filterable_components: Vec<Component>,
    pub(crate) groups:                Vec<Group>,
}


impl List {
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
}
