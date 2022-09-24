//! A module with the providers for searcher view, taking content from the Action List controller.

use crate::prelude::*;

use crate::controller::searcher::action::MatchInfo;
use crate::controller::searcher::component;
use crate::presenter;

use enso_text as text;
use ensogl_component::list_view;
use ensogl_component::list_view::entry::GlyphHighlightedLabel;
use ide_view as view;
use ide_view::component_browser::list_panel::LabeledAnyModelProvider;
use ide_view_component_group as component_group_view;



// ============================
// === Any Provider Helpers ===
// ============================

/// Wrappers for some instance of the structure being both entry and documentation provider.
pub type Any = (
    list_view::entry::AnyModelProvider<GlyphHighlightedLabel>,
    view::searcher::AnyDocumentationProvider,
);

/// Create providers from the current controller's action list.
pub fn create_providers_from_controller(logger: &Logger, controller: &controller::Searcher) -> Any {
    use controller::searcher::Actions;
    match controller.actions() {
        Actions::Loading => as_any(Rc::new(list_view::entry::EmptyProvider)),
        Actions::Loaded { list } => {
            let user_action = controller.current_user_action();
            let intended_function = controller.intended_function_suggestion();
            let provider = Action { actions: list, user_action, intended_function };
            as_any(Rc::new(provider))
        }
        Actions::Error(err) => {
            error!(logger, "Error while obtaining searcher action list: {err}");
            as_any(Rc::new(list_view::entry::EmptyProvider))
        }
    }
}

fn as_any<P>(provider: Rc<P>) -> Any
where P: list_view::entry::ModelProvider<view::searcher::Entry>
        + view::searcher::DocumentationProvider
        + 'static {
    (provider.clone_ref().into(), provider.into())
}



// ========================
// === provider::Action ===
// ========================

/// An searcher actions provider, based on the action list retrieved from the searcher controller.
#[derive(Clone, Debug)]
pub struct Action {
    actions:           Rc<controller::searcher::action::List>,
    user_action:       controller::searcher::UserAction,
    intended_function: Option<controller::searcher::action::Suggestion>,
}

impl Action {
    /// Get the documentation for a suggestion in case when this suggestion does not have
    /// a documentation.
    ///
    /// Usually something like "Function foo - no documentation available". The returned string is
    /// documentation in HTML format.
    pub fn doc_placeholder_for(suggestion: &controller::searcher::action::Suggestion) -> String {
        use controller::searcher::action::Suggestion;
        match suggestion {
            Suggestion::FromDatabase(suggestion) =>
                presenter::searcher::doc_placeholder_for(suggestion),
            Suggestion::Hardcoded(suggestion) => {
                format!(
                    "<div class=\"enso docs summary\"><p />{}<p />No documentation available</div>",
                    suggestion.name
                )
            }
        }
    }
}

impl list_view::entry::ModelProvider<GlyphHighlightedLabel> for Action {
    fn entry_count(&self) -> usize {
        // TODO[ao] Because of "All Search Results" category, the actions on list are duplicated.
        //     But we don't want to display duplicates on the old searcher list. To be fixed/removed
        //     once new searcher GUI will be implemented
        //     (https://github.com/enso-org/ide/issues/1681)
        self.actions.matching_count() / 2
    }

    fn get(&self, id: usize) -> Option<list_view::entry::GlyphHighlightedLabelModel> {
        let action = self.actions.get_cloned(id)?;
        if let MatchInfo::Matches { subsequence } = action.match_info {
            let label = action.action.to_string();
            let mut char_iter = label.char_indices().enumerate();
            let highlighted = subsequence
                .indices
                .iter()
                .filter_map(|idx| loop {
                    if let Some(char) = char_iter.next() {
                        let (char_idx, (byte_id, char)) = char;
                        if char_idx == *idx {
                            let start = enso_text::unit::Bytes(byte_id as i32);
                            let end = enso_text::unit::Bytes((byte_id + char.len_utf8()) as i32);
                            break Some(enso_text::Range::new(start, end));
                        }
                    } else {
                        break None;
                    }
                })
                .collect();
            Some(list_view::entry::GlyphHighlightedLabelModel { label, highlighted })
        } else {
            None
        }
    }
}

impl ide_view::searcher::DocumentationProvider for Action {
    fn get(&self) -> Option<String> {
        use controller::searcher::UserAction::*;
        self.intended_function.as_ref().and_then(|function| match self.user_action {
            StartingTypingArgument => function.documentation_html().map(ToOwned::to_owned),
            _ => None,
        })
    }

    fn get_for_entry(&self, id: usize) -> Option<String> {
        use controller::searcher::action::Action;
        match self.actions.get_cloned(id)?.action {
            Action::Suggestion(suggestion) => {
                let doc = suggestion.documentation_html().map(ToOwned::to_owned);
                Some(doc.unwrap_or_else(|| Self::doc_placeholder_for(&suggestion)))
            }
            Action::Example(example) => Some(example.documentation_html.clone()),
            Action::ProjectManagement(_) => None,
        }
    }
}



// ===========================
// === provider::Component ===
// ===========================

/// Component Provider getting entries from a [`controller::searcher::component::Group`].
#[derive(Clone, CloneRef, Debug)]
pub struct Component {
    group: controller::searcher::component::Group,
}

impl Component {
    /// Create component provider based of the given group.
    pub fn new(group: controller::searcher::component::Group) -> Self {
        Self { group }
    }
}

macro_rules! kind_to_icon {
    ([ $( $variant:ident ),* ] $kind:ident) => {
        {
            use component_group_view::icon::Id;
            use model::suggestion_database::entry::Kind;
            match $kind {
                $( Kind::$variant => Id::$variant, )*
            }
        }
    }
}

impl list_view::entry::ModelProvider<component_group_view::Entry> for Component {
    fn entry_count(&self) -> usize {
        self.group.matched_items.get()
    }

    fn get(&self, id: usize) -> Option<component_group_view::entry::Model> {
        use model::suggestion_database::entry::for_each_kind_variant;
        let component = self.group.get_entry(id)?;
        let is_enterable = component.can_be_entered();
        let match_info = component.match_info.borrow();
        let label = component.label();
        let highlighted = bytes_of_matched_letters(&*match_info, &label);
        let icon = match component.data {
            component::Data::FromDatabase { entry, .. } => {
                let kind = entry.kind;
                let icon_name = entry.icon_name.as_ref();
                let icon = icon_name.and_then(|n| n.to_pascal_case().parse().ok());
                icon.unwrap_or_else(|| for_each_kind_variant!(kind_to_icon(kind)))
            }
            component::Data::Virtual { snippet } => snippet.icon,
        };
        Some(component_group_view::entry::Model {
            icon,
            highlighted_text: list_view::entry::GlyphHighlightedLabelModel { label, highlighted },
            is_enterable,
        })
    }
}


// === Component Provider helpers ===

fn bytes_of_matched_letters(match_info: &MatchInfo, label: &str) -> Vec<text::Range<text::Bytes>> {
    if let MatchInfo::Matches { subsequence } = match_info {
        let mut char_iter = label.char_indices().enumerate();
        subsequence
            .indices
            .iter()
            .filter_map(|idx| loop {
                if let Some(char) = char_iter.next() {
                    let (char_idx, (byte_id, char)) = char;
                    if char_idx == *idx {
                        let start = enso_text::unit::Bytes(byte_id as i32);
                        let end = enso_text::unit::Bytes((byte_id + char.len_utf8()) as i32);
                        break Some(enso_text::Range::new(start, end));
                    }
                } else {
                    break None;
                }
            })
            .collect()
    } else {
        default()
    }
}



// ===========================
// === Converter functions ===
// ===========================

/// Get [`LabeledAnyModelProvider`] for given component group.
pub fn from_component_group(
    group: &controller::searcher::component::Group,
) -> LabeledAnyModelProvider {
    LabeledAnyModelProvider {
        label:                group.name.clone_ref(),
        content:              Rc::new(Component::new(group.clone_ref())).into(),
        original_entry_count: group.entries.borrow().len(),
    }
}

/// Get vector of [`LabeledAnyModelProvider`] for given component group list.
pub fn from_component_group_list(
    groups: &impl AsRef<controller::searcher::component::group::List>,
) -> Vec<LabeledAnyModelProvider> {
    groups.as_ref().iter().map(from_component_group).collect()
}
