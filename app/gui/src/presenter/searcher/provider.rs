//! A module with the providers for searcher view, taking content from the Action List controller.

use crate::prelude::*;

use crate::controller::searcher::action::MatchInfo;
use crate::model::suggestion_database;

use ensogl_component::list_view;
use ensogl_component::list_view::entry::GlyphHighlightedLabel;
use ide_view as view;



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
    fn doc_placeholder_for(suggestion: &controller::searcher::action::Suggestion) -> String {
        use controller::searcher::action::Suggestion;
        let code = match suggestion {
            Suggestion::FromDatabase(suggestion) => {
                let title = match suggestion.kind {
                    suggestion_database::entry::Kind::Atom => "Atom",
                    suggestion_database::entry::Kind::Function => "Function",
                    suggestion_database::entry::Kind::Local => "Local variable",
                    suggestion_database::entry::Kind::Method => "Method",
                    suggestion_database::entry::Kind::Module => "Module",
                };
                let code = suggestion.code_to_insert(None, true).code;
                format!("{} `{}`\n\nNo documentation available", title, code)
            }
            Suggestion::Hardcoded(suggestion) => {
                format!("{}\n\nNo documentation available", suggestion.name)
            }
        };
        let parser = parser::DocParser::new();
        match parser {
            Ok(p) => {
                let output = p.generate_html_doc_pure((*code).to_string());
                output.unwrap_or(code)
            }
            Err(_) => code,
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
