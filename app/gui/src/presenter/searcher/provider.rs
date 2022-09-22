//! A module with the providers for searcher view, taking content from the Action List controller.

use crate::prelude::*;

use crate::controller::searcher::action::MatchInfo;
use crate::controller::searcher::component;
use crate::model::suggestion_database::entry::for_each_kind_variant;
use crate::presenter;

use enso_frp as frp;
use enso_text as text;
use ensogl_component::list_view;
use ensogl_component::list_view::entry::GlyphHighlightedLabel;
use ide_view as view;
use ide_view::component_browser::component_list_panel;
use ide_view::component_browser::component_list_panel::grid as component_grid;



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

pub trait ControllerListExt {
    fn component_by_view_id(
        &self,
        id: component_grid::GroupEntryId,
    ) -> Option<component::Component>;

    fn group_by_view_id(&self, id: component_grid::GroupId) -> Option<component::Group>;

    fn create_grid_content_info(&self) -> component_grid::content::Info;

    fn get_entry_model(
        &self,
        entry_id: component_grid::GroupEntryId,
    ) -> Option<component_grid::EntryModel>;

    fn get_header_model(
        &self,
        group_id: component_grid::GroupId,
    ) -> Option<component_grid::HeaderModel>;
}

impl ControllerListExt for component::List {
    fn component_by_view_id(
        &self,
        id: component_grid::GroupEntryId,
    ) -> Option<component::Component> {
        let group = self.group_by_view_id(id.group);
        group.and_then(|group| group.get_entry(id.entry))
    }

    fn group_by_view_id(&self, id: component_grid::GroupId) -> Option<component::Group> {
        let opt_group = match id.section {
            component_grid::SectionId::Popular => self.favorites.get(id.index).cloned(),
            component_grid::SectionId::LocalScope =>
                (id.index == 0).as_some_from(|| self.local_scope.clone_ref()),
            component_grid::SectionId::SubModules => self.top_modules().get(id.index).cloned(),
        };
        opt_group
    }

    fn create_grid_content_info(&self) -> component_grid::content::Info {
        let popular_section =
            group_list_to_grid_group_infos(component_grid::SectionId::Popular, &self.favorites);
        let submodules_section = group_list_to_grid_group_infos(
            component_grid::SectionId::SubModules,
            &**self.top_modules(),
        );
        component_list_panel::grid::content::Info {
            groups:           popular_section.chain(submodules_section).collect(),
            local_scope_size: self.local_scope.matched_items.get(),
        }
    }

    fn get_entry_model(
        &self,
        entry_id: component_grid::GroupEntryId,
    ) -> Option<component_grid::EntryModel> {
        let component = self.component_by_view_id(entry_id)?;
        Some(component_to_entry_model(&component))
    }

    fn get_header_model(
        &self,
        group_id: component_grid::GroupId,
    ) -> Option<component_grid::HeaderModel> {
        let group = self.group_by_view_id(group_id)?;
        Some(group_to_header_model(&group))
    }
}

fn controller_group_to_grid_group_info(
    id: component_grid::GroupId,
    group: &component::Group,
) -> component_grid::content::Group {
    component_list_panel::grid::content::Group {
        id,
        height: group.len(),
        original_height: group.matched_items.get(),
        color: group.color,
    }
}

fn group_list_to_grid_group_infos<'a>(
    section: component_grid::SectionId,
    list: &'a component::group::List,
) -> impl Iterator<Item = component_grid::content::Group> + 'a {
    list.iter().enumerate().map(move |(index, group)| {
        let id = component_grid::GroupId { section, index };
        controller_group_to_grid_group_info(id, group)
    })
}

fn group_to_header_model(group: &component::Group) -> component_grid::HeaderModel {
    component_grid::HeaderModel { caption: group.name.clone_ref() }
}

macro_rules! kind_to_icon {
    ([ $( $variant:ident ),* ] $kind:ident) => {
        {
            use component_grid::entry::icon::Id;
            use model::suggestion_database::entry::Kind;
            match $kind {
                $( Kind::$variant => Id::$variant, )*
            }
        }
    }
}

fn component_to_entry_model(component: &component::Component) -> component_grid::EntryModel {
    let is_enterable = component.can_be_entered();
    let match_info = component.match_info.borrow();
    let caption = component.label();
    let highlighted = bytes_of_matched_letters(&*match_info, &caption);
    let icon = match &component.data {
        component::Data::FromDatabase { entry, .. } => {
            let kind = entry.kind;
            let icon_name = entry.icon_name.as_ref();
            let icon = icon_name.and_then(|n| n.to_pascal_case().parse().ok());
            icon.unwrap_or_else(|| for_each_kind_variant!(kind_to_icon(kind)))
        }
        component::Data::Virtual { snippet } => snippet.icon,
    };
    component_grid::EntryModel { caption: caption.into(), highlighted: Rc::new(highlighted), icon }
}

#[derive(Debug)]
pub struct Component {
    network: frp::Network,
    list:    component::List,
}

impl Component {
    pub fn provide_new_list(
        list: component::List,
        grid: &component_list_panel::grid::View,
    ) -> Self {
        let network = frp::Network::new("presenter::searcher::provider::Component");

        frp::extend! { network
            // Beware! We cannot connect the expression `grid.model_for...` to grid inputs, because
            // the connections alone does not belong to network - thus when provider's network would
            // be dropped, the connections would remain. Therefore we create intermediate nodes
            // `entry_model` and `header_model`.
            entry_model <- grid.model_for_entry_needed.filter_map(f!([list](&id) {
                Some((id, list.get_entry_model(id)?))
            }));
            header_model <- grid.model_for_header_needed.filter_map(f!([list](&id) {
                Some((id, list.get_header_model(id)?))
            }));
            grid.model_for_entry <+ entry_model;
            grid.model_for_header <+ header_model;
        }
        let content = list.create_grid_content_info();
        grid.reset(content);
        Self { network, list }
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
