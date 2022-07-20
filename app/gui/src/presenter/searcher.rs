//! The module containing [`Searcher`] presenter. See [`crate::presenter`] documentation to know
//! more about presenters in general.

use crate::prelude::*;

use crate::controller::searcher::action::Suggestion;
use crate::controller::searcher::Notification;
use crate::controller::searcher::UserAction;
use crate::executor::global::spawn_stream_handler;
use crate::model::suggestion_database::entry::Kind;
use crate::presenter;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;

use enso_frp as frp;
use ide_view as view;
use ide_view::component_browser::list_panel::LabeledAnyModelProvider;
use ide_view::graph_editor::component::node as node_view;
use ide_view::project::SearcherParams;
use ide_view::project::SearcherVariant;
use ide_view_component_group::set::SectionId;


// ==============
// === Export ===
// ==============

pub mod provider;



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
struct Model {
    logger:     Logger,
    controller: controller::Searcher,
    view:       view::project::View,
    input_view: ViewNodeId,
}

impl Model {
    #[profile(Debug)]
    fn new(
        parent: impl AnyLogger,
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let logger = parent.sub("presenter::Searcher");
        Self { logger, controller, view, input_view }
    }

    #[profile(Debug)]
    fn input_changed(&self, new_input: &str) {
        if let Err(err) = self.controller.set_input(new_input.to_owned()) {
            error!(self.logger, "Error while setting new searcher input: {err}");
        }
    }

    fn entry_used_as_suggestion(
        &self,
        entry_id: view::searcher::entry::Id,
    ) -> Option<(ViewNodeId, node_view::Expression)> {
        match self.controller.use_as_suggestion(entry_id) {
            Ok(new_code) => {
                let new_code_and_trees = node_view::Expression::new_plain(new_code);
                Some((self.input_view, new_code_and_trees))
            }
            Err(err) => {
                error!(self.logger, "Error while applying suggestion: {err}");
                None
            }
        }
    }

    fn commit_editing(&self, entry_id: Option<view::searcher::entry::Id>) -> Option<AstNodeId> {
        let result = match entry_id {
            Some(id) => self.controller.execute_action_by_index(id),
            None => self.controller.commit_node().map(Some),
        };
        result.unwrap_or_else(|err| {
            error!(self.logger, "Error while executing action: {err}");
            None
        })
    }

    fn create_providers(&self) -> provider::Any {
        provider::create_providers_from_controller(&self.logger, &self.controller)
    }

    fn suggestion_accepted(
        &self,
        id: view::component_browser::list_panel::EntryId,
    ) -> Option<(ViewNodeId, node_view::Expression)> {
        let component = self.component_by_view_id(id);
        let new_code = component.and_then(|component| {
            let suggestion = Suggestion::FromDatabase(component.suggestion.clone_ref());
            self.controller.use_suggestion(suggestion)
        });
        match new_code {
            Ok(new_code) => {
                let new_code_and_trees = node_view::Expression::new_plain(new_code);
                Some((self.input_view, new_code_and_trees))
            }
            Err(err) => {
                error!(self.logger, "Error while applying suggestion: {err}");
                None
            }
        }
    }

    fn expression_accepted(
        &self,
        entry_id: Option<view::component_browser::list_panel::EntryId>,
    ) -> Option<AstNodeId> {
        if let Some(entry_id) = entry_id {
            self.suggestion_accepted(entry_id);
        }
        self.controller.commit_node().map(Some).unwrap_or_else(|err| {
            error!(self.logger, "Error while committing node expression: {err}");
            None
        })
    }

    fn component_by_view_id(
        &self,
        id: view::component_browser::list_panel::EntryId,
    ) -> FallibleResult<controller::searcher::component::Component> {
        let components = self.controller.components();
        match id.group.section {
            SectionId::Favorites =>
                components.favorites_entry_by_index(id.group.index, id.entry_id),
            SectionId::LocalScope => components.local_scope_entry_by_index(id.entry_id),
            SectionId::SubModules =>
                components.top_module_entry_by_index(id.group.index, id.entry_id),
        }
    }

    fn create_submodules_providers(&self) -> Vec<LabeledAnyModelProvider> {
        provider::from_component_group_list(self.controller.components().top_modules())
    }

    fn create_favorites_providers(&self) -> Vec<LabeledAnyModelProvider> {
        provider::from_component_group_list(&self.controller.components().favorites)
    }

    fn create_local_scope_provider(&self) -> LabeledAnyModelProvider {
        provider::from_component_group(&self.controller.components().local_scope)
    }

    fn documentation_of_component(
        &self,
        id: Option<view::component_browser::list_panel::EntryId>,
    ) -> String {
        let component = id.and_then(|id| self.component_by_view_id(id).ok());
        if let Some(component) = component {
            if let Some(documentation) = &component.suggestion.documentation_html {
                let title = match component.suggestion.kind {
                    Kind::Atom => format!("Atom {}", component.suggestion.name),
                    Kind::Function => format!("Function {}", component.suggestion.name),
                    Kind::Local => format!("Node {}", component.suggestion.name),
                    Kind::Method => format!(
                        "Method {}{}{}",
                        component.suggestion.name,
                        if component.suggestion.self_type.is_some() { " of " } else { "" },
                        component.suggestion.self_type.as_ref().map_or("", |tp| &tp.name),
                    ),
                    Kind::Module => format!("Module {}", component.suggestion.name),
                };
                format!("<div class=\"enso docs summary\"><p />{title}</div>{documentation}")
            } else {
                provider::Action::doc_placeholder_for(&Suggestion::FromDatabase(
                    component.suggestion.clone_ref(),
                ))
            }
        } else {
            default()
        }
    }

    fn should_auto_select_first_action(&self) -> bool {
        let user_action = self.controller.current_user_action();
        let list_not_empty = matches!(self.controller.actions(), controller::searcher::Actions::Loaded {list} if list.matching_count() > 0);
        // Usually we don't want to select first entry and display docs when user finished typing
        // function or argument.
        let starting_typing = user_action == UserAction::StartingTypingArgument;
        !starting_typing && list_not_empty
    }
}

/// The Searcher presenter, synchronizing state between searcher view and searcher controller.
///
/// The presenter should be created for one instantiated searcher controller (when node starts to
/// being edited). Alternatively, the [`setup_controller`] method covers constructing the controller
/// and the presenter.
#[derive(Debug)]
pub struct Searcher {
    _network: frp::Network,
    model:    Rc<Model>,
}

impl Searcher {
    /// Constructor. The returned structure works right away.
    #[profile(Task)]
    pub fn new(
        parent: impl AnyLogger,
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let model = Rc::new(Model::new(parent, controller, view, input_view));
        let network = frp::Network::new("presenter::Searcher");

        let graph = &model.view.graph().frp;

        frp::extend! { network
            eval graph.node_expression_set ([model]((changed_node, expr)) {
                if *changed_node == input_view {
                    model.input_changed(expr);
                }
            });

            action_list_changed <- source::<()>();
            select_entry <- action_list_changed.filter(f_!(model.should_auto_select_first_action()));
        }

        match model.view.searcher() {
            SearcherVariant::ComponentBrowser(browser) => {
                let list_view = &browser.model().list;
                let documentation = &browser.model().documentation;
                frp::extend! { network
                    list_view.set_sub_modules_section <+
                        action_list_changed.map(f_!(model.create_submodules_providers()));
                    list_view.set_favourites_section <+
                        action_list_changed.map(f_!(model.create_favorites_providers()));
                    list_view.set_local_scope_section <+
                        action_list_changed.map(f_!(model.create_local_scope_provider().content));
                    new_input <- list_view.suggestion_accepted.filter_map(f!((e) model.suggestion_accepted(*e)));
                    trace new_input;
                    graph.set_node_expression <+ new_input;

                    current_docs <- all_with(
                        &action_list_changed,
                        &list_view.selected_entry,
                        f!((_, entry) model.documentation_of_component(*entry))
                    );
                    documentation.frp.display_documentation <+ current_docs;

                    eval_ list_view.suggestion_accepted([]analytics::remote_log_event("component_browser::suggestion_accepted"));
                }
            }
            SearcherVariant::OldNodeSearcher(searcher) => {
                let searcher = &searcher.frp;

                frp::extend! { network
                    new_providers <- action_list_changed.map(f_!(model.create_providers()));
                    searcher.set_actions <+ new_providers;
                    searcher.select_action <+ select_entry.constant(0);
                    used_as_suggestion <- searcher.used_as_suggestion.filter_map(|entry| *entry);
                    new_input <- used_as_suggestion.filter_map(f!((e) model.entry_used_as_suggestion(*e)));
                    graph.set_node_expression <+ new_input;

                    eval_ searcher.used_as_suggestion([]analytics::remote_log_event("searcher::used_as_suggestion"));
                }
            }
        }

        let weak_model = Rc::downgrade(&model);
        let notifications = model.controller.subscribe();
        spawn_stream_handler(weak_model, notifications, move |notification, _| {
            match notification {
                Notification::NewActionList => action_list_changed.emit(()),
            };
            std::future::ready(())
        });

        Self { model, _network: network }
    }

    /// Setup new, appropriate searcher controller for the edition of `node_view`, and construct
    /// presenter handling it.
    #[profile(Task)]
    pub fn setup_controller(
        parent: impl AnyLogger,
        ide_controller: controller::Ide,
        project_controller: controller::Project,
        graph_controller: controller::ExecutedGraph,
        graph_presenter: &presenter::Graph,
        view: view::project::View,
        parameters: SearcherParams,
    ) -> FallibleResult<Self> {
        let SearcherParams { input, source_node } = parameters;
        let ast_node = graph_presenter.ast_node_of_view(input);
        let mode = match ast_node {
            Some(node_id) => controller::searcher::Mode::EditNode { node_id },
            None => {
                let view_data = view.graph().model.nodes.get_cloned_ref(&input);
                let position = view_data.map(|node| node.position().xy());
                let position = position.map(|vector| model::module::Position { vector });
                let source_node =
                    source_node.and_then(|id| graph_presenter.ast_node_of_view(id.node));
                controller::searcher::Mode::NewNode { position, source_node }
            }
        };
        let searcher_controller = controller::Searcher::new_from_graph_controller(
            &parent,
            ide_controller,
            &project_controller.model,
            graph_controller,
            mode,
        )?;
        Ok(Self::new(parent, searcher_controller, view, input))
    }

    /// Commit editing in the old Node Searcher.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes. The `entry_id` might be none in case where the searcher should accept
    /// the node input without any entry selected. If the commitment results in creating a new
    /// node, its AST id is returned.
    #[profile(Task)]
    pub fn commit_editing(self, entry_id: Option<view::searcher::entry::Id>) -> Option<AstNodeId> {
        self.model.commit_editing(entry_id)
    }

    /// Expression accepted in Compnent Browser.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes. The `entry_id` might be none in case where the user want to accept
    /// the node input without any entry selected. If the commitment results in creating a new
    /// node, its AST id is returned.
    pub fn expression_accepted(
        self,
        entry_id: Option<view::component_browser::list_panel::EntryId>,
    ) -> Option<AstNodeId> {
        self.model.expression_accepted(entry_id)
    }

    /// Abort editing, without taking any action.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes.
    pub fn abort_editing(self) {}

    /// Returns true if the entry under given index is one of the examples.
    pub fn is_entry_an_example(&self, entry: view::searcher::entry::Id) -> bool {
        use crate::controller::searcher::action::Action::Example;

        let controller = &self.model.controller;
        let entry = controller.actions().list().and_then(|l| l.get_cloned(entry));
        entry.map_or(false, |e| matches!(e.action, Example(_)))
    }
}
