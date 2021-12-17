pub mod provider;

use crate::prelude::*;

use crate::controller::searcher::Notification;
use crate::controller::searcher::UserAction;
use crate::executor::global::spawn_stream_handler;
use crate::presenter;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;
use enso_frp as frp;
use ide_view as view;
use ide_view::graph_editor::component::node as node_view;



#[derive(Clone, CloneRef, Debug)]
struct Model {
    logger:     Logger,
    controller: controller::Searcher,
    view:       view::project::View,
    input_view: ViewNodeId,
}

impl Model {
    fn new(
        parent: impl AnyLogger,
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let logger = parent.sub("presenter::Searcher");
        Self { logger, controller, view, input_view }
    }

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

    fn editing_committed(&self, entry_id: Option<view::searcher::entry::Id>) -> Option<AstNodeId> {
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

    fn should_auto_select_first_action(&self) -> bool {
        let user_action = self.controller.current_user_action();
        let list_not_empty = matches!(self.controller.actions(), controller::searcher::Actions::Loaded {list} if list.matching_count() > 0);
        // Usually we don't want to select first entry and display docs when user finished typing
        // function or argument.
        let starting_typing = user_action == UserAction::StartingTypingArgument;
        !starting_typing && list_not_empty
    }
}

#[derive(Debug)]
pub struct Searcher {
    network: frp::Network,
    model:   Rc<Model>,
}

impl Searcher {
    pub fn new(
        parent: impl AnyLogger,
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let model = Rc::new(Model::new(parent, controller, view, input_view));
        let network = frp::Network::new("presenter::Searcher");

        let graph = &model.view.graph().frp;
        let project = &model.view.frp;
        let searcher = &model.view.searcher().frp;

        frp::extend! { network
            eval graph.node_expression_set ([model]((changed_node, expr)) {
                if *changed_node == input_view {
                    DEBUG!("Input changed for {input_view}: {expr}");
                    model.input_changed(expr);
                }
            });
            trace graph.node_expression_set;

            action_list_changed <- source::<()>();
            new_providers <- action_list_changed.map(f_!(model.create_providers()));
            searcher.set_actions <+ new_providers;
            select_entry <- action_list_changed.filter(f_!(model.should_auto_select_first_action()));
            searcher.select_action <+ select_entry.constant(0);

            used_as_suggestion <- searcher.used_as_suggestion.filter_map(|entry| *entry);
            new_input <- used_as_suggestion.filter_map(f!((e) model.entry_used_as_suggestion(*e)));
            graph.set_node_expression <+ new_input;
        }

        let weak_model = Rc::downgrade(&model);
        let notifications = model.controller.subscribe();
        spawn_stream_handler(weak_model, notifications, move |notification, _| {
            match notification {
                Notification::NewActionList => action_list_changed.emit(()),
            };
            std::future::ready(())
        });

        Self { model, network }
    }

    pub fn setup_controller(
        parent: impl AnyLogger,
        ide_controller: controller::Ide,
        project_controller: controller::Project,
        graph_controller: controller::ExecutedGraph,
        graph_presenter: &presenter::Graph,
        view: view::project::View,
        node_view: ViewNodeId,
    ) -> FallibleResult<Self> {
        let ast_node = graph_presenter.ast_node_of_view(node_view);
        let mode = match ast_node {
            Some(node_id) => controller::searcher::Mode::EditNode { node_id },
            None => {
                let view_data = view.graph().model.nodes.get_cloned_ref(&node_view);
                let position = view_data.map(|node| node.position().xy());
                let position = position.map(|vector| model::module::Position { vector });
                controller::searcher::Mode::NewNode { position }
            }
        };
        let selected_views = view.graph().model.nodes.all_selected();
        let selected_nodes =
            selected_views.iter().filter_map(|view| graph_presenter.ast_node_of_view(*view));
        DEBUG!("Creating searcher controller for {selected_nodes.clone().collect_vec():?}");
        DEBUG!("The mode is {mode:?}");
        let searcher_controller = controller::Searcher::new_from_graph_controller(
            &parent,
            ide_controller,
            &project_controller.model,
            graph_controller,
            mode,
            selected_nodes.collect(),
        )?;
        Ok(Self::new(parent, searcher_controller, view, node_view))
    }

    pub fn editing_committed(
        self,
        entry_id: Option<view::searcher::entry::Id>,
    ) -> Option<AstNodeId> {
        self.model.editing_committed(entry_id)
    }

    pub fn editing_aborted(self) {}
}
