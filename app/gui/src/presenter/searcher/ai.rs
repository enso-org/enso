//! AI Searcher presenter. This is a presenter for the AI Searcher component. It contains the
//! logic for handling the user input and the logic for processing the searcher input as a prompt
//! used for the AI model.

use crate::prelude::*;

use crate::controller::searcher::input;
use crate::controller::searcher::Mode;
use crate::controller::searcher::ThisNode;
use crate::controller::ExecutedGraph;
use crate::controller::Project;
use crate::model::execution_context::QualifiedMethodPointer;
use crate::model::execution_context::Visualization;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;
use crate::presenter::searcher::apply_this_argument;
use crate::presenter::searcher::SearcherPresenter;
use crate::presenter::Graph;

use enso_frp as frp;
use enso_prelude::FallibleResult;
use enso_text as text;
use enso_text::Byte;
use ide_view::component_browser::component_list_panel::grid::GroupEntryId;
use ide_view::graph_editor::GraphEditor;
use ide_view::graph_editor::NodeId;
use ide_view::project;
use ide_view::project::SearcherParams;



// =============
// === Model ===
// =============

#[derive(Debug)]
struct Model {
    view:             project::View,
    input_view:       ViewNodeId,
    graph_controller: ExecutedGraph,
    graph_presenter:  Graph,
    this_arg:         Rc<Option<ThisNode>>,
    input_expression: RefCell<String>,
    mode:             Mode,
    ide_controller:   controller::Ide,
}

impl Model {
    fn input_changed(&self, new_input: &str) {
        self.input_expression.replace(new_input.to_string());
    }
}


// ==================
// === AISearcher ===
// ==================

/// Searcher that uses the user input as a prompt for an AI model that then generates a new node
/// that is inserted into the graph.
#[derive(Clone, Debug)]
pub struct AISearcher {
    _network: frp::Network,
    model:    Rc<Model>,
}

impl SearcherPresenter for AISearcher {
    fn setup_searcher(
        ide_controller: controller::Ide,
        _project_controller: Project,
        graph_controller: ExecutedGraph,
        graph_presenter: &Graph,
        view: project::View,
        parameters: SearcherParams,
    ) -> FallibleResult<Self>
    where
        Self: Sized,
    {
        let mode = Self::init_input_node(
            parameters,
            graph_presenter,
            view.graph(),
            &graph_controller.graph(),
        )?;
        let this_arg = Rc::new(match mode {
            Mode::NewNode { source_node: Some(node), .. } =>
                ThisNode::new(node, &graph_controller.graph()),
            _ => None,
        });

        let model = Rc::new(Model {
            view,
            input_view: parameters.input,
            graph_controller,
            graph_presenter: graph_presenter.clone_ref(),
            this_arg,
            input_expression: default(),
            mode,
            ide_controller,
        });

        let network = frp::Network::new("AI Searcher");
        frp::extend! { network
            eval model.view.searcher_input_changed ([model]((expr, _selections)) {
                model.input_changed(expr);
            });
        }
        Ok(Self { model, _network: network })
    }

    fn expression_accepted(
        self: Box<Self>,
        node_id: NodeId,
        _entry_id: Option<GroupEntryId>,
    ) -> Option<AstNodeId> {
        let ast_id = self.model.graph_presenter.ast_node_of_view(node_id)?;
        let expression = self.model.input_expression.borrow().clone();
        if let Err(e) = self.handle_ai_query(expression.repr()) {
            warn!("Failed to handle AI query: {:?}", e);
            self.abort_editing();
        };
        Some(ast_id)
    }

    fn abort_editing(self: Box<Self>) {
        let node = self.model.mode.node_id();
        if let Err(e) = self.model.graph_controller.graph().remove_node(node) {
            warn!("Failed to remove searcher input after aborting editing: {:?}", e);
        }
    }

    fn input_view(&self) -> ViewNodeId {
        self.model.input_view
    }
}

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "An action cannot be executed when searcher is run without `this` argument.")]
pub struct CannotRunWithoutThisArgument;

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "No visualization data received for an AI suggestion.")]
pub struct NoAIVisualizationDataReceived;

/// The notification emitted by Searcher Controller
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Notification {
    /// Code should be inserted by means of using an AI autocompletion.
    AISuggestionUpdated(String, text::Range<Byte>),
}

impl AISearcher {
    const AI_STOP_SEQUENCE: &'static str = "`";
    const AI_GOAL_PLACEHOLDER: &'static str = "__$$GOAL$$__";

    /// Accepts the current AI query and exchanges it for actual expression.
    /// To accomplish this, it performs the following steps:
    /// 1. Attaches a visualization to `this`, calling `AI.build_ai_prompt`, to
    ///    get a data-specific prompt for Open AI;
    /// 2. Sends the prompt to the Open AI backend proxy, along with the user
    ///    query.
    /// 3. Replaces the query with the result of the Open AI call.
    async fn accept_ai_query(
        mode: Mode,
        query: String,
        this: ThisNode,
        graph: ExecutedGraph,
        _graph_view: GraphEditor,
        _input_view: ViewNodeId,
        ide: controller::Ide,
    ) -> FallibleResult {
        let vis_ptr = QualifiedMethodPointer::from_qualified_text(
            "Standard.Visualization.AI",
            "Standard.Visualization.AI",
            "build_ai_prompt",
        )?;
        let vis = Visualization::new(vis_ptr.module.clone(), this.id, vis_ptr, vec![]);
        let mut result = graph.attach_visualization(vis.clone()).await?;
        let next = result.next().await.ok_or(NoAIVisualizationDataReceived)?;
        let prompt = std::str::from_utf8(&next)?;
        let prompt_with_goal = prompt.replace(Self::AI_GOAL_PLACEHOLDER, &query);
        graph.detach_visualization(vis.id).await?;
        let completion = graph.get_ai_completion(&prompt_with_goal, Self::AI_STOP_SEQUENCE).await?;
        let parser = ide.parser();
        let new_expression = input::Input::parse(parser, completion, 0.into());
        let new_expression_ast = new_expression.ast().cloned().unwrap();
        let expression_to_insert = apply_this_argument(&this.var, &new_expression_ast);
        Self::commit_node(mode, graph, expression_to_insert, this)?;
        Ok(())
    }

    fn commit_node(
        mode: Mode,
        graph: ExecutedGraph,
        expression: Ast,
        this_arg: ThisNode,
    ) -> FallibleResult {
        let node_id = mode.node_id();
        let graph = graph.graph();
        graph.set_expression_ast(node_id, expression)?;
        if let Mode::NewNode { .. } = mode {
            graph.introduce_name_on(node_id)?;
        }
        this_arg.introduce_pattern(graph.clone_ref())?;

        Ok(())
    }

    /// Handles AI queries (i.e. searcher input starting with `"AI:"`). Doesn't
    /// do anything if the query doesn't end with a specified "accept"
    /// sequence. Otherwise, calls `Self::accept_ai_query` to perform the final
    /// replacement.
    fn handle_ai_query(&self, query: String) -> FallibleResult {
        let this = self.model.this_arg.clone();
        if this.is_none() {
            return Err(CannotRunWithoutThisArgument.into());
        }
        let this = this.as_ref().as_ref().unwrap().clone();
        let graph = self.model.graph_controller.clone_ref();
        let graph_view = self.model.view.graph().clone();
        let input_view = self.model.input_view;
        let mode = self.model.mode;
        let ide = self.model.ide_controller.clone_ref();
        executor::global::spawn(async move {
            let query_result = Self::accept_ai_query(
                mode,
                query,
                this,
                graph.clone_ref(),
                graph_view,
                input_view,
                ide.clone_ref(),
            )
            .await;
            if let Err(e) = query_result {
                let error_message = format!("Error when handling AI query: {e}");
                ide.status_notifications().publish_event(error_message);
                error!("Error when handling AI query: {e}");
                if let Err(e) = graph.graph().remove_node(mode.node_id()) {
                    warn!("Failed to remove searcher view node after AI query error: {e}");
                }
            }
        });

        Ok(())
    }
}
