use flo_stream::Subscriber;

use double_representation::graph::GraphInfo;
use double_representation::graph::LocationHint;
use double_representation::name::project;
use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use double_representation::node::NodeInfo;
use engine_protocol::language_server;
use enso_executor::global::spawn_stream_handler;
use enso_frp as frp;
use enso_prelude::FallibleResult;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::entry::Id as EntryId;
use enso_text as text;
use enso_text::Byte;
use enso_text::Location;
use enso_text::Rope;
use ensogl_component::list_view::entry::Id;
use ide_view as view;
use ide_view::component_browser::component_list_panel::grid::GroupEntryId;
use ide_view::graph_editor::component::node;
use ide_view::graph_editor::component::node::input::area::Expression;
use ide_view::graph_editor::GraphEditor;
use ide_view::graph_editor::NodeId;
use ide_view::project::SearcherParams;
use ide_view::project::View;

use crate::controller::graph::executed::Handle;
use crate::controller::graph::FailedToCreateNode;
use crate::controller::graph::ImportType;
use crate::controller::graph::RequiredImport;
use crate::controller::searcher::component::group;
use crate::controller::searcher::Mode;
use crate::controller::searcher::ThisNode;
use crate::controller::ExecutedGraph;
use crate::controller::Ide;
use crate::controller::Project;
use crate::model::execution_context::QualifiedMethodPointer;
use crate::model::execution_context::Visualization;
use crate::model::module::NodeEditStatus;
use crate::model::module::NodeMetadata;
use crate::model::suggestion_database;
use crate::model::traits::*;
use crate::prelude::*;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;
use crate::presenter::Graph;

// =============
// === Model ===
// =============
#[derive(Debug)]
struct Model {
    view:             view::project::View,
    input_view:       ViewNodeId,
    graph_controller: ExecutedGraph,
    graph_presenter:  Graph,
    this_arg:         Rc<Option<ThisNode>>,
    input_expression: RefCell<String>,
}

impl Model {
    fn input_changed(&self, new_input: &str) {
        self.input_expression.replace(new_input.to_string());
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, Debug)]
pub struct AISearcher {
    _network: frp::Network,
    model:    Rc<Model>,
}

impl crate::searcher::Searcher for AISearcher {
    fn setup_searcher(
        _ide_controller: Ide,
        _project_controller: Project,
        graph_controller: ExecutedGraph,
        graph_presenter: &Graph,
        view: View,
        parameters: SearcherParams,
    ) -> FallibleResult<Box<dyn crate::searcher::Searcher>>
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
            input_expression: RefCell::new("".to_string()),
        });

        let network = frp::Network::new("AI Searcher");
        frp::extend! { network
            eval model.view.searcher_input_changed ([model]((expr, _selections)) {
                model.input_changed(expr);
            });
        }
        Ok(Box::new(Self { model, _network: network }))
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
        };
        Some(ast_id)
    }

    fn abort_editing(self: Box<Self>) {
        console_log!("Aborting editing");
        let graph = self.model.view.graph().clone();
        let expression = node::Expression::new_plain(self.model.input_expression.borrow().clone());
        graph.set_node_expression((self.input_view(), expression));
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
        query: String,
        this: ThisNode,
        graph: Handle,
        graph_view: GraphEditor,
        input_view: ViewNodeId,
    ) -> FallibleResult {
        console_log!("Accepting AI query: {}", query);
        let vis_ptr = QualifiedMethodPointer::from_qualified_text(
            "Standard.Visualization.AI",
            "Standard.Visualization.AI",
            "build_ai_prompt",
        )?;
        let vis = Visualization::new(this.id, vis_ptr, vec![]);
        let mut result = graph.attach_visualization(vis.clone()).await?;
        console_log!("Attached visualization: {:?}", vis);
        let next = result.next().await.ok_or(NoAIVisualizationDataReceived)?;
        console_log!("Got next: {:?}", next);
        let prompt = std::str::from_utf8(&next)?;
        console_log!("Got prompt: {}", prompt);
        let prompt_with_goal = prompt.replace(Self::AI_GOAL_PLACEHOLDER, &query);
        graph.detach_visualization(vis.id).await?;
        console_log!("Detached visualization: {:?}", vis);
        let completion = graph.get_ai_completion(&prompt_with_goal, Self::AI_STOP_SEQUENCE).await?;
        console_log!("Got completion: {}", completion);
        let expression = node::Expression::new_plain(completion);
        graph_view.set_node_expression((input_view, expression));
        console_log!("Edited node expression");
        Ok(())
    }

    /// Handles AI queries (i.e. searcher input starting with `"AI:"`). Doesn't
    /// do anything if the query doesn't end with a specified "accept"
    /// sequence. Otherwise, calls `Self::accept_ai_query` to perform the final
    /// replacement.
    fn handle_ai_query(&self, query: String) -> FallibleResult {
        console_log!("Handling AI query: {}", query);
        let this = self.model.this_arg.clone();
        if this.is_none() {
            console_log!("Cannot run AI query without this argument");
            return Err(CannotRunWithoutThisArgument.into());
        }
        let this = this.as_ref().as_ref().unwrap().clone();
        let graph = self.model.graph_controller.clone_ref();
        let graph_view = self.model.view.graph().clone();
        let input_view = self.model.input_view;
        executor::global::spawn(async move {
            if let Err(e) = Self::accept_ai_query(query, this, graph, graph_view, input_view).await
            {
                error!("Error when handling AI query: {e}");
            }
        });

        Ok(())
    }
}
