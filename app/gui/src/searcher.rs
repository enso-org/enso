//! Searcher trait.
use crate::controller::graph::NewNodeInfo;
use crate::controller::searcher::Mode;
use crate::model::module::NodeMetadata;
use crate::prelude::*;
use crate::presenter;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;
use ensogl::application::Application;
use ensogl::display;
use ide_view as view;
use ide_view::component_browser::component_list_panel::grid as component_grid;
use ide_view::graph_editor::GraphEditor;
use ide_view::graph_editor::NodeId;
use ide_view::project::SearcherParams;
use ide_view::project::SearcherType;

/// Create a new input node for use in the searcher. Initiates a new node in the ast and
/// associates it with the already existing view.
///
/// Returns the new node id and optionally the source node which was selected/dragged when
/// creating this node.
fn create_input_node(
    parameters: SearcherParams,
    graph: &presenter::Graph,
    graph_editor: &GraphEditor,
    graph_controller: &controller::Graph,
) -> FallibleResult<(ast::Id, Option<ast::Id>)> {
    /// The expression to be used for newly created nodes when initialising the searcher without
    /// an existing node.
    const DEFAULT_INPUT_EXPRESSION: &str = "Nothing";
    let SearcherParams { input, source_node, .. } = parameters;

    let view_data = graph_editor.model.nodes.get_cloned_ref(&input);

    let position = view_data.map(|node| node.position().xy());
    let position = position.map(|vector| model::module::Position { vector });

    let metadata = NodeMetadata { position, ..default() };
    let mut new_node = NewNodeInfo::new_pushed_back(DEFAULT_INPUT_EXPRESSION);
    new_node.metadata = Some(metadata);
    new_node.introduce_pattern = false;
    let transaction_name = "Add code for created node's visualization preview.";
    let _transaction = graph_controller
        .undo_redo_repository()
        .open_ignored_transaction_or_ignore_current(transaction_name);
    let created_node = graph_controller.add_node(new_node)?;

    graph.assign_node_view_explicitly(input, created_node);

    let source_node = source_node.and_then(|id| graph.ast_node_of_view(id.node));

    Ok((created_node, source_node))
}

/// Trait for the searcher.
pub trait SearcherPresenter: Debug {
    /// Initiate the operating mode for the searcher based on the given [`SearcherParams`]. If the
    /// view associated with the input node given in the parameters does not yet exist, it will
    /// be created.
    ///
    /// Returns the [`Mode`] that should be used for the searcher.
    fn init_input_node(
        parameters: SearcherParams,
        graph: &presenter::Graph,
        graph_editor: &GraphEditor,
        graph_controller: &controller::Graph,
    ) -> FallibleResult<Mode>
    where
        Self: Sized,
    {
        let SearcherParams { input, .. } = parameters;
        let ast_node = graph.ast_node_of_view(input);

        let mode = match ast_node {
            Some(node_id) => Ok(Mode::EditNode { node_id }),
            None => {
                let (new_node, source_node) =
                    create_input_node(parameters, graph, graph_editor, graph_controller)?;
                Ok(Mode::NewNode { node_id: new_node, source_node })
            }
        };
        let target_node = mode.as_ref().map(|mode| mode.node_id());
        if let Ok(target_node) = target_node {
            if let Some(target_node_view) = graph.view_id_of_ast_node(target_node) {
                if matches!(parameters.searcher_type, SearcherType::ComponentBrowser) {
                    graph_editor.model.with_node(target_node_view, |node| node.show_preview());
                }
            } else {
                warn!("No view associated with node {:?}.", target_node);
            }
            graph.allow_expression_auto_updates(target_node, false);
        } else {
            warn!("No target node for searcher.");
        }
        mode
    }

    /// Setup new, appropriate searcher controller for the edition of `node_view`, and construct
    /// presenter handling it.
    fn setup_searcher(
        ide_controller: controller::Ide,
        project_controller: controller::Project,
        graph_controller: controller::ExecutedGraph,
        graph_presenter: &presenter::Graph,
        view: view::project::View,
        parameters: SearcherParams,
    ) -> FallibleResult<Box<dyn SearcherPresenter>>
    where
        Self: Sized;

    /// Expression accepted in Component Browser.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes. The `entry_id` might be none in case where the user want to accept
    /// the node input without any entry selected. If the commitment results in creating a new
    /// node, its AST ID is returned.
    fn expression_accepted(
        self: Box<Self>,
        node_id: NodeId,
        entry_id: Option<component_grid::GroupEntryId>,
    ) -> Option<AstNodeId>;

    /// Abort editing, without taking any action.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes.
    fn abort_editing(self: Box<Self>);

    /// Returns the node view that is being edited by the searcher.
    fn input_view(&self) -> ViewNodeId;

    // fn view(app: &ensogl::application::Application) -> Option<display::object::Instance>
    // where Self: Sized;
}
