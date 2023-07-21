//! Searcher trait. This trait is implemented by all searchers and exposes the API that is used by
//! the project presenter to interact with the searcher. Contains also some shared logic and
//! utility functions.

use crate::prelude::*;

use crate::controller::graph::NewNodeInfo;
use crate::controller::searcher::Mode;
use crate::model::module::NodeMetadata;
use crate::presenter;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;

use ide_view as view;
use ide_view::component_browser::component_list_panel::grid as component_grid;
use ide_view::graph_editor::GraphEditor;
use ide_view::graph_editor::NodeId;
use ide_view::project::SearcherParams;
use ide_view::project::SearcherType;


// ==============
// === Export ===
// ==============

pub mod ai;
pub mod component_browser;



/// Trait for the searcher.
pub trait SearcherPresenter: Debug {
    /// Initiate the operating mode for the searcher based on the given [`SearcherParams`]. If the
    /// view associated with the input node given in the parameters does not yet exist, it will
    /// be created.
    ///
    /// Returns the [`Mode`] that should be used for the searcher.
    fn init_input_node(
        parameters: SearcherParams,
        graph_presenter: &presenter::Graph,
        graph_editor: &GraphEditor,
        graph_controller: &controller::Graph,
    ) -> FallibleResult<Mode>
    where
        Self: Sized,
    {
        let SearcherParams { input, .. } = parameters;
        let ast_node = graph_presenter.ast_node_of_view(input);

        let mode = match ast_node {
            Some(node_id) => Mode::EditNode { node_id },
            None => {
                let (new_node, source_node) =
                    create_input_node(parameters, graph_presenter, graph_editor, graph_controller)?;
                Mode::NewNode { node_id: new_node, source_node }
            }
        };
        let target_node = mode.node_id();

        // We only want to show the preview of the node if it is a component browser searcher.
        if matches!(parameters.searcher_type, SearcherType::ComponentBrowser) {
            if let Some(target_node_view) = graph_presenter.view_id_of_ast_node(target_node) {
                graph_editor.model.with_node(target_node_view, |node| node.show_preview());
            }
        } else {
            warn!("No view associated with node {:?}.", target_node);
        }
        // We disable auto-updates for the expression of the node, so we can set the expression
        // of the input node without triggering an update of the graph. This is used, for example,
        // to show a preview of the item selected in the component browser without changing the
        // text the user has typed on the searcher input node.
        graph_presenter.allow_expression_auto_updates(target_node, false);

        Ok(mode)
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
    ) -> FallibleResult<Self>
    where
        Self: Sized;

    /// As [`setup_searcher`], but returns a boxed presenter.
    fn setup_searcher_boxed(
        ide_controller: controller::Ide,
        project_controller: controller::Project,
        graph_controller: controller::ExecutedGraph,
        graph_presenter: &presenter::Graph,
        view: view::project::View,
        parameters: SearcherParams,
    ) -> FallibleResult<Box<dyn SearcherPresenter>>
    where
        Self: Sized + 'static,
    {
        // Avoiding the cast would require a local variable, which would not be more readable.
        #![allow(trivial_casts)]
        Self::setup_searcher(
            ide_controller,
            project_controller,
            graph_controller,
            graph_presenter,
            view,
            parameters,
        )
        .map(|searcher| Box::new(searcher) as Box<dyn SearcherPresenter>)
    }

    /// Expression accepted in Component Browser.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes. The `entry_id` might be none in case where the user want to accept
    /// the node input without any entry selected. If the commitment results in creating a new
    /// node, its AST ID is returned.
    fn expression_accepted(
        self: Box<Self>,
        node_id: NodeId,
        entry_id: Option<component_grid::EntryId>,
    ) -> Option<AstNodeId>;

    /// Abort editing, without taking any action.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes.
    fn abort_editing(self: Box<Self>);

    /// Returns the node view that is being edited by the searcher.
    fn input_view(&self) -> ViewNodeId;
}


// === Helpers ===

/// Create a new AST that combines a `this` argument with the given AST. For example, to add a
/// method call `sort` to this argument `table`. That would result in an AST that represents
/// `table.sort`.  
pub fn apply_this_argument(this_var: &str, ast: &Ast) -> Ast {
    if let Ok(opr) = ast::known::Opr::try_from(ast) {
        let shape = ast::SectionLeft { arg: Ast::var(this_var), off: 1, opr: opr.into() };
        Ast::new(shape, None)
    } else if let Some(mut infix) = ast::opr::GeneralizedInfix::try_new(ast) {
        if let Some(ref mut larg) = &mut infix.left {
            larg.arg = apply_this_argument(this_var, &larg.arg);
        } else {
            infix.left = Some(ast::opr::ArgWithOffset { arg: Ast::var(this_var), offset: 1 });
        }
        infix.into_ast()
    } else if let Some(mut prefix_chain) = ast::prefix::Chain::from_ast(ast) {
        prefix_chain.func = apply_this_argument(this_var, &prefix_chain.func);
        prefix_chain.into_ast()
    } else {
        let shape = ast::Infix {
            larg: Ast::var(this_var),
            loff: 0,
            opr:  Ast::opr(ast::opr::predefined::ACCESS),
            roff: 0,
            rarg: ast.clone_ref(),
        };
        Ast::new(shape, None)
    }
}

/// Initialise the expression in case there is a source node for the new node. This allows us to
/// correctly render an edge from the source node to the new node.
fn initialise_with_this_argument(
    created_node: ast::Id,
    source_node: Option<ast::Id>,
    graph_controller: &controller::Graph,
) {
    let node = source_node.and_then(|node| graph_controller.node(node).ok());
    let this_expr =
        node.and_then(|node| node.variable_name().ok().flatten().map(|name| name.to_string()));
    let initial_expression =
        this_expr.map(|this_expr| apply_this_argument(&this_expr, &Ast::blank()));
    if let Some(initial_expression) = initial_expression {
        if let Err(e) = graph_controller.set_expression(created_node, initial_expression.repr()) {
            warn!("Failed to set initial expression for node {:?}: {}", created_node, e);
        }
    } else {
        warn!("Failed to create initial expression for node {:?}.", created_node);
    }
}

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

    initialise_with_this_argument(created_node, source_node, graph_controller);

    Ok((created_node, source_node))
}
