//! Widget controller.
//!
//! The Widget Controller is responsible for querying the language server for information about
//! the node's widget configuration or resolving it from local cache.



mod configuration;
mod response;

use crate::prelude::*;

use crate::controller::visualization::manager::Manager;
use crate::controller::visualization::manager::Notification;
use crate::controller::ExecutedGraph;
use crate::executor::global::spawn_stream_handler;
use crate::model::execution_context::VisualizationUpdateData;

use engine_protocol::language_server::SuggestionId;
use ensogl::define_endpoints_2;
use ide_view::graph_editor::component::visualization;
use ide_view::graph_editor::component::visualization::Metadata;
use ide_view::graph_editor::data::enso::Code;
use ide_view::graph_editor::ArgumentWidgetConfig;
use ide_view::graph_editor::CallWidgetsConfig;



// =================
// === Constants ===
// =================

/// A module containing the widget visualization method.
const WIDGET_VISUALIZATION_MODULE: &str = "Standard.Visualization.Widgets";
/// A name of the widget visualization method.
const WIDGET_VISUALIZATION_METHOD: &str = "get_widget_json";



// ===============
// === Aliases ===
// ===============

/// An ID of a node in the graph. Always refers to the root expression.
type NodeId = ast::Id;
// An ID of any sub expression in the node, which can have a widget attached to it.
type ExpressionId = ast::Id;



// ==================
// === Controller ===
// ==================

define_endpoints_2! {
    Input {
        /// Create or update widget query with given definition.
        request_widgets(Request),
        /// Remove all widget queries of given node that are not on this list.
        retain_node_expressions(NodeId, HashSet<ast::Id>),
        /// Remove all widget data associated with given node.
        remove_all_node_widgets(NodeId),
    }
    Output {
        /// Emitted when the node's visualization has been set.
        widget_data(NodeId, CallWidgetsConfig),
    }
}

/// Graph widgets controller. Handles requests for widget configuration using visualizations. Maps
/// response data to the relevant node Id updates, and dispatches them over the FRP output.
/// Guarantees that each individual query eventually receives an update. It internally caches the
/// results of the last queries, so that the configuration can be delivered to the presenter even
/// when no visualization change is necessary.
#[derive(Debug, Deref)]
pub struct Controller {
    #[deref]
    frp:   Frp,
    #[allow(dead_code)]
    model: Rc<RefCell<Model>>,
}

impl Controller {
    /// Constructor
    pub fn new(executed_graph: ExecutedGraph) -> Self {
        let (manager, manager_notifications) = Manager::new(executed_graph.clone_ref());
        let frp = Frp::new();

        let model = Rc::new(RefCell::new(Model {
            manager,
            graph: executed_graph.clone_ref(),
            widgets_of_node: default(),
            widget_queries: default(),
        }));

        let network = &frp.network;
        let input = &frp.input;
        let output = &frp.private.output;

        frp::extend! { network
            updates_from_cache <- input.request_widgets.filter_map(
                f!((definition) model.borrow_mut().request_widget(definition))
            );
            output.widget_data <+ updates_from_cache;
            eval input.retain_node_expressions(((node_id, expr_ids)) {
                model.borrow_mut().retain_node_expressions(*node_id, expr_ids)
            });
            eval input.remove_all_node_widgets((node_id) {
                model.borrow_mut().remove_all_node_widgets(*node_id)
            });
        };

        let out_widget_data = output.widget_data.clone_ref();
        let weak = Rc::downgrade(&model);
        spawn_stream_handler(weak, manager_notifications, move |notification, model| {
            let data = model.borrow_mut().handle_notification(notification);
            if let Some(data) = data {
                out_widget_data.emit(data);
            }
            std::future::ready(())
        });

        Self { frp, model }
    }
}



// =============
// === Model ===
// =============

/// Model of the Widget controller. Manages the widget queries, stores responses in cache. See
/// [`Controller`] for more information.
#[derive(Debug)]
pub struct Model {
    manager:         Rc<Manager>,
    graph:           ExecutedGraph,
    widgets_of_node: NodeToWidgetsMapping,
    /// Map of queries by the target expression ID. Required to be able to map visualization update
    /// responses to the corresponding widgets.
    widget_queries:  HashMap<ExpressionId, QueryData>,
}

impl Model {
    /// Visualization update notification handler. Updates the cache and returns the widget updates
    /// when the notification provides new data.
    fn handle_notification(
        &mut self,
        notification: Notification,
    ) -> Option<(NodeId, CallWidgetsConfig)> {
        let report_error = |message, error| {
            error!("{message}: {error}");
            None
        };

        match notification {
            Notification::ValueUpdate { target, data, .. } =>
                self.handle_visualization_value_update(target, data),
            Notification::FailedToAttach { error, .. } =>
                report_error("Failed to attach widget visualization", error),
            Notification::FailedToDetach { error, .. } =>
                report_error("Failed to detach widget visualization", error),
            Notification::FailedToModify { error, .. } =>
                report_error("Failed to modify widget visualization", error),
        }
    }

    /// Handle visualization data update. Return widget update data.
    fn handle_visualization_value_update(
        &mut self,
        target: ast::Id,
        data: VisualizationUpdateData,
    ) -> Option<(NodeId, CallWidgetsConfig)> {
        let query_data = self.widget_queries.get_mut(&target)?;

        let (definitions, errors) = configuration::deserialize_widget_definitions(
            &data,
            &self.graph.suggestion_db(),
            &self.graph.parser(),
        );

        for error in errors {
            error!("{:?}", error);
        }

        trace!("Widget definitions: {definitions:?}");
        let definitions = Rc::new(definitions);
        query_data.last_definitions = Some(definitions.clone());

        let call_id = query_data.call_expression;
        Some((query_data.node_id, CallWidgetsConfig { call_id, definitions }))
    }

    /// Handle a widget request from presenter. Returns the widget updates if the request can be
    /// immediately fulfilled from the cache.
    fn request_widget(&mut self, request: &Request) -> Option<(NodeId, CallWidgetsConfig)> {
        let suggestion_db = self.graph.suggestion_db();
        let suggestion = suggestion_db.lookup(request.call_suggestion).ok()?;

        use std::collections::hash_map::Entry;
        match self.widget_queries.entry(request.target_expression) {
            Entry::Occupied(mut occupied) => {
                let query = occupied.get_mut();
                if query.node_id != request.node_id {
                    self.widgets_of_node.remove_widget(query.node_id, request.target_expression);
                    self.widgets_of_node.insert_widget(request.node_id, request.target_expression);
                }

                let visualization_modified = query.update(&suggestion, request);
                if visualization_modified {
                    trace!("Updating widget visualization for {}", request.target_expression);
                    query.request_visualization(&self.manager, request.target_expression);

                    // The request is now pending. Once the request completes, the widget update
                    // will happen in the response handler.
                    None
                } else {
                    // In the event that the visualization was not modified, we want to respond with
                    // the last known visualization data. Each widget request needs to be responded
                    // to, otherwise the widget might not be displayed after the widget view has
                    // been temporarily removed and created again.
                    query.last_definitions()
                }
            }
            Entry::Vacant(vacant) => {
                self.widgets_of_node.insert_widget(request.node_id, request.target_expression);

                let query = vacant.insert(QueryData::new(&suggestion, request));
                trace!("Registering widget visualization for {}", request.target_expression);
                query.request_visualization(&self.manager, request.target_expression);

                // The request is now pending. Once the request completes, the widget update will
                // happen in the response handler.
                None
            }
        }
    }

    /// Remove all widget queries of given node that are attached to expressions outside of provided
    /// list. No widget update is emitted after a query is cleaned up.
    fn retain_node_expressions(&mut self, node_id: NodeId, expressions: &HashSet<ast::Id>) {
        self.widgets_of_node.retain_node_widgets(node_id, expressions, |expr_id| {
            self.manager.remove_visualization(expr_id);
        });
    }

    /// Remove all widget queries of given node. No widget update is emitted after a query is
    /// cleaned up.
    fn remove_all_node_widgets(&mut self, node_id: NodeId) {
        for expr_id in self.widgets_of_node.remove_node_widgets(node_id) {
            self.manager.remove_visualization(expr_id);
        }
    }
}



// ============================
// === NodeToWidgetsMapping ===
// ============================

/// A map of widgets attached to nodes. Used to perform cleanup of node widget queries when node is
/// removed.
#[derive(Debug, Default)]
struct NodeToWidgetsMapping {
    attached_widgets: HashMap<NodeId, Vec<ExpressionId>>,
}

impl NodeToWidgetsMapping {
    fn remove_widget(&mut self, node_id: NodeId, target: ast::Id) {
        self.attached_widgets.entry(node_id).and_modify(|exprs| {
            let Some(index) = exprs.iter().position(|e| *e == target) else { return };
            exprs.swap_remove(index);
        });
    }

    fn insert_widget(&mut self, node_id: NodeId, target: ast::Id) {
        self.attached_widgets.entry(node_id).or_default().push(target);
    }

    fn retain_node_widgets(
        &mut self,
        node_id: NodeId,
        remaining_expressions: &HashSet<ast::Id>,
        mut on_remove: impl FnMut(ExpressionId),
    ) {
        if let Some(registered) = self.attached_widgets.get_mut(&node_id) {
            registered.retain(|expr_id| {
                let retained = remaining_expressions.contains(expr_id);
                if !retained {
                    on_remove(*expr_id);
                }
                retained
            });
        }
    }

    fn remove_node_widgets(&mut self, node_id: NodeId) -> Vec<ExpressionId> {
        self.attached_widgets.remove(&node_id).unwrap_or_default()
    }
}



// ===============
// === Request ===
// ===============

/// Definition of a widget request. Defines the node subexpression that the widgets will be attached
/// to, and the method call that corresponds to that expression.
#[derive(Debug, Default, Clone, Copy)]
pub struct Request {
    /// The node ID of a node that contains the widget.
    pub node_id:           NodeId,
    /// Expression of the whole method call. Only used to correlate the visualization response with
    /// the widget view.
    pub call_expression:   ExpressionId,
    /// Target (`self`) argument in the call expression. Used as a visualization target.
    pub target_expression: ExpressionId,
    /// The suggestion ID of the method that this call refers to.
    pub call_suggestion:   SuggestionId,
}



// =================
// === QueryData ===
// =================

/// Data of ongoing widget query. Defines which expressions a visualization query is attached to,
/// and maintains enough data to correlate the response with respective widget view.
#[derive(Debug)]
struct QueryData {
    node_id:          NodeId,
    call_expression:  ExpressionId,
    method_name:      ImString,
    arguments:        Vec<ImString>,
    last_definitions: Option<Rc<Vec<ArgumentWidgetConfig>>>,
}

impl QueryData {
    fn new(suggestion: &enso_suggestion_database::Entry, req: &Request) -> Self {
        let node_id = req.node_id;
        let arguments = suggestion.arguments.iter().map(|arg| arg.name.clone().into()).collect();
        let method_name = suggestion.name.clone();
        let call_expression = req.call_expression;
        let last_definitions = None;
        QueryData { node_id, arguments, method_name, call_expression, last_definitions }
    }

    /// Update existing query data on new request. Returns true if the visualization query needs to
    /// be updated.
    fn update(&mut self, suggestion: &enso_suggestion_database::Entry, req: &Request) -> bool {
        let mut visualization_modified = false;

        if self.method_name != suggestion.name {
            self.method_name = suggestion.name.clone();
            visualization_modified = true;
        }

        let mut zipped_arguments = self.arguments.iter().zip(&suggestion.arguments);
        if self.arguments.len() != suggestion.arguments.len()
            || !zipped_arguments.all(|(a, b)| a == &b.name)
        {
            self.arguments =
                suggestion.arguments.iter().map(|arg| arg.name.clone().into()).collect();
            visualization_modified = true;
        }

        self.node_id = req.node_id;
        self.call_expression = req.call_expression;
        visualization_modified
    }

    fn last_definitions(&self) -> Option<(NodeId, CallWidgetsConfig)> {
        self.last_definitions.as_ref().map(|definitions| {
            let call_id = self.call_expression;
            let config = CallWidgetsConfig { call_id, definitions: definitions.clone() };
            (self.node_id, config)
        })
    }

    fn request_visualization(&mut self, manager: &Rc<Manager>, target_expression: ast::Id) {
        // When visualization is requested, remove stale queried value to prevent updates  while
        // language server request is pending.
        self.last_definitions.take();
        let vis_metadata = self.visualization_metadata();
        manager.request_visualization(target_expression, vis_metadata);
    }

    /// Generate visualization metadata for this query.
    fn visualization_metadata(&self) -> Metadata {
        let arguments: Vec<Code> = vec![
            Self::as_unresolved_symbol(&self.method_name).into(),
            Self::arg_sequence(&self.arguments).into(),
        ];

        let preprocessor = visualization::instance::PreprocessorConfiguration {
            module:    WIDGET_VISUALIZATION_MODULE.into(),
            method:    WIDGET_VISUALIZATION_METHOD.into(),
            arguments: Rc::new(arguments),
        };
        Metadata { preprocessor }
    }

    /// Escape a string to be used as a visualization argument. Transforms the string into an enso
    /// expression with string literal.
    fn escape_visualization_argument(arg: &str) -> String {
        Ast::raw_text_literal(arg).repr()
    }

    /// Creates unresolved symbol via ".name" syntax. Unresolved symbol contains name and also
    /// module scope to resolve it properly.
    fn as_unresolved_symbol(arg: &str) -> String {
        format!(".{arg}")
    }

    /// Escape a list of strings to be used as a visualization argument. Transforms the strings into
    /// an enso expression with a list of string literals.
    fn arg_sequence(args: &[ImString]) -> String {
        let mut buffer = String::from("[");
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                buffer.push_str(", ");
            }
            buffer.push_str(&Self::escape_visualization_argument(arg));
        }
        buffer.push(']');
        buffer
    }
}
