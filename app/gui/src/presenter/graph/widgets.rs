//! Widgets controller.
//!
//! The Widgets Controller is responsible for querying the language server for  information about
//! the node's widget metadata or resolving from local cache.

use crate::prelude::*;

use crate::executor::global::spawn_stream_handler;
use crate::presenter::graph::visualization::manager::Manager;
use crate::presenter::graph::visualization::manager::Notification;
use crate::presenter::graph::ViewNodeId;

use controller::ExecutedGraph;
use engine_protocol::language_server::SuggestionId;
use ensogl::define_endpoints_2;
use ide_view::graph_editor::component::node::input::widget;
use ide_view::graph_editor::component::visualization;
use ide_view::graph_editor::component::visualization::Metadata;
use ide_view::graph_editor::data::enso::Code;
use ide_view::graph_editor::WidgetUpdate;
use ide_view::graph_editor::WidgetUpdates;



/// =================
/// === Constants ===
/// =================

const WIDGET_VISUALIZATION_MODULE: &str = "Standard.Visualization.Widgets";
const WIDGET_VISUALIZATION_METHOD: &str = "get_full_annotations_json";



// ===============
// === Widgets ===
// ===============

define_endpoints_2! {
    Input {
        /// Create or update widget query with given definition.
        request_widgets(Request),
        /// Remove all widget queries of given node that are not on this list.
        retain_node_expressions(ViewNodeId, HashSet<ast::Id>),
        /// Remove all widget queries of given node.
        remove_node(ViewNodeId),
    }
    Output {
        /// Emitted when the node's visualization has been set.
        widget_data(ViewNodeId, WidgetUpdates),
    }
}

/// Graph widgets controller. One requester per function call. Responsible for querying the language
/// server for information about the node's widget metadata or resolving from local cache.
#[derive(Debug)]
pub struct Widgets {
    frp:   Frp,
    #[allow(dead_code)]
    model: Rc<RefCell<Model>>,
}

impl Deref for Widgets {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Widgets {
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
            eval input.remove_node((node_id) model.borrow_mut().remove_node(*node_id));
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

/// Model of the Widgets controller. Manages the widget queries.
#[derive(Debug)]
pub struct Model {
    manager:         Rc<Manager>,
    graph:           ExecutedGraph,
    widgets_of_node: HashMap<ViewNodeId, Vec<ast::Id>>,
    widget_queries:  HashMap<ast::Id, QueryData>,
}

/// Data of the query that
#[derive(Debug)]
struct QueryData {
    node_id:         ViewNodeId,
    call_expression: ast::Id,
    method_name:     ImString,
    arguments:       Vec<ImString>,
    last_update:     Option<WidgetUpdates>,
}

/// Definition of a widget request. Defines the operation expression that the widgets will be
/// attached to.
#[derive(Debug, Default, Clone, Copy)]
pub struct Request {
    /// The node id of a node that contains the operation expression.
    pub node_id:           ViewNodeId,
    /// Expression of the whole method call.
    pub call_expression:   ast::Id,
    /// Expression of the call target. Used as a visualization target.
    pub target_expression: ast::Id,
    /// The suggestion id of the method that this call refers to.
    pub call_suggestion:   SuggestionId,
}

impl Model {
    fn handle_notification(
        &mut self,
        notification: Notification,
    ) -> Option<(ViewNodeId, WidgetUpdates)> {
        match notification {
            Notification::ValueUpdate { target, data, .. } => {
                let query_data = self.widget_queries.get_mut(&target)?;
                let args: Vec<(String, serde_json::Value)> = serde_json::from_slice(&data).ok()?;
                let updates = args
                    .into_iter()
                    .map(|(argument_name, meta_json)| {
                        // Treat failed deserialization as non-widget data. The returned annotation
                        // can be an arbitrary structure, so there will be cases when we fail to
                        // deserialize it, as it may not contain the widget data in the first place.
                        let deserialized = serde_json::from_value(meta_json).ok();
                        let meta = deserialized.map(WidgetVisualizationData::into_metadata);
                        WidgetUpdate { argument_name, meta }
                    })
                    .collect();


                let updates = WidgetUpdates { target_id: target, updates: Rc::new(updates) };
                trace!("Widget updates: {updates:?}");
                query_data.last_update = Some(updates.clone());
                Some((query_data.node_id, updates))
            }
            Notification::FailedToAttach { error, .. } => {
                error!("Failed to attach widget visualization: {error}");
                None
            }
            Notification::FailedToDetach { error, .. } => {
                error!("Failed to detach widget visualization: {error}");
                None
            }
            Notification::FailedToModify { error, .. } => {
                error!("Failed to modify widget visualization: {error}");
                None
            }
        }
    }

    fn request_widget(&mut self, req: &Request) -> Option<(ViewNodeId, WidgetUpdates)> {
        let suggestion_db = self.graph.suggestion_db();
        let entry = suggestion_db.lookup(req.call_suggestion).ok()?;

        use std::collections::hash_map::Entry;
        match self.widget_queries.entry(req.target_expression) {
            Entry::Occupied(mut occupied) => {
                let query = occupied.get_mut();
                if req.node_id != query.node_id {
                    self.widgets_of_node
                        .entry(req.node_id)
                        .or_default()
                        .push(req.target_expression);
                    self.widgets_of_node.entry(query.node_id).and_modify(|exprs| {
                        exprs.retain(|id| *id != req.target_expression);
                    });
                    query.node_id = req.node_id;
                }

                let mut visualization_modified = false;
                if query.method_name != entry.name {
                    query.method_name = entry.name.clone().into();
                    visualization_modified = true;
                }

                let mut zipped_arguments = query.arguments.iter().zip(&entry.arguments);
                if query.arguments.len() != entry.arguments.len()
                    || !zipped_arguments.all(|(a, b)| a == &b.name)
                {
                    query.arguments =
                        entry.arguments.iter().map(|arg| arg.name.clone().into()).collect();
                    visualization_modified = true;
                }

                query.call_expression = req.call_expression;

                if visualization_modified {
                    let vis_metadata = Self::visualization_metadata(query);
                    let manager = self.manager.clone_ref();
                    let target_expression = req.target_expression;

                    // When visualization is modified, remove stale queried value to prevent updates
                    // while language server request is pending.
                    query.last_update.take();
                    manager.request_visualization(target_expression, vis_metadata);
                    // The request is now pending. Once the request completes, the widget update
                    // will happen in the response handler.
                    None
                } else {
                    // In the event that the visualization was not modified, we want to respond with
                    // the last known visualization data. Each widget request needs to be responded
                    // to, otherwise the widget might not be displayed after the widget view has
                    // been temporarily removed and created again.
                    query.last_update.as_ref().map(|updates| (query.node_id, updates.clone()))
                }
            }
            Entry::Vacant(vacant) => {
                let node_id = req.node_id;
                let method_name = entry.name.clone().into();
                let call_expression = req.call_expression;
                self.widgets_of_node.entry(req.node_id).or_default().push(req.target_expression);
                let arguments = entry.arguments.iter().map(|arg| arg.name.clone().into()).collect();

                let query_data = QueryData {
                    node_id,
                    arguments,
                    method_name,
                    call_expression,
                    last_update: None,
                };
                let query = vacant.insert(query_data);
                let vis_metadata = Self::visualization_metadata(query);

                let manager = self.manager.clone_ref();
                trace!("Registering widget visualization for {}", req.target_expression);
                manager.request_visualization(req.target_expression, vis_metadata);
                None
            }
        }
    }

    /// Remove all widget queries of given node that are connected to expression not on this list.
    fn retain_node_expressions(&mut self, node_id: ViewNodeId, expressions: &HashSet<ast::Id>) {
        let registered = self.widgets_of_node.get_mut(&node_id);
        if let Some(registered) = registered {
            registered.retain(|expr_id| {
                let retained = expressions.contains(expr_id);
                if !retained {
                    self.manager.remove_visualization(*expr_id);
                }
                retained
            });
        }
    }

    fn remove_node(&mut self, node_id: ViewNodeId) {
        if let Some(registered) = self.widgets_of_node.remove(&node_id) {
            registered.into_iter().for_each(|expr_id| self.manager.remove_visualization(expr_id));
        }
    }

    fn visualization_metadata(query: &QueryData) -> Metadata {
        let relevant_arguments = query.arguments.split_first().map_or_default(|(_self, args)| args);
        let arguments: Vec<Code> = vec![
            Self::escape_visualization_argument(&query.method_name).into(),
            Self::arg_sequence(relevant_arguments).into(),
        ];

        let preprocessor = visualization::instance::PreprocessorConfiguration {
            module:    WIDGET_VISUALIZATION_MODULE.into(),
            method:    WIDGET_VISUALIZATION_METHOD.into(),
            arguments: Rc::new(arguments),
        };
        Metadata { preprocessor }
    }

    fn escape_visualization_argument(arg: &str) -> String {
        let segment = ast::SegmentPlain { value: arg.into() };
        let text = ast::TextLineRaw { text: vec![segment.into()] };
        text.repr()
    }

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

/// ===============================
/// === WidgetVisualizationData ===
/// ===============================

/// type representing the data received from the widget visualization for single widget.
#[derive(Debug, serde::Deserialize)]
struct WidgetVisualizationData {
    constructor: widget::Kind,
    display:     WidgetVisualizationDataDisplay,
    values:      Vec<String>,
}

#[derive(Debug, serde::Deserialize)]
struct WidgetVisualizationDataDisplay {
    constructor: widget::Display,
}

impl WidgetVisualizationData {
    fn into_metadata(self) -> widget::Metadata {
        let kind = self.constructor;
        let display = self.display.constructor;
        let dynamic_entries = self.values.iter().map(Into::into).collect();
        widget::Metadata { kind, display, dynamic_entries }
    }
}
