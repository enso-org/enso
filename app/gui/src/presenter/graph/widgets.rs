//! Widgets controller.
//!
//! The Widgets Controller is responsible for querying the language server for  information about
//! the node's widget metadata or resolving from local cache.

use crate::prelude::*;

use crate::executor::global::spawn_stream_handler;
use crate::presenter::graph;
use crate::presenter::graph::visualization::manager::Manager;
use crate::presenter::graph::visualization::manager::Notification;
use crate::presenter::graph::ViewNodeId;

use controller::ExecutedGraph;
use engine_protocol::language_server::SuggestionId;
use ensogl::define_endpoints_2;
use ide_view::graph_editor::component::node as node_view;
use ide_view::graph_editor::component::node::input::widget;
use ide_view::graph_editor::component::visualization;
use ide_view::graph_editor::component::visualization::Metadata;
use ide_view::graph_editor::GraphEditor;


// ===============
// === Widgets ===
// ===============

define_endpoints_2! {
    Input {
        /// Create or update widget query with given definition.
        run_query(QueryDefinition),
        /// Remove all widget queries of given node that are not on this list.
        retain_node_expressions(ViewNodeId, HashSet<ast::Id>),
        /// Remove all widget queries of given node.
        remove_node(ViewNodeId),
    }
    Output {
        /// Emitted when the node's visualization has been set.
        widget_data((ViewNodeId,WidgetData)),
    }
}

/// Graph widgets controller. One requester per function call. Responsible for querying the language
/// server for information about the node's widget metadata or resolving from local cache.
#[derive(Debug)]
pub struct Widgets {
    frp:   Frp,
    model: Rc<RefCell<Model>>,
}

impl Deref for Widgets {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &*self.frp
    }
}

struct WidgetData {
    node_id:   ViewNodeId,
    arguments: Vec<ArgumentData>,
}

struct ArgumentData {
    name: String,
    meta: widget::Metadata,
}

impl Widgets {
    /// Constructor
    pub fn new(
        executed_graph: ExecutedGraph,
        project: model::Project,
        _view: GraphEditor,
        state: Rc<graph::state::State>,
    ) -> Self {
        let (manager, manager_notifications) = Manager::new(executed_graph.clone_ref());
        let frp = Frp::new();

        // let segments =
        //     project.main_module_path().id().parent_modules.iter().chain(Uuid::new_v4().
        // to_string()); let temp_module_path =
        //     model::module::Path::from_name_segments(project.project_content_root_id(), segments);

        let model = Rc::new(RefCell::new(Model {
            manager,
            project,
            graph: executed_graph.clone_ref(),
            state,
            expr_of_node: default(),
            expr_queries: default(),
        }));

        let network = &frp.network;
        let input = &frp.private.input;
        let output = &frp.output;

        frp::extend! { network
            eval input.run_query((definition) model.borrow_mut().run_query(definition));
            eval input.retain_node_expressions((node_id,expr_ids) {
                model.borrow_mut().retain_node_expressions(node_id,expr_ids)
            });
            eval input.remove_node((node_id) model.borrow_mut().remove_node(node_id));
        };

        let out_widget_data = output.widget_data.clone_ref();
        let weak = Rc::downgrade(&model);
        spawn_stream_handler(weak, manager_notifications, move |notification, model| {
            let data = model.borrow_mut().handle_notification(notification);
            data.map(move |data| out_widget_data.emit(data));
            std::future::ready(())
        });

        Self { frp, model }
    }

    // fn setup_graph_listener(self, graph_controller: ExecutedGraph) -> Self {
    //     use controller::graph;
    //     use controller::graph::executed::Notification;
    //     let notifications = graph_controller.subscribe();
    //     let weak = Rc::downgrade(&self.model);
    //     spawn_stream_handler(weak, notifications, move |notification, model| {
    //         match notification {
    //             Notification::Graph(graph::Notification::Invalidate)
    //             | Notification::EnteredNode(_)
    //             | Notification::SteppedOutOfNode(_) => match graph_controller.graph().nodes() {
    //                 Ok(nodes) => {
    //                     let nodes_set = nodes.into_iter().map(|n| n.id()).collect();
    //                     model.manager.retain_visualizations(&nodes_set);
    //                 }
    //                 Err(err) => {
    //                     error!("Cannot update visualization after graph change: {err}");
    //                 }
    //             },
    //             _ => {}
    //         }
    //         std::future::ready(())
    //     });
    //     self
    // }
}



// =============
// === Model ===
// =============

#[derive(Debug)]
pub struct Model {
    manager:      Rc<Manager>,
    graph:        ExecutedGraph,
    project:      model::Project,
    state:        Rc<graph::state::State>,
    expr_of_node: HashMap<ViewNodeId, Vec<ast::Id>>,
    expr_queries: HashMap<ast::Id, QueryData>,
}

const PREFIX_FUNC: ast::Crumb = ast::Crumb::Prefix(ast::crumbs::PrefixCrumb::Func);
const INFIX_LEFT: ast::Crumb = ast::Crumb::Infix(ast::crumbs::InfixCrumb::LeftOperand);
const INFIX_RIGHT: ast::Crumb = ast::Crumb::Infix(ast::crumbs::InfixCrumb::RightOperand);

#[derive(Debug, Default)]
pub struct QueryDefinition {
    pub node_id:      ViewNodeId,
    /// Expression of the method call.
    pub call_expr_id: ast::Id,
    pub method_id:    SuggestionId,
}

#[derive(Debug)]
struct QueryData {
    /// The target expression of function call, also used as a visualization id in manager.
    node_id:        ViewNodeId,
    target_expr_id: ast::Id,
    method_name:    String,
    arguments:      Vec<String>,
}

impl Model {
    fn handle_notification(&mut self, notification: Notification) -> Option<WidgetData> {
        match notification {
            Notification::ValueUpdate { target, visualization_id, data } => {
                let query_data = self.expr_queries.get(&visualization_id)?;
                let data_string = String::from_utf8_lossy(&data);
                warn!("Received value for {visualization_id}:\n{data_string:?}");
                // let node_id = query_data.node_id;

                None
            }
            Notification::FailedToAttach { error, .. } => {
                error!("failed to attach widget visualization: {error}");
                None
            }
            Notification::FailedToDetach { error, .. } => {
                error!("failed to detach widget visualization: {error}");
                None
            }
            Notification::FailedToModify { error, .. } => {
                error!("failed to modify widget visualization: {error}");
                None
            }
        }
    }

    fn run_query(&mut self, def: QueryDefinition) {
        use std::collections::hash_map::Entry;
        match self.expr_queries.entry(def.call_expr_id) {
            Entry::Occupied(mut occupied) => {
                let query = occupied.get_mut();
                if def.node_id != query.node_id {
                    self.expr_of_node.entry(def.node_id).or_default().push(def.call_expr_id);
                    self.expr_of_node[&query.node_id].remove(&def.call_expr_id);
                    query.node_id = def.node_id;
                }

                let mut modified = false;
                if query.method_name != &entry.name {
                    query.method_name = entry.name.into();
                    modified = true;
                }

                let zipped = query.arguments.iter().zip(&entry.arguments);
                if query.arguments.len() != entry.arguments.len()
                    || !zipped.all(|(a, b)| a == &b.name)
                {
                    query.arguments = entry.arguments.iter().map(|arg| arg.name.into()).collect();
                    modified = true;
                }

                if modified {
                    let vis_metadata = self.query_vis_metadata(query);
                    self.manager.request_visualization(def.call_expr_id, vis_metadata);
                }
            }
            Entry::Vacant(entry) => {
                self.expr_of_node.entry(def.node_id).or_default().push(def.call_expr_id);
                let query = entry.insert(QueryData {
                    node_id:     def.node_id,
                    arguments:   def.arguments,
                    method_name: def.method_name,
                });
                let vis_metadata = self.query_vis_metadata(query);
                self.manager.request_visualization(def.call_expr_id, vis_metadata);
            }
        }
    }

    fn query_data_for_definition(&self, def: QueryDefinition) -> Option<QueryData> {
        let suggestion_db = self.graph.borrow_graph().suggestion_db;
        let Ok(entry) = suggestion_db.lookup(def.suggestion_id).ok()?;
        // let ast_node = self.state.ast_node_id_of_view(def.node_id)?;
        let ast_node = self.state.expressions_of_node(def.node_id)?;
        // let ast = self.graph.borrow_graph().node_info(ast_node).;
        self.state.expressions_of_node(node);

        def.call_expr_id.and_then(|id| self.expr_queries.get(&id).cloned())
    }

    /// Remove all widget queries of given node that are connected to expression not on this list.
    fn retain_node_expressions(&self, node_id: ViewNodeId, expressions: &HashSet<ast::Id>) {
        let registered = self.expr_of_node.get_mut(&node_id);
        if let Some(registered) = registered {
            registered.retain(|expr_id| {
                let retained = expressions.contains(expr_id);
                if !retained {
                    self.manager.remove_visualization(expr_id);
                }
                retained
            });
        }
    }

    fn remove_node(&self, node_id: ViewNodeId) {
        if let Some(registered) = self.expr_of_node.remove(&node_id) {
            registered.for_each(|expr_id| self.manager.remove_visualization(expr_id));
        }
    }

    fn query_vis_metadata(&self, query: &QueryData) -> Metadata {
        let main_name =
            self.graph.module_qualified_name(&*self.project).to_string_with_main_segment();
        let method = "get_annotation_vis";

        let mut arguments: Vec<ide_view::graph_editor::data::enso::Code> =
            Vec::with_capacity(query.arguments.len() + 1);
        arguments.push(query.method_name.into());
        arguments.extend(query.arguments.iter().map(|arg| arg.into()));

        let preprocessor = visualization::instance::PreprocessorConfiguration {
            module: main_name.into(),
            method: method.into(),
            arguments,
        };
        Metadata { preprocessor }
    }


    // pub fn node_expression_updated(&self, node_id: ViewNodeId, expression:
    // &node_view::Expression) {     let manager = &self.manager;
    //     let input_tree = &expression.input_span_tree;
    //     let Some(ast_id) = self.state.ast_node_id_of_view(node_id) else { return };
    //     let code = &expression.code;
    //     warn!("[WIDGET] Node {ast_id:?} expression updated: {code:?}/nSpanTree: {input_tree:?}");

    //     let root: span_tree::node::Ref = input_tree.root_ref();
    //     let main_name =
    //         self.graph.module_qualified_name(&*self.project).to_string_with_main_segment();


    //     // find pattern:
    //     // PREFIX_FUNC with children:
    //     // - INFIX_LEFT - get ast id
    //     // - INFIX_RIGHT - get as text
    //     // Then for all named arguments after that:
    //     // register visualization

    //     enum FuncExprFinder {
    //         Undefined,
    //         FoundFunc(usize),
    //         FuncRoot(usize),
    //         FoundContext(usize, ast::Id),
    //         FoundCall,
    //     }

    //     struct Func {
    //         this_expr: ast::Id,
    //         fn_name:   String,
    //     }

    //     let mut funcs = Vec::new();

    //     root.dfs_with_layer_data(
    //         FuncExprFinder::Undefined,
    //         |node: &mut span_tree::node::Ref, data| {
    //             if span_tree::node::Kind::Operation == node.kind
    //                 && node.ast_crumbs.ends_with(&[PREFIX_FUNC])
    //             {
    //                 let reserved = funcs.len();
    //                 funcs.push(None);
    //                 warn!("[WIDGET] Found PREFIX_FUNC {reserved}");
    //                 *data = FuncExprFinder::FoundFunc(reserved);
    //                 return FuncExprFinder::FuncRoot(reserved);
    //             } else if let (FuncExprFinder::FuncRoot(idx), Some(ast_id)) = (&data,
    // &node.ast_id)             {
    //                 if node.ast_crumbs.ends_with(&[INFIX_LEFT]) {
    //                     warn!("[WIDGET] Found INFIX_LEFT {ast_id:?}");
    //                     *data = FuncExprFinder::FoundContext(*idx, *ast_id);
    //                 }
    //             } else if let FuncExprFinder::FoundContext(idx, ast_id) = &data {
    //                 if node.ast_crumbs.ends_with(&[INFIX_RIGHT]) {
    //                     warn!("[WIDGET] Found INFIX_RIGHT {ast_id:?}");
    //                     let fn_name = format!("\"{}\"", &expression.code[node.span()]);
    //                     funcs[*idx] = Some(Func { this_expr: *ast_id, fn_name });
    //                     *data = FuncExprFinder::FoundCall;
    //                 }
    //             } else if let (
    //                 FuncExprFinder::FoundFunc(idx),
    //                 span_tree::node::Kind::Argument(span_tree::node::Argument {
    //                     name: Some(arg_name),
    //                     ..
    //                 }),
    //             ) = (data, &node.kind)
    //             {
    //                 if let Some(Func { this_expr, fn_name }) = &funcs[*idx] {
    //                     warn!("[WIDGET] attach Meta {this_expr} {fn_name}");
    //                     let arg_name_wrapped = format!("\"{}\"", arg_name);

    //                     // local function call, needs to be declared in project Main for now.
    //                     let preprocessor =
    // visualization::instance::PreprocessorConfiguration::new(
    // &main_name,                         "get_annotation_vis",
    //                         vec![fn_name, &arg_name_wrapped],
    //                     );
    //                     let metadata = ide_view::graph_editor::component::visualization::Metadata
    // {                         preprocessor,
    //                     };
    //                     // For now we don't care about cleanup. The old visualisation will be
    //                     // unregistered if we register it on the same
    //                     // expression again, but when node is deleted
    //                     // it will leak. TBD
    //                     manager.set_visualization(*this_expr, Some(metadata));
    //                 }
    //             }
    //             FuncExprFinder::Undefined
    //         },
    //     );
    // }
}
