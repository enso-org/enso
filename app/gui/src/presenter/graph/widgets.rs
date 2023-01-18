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
use executor::global::spawn;
use ide_view::graph_editor::component::node::input::widget;
use ide_view::graph_editor::component::visualization;
use ide_view::graph_editor::component::visualization::Metadata;
use ide_view::graph_editor::data::enso::Code;
use ide_view::graph_editor::GraphEditor;
use ide_view::graph_editor::WidgetUpdate;


// ===============
// === Widgets ===
// ===============

define_endpoints_2! {
    Input {
        /// Create or update widget query with given definition.
        register_query(QueryDefinition),
        /// Remove all widget queries of given node that are not on this list.
        retain_node_expressions(ViewNodeId, HashSet<ast::Id>),
        /// Remove all widget queries of given node.
        remove_node(ViewNodeId),
    }
    Output {
        /// Emitted when the node's visualization has been set.
        widget_data(ViewNodeId, Vec<WidgetUpdate>),
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
    pub fn new(executed_graph: ExecutedGraph, project: model::Project, _view: GraphEditor) -> Self {
        let (manager, manager_notifications) = Manager::new(executed_graph.clone_ref());
        let frp = Frp::new();

        let module = executed_graph.module_qualified_name(&*project).to_string_with_main_segment();
        let method = "get_annotation_vis";

        let model = Rc::new(RefCell::new(Model {
            manager,
            graph: executed_graph.clone_ref(),
            expr_of_node: default(),
            expr_queries: default(),
            module: module.into(),
            method: method.into(),
        }));

        let network = &frp.network;
        let input = &frp.input;
        let output = &frp.private.output;

        frp::extend! { network
            eval input.register_query((definition) model.borrow_mut().register_query(definition));
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
    manager:      Rc<Manager>,
    graph:        ExecutedGraph,
    expr_of_node: HashMap<ViewNodeId, Vec<ast::Id>>,
    expr_queries: HashMap<ast::Id, QueryData>,
    module:       ImString,
    method:       ImString,
}

/// Definition of a widget query. Defines the operation expression that the widgets will be attached
/// to.
#[derive(Debug, Default, Clone, Copy)]
pub struct QueryDefinition {
    /// The node id of a node that contains the operation expression.
    pub node_id:        ViewNodeId,
    /// Expression of the whole method call.
    pub call_expr_id:   ast::Id,
    /// Expression of the call target. Used as a visualization target.
    pub target_expr_id: ast::Id,
    /// The suggestion id of the method that this call refers to.
    pub method_id:      SuggestionId,
}

#[derive(Debug)]
struct QueryData {
    node_id:      ViewNodeId,
    call_expr_id: ast::Id,
    method_name:  ImString,
    arguments:    Vec<ImString>,
}


impl Model {
    fn handle_notification(
        &mut self,
        notification: Notification,
    ) -> Option<(ViewNodeId, Vec<WidgetUpdate>)> {
        match notification {
            Notification::ValueUpdate { target, data, .. } => {
                let query_data = self.expr_queries.get(&target)?;
                let data: serde_json::Value = serde_json::from_slice(&data).ok()?;
                warn!("[WIDGETS] Received value for {target}:\n{data:?}");
                let args = data.as_array()?;
                let updates = args
                    .iter()
                    .filter_map(|arg_value| {
                        let meta_value = arg_value.get(1)?;
                        /*
                           Array [
                               Array [String("selector"), Object {"allow_custom": Bool(true), "constructor": String("Single_Choice"), "display": Object {"constructor": String("Always"), "type": String("Display")}, "label": Null, "quote_values": Bool(false), "type": String("Widget"), "values": Array [String("Foo"), String("Bar")]}]
                           ]
                        */

                        let kind: widget::Kind =
                            serde_json::from_value(meta_value.get("constructor")?.clone()).ok()?;
                        let display: widget::Display = serde_json::from_value(
                            meta_value.get("display")?.get("constructor")?.clone(),
                        )
                        .unwrap_or_default();
                        let dynamic_entries: Vec<String> =
                            serde_json::from_value(meta_value.get("values")?.clone())
                                .unwrap_or_default();
                        let dynamic_entries = dynamic_entries.into_iter().map(Into::into).collect();
                        let meta = widget::Metadata { kind, display, dynamic_entries };

                        let argument_name = arg_value.get(0)?.as_str()?.to_owned().into();
                        Some(WidgetUpdate { argument_name, target_id: target, meta: Some(meta) })
                    })
                    .collect();

                warn!("[WIDGETS] updates: {updates:?}");


                Some((query_data.node_id, updates))
            }
            Notification::FailedToAttach { error, .. } => {
                error!("[WIDGETS] failed to attach widget visualization: {error}");
                None
            }
            Notification::FailedToDetach { error, .. } => {
                error!("[WIDGETS] failed to detach widget visualization: {error}");
                None
            }
            Notification::FailedToModify { error, .. } => {
                error!("[WIDGETS] failed to modify widget visualization: {error}");
                None
            }
        }
    }

    fn register_query(&mut self, def: &QueryDefinition) {
        let suggestion_db = &self.graph.borrow_graph().suggestion_db;
        let Ok(entry) = suggestion_db.lookup(def.method_id) else { return };

        use std::collections::hash_map::Entry;
        match self.expr_queries.entry(def.target_expr_id) {
            Entry::Occupied(mut occupied) => {
                let query = occupied.get_mut();
                if def.node_id != query.node_id {
                    self.expr_of_node.entry(def.node_id).or_default().push(def.target_expr_id);
                    self.expr_of_node.entry(query.node_id).and_modify(|exprs| {
                        exprs.retain(|id| *id != def.target_expr_id);
                    });
                    query.node_id = def.node_id;
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

                query.call_expr_id = def.call_expr_id;

                if visualization_modified {
                    let vis_metadata =
                        Self::query_vis_metadata(self.module.clone(), self.method.clone(), query);
                    warn!("[WIDGETS] Modifying visualization for {}", def.target_expr_id);
                    let manager = self.manager.clone_ref();
                    let target_expr_id = def.target_expr_id;
                    spawn(async move {
                        manager.request_visualization(target_expr_id, vis_metadata);
                    });
                }
            }
            Entry::Vacant(vacant) => {
                let node_id = def.node_id;
                let method_name = entry.name.clone().into();
                let call_expr_id = def.call_expr_id;
                self.expr_of_node.entry(def.node_id).or_default().push(def.target_expr_id);
                let arguments = entry.arguments.iter().map(|arg| arg.name.clone().into()).collect();

                let query_data = QueryData { node_id, arguments, method_name, call_expr_id };
                let query = vacant.insert(query_data);
                let vis_metadata =
                    Self::query_vis_metadata(self.module.clone(), self.method.clone(), query);
                warn!("[WIDGETS] Registering visualization for {}", def.target_expr_id);

                let manager = self.manager.clone_ref();
                let target_expr_id = def.target_expr_id;
                spawn(async move {
                    manager.request_visualization(target_expr_id, vis_metadata);
                });
            }
        }
    }

    /// Remove all widget queries of given node that are connected to expression not on this list.
    fn retain_node_expressions(&mut self, node_id: ViewNodeId, expressions: &HashSet<ast::Id>) {
        let registered = self.expr_of_node.get_mut(&node_id);
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
        if let Some(registered) = self.expr_of_node.remove(&node_id) {
            registered.into_iter().for_each(|expr_id| self.manager.remove_visualization(expr_id));
        }
    }

    fn query_vis_metadata(module: ImString, method: ImString, query: &QueryData) -> Metadata {
        let relevant_arguments = query.arguments.split_first().map_or_default(|(_self, args)| args);
        let arguments: Vec<Code> = vec![
            Self::escape_arg(&query.method_name).into(),
            Self::arg_sequence(relevant_arguments).into(),
        ];

        let preprocessor = visualization::instance::PreprocessorConfiguration {
            module:    module.into(),
            method:    method.into(),
            arguments: Rc::new(arguments),
        };
        Metadata { preprocessor }
    }

    fn escape_arg(arg: &str) -> String {
        let segment = ast::SegmentPlain { value: arg.into() };
        let text = ast::TextLineRaw { text: vec![segment.into()] };
        warn!("[WIDGETS] Escaping arg: {arg:?} -> {:?}", text.repr());
        text.repr()
    }

    fn arg_sequence(args: &[ImString]) -> String {
        let mut buffer = String::from("[");
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                buffer.push_str(", ");
            }
            buffer.push_str(&Self::escape_arg(&arg));
        }
        buffer.push_str("]");
        buffer
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


// struct WidgetVisualizationData {
//     widget_type: String,
//     display:     Display,
//     values:      Vec<String>,
// }
