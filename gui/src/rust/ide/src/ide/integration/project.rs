//! The structure integrating the controllers related to currently opened project with the project
//! view.
// TODO[ao] this module should be completely reworked when doing the
//  https://github.com/enso-org/ide/issues/597
//  There should be a wrapper for each view which "fences" the input : emitting events in this
//  wrapper should not notify the outputs.

use crate::prelude::*;

use crate::controller::graph::Connections;
use crate::controller::graph::NodeTrees;
use crate::controller::searcher::action::MatchInfo;
use crate::controller::searcher::Actions;
use crate::controller::upload;
use crate::controller::upload::NodeFromDroppedFileHandler;
use crate::executor::global::spawn;
use crate::executor::global::spawn_stream_handler;
use crate::ide::integration::file_system::create_node_from_file;
use crate::ide::integration::file_system::do_file_operation;
use crate::ide::integration::file_system::FileOperation;
use crate::ide::integration::file_system::FileProvider;
use crate::ide::integration::visualization::Manager as VisualizationManager;
use crate::model::execution_context::ComputedValueInfo;
use crate::model::execution_context::ExpressionId;
use crate::model::execution_context::LocalCall;
use crate::model::execution_context::VisualizationUpdateData;
use crate::model::module::ProjectMetadata;
use crate::model::suggestion_database;
use crate::model::traits::*;

use analytics;
use bimap::BiMap;
use engine_protocol::language_server::ExpressionUpdatePayload;
use enso_data::text::TextChange;
use enso_frp as frp;
use ensogl::display::traits::*;
use ensogl_gui_component::file_browser::model::AnyFolderContent;
use ensogl_gui_component::list_view;
use futures::future::LocalBoxFuture;
use ide_view::graph_editor;
use ide_view::graph_editor::component::node;
use ide_view::graph_editor::component::visualization;
use ide_view::graph_editor::EdgeEndpoint;
use ide_view::graph_editor::GraphEditor;
use ide_view::graph_editor::SharedHashMap;
use ide_view::open_dialog;
use ide_view::searcher::entry::AnyModelProvider;
use ide_view::searcher::entry::GlyphHighlightedLabel;
use ide_view::searcher::new::Icon;



// ==============
// === Errors ===
// ==============

/// Error returned by various function inside GraphIntegration, when our mappings from controller
/// items (node or connections) to displayed items are missing some information.
#[derive(Copy, Clone, Debug, Fail)]
enum MissingMappingFor {
    #[fail(display = "Displayed node {:?} is not bound to any controller node.", _0)]
    DisplayedNode(graph_editor::NodeId),
    #[fail(display = "Controller node {:?} is not bound to any displayed node.", _0)]
    ControllerNode(ast::Id),
    #[fail(display = "Displayed connection {:?} is not bound to any controller connection.", _0)]
    DisplayedConnection(graph_editor::EdgeId),
}

/// Error raised when reached some fatal inconsistency in data provided by GraphEditor.
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Discrepancy in a GraphEditor component")]
struct GraphEditorInconsistency;

#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "No visualization associated with view node {} found.", _0)]
struct NoSuchVisualization(graph_editor::NodeId);

#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Graph node {} already has visualization attached.", _0)]
struct VisualizationAlreadyAttached(graph_editor::NodeId);

#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "The Graph Integration hsd no SearcherController.")]
struct MissingSearcherController;

/// Denotes visualizations set in the graph editor.
#[derive(Clone, Copy, Debug, Display)]
pub enum WhichVisualization {
    /// Usual visualization, triggered by the user.
    Normal,
    /// Special visualization, attached automatically when there is an error on the node.
    Error,
}

// ====================
// === FencedAction ===
// ====================

/// An utility to FRP network. It is wrapped closure in a set of FRP nodes. The closure is called
/// on `trigger`, and `is_running` contains information if we are still inside closure call. It
/// allows us to block some execution path to avoid infinite loops.
///
/// ### Example
///
/// Here we want to do some updates when node was added to graph, but not during set up.
/// ```rust,compile_fail
/// frp::new_network! { network
///     let set_up = FencedAction::fence(&network, |()| {
///         frp.add_node.emit(());
///         // other things.
///     });
///     def _update = frp.node_added.map2(&set_up.is_running, |id,is_set_up| {
///         if !is_set_up {
///             update_something(id)
///         }
///     });
/// }
/// // This will run the set up closure, but without calling update_something.
/// set_up.trigger.emit(());
/// ```
#[derive(Clone, CloneRef)]
struct FencedAction<Parameter: frp::Data> {
    trigger:    frp::Source<Parameter>,
    is_running: frp::Stream<bool>,
}

impl<Parameter: frp::Data> FencedAction<Parameter> {
    /// Wrap the `action` in `FencedAction`.
    fn fence(network: &frp::Network, action: impl Fn(&Parameter) + 'static) -> Self {
        frp::extend! { network
            trigger    <- source::<Parameter>();
            triggered  <- trigger.constant(());
            switch     <- any(...);
            switch     <+ triggered;
            performed  <- trigger.map(move |param| action(param));
            switch     <+ performed;
            is_running <- switch.toggle();
        }
        Self { trigger, is_running }
    }
}



// ==============================
// === GraphEditorIntegration ===
// ==============================

/// The identifier base that will be used to name the methods introduced by "collapse nodes"
/// refactoring. Names are typically generated by taking base and appending subsequent integers,
/// until the generated name does not collide with any known identifier.
const COLLAPSED_FUNCTION_NAME: &str = "func";

/// The default X position of the node when user did not set any position of node - possibly when
/// node was added by editing text.
const DEFAULT_NODE_X_POSITION: f32 = -100.0;
/// The default Y position of the node when user did not set any position of node - possibly when
/// node was added by editing text.
const DEFAULT_NODE_Y_POSITION: f32 = 200.0;

/// Default node position -- acts as a starting points for laying out nodes with no position defined
/// in the metadata.
pub fn default_node_position() -> Vector2 {
    Vector2::new(DEFAULT_NODE_X_POSITION, DEFAULT_NODE_Y_POSITION)
}

/// A structure which handles integration between controller and graph_editor EnsoGl control.
/// All changes made by user in view are reflected in controller, and all controller notifications
/// update view accordingly.
//TODO[ao] soon we should rearrange modules and crates to avoid such long names.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Integration {
    model:   Rc<Model>,
    network: frp::Network,
}

impl Integration {
    /// Get GraphEditor.
    pub fn graph_editor(&self) -> GraphEditor {
        self.model.view.graph().clone_ref()
    }

    /// Get the controller associated with this graph editor.
    pub fn graph_controller(&self) -> &controller::ExecutedGraph {
        &self.model.graph
    }
}

#[derive(Debug)]
struct Model {
    logger:                  Logger,
    view:                    ide_view::project::View,
    graph:                   controller::ExecutedGraph,
    text:                    controller::Text,
    ide:                     controller::Ide,
    searcher:                RefCell<Option<controller::Searcher>>,
    project:                 model::Project,
    main_module:             model::Module,
    node_views:              RefCell<BiMap<ast::Id, graph_editor::NodeId>>,
    node_view_by_expression: RefCell<HashMap<ast::Id, graph_editor::NodeId>>,
    expression_views:
        RefCell<HashMap<graph_editor::NodeId, graph_editor::component::node::Expression>>,
    expression_types:        SharedHashMap<ExpressionId, Option<graph_editor::Type>>,
    connection_views:        RefCell<BiMap<controller::graph::Connection, graph_editor::EdgeId>>,
    code_view:               CloneRefCell<ensogl_text::Text>,
    visualizations:          Rc<VisualizationManager>,
    error_visualizations:    Rc<VisualizationManager>,
    prompt_was_shown:        Cell<bool>,
    displayed_project_list:  CloneRefCell<ProjectsToOpen>,
}


// === Construction And Setup ===

impl Integration {
    /// Constructor. It creates GraphEditor and integrates it with given controller handle.
    pub fn new(
        view: ide_view::project::View,
        graph: controller::ExecutedGraph,
        text: controller::Text,
        ide: controller::Ide,
        project: model::Project,
        main_module: model::Module,
    ) -> Self {
        let logger = Logger::new("ViewIntegration");
        let model = Model::new(logger, view, graph, text, ide, project, main_module);
        let editor_outs = &model.view.graph().frp.output;
        let code_editor = &model.view.code_editor().text_area();
        let searcher_frp = &model.view.searcher().frp;
        let project_frp = &model.view.frp;
        frp::new_network! {network
            let invalidate = FencedAction::fence(&network,f!([model](()) {
                let result = model.refresh_graph_view();
                if let Err(err) = result {
                    error!(model.logger,"Error while invalidating graph: {err}");
                }
            }));
        }


        // === Breadcrumb Selection ===

        let breadcrumbs = &model.view.graph().model.breadcrumbs;
        frp::extend! {network
            eval_ breadcrumbs.output.breadcrumb_pop(model.node_exited_in_ui());
            eval  breadcrumbs.output.breadcrumb_push((local_call) {
                model.expression_entered_in_ui(&local_call.as_ref().map(|local_call| {
                    let definition = (**local_call.definition).clone();
                    let call       = local_call.call;
                    LocalCall{call,definition}
                })).ok()
            });
        }


        // === Project Renaming ===

        let breadcrumbs = &model.view.graph().model.breadcrumbs;
        frp::extend! {network
            eval breadcrumbs.output.project_name((name) {
                model.rename_project(name);
            });
        }


        // === Setting Visualization Preprocessor ===

        frp::extend! { network
            eval editor_outs.visualization_preprocessor_changed ([model]((node_id,preprocessor)) {
                if let Err(err) = model.visualization_preprocessor_changed(*node_id,preprocessor.clone_ref()) {
                    error!(model.logger, "Error when handling request for setting new \
                        visualization's preprocessor code: {err}");
                }
            });
        }


        // === Visualization Reload ===

        frp::extend! { network
            eval editor_outs.visualization_registry_reload_requested ([model](()) {
                model.view.graph().reset_visualization_registry();
                model.load_visualizations();
            });
        }


        // === Dropping Files ===

        let dropping_enabled = model.view.drop_files_enabled.clone_ref();
        let file_dropped = model.view.graph().file_dropped.clone_ref();
        frp::extend! { network
            file_upload_requested <- file_dropped.gate(&dropping_enabled);
            eval file_upload_requested ([model]((file,position)) {
                let project   = model.project.clone_ref();
                let graph     = model.graph.graph();
                let to_upload = upload::FileToUpload {
                    name : file.name.clone_ref().into(),
                    size : file.size,
                    data : file.clone_ref(),
                };
                let position = model::module::Position {vector:*position};
                let handler  = NodeFromDroppedFileHandler::new(&model.logger,project,graph);
                let res      = handler.create_node_and_start_uploading(to_upload,position);
                if let Err(err) = res {
                    error!(model.logger, "Error when creating node from dropped file: {err}");
                }
            });
        }


        // === Open File or Project Dialog ===

        let file_browser = &model.view.open_dialog().file_browser;
        let project_list = &model.view.open_dialog().project_list;
        frp::extend! { network
            let chosen_project = project_list.chosen_entry.clone_ref();
            let file_chosen    = file_browser.entry_chosen.clone_ref();
            project_chosen     <- chosen_project.filter_map(|p| *p);
            dialog_is_shown    <- project_frp.open_dialog_shown.filter(|v| *v);
            eval_ dialog_is_shown (       model.open_dialog_opened_in_ui());
            eval  project_chosen  ((id)   model.project_opened_in_ui(id));
            eval  file_chosen     ((path) model.file_opened_in_ui(path));

            file_copied      <- file_browser.copy.map(|p| (p.clone(),FileOperation::Copy));
            file_cut         <- file_browser.cut .map(|p| (p.clone(),FileOperation::Move));
            source_operation <- any(file_copied,file_cut);
            file_operation   <- file_browser.paste_into.map2(&source_operation,
                |dest,(src,op)| (src.clone(),dest.clone(),*op)
            );
            eval file_operation ([model]((src,dest,op))
                let logger    = model.logger.clone_ref();
                let project   = model.project.clone_ref();
                let source    = src.clone();
                let dest      = dest.clone();
                let operation = *op;
                let model     = model.clone_ref();
                spawn(async move {
                    if let Err(err) = do_file_operation(&project,&source,&dest,operation).await {
                        error!(logger, "Failed to {operation.verb()} file: {err}");
                    } else {
                        model.reload_files_in_file_browser();
                    }
                })

            );
        }


        // === Searcher 2.0 ===

        let searcher = model.view.searcher().new_frp();
        frp::extend! { network
            eval searcher.list_directory ([model,searcher](crumbs) {
                for (id,entry) in model.get_category_content(crumbs) {
                    let mut new_crumbs     = crumbs.clone();
                    Rc::make_mut(&mut new_crumbs).push(id);
                    searcher.directory_content(new_crumbs,entry);
                }
            });
        }


        // === UI Actions ===

        let inv = &invalidate.trigger;
        let node_editing_in_ui = Model::node_editing_in_ui(Rc::downgrade(&model));
        let searcher_opened_in_ui = Model::searcher_opened_in_ui(Rc::downgrade(&model));
        let code_changed = Self::ui_action(&model, Model::code_changed_in_ui, inv);
        let node_removed = Self::ui_action(&model, Model::node_removed_in_ui, inv);
        let nodes_collapsed = Self::ui_action(&model, Model::nodes_collapsed_in_ui, inv);
        let node_selected = Self::ui_action(&model, Model::node_selected_in_ui, inv);
        let node_deselected = Self::ui_action(&model, Model::node_deselected_in_ui, inv);
        let node_entered = Self::ui_action(&model, Model::node_entered_in_ui, inv);
        let node_exited = Self::ui_action(
            &model,
            |model, _| {
                model.node_exited_in_ui();
                Ok(())
            },
            inv,
        );
        let connection_created = Self::ui_action(&model, Model::connection_created_in_ui, inv);
        let connection_removed = Self::ui_action(&model, Model::connection_removed_in_ui, inv);
        let node_moved = Self::ui_action(&model, Model::node_moved_in_ui, inv);
        let searcher_opened = Self::ui_action(&model, searcher_opened_in_ui, inv);
        let node_editing = Self::ui_action(&model, node_editing_in_ui, inv);
        let node_expression_set = Self::ui_action(&model, Model::node_expression_set_in_ui, inv);
        let used_as_suggestion = Self::ui_action(&model, Model::used_as_suggestion_in_ui, inv);
        let node_editing_committed =
            Self::ui_action(&model, Model::node_editing_committed_in_ui, inv);
        let node_editing_aborted = Self::ui_action(&model, Model::node_editing_aborted_in_ui, inv);
        let visualization_path_changed =
            Self::ui_action(&model, Model::visualization_path_changed_in_ui, inv);
        frp::extend! { network
            eval code_editor.content ((content) model.code_view.set(content.clone_ref()));

            // Notifications from graph controller
            let handle_graph_notification = FencedAction::fence(&network,
                f!((notification:&Option<controller::graph::executed::Notification>)
                    model.handle_graph_notification(notification);
            ));

            // Notifications from graph controller
            let handle_text_notification = FencedAction::fence(&network,
                f!((notification:&Option<controller::text::Notification>)
                    model.handle_text_notification(*notification);
            ));

            // Changes in Graph Editor
            is_handling_notification <- handle_graph_notification.is_running
                                     || handle_text_notification.is_running;
            is_hold                  <- is_handling_notification || invalidate.is_running;
            on_connection_removed    <- editor_outs.on_edge_endpoint_unset._0();
            _action <- code_editor.changed                  .map2(&is_hold,code_changed);
            _action <- editor_outs.node_removed             .map2(&is_hold,node_removed);
            _action <- editor_outs.nodes_collapsed          .map2(&is_hold,nodes_collapsed);
            _action <- editor_outs.node_selected            .map2(&is_hold,node_selected);
            _action <- editor_outs.node_deselected          .map2(&is_hold,node_deselected);
            _action <- editor_outs.node_entered             .map2(&is_hold,node_entered);
            _action <- editor_outs.node_exited              .map2(&is_hold,node_exited);
            _action <- editor_outs.on_edge_endpoints_set    .map2(&is_hold,connection_created);
            _action <- on_connection_removed                .map2(&is_hold,connection_removed);
            _action <- editor_outs.node_position_set_batched.map2(&is_hold,node_moved);
            _action <- editor_outs.node_being_edited        .map2(&is_hold,node_editing);
            _action <- project_frp.searcher_opened          .map2(&is_hold,searcher_opened);
            _action <- editor_outs.node_expression_set      .map2(&is_hold,node_expression_set);
            _action <- searcher_frp.used_as_suggestion      .map2(&is_hold,used_as_suggestion);
            _action <- project_frp.editing_committed        .map2(&is_hold,node_editing_committed);
            _action <- project_frp.editing_aborted          .map2(&is_hold,node_editing_aborted);
            _action <- editor_outs.enabled_visualization_path.map2(&is_hold,visualization_path_changed);

            // These two endpoints for toggling visualizations visibility do not follow the pattern
            // above intentionally. Visualization can be shown both due to user's action in UI and
            // node's metadata change or integration layer logic.
            // Thus, we don't want to suppress calls happening as consequence of ongoing action.
            eval editor_outs.visualization_shown([model,inv](arg) {
                Self::execute_fallible_ui_action(&model, &inv, || model.visualization_shown_in_ui(arg))
            });
            eval editor_outs.visualization_hidden([model,inv](arg) {
                Self::execute_fallible_ui_action(&model, &inv, || model.visualization_hidden_in_ui(arg))
            });

            eval_ project_frp.editing_committed (invalidate.trigger.emit(()));
            eval_ project_frp.editing_aborted   (invalidate.trigger.emit(()));
            eval_ project_frp.save_module       (model.module_saved_in_ui());
            eval_ project_frp.undo              (model.undo_in_ui());
            eval_ project_frp.redo              (model.redo_in_ui());
        }

        frp::extend! { network
            eval_ editor_outs.node_editing_started([]analytics::remote_log_event("graph_editor::node_editing_started"));
            eval_ editor_outs.node_editing_finished([]analytics::remote_log_event("graph_editor::node_editing_finished"));
            eval_ editor_outs.node_added([]analytics::remote_log_event("graph_editor::node_added"));
            eval_ editor_outs.node_removed([]analytics::remote_log_event("graph_editor::node_removed"));
            eval_ editor_outs.nodes_collapsed([]analytics::remote_log_event("graph_editor::nodes_collapsed"));
            eval_ editor_outs.node_entered([]analytics::remote_log_event("graph_editor::node_enter_request"));
            eval_ editor_outs.node_exited([]analytics::remote_log_event("graph_editor::node_exit_request"));
            eval_ editor_outs.on_edge_endpoints_set([]analytics::remote_log_event("graph_editor::edge_endpoints_set"));
            eval_ editor_outs.visualization_shown([]analytics::remote_log_event("graph_editor::visualization_shown"));
            eval_ editor_outs.visualization_hidden([]analytics::remote_log_event("graph_editor::visualization_hidden"));
            eval_ on_connection_removed([]analytics::remote_log_event("graph_editor::connection_removed"));
            eval_ searcher_frp.used_as_suggestion([]analytics::remote_log_event("searcher::used_as_suggestion"));
            eval_ project_frp.editing_committed([]analytics::remote_log_event("project::editing_committed"));
        }


        let ret = Self { model, network };
        ret.connect_frp_to_graph_controller_notifications(handle_graph_notification.trigger);
        ret.connect_frp_text_controller_notifications(handle_text_notification.trigger);
        ret.setup_handling_project_notifications();
        ret.show_initial_visualizations();
        ret
    }

    fn spawn_sync_stream_handler<Stream, Function>(&self, stream: Stream, handler: Function)
    where
        Stream: StreamExt + Unpin + 'static,
        Function: Fn(Stream::Item, Rc<Model>) + 'static, {
        let model = Rc::downgrade(&self.model);
        spawn_stream_handler(model, stream, move |item, model| {
            handler(item, model);
            futures::future::ready(())
        })
    }

    fn setup_handling_project_notifications(&self) {
        let stream = self.model.project.subscribe();
        let logger = self.model.logger.clone_ref();
        let status_bar = self.model.view.status_bar().clone_ref();
        self.spawn_sync_stream_handler(stream, move |notification, _| {
            info!(logger, "Processing notification {notification:?}");
            let message = match notification {
                model::project::Notification::ConnectionLost(_) =>
                    crate::BACKEND_DISCONNECTED_MESSAGE,
            };
            let message = ide_view::status_bar::event::Label::from(message);
            status_bar.add_event(message);
        })
    }

    fn show_initial_visualizations(&self) {
        let logger = self.model.logger.clone_ref();
        info!(logger, "Attaching initially opened visualization");
        let node_ids = self.model.node_views.borrow().right_values().cloned().collect_vec();
        for node_id in node_ids {
            if let Some(metadata) = self.model.view.graph().model.enabled_visualization(node_id) {
                self.model
                    .visualization_shown_in_ui(&(node_id, metadata))
                    .handle_err(|err| error!(logger, "Failed to enable visualization: {err}"));
            }
        }
    }

    fn connect_frp_to_graph_controller_notifications(
        &self,
        frp_endpoint: frp::Source<Option<controller::graph::executed::Notification>>,
    ) {
        let stream = self.model.graph.subscribe();
        let logger = self.model.logger.clone_ref();
        self.spawn_sync_stream_handler(stream, move |notification, _model| {
            info!(logger, "Processing notification {notification:?}");
            frp_endpoint.emit(&Some(notification));
        })
    }

    fn connect_frp_text_controller_notifications(
        &self,
        frp_endpoint: frp::Source<Option<controller::text::Notification>>,
    ) {
        let stream = self.model.text.subscribe();
        let logger = self.model.logger.clone_ref();
        self.spawn_sync_stream_handler(stream, move |notification, _model| {
            info!(logger, "Processing notification {notification:?}");
            frp_endpoint.emit(&Some(notification));
        });
    }

    /// Convert a function being a method of GraphEditorIntegratedWithControllerModel to a closure
    /// suitable for connecting to GraphEditor frp network. Returned lambda takes `Parameter` and a
    /// bool, which indicates if this action is currently on hold (e.g. due to performing
    /// invalidation).
    fn ui_action<Action, Parameter>(
        model: &Rc<Model>,
        action: Action,
        invalidate: &frp::Source<()>,
    ) -> impl Fn(&Parameter, &bool)
    where
        Action: Fn(&Model, &Parameter) -> FallibleResult + 'static,
    {
        f!([model,invalidate] (parameter,is_hold) {
            if !*is_hold {
                Self::execute_fallible_ui_action(&model, &invalidate, || action(&*model,parameter))
            }
        })
    }

    fn execute_fallible_ui_action(
        model: &Rc<Model>,
        invalidate: &frp::Source<()>,
        action: impl FnOnce() -> FallibleResult,
    ) {
        if let Err(err) = action() {
            error!(model.logger, "Error while performing an UI action on controllers: {err}");
            info!(model.logger, "Invalidating the displayed graph.");
            invalidate.emit(());
        }
    }
}

impl Model {
    fn new(
        logger: Logger,
        view: ide_view::project::View,
        graph: controller::ExecutedGraph,
        text: controller::Text,
        ide: controller::Ide,
        project: model::Project,
        main_module: model::Module,
    ) -> Rc<Self> {
        let node_views = default();
        let node_view_by_expression = default();
        let connection_views = default();
        let expression_views = default();
        let expression_types = default();
        let code_view = default();
        let searcher = default();
        let prompt_was_shown = default();
        let displayed_project_list = default();
        let (visualizations, visualizations_notifications) =
            crate::integration::visualization::Manager::new(
                logger.sub("visualizations"),
                graph.clone_ref(),
                project.clone_ref(),
            );
        let (error_visualizations, error_visualizations_notifications) =
            crate::integration::visualization::Manager::new(
                logger.sub("error_visualizations"),
                graph.clone_ref(),
                project.clone_ref(),
            );
        let this = Model {
            logger,
            view,
            graph,
            text,
            ide,
            searcher,
            project,
            main_module,
            node_views,
            node_view_by_expression,
            expression_views,
            expression_types,
            connection_views,
            code_view,
            visualizations,
            error_visualizations,
            prompt_was_shown,
            displayed_project_list,
        };
        let this = Rc::new(this);

        this.spawn_visualization_handler(visualizations_notifications, WhichVisualization::Normal);
        this.spawn_visualization_handler(
            error_visualizations_notifications,
            WhichVisualization::Error,
        );

        let graph_frp = this.view.graph().frp.clone_ref();
        graph_frp.remove_all_nodes();
        this.view.status_bar().clear_all();
        this.init_project_name();
        this.init_crumbs();
        this.load_visualizations();
        if let Err(err) = this.refresh_graph_view() {
            error!(this.logger, "Error while initializing graph editor: {err}.");
        }
        if let Err(err) = this.refresh_code_editor() {
            error!(this.logger, "Error while initializing code editor: {err}.");
        }
        this
    }

    fn spawn_visualization_handler(
        self: &Rc<Self>,
        notifier: impl Stream<Item = crate::integration::visualization::Notification> + Unpin + 'static,
        visualizations_kind: WhichVisualization,
    ) {
        let weak = Rc::downgrade(self);
        let processor = async move |notification, this: Rc<Self>| {
            this.handle_visualization_update(visualizations_kind, notification);
        };
        spawn_stream_handler(weak, notifier, processor);
    }

    fn load_visualizations(&self) {
        let logger = self.logger.clone_ref();
        let controller = self.project.visualization().clone_ref();
        let graph_editor = self.view.graph().clone_ref();
        spawn(async move {
            let identifiers = controller.list_visualizations().await;
            let identifiers = identifiers.unwrap_or_default();
            for identifier in identifiers {
                match controller.load_visualization(&identifier).await {
                    Ok(visualization) => {
                        graph_editor.frp.register_visualization.emit(Some(visualization));
                    }
                    Err(err) => {
                        error!(logger, "Error while loading visualization {identifier}: {err:?}");
                    }
                }
            }
            info!(logger, "Visualizations Initialized.");
        });
    }
}


// === Project renaming ===

impl Model {
    fn init_project_name(&self) {
        let project_name = self.project.name().to_string();
        self.view.graph().model.breadcrumbs.input.project_name.emit(project_name);
    }

    fn rename_project(&self, name: impl Str) {
        if self.project.name() != name.as_ref() {
            let project = self.project.clone_ref();
            let breadcrumbs = self.view.graph().model.breadcrumbs.clone_ref();
            let logger = self.logger.clone_ref();
            let name = name.into();
            spawn(async move {
                if let Err(e) = project.rename_project(name).await {
                    info!(logger, "The project couldn't be renamed: {e}");
                    breadcrumbs.cancel_project_name_editing.emit(());
                }
            });
        }
    }
}

// === Updating breadcrumbs ===

impl Model {
    fn push_crumb(&self, frame: &LocalCall) {
        let definition = frame.definition.clone().into();
        let call = frame.call;
        let local_call = graph_editor::LocalCall { call, definition };
        self.view.graph().model.breadcrumbs.input.push_breadcrumb.emit(Some(local_call))
    }

    fn pop_crumb(&self) {
        self.view.graph().model.breadcrumbs.pop_breadcrumb.emit(());
    }

    fn init_crumbs(&self) {
        for frame in self.graph.call_stack() {
            self.push_crumb(&frame)
        }
    }
}


// === Updating Graph View ===

impl Model {
    /// Refresh displayed code to be up to date with module state.
    pub fn refresh_code_editor(&self) -> FallibleResult {
        let current_code = self.code_view.get().to_string();
        let new_code = self.graph.graph().module.ast().repr();
        if new_code != current_code {
            self.code_view.set(new_code.as_str().into());
            self.view.code_editor().text_area().set_content(new_code);
        }
        Ok(())
    }

    pub fn refresh_call_stack(&self) -> FallibleResult {
        // If graph controller displays a different graph
        let current_call_stack = self.graph.call_stack();
        let get_call_stack = |metadata: &ProjectMetadata| metadata.call_stack.clone();
        let metadata_call_stack = self.main_module.with_project_metadata(get_call_stack);

        let mut current = current_call_stack.into_iter().fuse();
        let mut metadata = metadata_call_stack.into_iter().fuse();
        loop {
            match (current.next(), metadata.next()) {
                (Some(current), Some(metadata)) if current == metadata => {}
                (Some(_), Some(metadata)) => {
                    // Wrong frame on current stack. Drop all trailing frames.
                    #[allow(clippy::while_let_on_iterator)]
                    while let Some(_) = current.next() {
                        self.node_exited_in_ui();
                    }
                    self.expression_entered_in_ui(&Some(metadata))?;
                }
                (None, Some(metadata)) => {
                    self.expression_entered_in_ui(&Some(metadata))?;
                }
                (Some(_), None) => {
                    self.node_exited_in_ui();
                }
                (None, None) => break FallibleResult::Ok(()),
            }
        }
    }

    /// Reload whole displayed content to be up to date with module state.
    pub fn refresh_graph_view(&self) -> FallibleResult {
        info!(self.logger, "Refreshing the graph view.");
        let connections_info = self.graph.connections()?;
        self.refresh_node_views(&connections_info, true)?;
        self.refresh_connection_views(connections_info.connections)?;
        Ok(())
    }

    fn refresh_node_views(
        &self,
        connections_info: &Connections,
        update_position: bool,
    ) -> FallibleResult {
        let base_default_position = default_node_position();
        let mut trees = connections_info.trees.clone();
        let nodes = self.graph.graph().nodes()?;
        let (without_pos, with_pos): (Vec<_>, Vec<_>) =
            nodes.iter().partition(|n| n.has_position());
        let bottommost_node_pos = with_pos
            .iter()
            .filter_map(|node| node.metadata.as_ref()?.position)
            .min_by(model::module::Position::ord_by_y)
            .map(|pos| pos.vector)
            .unwrap_or(base_default_position);

        let default_positions: HashMap<_, _> = (1..)
            .zip(&without_pos)
            .map(|(i, node)| {
                let gap_between_nodes = self.view.graph().default_y_gap_between_nodes.value();
                let offset_between_nodes = gap_between_nodes + node::HEIGHT;
                let dy = i as f32 * offset_between_nodes;
                let pos = Vector2::new(bottommost_node_pos.x, bottommost_node_pos.y - dy);
                (node.info.id(), pos)
            })
            .collect();

        let ids = nodes.iter().map(|node| node.info.id()).collect();
        self.retain_node_views(&ids);
        for node_info in &nodes {
            let id = node_info.info.id();
            let node_trees = trees.remove(&id).unwrap_or_else(default);
            let default_pos = default_positions.get(&id).unwrap_or(&base_default_position);
            let displayed = self.node_views.borrow_mut().get_by_left(&id).cloned();
            match displayed {
                Some(displayed) => {
                    if update_position {
                        self.refresh_node_position(displayed, node_info);
                        self.refresh_node_selection(displayed, node_info);
                        self.refresh_node_visualization(displayed, node_info);
                    };
                    self.refresh_node_comment(displayed, node_info);
                    self.refresh_node_expression(displayed, node_info, node_trees);
                }
                None => self.create_node_view(node_info, node_trees, *default_pos),
            }
        }
        Ok(())
    }

    /// Refresh the expressions (e.g., types, ports) for all nodes.
    fn refresh_graph_expressions(&self) -> FallibleResult {
        info!(self.logger, "Refreshing the graph expressions.");
        let connections = self.graph.connections()?;
        self.refresh_node_views(&connections, false)?;
        self.refresh_connection_views(connections.connections)
    }

    /// Retain only given nodes in displayed graph.
    fn retain_node_views(&self, ids: &HashSet<ast::Id>) {
        let to_remove = {
            let borrowed = self.node_views.borrow();
            let filtered = borrowed.iter().filter(|(id, _)| !ids.contains(id));
            filtered.map(|(k, v)| (*k, *v)).collect_vec()
        };
        for (id, displayed_id) in to_remove {
            self.view.graph().frp.input.remove_node.emit(&displayed_id);
            self.node_views.borrow_mut().remove_by_left(&id);
        }
    }

    fn create_node_view(
        &self,
        info: &controller::graph::Node,
        trees: NodeTrees,
        default_pos: Vector2,
    ) {
        let id = info.info.id();
        let displayed_id = self.view.graph().add_node();
        self.node_views.borrow_mut().insert(id, displayed_id);
        self.refresh_node_view(displayed_id, info, trees);
        if info.metadata.as_ref().and_then(|md| md.position).is_none() {
            self.view.graph().frp.input.set_node_position.emit(&(displayed_id, default_pos));
            // If position wasn't present in metadata, we initialize it.
            if let Err(err) = self.graph.graph().set_node_position(id, default_pos) {
                debug!(
                    self.logger,
                    "Failed to set default position information IDs: {id:?} \
                because of the error: {err:?}"
                );
            }
        }
    }

    fn deserialize_visualization_data(
        data: VisualizationUpdateData,
    ) -> FallibleResult<visualization::Data> {
        let binary = data.as_ref();
        let as_text = std::str::from_utf8(binary)?;
        let as_json: serde_json::Value = serde_json::from_str(as_text)?;
        Ok(visualization::Data::from(as_json))
    }

    fn refresh_node_view(
        &self,
        id: graph_editor::NodeId,
        node: &controller::graph::Node,
        trees: NodeTrees,
    ) {
        self.refresh_node_position(id, node);
        self.refresh_node_selection(id, node);
        self.refresh_node_comment(id, node);
        self.refresh_node_expression(id, node, trees);
        self.refresh_node_visualization(id, node);
    }

    /// Update the position of the node based on its current metadata.
    fn refresh_node_position(&self, id: graph_editor::NodeId, node: &controller::graph::Node) {
        let position = node.metadata.as_ref().and_then(|md| md.position);
        if let Some(position) = position {
            let view_position = self.view.graph().model.get_node_position(id).map(|p| p.xy());
            if Some(position.vector) != view_position {
                self.view.graph().frp.input.set_node_position.emit(&(id, position.vector));
            }
        }
    }

    /// Update the position of the node based on its current metadata.
    fn refresh_node_selection(&self, id: graph_editor::NodeId, node: &controller::graph::Node) {
        let selected_in_metadata = node.metadata.as_ref().map_or_default(|md| md.selected);
        let selected_in_view = self.view.graph().model.nodes.is_selected(id);
        if selected_in_metadata && !selected_in_view {
            self.view.graph().frp.input.select_node.emit(&id)
        } else if !selected_in_metadata && selected_in_view {
            self.view.graph().frp.input.deselect_node.emit(&id)
        }
    }

    /// Update the enabled visualization on a node to follow the metadata.
    fn refresh_node_visualization(&self, id: graph_editor::NodeId, node: &controller::graph::Node) {
        let path_in_metadata = node.metadata.as_ref().and_then(|md| {
            // It is perfectly fine to ignore deserialization errors here. This is metadata, that
            // might not even be initialized.
            serde_json::from_value(md.visualization.clone()).ok()
        });

        let visualization_frp = match self.view.graph().model.nodes.all.get_cloned_ref(&id) {
            Some(node) => node.model.visualization.frp.clone_ref(),
            // If there is no node view, there is nothing to be done with visualization view.
            None => return,
        };
        let is_visible = visualization_frp.visible.value();
        let path_in_visible_visualization = is_visible
            .and_option(visualization_frp.visualisation.value())
            .map(|definition| definition.path());

        if path_in_visible_visualization != path_in_metadata {
            if path_in_metadata.is_some() {
                let visualization = (id, path_in_metadata);
                self.view.graph().frp.input.set_visualization.emit(&visualization);
                self.view.graph().frp.input.enable_visualization.emit(&id);
            } else {
                self.view.graph().frp.input.disable_visualization.emit(&id);
            }
        }
    }
    /// Update the documentation comment on the node.
    fn refresh_node_comment(&self, id: graph_editor::NodeId, node: &controller::graph::Node) {
        if let Some(node_view) = self.view.graph().model.nodes.get_cloned_ref(&id) {
            let comment_as_per_controller = node.info.documentation_text().unwrap_or_default();
            let comment_as_per_view = node_view.comment.value();
            if comment_as_per_controller != comment_as_per_view {
                node_view.set_comment(comment_as_per_controller);
            }
        }
    }

    /// Update the expression of the node and all related properties e.g., types, ports).
    fn refresh_node_expression(
        &self,
        id: graph_editor::NodeId,
        node: &controller::graph::Node,
        trees: NodeTrees,
    ) {
        let code_and_trees = graph_editor::component::node::Expression {
            pattern:             node.info.pattern().map(|t| t.repr()),
            code:                node.info.expression().repr(),
            whole_expression_id: node.info.expression().id,
            input_span_tree:     trees.inputs,
            output_span_tree:    trees.outputs.unwrap_or_else(default),
        };
        let expression_changed =
            !self.expression_views.borrow().get(&id).contains(&&code_and_trees);
        let node_is_being_edited = self.view.graph().frp.node_being_edited.value().contains(&id);
        if expression_changed && !node_is_being_edited {
            for sub_expression in node.info.ast().iter_recursive() {
                if let Some(expr_id) = sub_expression.id {
                    self.node_view_by_expression.borrow_mut().insert(expr_id, id);
                }
            }
            info!(self.logger, "Refreshing node {id:?} expression");
            self.view.graph().frp.input.set_node_expression.emit(&(id, code_and_trees.clone()));
            self.expression_views.borrow_mut().insert(id, code_and_trees);
        }

        // Set initially available type information on ports (identifiable expression's sub-parts).
        for expression_part in node.info.expression().iter_recursive() {
            if let Some(id) = expression_part.id {
                self.refresh_computed_info(id, expression_changed);
            }
        }
    }

    /// Like `refresh_computed_info` but for multiple expressions.
    fn refresh_computed_infos(&self, expressions_to_refresh: &[ExpressionId]) -> FallibleResult {
        debug!(self.logger, "Refreshing type information for IDs: {expressions_to_refresh:?}.");
        for id in expressions_to_refresh {
            self.refresh_computed_info(*id, false)
        }
        Ok(())
    }

    /// Look up the computed information for a given expression and pass the information to the
    /// graph editor view.
    ///
    /// The computed value information includes the expression type and the target method pointer.
    fn refresh_computed_info(&self, id: ExpressionId, force_type_info_refresh: bool) {
        let info = self.lookup_computed_info(&id);
        let info = info.as_ref();
        let typename = info.and_then(|info| info.typename.clone().map(graph_editor::Type));
        let node_id = self.node_view_by_expression.borrow().get(&id).cloned();
        if let Some(node_id) = node_id {
            self.set_type(node_id, id, typename, force_type_info_refresh);
            let method_pointer = info.and_then(|info| {
                info.method_call.and_then(|entry_id| {
                    let opt_method = self.project.suggestion_db().lookup_method_ptr(entry_id).ok();
                    opt_method.map(|method| graph_editor::MethodPointer(Rc::new(method)))
                })
            });
            self.set_method_pointer(id, method_pointer);
            if self.node_views.borrow().get_by_left(&id).contains(&&node_id) {
                let set_error_result = self.set_error(node_id, info.map(|info| &info.payload));
                if let Err(error) = set_error_result {
                    error!(self.logger, "Error when setting error on node: {error}");
                }
            }
        }
    }

    /// Set given type (or lack of such) on the given sub-expression.
    fn set_type(
        &self,
        node_id: graph_editor::NodeId,
        id: ExpressionId,
        typename: Option<graph_editor::Type>,
        force_refresh: bool,
    ) {
        // We suppress spurious type information updates here, as they were causing performance
        // issues. See: https://github.com/enso-org/ide/issues/952
        let previous_type_opt = self.expression_types.insert(id, typename.clone());
        if force_refresh || previous_type_opt.as_ref() != Some(&typename) {
            let event = (node_id, id, typename);
            self.view.graph().frp.input.set_expression_usage_type.emit(&event);
        }
    }

    /// Set given method pointer (or lack of such) on the given sub-expression.
    fn set_method_pointer(&self, id: ExpressionId, method: Option<graph_editor::MethodPointer>) {
        let event = (id, method);
        self.view.graph().frp.input.set_method_pointer.emit(&event);
    }

    fn visualization_manager(&self, which: WhichVisualization) -> &Rc<VisualizationManager> {
        match which {
            WhichVisualization::Normal => &self.visualizations,
            WhichVisualization::Error => &self.error_visualizations,
        }
    }

    fn handle_visualization_update(
        &self,
        which: WhichVisualization,
        notification: crate::integration::visualization::Notification,
    ) {
        use crate::integration::visualization::Notification;
        warning!(self.logger, "Received update for {which} visualization: {notification:?}");
        match notification {
            Notification::ValueUpdate { target, data, .. } => {
                if let Ok(view_id) = self.get_displayed_node_id(target) {
                    let endpoint = &self.view.graph().frp.input.set_visualization_data;
                    match Self::deserialize_visualization_data(data) {
                        Ok(data) => endpoint.emit((view_id, data)),
                        Err(error) =>
                        // TODO [mwu]
                        //  We should consider having the visualization also accept error input.
                            error!(
                                self.logger,
                                "Failed to deserialize visualization update: {error}"
                            ),
                    }
                }
            }
            Notification::FailedToAttach { visualization, error } => {
                error!(self.logger, "Visualization {visualization.id} failed to attach: {error}.");
                if let Ok(node_view_id) = self.get_displayed_node_id(visualization.expression_id) {
                    self.view.graph().disable_visualization(node_view_id);
                }
            }
            Notification::FailedToDetach { visualization, error } => {
                error!(self.logger, "Visualization {visualization.id} failed to detach: {error}.");
                // Here we cannot really do much. Failing to detach might mean that visualization
                // was already detached, that we detached it but failed to observe this (e.g. due to
                // a connectivity issue) or that we did something really wrong.
                // For now, we will just forget about this visualization. Better to unlikely "leak"
                // it rather than likely break visualizations on the node altogether.
                let manager = self.visualization_manager(which);
                let forgotten = manager.forget_visualization(visualization.expression_id);
                if let Some(forgotten) = forgotten {
                    error!(self.logger, "The visualization will be forgotten: {forgotten:?}")
                }
            }
            Notification::FailedToModify { desired, error } => {
                error!(
                    self.logger,
                    "Visualization {desired.id} failed to be modified: {error} \
                Will hide it in GUI."
                );
                // Actually it would likely have more sense if we had just restored the previous
                // visualization, as its LS state should be preserved. However, we already scrapped
                // it on the GUI side and we don't even know its path anymore.
                if let Ok(node_view_id) = self.get_displayed_node_id(desired.expression_id) {
                    self.view.graph().disable_visualization(node_view_id);
                }
            }
        }
    }

    /// Route the metadata description as a desired visualization state to the Manager.
    fn update_visualization(
        &self,
        node_id: graph_editor::NodeId,
        which: WhichVisualization,
        metadata: Option<visualization::Metadata>,
    ) -> FallibleResult {
        let target_id = self.get_controller_node_id(node_id)?;
        let manager = self.visualization_manager(which);
        manager.set_visualization(target_id, metadata);
        Ok(())
    }

    /// Mark node as erroneous if given payload contains an error.
    fn set_error(
        &self,
        node_id: graph_editor::NodeId,
        error: Option<&ExpressionUpdatePayload>,
    ) -> FallibleResult {
        let error = self.convert_payload_to_error_view(error, node_id);
        let has_error = error.is_some();
        self.view.graph().set_node_error_status(node_id, error);
        let metadata =
            has_error.then(graph_editor::builtin::visualization::native::error::metadata);
        self.update_visualization(node_id, WhichVisualization::Error, metadata)
    }

    fn convert_payload_to_error_view(
        &self,
        payload: Option<&ExpressionUpdatePayload>,
        node_id: graph_editor::NodeId,
    ) -> Option<node::error::Error> {
        use node::error::Kind;
        use ExpressionUpdatePayload::*;
        let (kind, message, trace) = match payload {
            None | Some(Value) => None,
            Some(DataflowError { trace }) => Some((Kind::Dataflow, None, trace)),
            Some(Panic { message, trace }) => Some((Kind::Panic, Some(message), trace)),
        }?;
        let propagated = if kind == Kind::Panic {
            let root_cause = self.get_node_causing_error_on_current_graph(trace);
            !root_cause.contains(&node_id)
        } else {
            // TODO[ao]: traces are not available for Dataflow errors.
            false
        };

        let kind = Immutable(kind);
        let message = Rc::new(message.cloned());
        let propagated = Immutable(propagated);
        Some(node::error::Error { kind, message, propagated })
    }

    /// Get the node being a main cause of some error from the current nodes on the scene. Returns
    /// [`None`] if the error is not present on the scene at all.
    fn get_node_causing_error_on_current_graph(
        &self,
        trace: &[ExpressionId],
    ) -> Option<graph_editor::NodeId> {
        let node_view_by_expression = self.node_view_by_expression.borrow();
        trace.iter().find_map(|expr_id| node_view_by_expression.get(expr_id).copied())
    }

    fn refresh_connection_views(
        &self,
        connections: Vec<controller::graph::Connection>,
    ) -> FallibleResult {
        self.retain_connection_views(&connections);
        for con in connections {
            if !self.connection_views.borrow().contains_left(&con) {
                let targets = self.edge_targets_from_controller_connection(con.clone())?;
                self.view.graph().frp.input.connect_nodes.emit(&targets);
                let edge_id = self.view.graph().frp.output.on_edge_add.value();
                self.connection_views.borrow_mut().insert(con, edge_id);
            }
        }
        Ok(())
    }

    fn edge_targets_from_controller_connection(
        &self,
        connection: controller::graph::Connection,
    ) -> FallibleResult<(EdgeEndpoint, EdgeEndpoint)> {
        let src_node = self.get_displayed_node_id(connection.source.node)?;
        let dst_node = self.get_displayed_node_id(connection.destination.node)?;
        let src = EdgeEndpoint::new(src_node, connection.source.port);
        let data = EdgeEndpoint::new(dst_node, connection.destination.port);
        Ok((src, data))
    }

    /// Retain only given connections in displayed graph.
    fn retain_connection_views(&self, connections: &[controller::graph::Connection]) {
        let to_remove = {
            let borrowed = self.connection_views.borrow();
            let filtered = borrowed.iter().filter(|(con, _)| !connections.contains(con));
            filtered.map(|(_, edge_id)| *edge_id).collect_vec()
        };
        for edge_id in to_remove {
            self.view.graph().frp.input.remove_edge.emit(&edge_id);
            self.connection_views.borrow_mut().remove_by_right(&edge_id);
        }
    }
}


// === Updating Searcher View ===

impl Model {
    pub fn get_category_content(
        &self,
        crumbs: &[usize],
    ) -> Vec<(usize, ide_view::searcher::new::Entry)> {
        let maybe_actions = self.searcher.borrow().as_ref().map(controller::Searcher::actions);
        let is_filtering = self.searcher.borrow().contains_if(controller::Searcher::is_filtering);
        let maybe_list = maybe_actions.as_ref().and_then(|actions| actions.list());
        if let Some(list) = maybe_list {
            match crumbs.len() {
                0 => {
                    // We skip the first "All Search Result" category if we're not currently
                    // filtering
                    let skipped_categories = if is_filtering { 0 } else { 1 };
                    list.root_categories()
                        .skip(skipped_categories)
                        .map(|(id, cat)| {
                            (id, ide_view::searcher::new::Entry {
                                label:     cat.name.to_string().into(),
                                is_folder: Immutable(true),
                                icon:      Icon(cat.icon.clone_ref()),
                            })
                        })
                        .collect()
                }
                1 => list
                    .subcategories_of(*crumbs.last().unwrap())
                    .map(|(id, cat)| {
                        (id, ide_view::searcher::new::Entry {
                            label:     cat.name.to_string().into(),
                            is_folder: Immutable(true),
                            icon:      Icon(cat.icon.clone_ref()),
                        })
                    })
                    .collect(),
                2 => list
                    .actions_of(*crumbs.last().unwrap())
                    .map(|(id, action)| {
                        (id, ide_view::searcher::new::Entry {
                            label:     action.action.to_string().into(),
                            is_folder: Immutable(false),
                            icon:      Icon(action.action.icon()),
                        })
                    })
                    .collect(),
                _ => vec![],
            }
        } else {
            vec![]
        }
    }
}


// === Handling Controller Notifications ===

impl Model {
    /// Handle notification received from controller about the whole graph being invalidated.
    pub fn on_graph_invalidated(&self) -> FallibleResult {
        // We drop error, because in general this scenario should not be considered an error from a
        // higher perspective. Getting invalid call stack entry is a very possible result of e.g.
        // user editing text.
        // In some future (where we can deal better with such scenarios) this code should be
        // reconsidered.
        let _ = self.refresh_call_stack().ok();
        self.refresh_graph_view()
    }

    /// Handle notification received from controller about the graph receiving new type information.
    pub fn on_graph_expression_update(&self) -> FallibleResult {
        self.refresh_graph_expressions()
    }

    /// Handle notification received from controller about the whole graph being invalidated.
    pub fn on_text_invalidated(&self) -> FallibleResult {
        self.refresh_code_editor()
    }

    /// Handle notification received from controller about values having been entered.
    pub fn on_node_entered(&self, local_call: &LocalCall) -> FallibleResult {
        analytics::remote_log_event("integration::node_entered");
        self.view.graph().frp.deselect_all_nodes.emit(&());
        self.push_crumb(local_call);
        self.refresh_graph_view()
    }

    /// Handle notification received from controller about node having been exited.
    pub fn on_node_exited(&self, id: double_representation::node::Id) -> FallibleResult {
        analytics::remote_log_event("integration::node_exited");
        self.view.graph().frp.deselect_all_nodes.emit(&());
        self.refresh_graph_view()?;
        self.pop_crumb();
        let id = self.get_displayed_node_id(id)?;
        self.view.graph().frp.select_node.emit(&id);
        Ok(())
    }

    /// Handle notification received from controller about values having been computed.
    pub fn on_values_computed(&self, expressions: &[ExpressionId]) -> FallibleResult {
        if !self.prompt_was_shown.get() {
            self.view.show_prompt();
            self.prompt_was_shown.set(true);
        }
        self.refresh_computed_infos(expressions)
    }

    /// Handle notification received from Graph Controller.
    pub fn handle_graph_notification(
        &self,
        notification: &Option<controller::graph::executed::Notification>,
    ) {
        use controller::graph::executed::Notification;
        use controller::graph::Notification::*;

        debug!(self.logger, "Received graph notification {notification:?}");
        let result = match notification {
            Some(Notification::Graph(Invalidate)) => self.on_graph_invalidated(),
            Some(Notification::Graph(PortsUpdate)) => self.on_graph_expression_update(),
            Some(Notification::ComputedValueInfo(update)) => self.on_values_computed(update),
            Some(Notification::SteppedOutOfNode(id)) => self.on_node_exited(*id),
            Some(Notification::EnteredNode(local_call)) => self.on_node_entered(local_call),
            None => {
                warning!(
                    self.logger,
                    "Handling `None` notification is not implemented; \
                    performing full invalidation"
                );
                self.refresh_graph_view()
            }
        };
        if let Err(err) = result {
            error!(
                self.logger,
                "Error while updating graph after receiving {notification:?} from \
                controller: {err}"
            );
        }
    }

    /// Handle notification received from Text Controller.
    pub fn handle_text_notification(&self, notification: Option<controller::text::Notification>) {
        use controller::text::Notification;

        debug!(self.logger, "Received text notification {notification:?}");
        let result = match notification {
            Some(Notification::Invalidate) => self.on_text_invalidated(),
            other => {
                warning!(
                    self.logger,
                    "Handling notification {other:?} is not implemented; \
                    performing full invalidation"
                );
                self.refresh_code_editor()
            }
        };
        if let Err(err) = result {
            error!(
                self.logger,
                "Error while updating graph after receiving {notification:?} from \
                controller: {err}"
            );
        }
    }

    pub fn handle_searcher_notification(&self, notification: controller::searcher::Notification) {
        use controller::searcher::Notification;
        use controller::searcher::UserAction;
        debug!(self.logger, "Received searcher notification {notification:?}");
        match notification {
            Notification::NewActionList => with(self.searcher.borrow(), |searcher| {
                if let Some(searcher) = &*searcher {
                    match searcher.actions() {
                        Actions::Loading => self.view.searcher().clear_actions(),
                        Actions::Loaded { list: actions } => {
                            let list_is_empty = actions.matching_count() == 0;
                            let user_action = searcher.current_user_action();
                            let intended_function = searcher.intended_function_suggestion();
                            let provider = SuggestionsProviderForView {
                                actions,
                                user_action,
                                intended_function,
                            };
                            self.view.searcher().set_actions(Rc::new(provider));

                            // the Searcher 2.0
                            self.view.searcher().new_frp().reset();

                            // Usually we want to select first entry and display docs for it
                            // But not when user finished typing function or argument.
                            let starting_typing = user_action == UserAction::StartingTypingArgument;
                            if !starting_typing && !list_is_empty {
                                self.view.searcher().select_action(0);
                            }
                        }
                        Actions::Error(err) => {
                            error!(self.logger, "Error while obtaining list from searcher: {err}");
                            self.view.searcher().clear_actions();
                        }
                    };
                }
            }),
        }
    }
}


// === Passing UI Actions To Controllers ===

// These functions are called with FRP event values as arguments. The FRP values are always provided
// by reference, including "trivially-copy" types and Vecs, To keep code cleaner we take
// all parameters by reference.
#[allow(clippy::trivially_copy_pass_by_ref)]
#[allow(clippy::ptr_arg)]
impl Model {
    fn node_removed_in_ui(&self, node: &graph_editor::NodeId) -> FallibleResult {
        debug!(self.logger, "Removing node.");
        let id = self.get_controller_node_id(*node)?;
        self.node_views.borrow_mut().remove_by_left(&id);
        self.graph.graph().remove_node(id)?;
        Ok(())
    }

    fn node_selected_in_ui(&self, displayed_id: &graph_editor::NodeId) -> FallibleResult {
        debug!(self.logger, "Selecting node {displayed_id}.");
        let id = self.get_controller_node_id(*displayed_id)?;
        self.graph.graph().module.with_node_metadata(
            id,
            Box::new(|metadata| {
                metadata.selected = true;
            }),
        )?;
        Ok(())
    }

    fn node_deselected_in_ui(&self, displayed_id: &graph_editor::NodeId) -> FallibleResult {
        debug!(self.logger, "Deselecting node {displayed_id}.");
        let id = self.get_controller_node_id(*displayed_id)?;
        self.graph.graph().module.with_node_metadata(
            id,
            Box::new(|metadata| {
                metadata.selected = false;
            }),
        )?;
        Ok(())
    }

    fn node_moved_in_ui(
        &self,
        (displayed_id, pos): &(graph_editor::NodeId, Vector2),
    ) -> FallibleResult {
        debug!(self.logger, "Moving node {displayed_id}.");
        if let Ok(id) = self.get_controller_node_id(*displayed_id) {
            self.graph.graph().set_node_position(id, *pos)?
        }
        Ok(())
    }

    fn nodes_collapsed_in_ui(
        &self,
        (collapsed, _new_node_view_id): &(Vec<graph_editor::NodeId>, graph_editor::NodeId),
    ) -> FallibleResult {
        debug!(self.logger, "Collapsing node.");
        let ids = self.get_controller_node_ids(collapsed)?;
        let _new_node_id = self.graph.graph().collapse(ids, COLLAPSED_FUNCTION_NAME)?;
        // TODO [mwu] https://github.com/enso-org/ide/issues/760
        //   As part of this issue, storing relation between new node's controller and view ids will
        //   be necessary.
        Ok(())
    }

    fn node_expression_set_in_ui(
        &self,
        (displayed_id, expression): &(graph_editor::NodeId, String),
    ) -> FallibleResult {
        debug!(self.logger, "Setting node {displayed_id} expression: {expression}.");
        let searcher = self.searcher.borrow();
        let code_and_trees = graph_editor::component::node::Expression::new_plain(expression);
        self.expression_views.borrow_mut().insert(*displayed_id, code_and_trees);
        if let Some(searcher) = searcher.as_ref() {
            searcher.set_input(expression.clone())?;
        }
        Ok(())
    }

    fn searcher_opened_in_ui(
        weak_self: Weak<Self>,
    ) -> impl Fn(&Self, &graph_editor::NodeId) -> FallibleResult {
        move |this, displayed_id| {
            let node_view = this.view.graph().model.nodes.get_cloned_ref(displayed_id);
            let position = node_view.map(|node| node.position().xy());
            let position = position.map(|vector| model::module::Position { vector });
            let mode = controller::searcher::Mode::NewNode { position };
            this.setup_searcher_controller(&weak_self, mode)
        }
    }

    fn node_editing_in_ui(
        weak_self: Weak<Self>,
    ) -> impl Fn(&Self, &Option<graph_editor::NodeId>) -> FallibleResult {
        move |this, displayed_id| {
            if this.searcher.borrow().is_some() {
                // This is a normal situation: the searcher controller might be created when
                // one of "open searcher" signals was emitted by searcher view.
                debug!(this.logger, "Searcher controller already created.");
            } else if let Some(displayed_id) = displayed_id {
                debug!(this.logger, "Starting node editing.");
                let id = this.get_controller_node_id(*displayed_id);
                let mode = match id {
                    Ok(node_id) => controller::searcher::Mode::EditNode { node_id },
                    Err(MissingMappingFor::DisplayedNode(id)) => {
                        let node_view = this.view.graph().model.nodes.get_cloned_ref(&id);
                        let position = node_view.map(|node| node.position().xy());
                        let position = position.map(|vector| model::module::Position { vector });
                        controller::searcher::Mode::NewNode { position }
                    }
                    Err(other) => return Err(other.into()),
                };
                this.setup_searcher_controller(&weak_self, mode)?;
            } else {
                debug!(this.logger, "Finishing node editing.");
            }
            Ok(())
        }
    }

    fn used_as_suggestion_in_ui(
        &self,
        entry: &Option<ide_view::searcher::entry::Id>,
    ) -> FallibleResult {
        debug!(self.logger, "Using as suggestion.");
        if let Some(entry) = entry {
            let graph_frp = &self.view.graph().frp;
            let error = || MissingSearcherController;
            let searcher = self.searcher.borrow().clone().ok_or_else(error)?;
            let error = || GraphEditorInconsistency;
            let edited_node = graph_frp.output.node_being_edited.value().ok_or_else(error)?;
            let code = searcher.use_as_suggestion(*entry)?;
            let code_and_trees = node::Expression::new_plain(code);
            graph_frp.input.set_node_expression.emit(&(edited_node, code_and_trees));
        }
        Ok(())
    }

    fn node_editing_committed_in_ui(
        &self,
        (displayed_id, entry_id): &(graph_editor::NodeId, Option<ide_view::searcher::entry::Id>),
    ) -> FallibleResult {
        use crate::controller::searcher::action::Action::Example;
        debug!(self.logger, "Committing node expression.");
        let error = || MissingSearcherController;
        let searcher = self.searcher.replace(None).ok_or_else(error)?;
        let entry = searcher.actions().list().zip(*entry_id).and_then(|(l, i)| l.get_cloned(i));
        let is_example = entry.map_or(false, |e| matches!(e.action, Example(_)));
        let result = if let Some(id) = entry_id {
            searcher.execute_action_by_index(*id)
        } else {
            searcher.commit_node().map(Some)
        };
        match result {
            Ok(Some(node_id)) => {
                // Node editor wants to decide whether node is selected after adding/updating.
                // However when adding a new node, signals about selecting node are emitted before
                // the node is connected to the controller.
                // Thus here we need to initialize metadata with selection state from the view.
                // We ignore any error, because it can happen only when metadata is not serializable
                // to JSON.
                let _ = self.graph.graph().module.with_node_metadata(
                    node_id,
                    Box::new(|metadata| {
                        metadata.selected =
                            self.view.graph().model.nodes.is_selected(*displayed_id);
                    }),
                );
                self.node_views.borrow_mut().insert(node_id, *displayed_id);
                if is_example {
                    self.view.graph().frp.enable_visualization(displayed_id);
                }
                Ok(())
            }
            Ok(None) => {
                self.view.graph().frp.remove_node(displayed_id);
                Ok(())
            }
            Err(err) => {
                self.view.graph().frp.remove_node(displayed_id);
                Err(err)
            }
        }
    }

    fn node_editing_aborted_in_ui(&self, _displayed_id: &graph_editor::NodeId) -> FallibleResult {
        *self.searcher.borrow_mut() = None;
        Ok(())
    }

    fn connection_created_in_ui(&self, edge_id: &graph_editor::EdgeId) -> FallibleResult {
        debug!(self.logger, "Creating connection.");
        let displayed =
            self.view.graph().model.edges.get_cloned(edge_id).ok_or(GraphEditorInconsistency)?;
        let con = self.controller_connection_from_displayed(&displayed)?;
        let inserting = self.connection_views.borrow_mut().insert(con.clone(), *edge_id);
        if inserting.did_overwrite() {
            warning!(
                self.logger,
                "Created connection {edge_id} overwrite some old mappings in \
                GraphEditorIntegration."
            )
        }
        self.graph.connect(&con)?;
        Ok(())
    }

    fn connection_removed_in_ui(&self, edge_id: &graph_editor::EdgeId) -> FallibleResult {
        debug!(self.logger, "Removing connection.");
        let connection = self.get_controller_connection(*edge_id)?;
        self.connection_views.borrow_mut().remove_by_left(&connection);
        self.graph.disconnect(&connection)?;
        Ok(())
    }

    fn visualization_path_changed_in_ui(
        &self,
        (node_id, vis_path): &(graph_editor::NodeId, Option<visualization::Path>),
    ) -> FallibleResult {
        debug!(self.logger, "Visualization path changed on {node_id}: {vis_path:?}.");
        let ast_id = self.get_controller_node_id(*node_id)?;
        let serialized_path = serde_json::to_value(&vis_path)?;
        self.graph.graph().module.with_node_metadata(
            ast_id,
            Box::new(|node_metadata| {
                node_metadata.visualization = serialized_path;
            }),
        )?;
        Ok(())
    }

    fn visualization_shown_in_ui(
        &self,
        (node_id, vis_metadata): &(graph_editor::NodeId, visualization::Metadata),
    ) -> FallibleResult {
        debug!(self.logger, "Visualization shown on {node_id}: {vis_metadata:?}.");
        self.update_visualization(*node_id, WhichVisualization::Normal, Some(vis_metadata.clone()))
    }

    fn visualization_hidden_in_ui(&self, node_id: &graph_editor::NodeId) -> FallibleResult {
        debug!(self.logger, "Visualization hidden on {node_id}.");
        self.update_visualization(*node_id, WhichVisualization::Normal, None)
    }

    fn store_updated_stack_task(&self) -> impl FnOnce() -> FallibleResult + 'static {
        let main_module = self.main_module.clone_ref();
        let graph = self.graph.clone_ref();
        move || {
            main_module.update_project_metadata(|metadata| {
                metadata.call_stack = graph.call_stack();
            })
        }
    }

    fn expression_entered_in_ui(&self, local_call: &Option<LocalCall>) -> FallibleResult {
        if let Some(local_call) = local_call {
            let local_call = local_call.clone();
            let controller = self.graph.clone_ref();
            let logger = self.logger.clone_ref();
            let store_stack = self.store_updated_stack_task();
            let enter_action = async move {
                info!(logger, "Entering expression {local_call:?}.");
                if let Err(e) = controller.enter_method_pointer(&local_call).await {
                    error!(logger, "Entering node failed: {e}.");

                    let event = "integration::entering_node_failed";
                    let field = "error";
                    let data = analytics::AnonymousData(|| e.to_string());
                    analytics::remote_log_value(event, field, data)
                } else {
                    // We cannot really do anything when updating metadata fails.
                    // Can happen in improbable case of serialization failure.
                    if let Err(e) = store_stack() {
                        error!(logger, "Failed to store an updated call stack: {e}");
                    }
                }
            };
            spawn(enter_action);
        }
        Ok(())
    }

    fn node_entered_in_ui(&self, node_id: &graph_editor::NodeId) -> FallibleResult {
        debug!(self.logger, "Requesting entering the node {node_id}.");
        let call = self.get_controller_node_id(*node_id)?;
        let method_pointer = self.graph.node_method_pointer(call)?;
        let definition = (*method_pointer).clone();
        let local_call = LocalCall { call, definition };
        self.expression_entered_in_ui(&Some(local_call))
    }

    fn node_exited_in_ui(&self) {
        debug!(self.logger, "Requesting exiting the current node.");
        let controller = self.graph.clone_ref();
        let logger = self.logger.clone_ref();
        let update_metadata = self.store_updated_stack_task();
        let exit_node_action = async move {
            info!(logger, "Exiting node.");
            if let Err(e) = controller.exit_node().await {
                debug!(logger, "Exiting node failed: {e}.");

                let event = "integration::exiting_node_failed";
                let field = "error";
                let data = analytics::AnonymousData(|| e.to_string());
                analytics::remote_log_value(event, field, data)
            } else {
                // This should never happen, as our metadata structures are always serializable to
                // JSON.
                let _ = update_metadata().ok();
            }
        };
        spawn(exit_node_action);
    }

    fn code_changed_in_ui(&self, changes: &Vec<ensogl_text::Change>) -> FallibleResult {
        for change in changes {
            let range_start = data::text::Index::new(change.range.start.value as usize);
            let range_end = data::text::Index::new(change.range.end.value as usize);
            let converted = TextChange::replace(range_start..range_end, change.text.to_string());
            self.text.apply_text_change(converted)?;
        }
        Ok(())
    }

    fn module_saved_in_ui(&self) {
        let logger = self.logger.clone_ref();
        let controller = self.text.clone_ref();
        let content = self.code_view.get().to_string();
        spawn(async move {
            if let Err(err) = controller.store_content(content).await {
                error!(logger, "Error while saving file: {err:?}");
            }
        });
    }

    fn undo_in_ui(&self) {
        debug!(self.logger, "Undo triggered in UI.");
        if let Err(e) = self.project.urm().undo() {
            error!(self.logger, "Undo failed: {e}");
        }
    }

    fn redo_in_ui(&self) {
        debug!(self.logger, "Redo triggered in UI.");
        if let Err(e) = self.project.urm().redo() {
            error!(self.logger, "Redo failed: {e}");
        }
    }

    fn visualization_preprocessor_changed(
        &self,
        node_id: graph_editor::NodeId,
        preprocessor: visualization::instance::PreprocessorConfiguration,
    ) -> FallibleResult {
        let metadata = visualization::Metadata { preprocessor };
        self.update_visualization(node_id, WhichVisualization::Normal, Some(metadata))
    }

    fn open_dialog_opened_in_ui(self: &Rc<Self>) {
        debug!(self.logger, "Opened file dialog in ui. Providing content root list");
        self.reload_files_in_file_browser();
        let model = Rc::downgrade(self);
        spawn(async move {
            if let Some(this) = model.upgrade() {
                if let Ok(manage_projects) = this.ide.manage_projects() {
                    match manage_projects.list_projects().await {
                        Ok(projects) => {
                            let entries = ProjectsToOpen::new(projects);
                            this.displayed_project_list.set(entries.clone_ref());
                            let any_entries = AnyModelProvider::new(entries);
                            this.view.open_dialog().project_list.set_entries(any_entries)
                        }
                        Err(error) =>
                            error!(this.logger, "Error when loading project's list: {error}"),
                    }
                }
            }
        });
    }

    fn reload_files_in_file_browser(&self) {
        let provider = FileProvider::new(&self.project);
        let provider: AnyFolderContent = provider.into();
        self.view.open_dialog().file_browser.set_content(provider);
    }

    fn project_opened_in_ui(&self, entry_id: &list_view::entry::Id) {
        if let Some(id) = self.displayed_project_list.get().get_project_id_by_index(*entry_id) {
            let logger = self.logger.clone_ref();
            let ide = self.ide.clone_ref();
            spawn(async move {
                if let Ok(manage_projects) = ide.manage_projects() {
                    if let Err(err) = manage_projects.open_project(id).await {
                        error!(logger, "Error while opening project: {err}");
                    }
                }
            });
        }
    }

    fn file_opened_in_ui(&self, path: &std::path::Path) {
        if let Err(err) = create_node_from_file(&self.project, &self.graph.graph(), path) {
            error!(self.logger, "Error while creating node from file: {err}");
        }
    }
}


// === Utilities ===

impl Model {
    fn get_controller_node_id(
        &self,
        displayed_id: graph_editor::NodeId,
    ) -> Result<ast::Id, MissingMappingFor> {
        let err = MissingMappingFor::DisplayedNode(displayed_id);
        self.node_views.borrow().get_by_right(&displayed_id).cloned().ok_or(err)
    }

    fn get_controller_node_ids(
        &self,
        displayed_ids: impl IntoIterator<Item: std::borrow::Borrow<graph_editor::NodeId>>,
    ) -> Result<Vec<ast::Id>, MissingMappingFor> {
        use std::borrow::Borrow;
        displayed_ids
            .into_iter()
            .map(|id| {
                let id = id.borrow();
                self.get_controller_node_id(*id)
            })
            .collect()
    }

    fn get_displayed_node_id(
        &self,
        node_id: ast::Id,
    ) -> Result<graph_editor::NodeId, MissingMappingFor> {
        let err = MissingMappingFor::ControllerNode(node_id);
        self.node_views.borrow().get_by_left(&node_id).cloned().ok_or(err)
    }

    fn get_controller_connection(
        &self,
        displayed_id: graph_editor::EdgeId,
    ) -> Result<controller::graph::Connection, MissingMappingFor> {
        let err = MissingMappingFor::DisplayedConnection(displayed_id);
        self.connection_views.borrow().get_by_right(&displayed_id).cloned().ok_or(err)
    }

    fn controller_connection_from_displayed(
        &self,
        connection: &graph_editor::Edge,
    ) -> FallibleResult<controller::graph::Connection> {
        let src = connection.source().ok_or(GraphEditorInconsistency {})?;
        let dst = connection.target().ok_or(GraphEditorInconsistency {})?;
        let src_node = self.get_controller_node_id(src.node_id)?;
        let dst_node = self.get_controller_node_id(dst.node_id)?;
        Ok(controller::graph::Connection {
            source:      controller::graph::Endpoint::new(src_node, &src.port),
            destination: controller::graph::Endpoint::new(dst_node, &dst.port),
        })
    }

    fn lookup_computed_info(&self, id: &ExpressionId) -> Option<Rc<ComputedValueInfo>> {
        let registry = self.graph.computed_value_info_registry();
        registry.get(id)
    }

    fn setup_searcher_controller(
        &self,
        weak_self: &Weak<Self>,
        mode: controller::searcher::Mode,
    ) -> FallibleResult {
        let selected_nodes = self
            .view
            .graph()
            .model
            .nodes
            .all_selected()
            .iter()
            .filter_map(|id| self.get_controller_node_id(*id).ok())
            .collect_vec();
        let controller = self.graph.clone_ref();
        let ide = self.ide.clone_ref();
        let searcher = controller::Searcher::new_from_graph_controller(
            &self.logger,
            ide,
            &self.project,
            controller,
            mode,
            selected_nodes,
        )?;
        spawn(searcher.subscribe().for_each(f!([weak_self](notification) {
            if let Some(this) = weak_self.upgrade() {
                this.handle_searcher_notification(notification);
            }
            futures::future::ready(())
        })));
        *self.searcher.borrow_mut() = Some(searcher);
        self.view.searcher().new_frp().reset();
        Ok(())
    }
}



// =======================
// === AttachingResult ===
// =======================

/// Result of an attempt to attach a visualization.
#[derive(Debug)]
pub enum AttachingResult<T> {
    /// Visualization has been successfully attached.
    Attached(T),
    /// Attaching visualization has been aborted before it was attached.
    Aborted,
    /// Attaching visualization failed.
    Failed(failure::Error),
}



// ===========================
// === DataProviderForView ===
// ===========================

#[derive(Clone, Debug)]
struct SuggestionsProviderForView {
    actions:           Rc<controller::searcher::action::List>,
    user_action:       controller::searcher::UserAction,
    intended_function: Option<controller::searcher::action::Suggestion>,
}

impl SuggestionsProviderForView {
    fn doc_placeholder_for(suggestion: &controller::searcher::action::Suggestion) -> String {
        use controller::searcher::action::Suggestion;
        let code = match suggestion {
            Suggestion::FromDatabase(suggestion) => {
                let title = match suggestion.kind {
                    suggestion_database::entry::Kind::Atom => "Atom",
                    suggestion_database::entry::Kind::Function => "Function",
                    suggestion_database::entry::Kind::Local => "Local variable",
                    suggestion_database::entry::Kind::Method => "Method",
                    suggestion_database::entry::Kind::Module => "Module",
                };
                let code = suggestion.code_to_insert(None, true).code;
                format!("{} `{}`\n\nNo documentation available", title, code)
            }
            Suggestion::Hardcoded(suggestion) => {
                format!("{}\n\nNo documentation available", suggestion.name)
            }
        };
        let parser = parser::DocParser::new();
        match parser {
            Ok(p) => {
                let output = p.generate_html_doc_pure((*code).to_string());
                output.unwrap_or(code)
            }
            Err(_) => code,
        }
    }
}

impl list_view::entry::ModelProvider<GlyphHighlightedLabel> for SuggestionsProviderForView {
    fn entry_count(&self) -> usize {
        // TODO[ao] Because of "All Search Results" category, the actions on list are duplicated.
        //     But we don't want to display duplicates on the old searcher list. To be fixed/removed
        //     once new searcher GUI will be implemented
        //     (https://github.com/enso-org/ide/issues/1681)
        self.actions.matching_count() / 2
    }

    fn get(&self, id: usize) -> Option<list_view::entry::GlyphHighlightedLabelModel> {
        let action = self.actions.get_cloned(id)?;
        if let MatchInfo::Matches { subsequence } = action.match_info {
            let label = action.action.to_string();
            let mut char_iter = label.char_indices().enumerate();
            let highlighted = subsequence
                .indices
                .iter()
                .filter_map(|idx| loop {
                    if let Some(char) = char_iter.next() {
                        let (char_idx, (byte_id, char)) = char;
                        if char_idx == *idx {
                            let start = ensogl_text::Bytes(byte_id as i32);
                            let end = ensogl_text::Bytes((byte_id + char.len_utf8()) as i32);
                            break Some(ensogl_text::Range::new(start, end));
                        }
                    } else {
                        break None;
                    }
                })
                .collect();
            Some(list_view::entry::GlyphHighlightedLabelModel { label, highlighted })
        } else {
            None
        }
    }
}

impl ide_view::searcher::DocumentationProvider for SuggestionsProviderForView {
    fn get(&self) -> Option<String> {
        use controller::searcher::UserAction::*;
        self.intended_function.as_ref().and_then(|function| match self.user_action {
            StartingTypingArgument => function.documentation_html().map(ToOwned::to_owned),
            _ => None,
        })
    }

    fn get_for_entry(&self, id: usize) -> Option<String> {
        use controller::searcher::action::Action;
        match self.actions.get_cloned(id)?.action {
            Action::Suggestion(suggestion) => {
                let doc = suggestion.documentation_html().map(ToOwned::to_owned);
                Some(doc.unwrap_or_else(|| Self::doc_placeholder_for(&suggestion)))
            }
            Action::Example(example) => Some(example.documentation_html.clone()),
            Action::ProjectManagement(_) => None,
        }
    }
}

impl upload::DataProvider for ensogl_drop_manager::File {
    fn next_chunk(&mut self) -> LocalBoxFuture<FallibleResult<Option<Vec<u8>>>> {
        self.read_chunk().map(|f| f.map_err(|e| e.into())).boxed_local()
    }
}



// ========================
// === Project Provider ===
// ========================

#[derive(Clone, CloneRef, Debug, Default)]
struct ProjectsToOpen {
    projects: Rc<Vec<controller::ide::ProjectMetadata>>,
}

impl ProjectsToOpen {
    fn new(projects: Vec<controller::ide::ProjectMetadata>) -> Self {
        Self { projects: Rc::new(projects) }
    }

    fn get_project_id_by_index(&self, index: usize) -> Option<Uuid> {
        self.projects.get(index).map(|md| md.id)
    }
}

impl list_view::entry::ModelProvider<open_dialog::project_list::Entry> for ProjectsToOpen {
    fn entry_count(&self) -> usize {
        self.projects.len()
    }

    fn get(
        &self,
        id: list_view::entry::Id,
    ) -> Option<<open_dialog::project_list::Entry as list_view::Entry>::Model> {
        Some(<[controller::ide::ProjectMetadata]>::get(&self.projects, id)?.name.clone().into())
    }
}
