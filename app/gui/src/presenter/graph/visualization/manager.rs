//! A module containing helpers for attaching, detaching and updating visualizations in controllers.

use crate::prelude::*;

use crate::controller::ExecutedGraph;
use crate::executor::global::spawn;
use crate::model::execution_context::Visualization;
use crate::model::execution_context::VisualizationId;
use crate::model::execution_context::VisualizationUpdateData;
use crate::sync::Synchronized;

use futures::channel::mpsc::UnboundedReceiver;
use futures::future::ready;
use ide_view::graph_editor::component::visualization;
use ide_view::graph_editor::component::visualization::instance::ContextModule;
use ide_view::graph_editor::component::visualization::Metadata;
use ide_view::graph_editor::SharedHashMap;



// ================================
// === Resolving Context Module ===
// ================================

/// Resolve the context module to a fully qualified name.
pub fn resolve_context_module(
    context_module: &ContextModule,
    main_module_name: impl FnOnce() -> model::module::QualifiedName,
) -> FallibleResult<model::module::QualifiedName> {
    use visualization::instance::ContextModule::*;
    match context_module {
        ProjectMain => Ok(main_module_name()),
        Specific(module_name) => model::module::QualifiedName::from_text(module_name),
    }
}



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "No visualization information for expression {}.", _0)]
pub struct NoVisualization(ast::Id);



// ====================
// === Notification ===
// ====================

/// Updates emitted by the Visualization Manager.
#[derive(Debug)]
pub enum Notification {
    /// New update data has been received from Language Server.
    ValueUpdate {
        /// Expression on which the visualization is attached.
        target:           ast::Id,
        /// Identifier of the visualization that received data.
        visualization_id: VisualizationId,
        /// Serialized binary data payload -- result of visualization evaluation.
        data:             VisualizationUpdateData,
    },
    /// An attempt to attach a new visualization has failed.
    FailedToAttach {
        /// Visualization that failed to be attached.
        visualization: Visualization,
        /// Error from the request.
        error:         failure::Error,
    },
    /// An attempt to detach a new visualization has failed.
    FailedToDetach {
        /// Visualization that failed to be detached.
        visualization: Visualization,
        /// Error from the request.
        error:         failure::Error,
    },
    /// An attempt to modify a visualization has failed.
    FailedToModify {
        /// Visualization that failed to be modified.
        desired: Visualization,
        /// Error from the request.
        error:   failure::Error,
    },
}



// ==============
// === Status ===
// ==============

/// Describes the state of the visualization on the Language Server.
#[derive(Clone, Debug, PartialEq)]
pub enum Status {
    /// Not attached and no ongoing background work.
    NotAttached,
    /// Attaching has been requested but result is still unknown.
    BeingAttached(Visualization),
    /// Attaching has been requested but result is still unknown.
    BeingModified {
        /// Current visualization state.
        from: Visualization,
        /// Target visualization state (will be achieved if operation completed successfully).
        to:   Visualization,
    },
    /// Attaching has been requested but result is still unknown.
    BeingDetached(Visualization),
    /// Visualization attached and no ongoing background work.
    Attached(Visualization),
}

impl Status {
    /// What is the expected eventual visualization, assuming that any ongoing request will succeed.
    pub fn target(&self) -> Option<&Visualization> {
        match self {
            Status::NotAttached => None,
            Status::BeingAttached(v) => Some(v),
            Status::BeingModified { to, .. } => Some(to),
            Status::BeingDetached(_) => None,
            Status::Attached(v) => Some(v),
        }
    }

    /// Check if there is an ongoing request to the Language Server for this visualization.
    pub fn has_ongoing_work(&self) -> bool {
        match self {
            Status::NotAttached => false,
            Status::BeingAttached(_) => true,
            Status::BeingModified { .. } => true,
            Status::BeingDetached(_) => true,
            Status::Attached(_) => false,
        }
    }

    /// Get the target visualization id, or current visualization id otherwise.
    ///
    /// Note that this might include id of a visualization that is not yet attached.
    pub fn latest_id(&self) -> Option<VisualizationId> {
        match self {
            Status::NotAttached => None,
            Status::BeingAttached(v) => Some(v.id),
            Status::BeingModified { to, .. } => Some(to.id),
            Status::BeingDetached(v) => Some(v.id),
            Status::Attached(v) => Some(v.id),
        }
    }

    /// LS state of the currently attached visualization.
    pub fn currently_attached(&self) -> Option<&Visualization> {
        match self {
            Status::NotAttached => None,
            Status::BeingAttached(_) => None,
            Status::BeingModified { from, .. } => Some(from),
            Status::BeingDetached(v) => Some(v),
            Status::Attached(v) => Some(v),
        }
    }
}

impl Default for Status {
    fn default() -> Self {
        Status::NotAttached
    }
}



// ===============
// === Desired ===
// ===============

/// Desired visualization described using unresolved view metadata structure.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq)]
pub struct Desired {
    pub visualization_id: VisualizationId,
    pub expression_id:    ast::Id,
    pub metadata:         Metadata,
}



// ===================
// === Description ===
// ===================

/// Information on visualization that are stored by the Manager.
#[derive(Clone, Debug, Default)]
pub struct Description {
    /// The visualization desired by the View. `None` denotes detached visualization.
    pub desired: Option<Desired>,
    /// What we know about Language Server state of the visualization.
    pub status:  Synchronized<Status>,
}

impl Description {
    /// Future that gets resolved when ongoing LS call for this visualization is done.
    ///
    /// The yielded value is a new visualization status, or `None` if the operation has been
    /// aborted.
    pub fn when_done(&self) -> impl Future<Output = Option<Status>> {
        self.status.when_map(|status| (!status.has_ongoing_work()).then(|| status.clone()))
    }

    /// Get the target visualization id, or current visualization id otherwise.
    ///
    /// Note that this might include id of a visualization that is not yet attached.
    pub fn latest_id(&self) -> Option<VisualizationId> {
        self.desired
            .as_ref()
            .map(|desired| desired.visualization_id)
            .or_else(|| self.status.get_cloned().latest_id())
    }
}

/// Handles mapping between node expression id and the attached visualization, synchronizing desired
/// state with the Language Server.
///
/// As this type wraps asynchronous operations, it should be stored using `Rc` pointer.
#[derive(Debug)]
pub struct Manager {
    logger:              Logger,
    visualizations:      SharedHashMap<ast::Id, Description>,
    executed_graph:      ExecutedGraph,
    project:             model::Project,
    notification_sender: futures::channel::mpsc::UnboundedSender<Notification>,
}

impl Manager {
    /// Create a new manager for a given execution context.
    ///
    /// Return a handle to the Manager and the receiver for notifications.
    /// Note that receiver cannot be re-retrieved or changed in the future.
    pub fn new(
        logger: impl AnyLogger,
        executed_graph: ExecutedGraph,
        project: model::Project,
    ) -> (Rc<Self>, UnboundedReceiver<Notification>) {
        let logger = logger.sub("visualization::Manager");
        let (notification_sender, notification_receiver) = futures::channel::mpsc::unbounded();
        let ret = Self {
            logger,
            visualizations: default(),
            executed_graph,
            project,
            notification_sender,
        };
        (Rc::new(ret), notification_receiver)
    }

    /// Borrow mutably a description of a given visualization.
    fn borrow_mut(&self, target: ast::Id) -> FallibleResult<RefMut<Description>> {
        let map = self.visualizations.raw.borrow_mut();
        RefMut::filter_map(map, |map| map.get_mut(&target))
            .map_err(|_| NoVisualization(target).into())
    }


    /// Set a new status for the visualization.
    fn update_status(&self, target: ast::Id, new_status: Status) {
        if let Ok(visualization) = self.borrow_mut(target) {
            visualization.status.replace(new_status);
        } else if Status::NotAttached == new_status {
            // No information about detached visualization.
            // Good, no need to fix anything.
        } else {
            // Something is going on with a visualization we dropped info about. Unexpected.
            // Insert it back, so it can be properly detached (or whatever) later.
            let visualization =
                Description { desired: default(), status: Synchronized::new(new_status) };
            self.visualizations.insert(target, visualization);
        };
    }

    /// Get a copy of a visualization description.
    pub fn get_cloned(&self, target: ast::Id) -> FallibleResult<Description> {
        self.visualizations.get_cloned(&target).ok_or_else(|| NoVisualization(target).into())
    }

    /// Get the visualization state that is desired (i.e. requested from GUI side) for a given node.
    pub fn get_desired_visualization(&self, target: ast::Id) -> FallibleResult<Desired> {
        self.get_cloned(target).and_then(|v| {
            v.desired
                .ok_or_else(|| failure::format_err!("No desired visualization set for {}", target))
        })
    }

    /// Request removing visualization from te expression, if present.
    pub fn remove_visualization(self: &Rc<Self>, target: ast::Id) {
        self.set_visualization(target, None)
    }

    /// Drops the information about visualization on a given node.
    ///
    /// Should be used only if the visualization was detached (or otherwise broken) outside of the
    /// `[Manager]` knowledge. Otherwise, the visualization will be dangling on the LS side.
    pub fn forget_visualization(self: &Rc<Self>, target: ast::Id) -> Option<Description> {
        self.visualizations.remove(&target)
    }

    /// Request setting a given visualization on the node.
    ///
    /// Note that `[Manager]` allows setting at most one visualization per expression. Subsequent
    /// calls will chnge previous visualization to the a new one.
    pub fn request_visualization(self: &Rc<Self>, target: ast::Id, requested: Metadata) {
        self.set_visualization(target, Some(requested))
    }

    /// Set desired state of visualization on a node.
    pub fn set_visualization(self: &Rc<Self>, target: ast::Id, new_desired: Option<Metadata>) {
        let current = self.visualizations.get_cloned(&target);
        if current.is_none() && new_desired.is_none() {
            // Early return: requested to remove visualization that was already removed.
            return;
        };
        let current_id = current.as_ref().and_then(|current| current.latest_id());
        let new_desired = new_desired.map(|new_desired| Desired {
            expression_id:    target,
            visualization_id: current_id.unwrap_or_else(VisualizationId::new_v4),
            metadata:         new_desired,
        });
        self.write_new_desired(target, new_desired)
    }

    fn write_new_desired(self: &Rc<Self>, target: ast::Id, new_desired: Option<Desired>) {
        debug!(self.logger, "Requested to set visualization {target}: {new_desired:?}");
        let mut current = match self.visualizations.get_cloned(&target) {
            None => {
                if new_desired.is_none() {
                    // Already done.
                    return;
                } else {
                    Description::default()
                }
            }
            Some(v) => v,
        };

        if current.desired != new_desired {
            current.desired = new_desired;
            self.visualizations.insert(target, current);
            self.synchronize(target);
        } else {
            debug!(
                self.logger,
                "Visualization for {target} was already in the desired state: \
            {new_desired:?}"
            );
        }
    }

    fn resolve_context_module(
        &self,
        context_module: &ContextModule,
    ) -> FallibleResult<model::module::QualifiedName> {
        resolve_context_module(context_module, || self.project.main_module())
    }

    fn prepare_visualization(&self, desired: Desired) -> FallibleResult<Visualization> {
        let context_module = desired.metadata.preprocessor.module;
        let resolved_module = self.resolve_context_module(&context_module)?;
        Ok(Visualization {
            id:                desired.visualization_id,
            expression_id:     desired.expression_id,
            preprocessor_code: desired.metadata.preprocessor.code.to_string(),
            context_module:    resolved_module,
        })
    }

    /// Remove (set desired state to None) each visualization not attached to any of the `targets`.
    pub fn retain_visualizations(self: &Rc<Self>, targets: &HashSet<ast::Id>) {
        let to_remove = self.visualizations.keys().into_iter().filter(|id| !targets.contains(id));
        for target in to_remove {
            self.set_visualization(target, None);
        }
    }

    /// Schedule an asynchronous task that will try applying local desired state of the
    /// visualization to the language server.
    #[profile(Detail)]
    fn synchronize(self: &Rc<Self>, target: ast::Id) {
        let context = self.executed_graph.when_ready();
        let weak = Rc::downgrade(self);
        let task = async move {
            context.await;
            let description = weak.upgrade()?.visualizations.get_cloned(&target)?;
            let status = description.when_done().await?;
            // We re-get the visualization here, because desired visualization could have been
            // modified while we were awaiting completion of previous request.
            let this = weak.upgrade()?;
            let description = this.visualizations.get_cloned(&target)?;
            let desired_vis_id = description.desired.as_ref().map(|v| v.visualization_id);
            let new_visualization = description.desired.and_then(|desired| {
                this.prepare_visualization(desired.clone()).handle_err(|error| {
                    error!(this.logger, "Failed to prepare visualization {desired:?}: {error}")
                })
            });
            match (status, new_visualization) {
                // Nothing attached and we want to have something.
                (Status::NotAttached, Some(new_visualization)) =>
                    this.attach_visualization(target, new_visualization).await,
                (Status::Attached(so_far), None) | (Status::Attached(so_far), Some(_))
                    if !desired_vis_id.contains(&so_far.id) =>
                    this.detach_visualization(target, so_far).await,
                (Status::Attached(so_far), Some(new_visualization))
                    if so_far != new_visualization && so_far.id == new_visualization.id =>
                    this.modify_visualization(target, so_far, new_visualization).await,
                _ => {}
            };
            Some(())
        };
        spawn(async move {
            task.await;
        });
    }

    #[profile(Detail)]
    async fn attach_visualization(
        self: Rc<Self>,
        target: ast::Id,
        new_visualization: Visualization,
    ) {
        info!(
            self.logger,
            "Will attach visualization {new_visualization.id} to expression {target}"
        );
        let status = Status::BeingAttached(new_visualization.clone());
        self.update_status(target, status);
        let notifier = self.notification_sender.clone();
        let attaching_result = self.executed_graph.attach_visualization(new_visualization.clone());
        match attaching_result.await {
            Ok(update_receiver) => {
                let visualization_id = new_visualization.id;
                let status = Status::Attached(new_visualization);
                self.update_status(target, status);
                spawn(update_receiver.for_each(move |data| {
                    let notification = Notification::ValueUpdate { target, visualization_id, data };
                    let _ = notifier.unbounded_send(notification);
                    ready(())
                }))
            }
            Err(error) => {
                // TODO [mwu]
                //   We should somehow deal with this, but we have really no information, how to.
                //   If this failed because e.g. the visualization was already removed (or another
                //   reason to that effect), we should just do nothing.
                //   However, if it is issue like connectivity problem, then we should retry.
                //   However, even if had better error recognition, we won't always know.
                //   So we should also handle errors like unexpected visualization updates and use
                //   them to drive cleanups on such discrepancies.
                let status = Status::NotAttached;
                self.update_status(target, status);
                let notification =
                    Notification::FailedToAttach { visualization: new_visualization, error };
                let _ = notifier.unbounded_send(notification);
            }
        };
    }

    #[profile(Detail)]
    async fn detach_visualization(self: Rc<Self>, target: ast::Id, so_far: Visualization) {
        info!(self.logger, "Will detach from {target}: {so_far:?}");
        let status = Status::BeingDetached(so_far.clone());
        self.update_status(target, status);
        let detaching_result = self.executed_graph.detach_visualization(so_far.id);
        match detaching_result.await {
            Ok(_) => {
                let status = Status::NotAttached;
                self.update_status(target, status);
                if let Some(vis) = self.visualizations.remove(&so_far.expression_id) {
                    if vis.desired.is_some() {
                        // Restore visualization that was re-requested while being detached.
                        self.visualizations.insert(so_far.expression_id, vis);
                        self.synchronize(so_far.expression_id);
                    }
                }
            }
            Err(error) => {
                let status = Status::Attached(so_far.clone());
                self.update_status(target, status);
                let notification = Notification::FailedToDetach { visualization: so_far, error };
                let _ = self.notification_sender.unbounded_send(notification);
            }
        };
    }

    #[profile(Detail)]
    async fn modify_visualization(
        self: Rc<Self>,
        target: ast::Id,
        so_far: Visualization,
        new_visualization: Visualization,
    ) {
        info!(
            self.logger,
            "Will modify visualization on {target} from {so_far:?} to {new_visualization:?}"
        );
        let status =
            Status::BeingModified { from: so_far.clone(), to: new_visualization.clone() };
        self.update_status(target, status);
        let id = so_far.id;
        let expression = new_visualization.preprocessor_code.clone();
        let module = new_visualization.context_module.clone();
        let modifying_result =
            self.executed_graph.modify_visualization(id, Some(expression), Some(module));
        match modifying_result.await {
            Ok(_) => {
                let status = Status::Attached(new_visualization);
                self.update_status(target, status);
            }
            Err(error) => {
                let status = Status::Attached(so_far);
                self.update_status(target, status);
                let notification =
                    Notification::FailedToModify { desired: new_visualization, error };
                let _ = self.notification_sender.unbounded_send(notification);
            }
        };
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use futures::future::ready;
    use ide_view::graph_editor::component::visualization::instance::ContextModule;
    use ide_view::graph_editor::component::visualization::instance::PreprocessorConfiguration;
    use std::assert_matches::assert_matches;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[derive(Shrinkwrap)]
    #[shrinkwrap(mutable)]
    struct Fixture {
        #[shrinkwrap(main_field)]
        inner:   crate::test::mock::Fixture,
        node_id: ast::Id,
    }

    impl Fixture {
        fn new() -> Self {
            let inner = crate::test::mock::Unified::new().fixture();
            let node_id = inner.graph.nodes().unwrap().first().unwrap().id();
            Self { inner, node_id }
        }

        fn vis_metadata(&self, code: impl Into<String>) -> Metadata {
            Metadata {
                preprocessor: PreprocessorConfiguration {
                    module: ContextModule::Specific(self.inner.module_name().to_string().into()),
                    code:   code.into().into(),
                },
            }
        }
    }

    #[derive(Clone, Debug)]
    enum ExecutionContextRequest {
        Attach(Visualization),
        Detach(VisualizationId),
        Modify {
            id:         VisualizationId,
            expression: Option<String>,
            module:     Option<model::module::QualifiedName>,
        },
    }

    #[derive(Shrinkwrap)]
    #[shrinkwrap(mutable)]
    struct VisOperationsTester {
        #[shrinkwrap(main_field)]
        pub inner:    Fixture,
        pub is_ready: Synchronized<bool>,
        pub manager:  Rc<Manager>,
        #[allow(dead_code)]
        //TODO [mwu]: there should be a check that visualization manager notifies about updates
        //      from the engine
        pub notifier: UnboundedReceiver<Notification>,
        pub requests: StaticBoxStream<ExecutionContextRequest>,
    }

    impl VisOperationsTester {
        fn new(inner: Fixture) -> Self {
            let faux_vis = Visualization {
                id:                default(),
                expression_id:     default(),
                context_module:    inner.project.qualified_module_name(inner.module.path()),
                preprocessor_code: "faux value".into(),
            };
            let is_ready = Synchronized::new(false);
            let mut execution_context = model::execution_context::MockAPI::new();
            let (request_sender, requests_receiver) = futures::channel::mpsc::unbounded();
            let requests = requests_receiver.boxed_local();

            execution_context
                .expect_when_ready()
                .returning_st(f! {[is_ready]() is_ready.when_eq(&true).boxed_local()});

            let sender = request_sender.clone();
            execution_context.expect_attach_visualization().returning_st(move |vis| {
                sender.unbounded_send(ExecutionContextRequest::Attach(vis)).unwrap();
                ready(Ok(futures::channel::mpsc::unbounded().1)).boxed_local()
            });

            let sender = request_sender.clone();
            execution_context.expect_detach_visualization().returning_st(move |vis_id| {
                sender.unbounded_send(ExecutionContextRequest::Detach(vis_id)).unwrap();
                ready(Ok(faux_vis.clone())).boxed_local()
            });

            let sender = request_sender;
            execution_context.expect_modify_visualization().returning_st(
                move |id, expression, module| {
                    let request = ExecutionContextRequest::Modify { id, expression, module };
                    sender.unbounded_send(request).unwrap();
                    ready(Ok(())).boxed_local()
                },
            );

            let execution_context = Rc::new(execution_context);
            let executed_graph = controller::ExecutedGraph::new_internal(
                inner.graph.clone_ref(),
                inner.project.clone_ref(),
                execution_context,
            );
            let logger: Logger = inner.logger.sub("manager");
            let (manager, notifier) =
                Manager::new(logger, executed_graph.clone_ref(), inner.project.clone_ref());
            Self { inner, is_ready, manager, notifier, requests }
        }
    }

    fn matching_metadata(
        manager: &Manager,
        visualization: &Visualization,
        metadata: &Metadata,
    ) -> bool {
        let PreprocessorConfiguration { module, code } = &metadata.preprocessor;
        visualization.preprocessor_code == code.to_string()
            && visualization.context_module == manager.resolve_context_module(module).unwrap()
    }

    #[wasm_bindgen_test]
    fn test_visualization_manager() {
        let fixture = Fixture::new();
        let node_id = fixture.node_id;
        let fixture = VisOperationsTester::new(fixture);
        let desired_vis_1 = fixture.vis_metadata("expr1");
        let desired_vis_2 = fixture.vis_metadata("expr2");
        let VisOperationsTester { mut requests, manager, mut inner, is_ready, .. } = fixture;

        // No requests are sent before execution context is ready.
        manager.request_visualization(node_id, desired_vis_1.clone());
        manager.request_visualization(node_id, desired_vis_2.clone());
        manager.request_visualization(node_id, desired_vis_1.clone());
        manager.request_visualization(node_id, desired_vis_1.clone());
        manager.request_visualization(node_id, desired_vis_2.clone());
        inner.run_until_stalled();
        requests.expect_pending();

        // After signalling readiness, only the most recent visualization is attached.
        is_ready.replace(true);
        inner.run_until_stalled();
        let request = requests.expect_one();
        let attached_id = if let ExecutionContextRequest::Attach(vis) = request {
            vis.id
        } else {
            panic!("Expected request to be `ExecutionContextRequest::Attach`, found {:?}", request)
        };

        // Multiple detach-attach requests are collapsed into a single modify request.
        requests.expect_pending();
        manager.remove_visualization(node_id);
        manager.request_visualization(node_id, desired_vis_2);
        manager.remove_visualization(node_id);
        manager.remove_visualization(node_id);
        manager.request_visualization(node_id, desired_vis_1.clone());
        manager.request_visualization(node_id, desired_vis_1.clone());
        inner.run_until_stalled();
        if let ExecutionContextRequest::Modify { id, expression, module } = requests.expect_one() {
            assert!(expression.contains(&desired_vis_1.preprocessor.code.to_string()));
            assert_eq!(id, attached_id);
            let get_main_module = || inner.inner.project.main_module();
            let expected_module =
                resolve_context_module(&desired_vis_1.preprocessor.module, get_main_module)
                    .unwrap();
            assert_eq!(module, Some(expected_module));
            // assert!(module.is_none());
        }

        // If visualization changes ID, then we need to use detach-attach API.
        // We don't attach it separately, as Manager identifies visualizations by their
        // expression id rather than visualization id.
        let desired_vis_3 = Desired {
            visualization_id: VisualizationId::from_u128(900),
            expression_id:    node_id,
            metadata:         desired_vis_1,
        };
        let visualization_so_far = manager.get_cloned(node_id).unwrap().status.get_cloned();
        manager.write_new_desired(node_id, Some(desired_vis_3.clone()));
        inner.run_until_stalled();

        // let request = requests.expect_next();
        match requests.expect_next() {
            ExecutionContextRequest::Detach(id) =>
                assert_eq!(id, visualization_so_far.latest_id().unwrap()),
            other => panic!("Expected a detach request, got: {:?}", other),
        }
        assert_matches!(requests.expect_next(), ExecutionContextRequest::Attach(vis)
            if matching_metadata(&manager,&vis,&desired_vis_3.metadata));
    }
}
