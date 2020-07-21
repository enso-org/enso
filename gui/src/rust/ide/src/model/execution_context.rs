//! This module consists of all structures describing Execution Context.

pub mod plain;
pub mod synchronized;

use crate::prelude::*;

use crate::model::module::QualifiedName as ModuleQualifiedName;
use crate::notification::Publisher;

use enso_protocol::language_server;
use enso_protocol::language_server::ExpressionValueUpdate;
use enso_protocol::language_server::ExpressionValuesComputed;
use enso_protocol::language_server::MethodPointer;
use enso_protocol::language_server::VisualisationConfiguration;
use flo_stream::Subscriber;
use std::collections::HashMap;
use uuid::Uuid;



// ===============
// === Aliases ===
// ===============

/// An identifier of called definition in module.
pub type DefinitionId = crate::double_representation::definition::Id;

/// An identifier of expression.
pub type ExpressionId = ast::Id;



// =========================
// === ComputedValueInfo ===
// =========================

/// Information about some computed value.
///
/// Contains "meta-data" like type or method pointer, not the computed value representation itself.
#[derive(Clone,Debug)]
pub struct ComputedValueInfo {
    /// The string representing the typename of the computed value, e.g. "Number" or "Unit".
    pub typename:Option<ImString>,
    /// If the expression is a method call (i.e. can be entered), this points to the target method.
    pub method_pointer:Option<Rc<MethodPointer>>,
}

impl From<ExpressionValueUpdate> for ComputedValueInfo {
    fn from(update:ExpressionValueUpdate) -> Self {
        ComputedValueInfo {
            typename       : update.typename.map(ImString::new),
            method_pointer : update.method_call.map(Rc::new),
        }
    }
}

/// Ids of expressions that were computed and received updates in this batch.
pub type ComputedValueExpressions = Vec<ExpressionId>;



// =================================
// === ComputedValueInfoRegistry ===
// =================================

/// Registry that receives the `executionContext/expressionValuesComputed` notifications from the
/// Language Server. Caches the received data. Emits notifications when the data is changed.
#[derive(Clone,Default,Derivative)]
#[derivative(Debug)]
pub struct ComputedValueInfoRegistry {
    map : RefCell<HashMap<ExpressionId,Rc<ComputedValueInfo>>>,
    /// A publisher that emits an update every time a new batch of updates is received from language
    /// server.
    #[derivative(Debug="ignore")]
    updates : Publisher<ComputedValueExpressions>,
}

impl ComputedValueInfoRegistry {
    fn emit(&self, update: ComputedValueExpressions) {
        let future = self.updates.publish(update);
        executor::global::spawn(future);
    }

    /// Clear the stored values.
    ///
    /// When change is made to execution context (like adding or removing the call stack frame), the
    /// cache should be cleaned.
    fn clear(&self)  {
        let removed_keys = self.map.borrow().keys().copied().collect_vec();
        self.map.borrow_mut().clear();
        if !removed_keys.is_empty() {
            self.emit(removed_keys);
        }
    }

    /// Store the information from the given update received from the Language Server.
    pub fn apply_update(&self, values_computed:ExpressionValuesComputed) {
        let updated_expressions = values_computed.updates.iter().map(|update| update.id).collect();
        with(self.map.borrow_mut(), |mut map| {
            for update in values_computed.updates {
                let id   = update.id;
                let info = Rc::new(ComputedValueInfo::from(update));
                map.insert(id,info);
            };
        });
        self.emit(updated_expressions);
    }

    /// Subscribe to notifications about changes in the registry.
    pub fn subscribe(&self) -> Subscriber<ComputedValueExpressions> {
        self.updates.subscribe()
    }

    /// Look up the registry for information about given expression.
    pub fn get(&self, id:&ExpressionId) -> Option<Rc<ComputedValueInfo>> {
        self.map.borrow_mut().get(id).cloned()
    }
}



// ===============================
// === VisualizationUpdateData ===
// ===============================

/// An update data that notification receives from the interpreter. Owns the binary data generated
/// for visualization by the Language Server.
///
/// Binary data can be accessed through `Deref` or `AsRef` implementations.
///
/// The inner storage is private and users should not make any assumptions about it.
#[derive(Clone,Debug,PartialEq)]
pub struct VisualizationUpdateData(Vec<u8>);

impl VisualizationUpdateData {
    /// Wraps given vector with binary data into a visualization update data.
    pub fn new(data:Vec<u8>) -> VisualizationUpdateData {
        VisualizationUpdateData(data)
    }
}

impl AsRef<[u8]> for VisualizationUpdateData {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl Deref for VisualizationUpdateData {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}



// =================
// === StackItem ===
// =================

/// A specific function call occurring within another function's definition body.
///
/// This is a single item in ExecutionContext stack.
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct LocalCall {
    /// An expression being a call to a method.
    pub call       : ExpressionId,
    /// A pointer to the called method.
    pub definition : MethodPointer,
}



// =====================
// === Visualization ===
// =====================

/// Unique Id for visualization.
pub type VisualizationId = Uuid;

/// Description of the visualization setup.
#[derive(Clone,Debug)]
pub struct Visualization {
    /// Unique identifier of this visualization.
    pub id: VisualizationId,
    /// Node that is to be visualized.
    pub ast_id: ExpressionId,
    /// An enso lambda that will transform the data into expected format, e.g. `a -> a.json`.
    pub expression: String,
    /// Visualization module - the module in which context the expression should be evaluated.
    pub visualisation_module:ModuleQualifiedName
}

impl Visualization {
    /// Creates a new visualization description. The visualization will get a randomly assigned
    /// identifier.
    pub fn new
    (ast_id:ExpressionId, expression:impl Into<String>, visualisation_module:ModuleQualifiedName)
    -> Visualization {
        let id         = VisualizationId::new_v4();
        let expression = expression.into();
        Visualization {id,ast_id,expression,visualisation_module}
    }

    /// Creates a `VisualisationConfiguration` that is used in communication with language server.
    pub fn config
    (&self, execution_context_id:Uuid) -> VisualisationConfiguration {
        let expression           = self.expression.clone();
        let visualisation_module = self.visualisation_module.to_string();
        VisualisationConfiguration {execution_context_id,visualisation_module,expression}
    }
}

/// An identifier of ExecutionContext.
pub type Id  = language_server::ContextId;



// =============================
// === AttachedVisualization ===
// =============================

/// The information about active visualization. Includes the channel endpoint allowing sending
/// the visualization update's data to the visualization's attacher (presumably the view).
#[derive(Clone,Debug)]
pub struct AttachedVisualization {
    visualization : Visualization,
    update_sender : futures::channel::mpsc::UnboundedSender<VisualizationUpdateData>,
}



// =============
// === Model ===
// =============

/// Execution Context Model API.
pub trait API : Debug {
    /// Obtain the method pointer to the method of the call stack's top frame.
    fn current_method(&self) -> MethodPointer;

    /// Get the information about the given visualization. Fails, if there's no such visualization
    /// active.
    fn visualization_info(&self, id:VisualizationId) -> FallibleResult<Visualization>;

    /// Get the information about all the active visualizations.
    fn all_visualizations_info(&self) -> Vec<Visualization>;

    /// Returns IDs of all active visualizations.
    fn active_visualizations(&self) -> Vec<VisualizationId>;

    /// Get the registry of computed values.
    fn computed_value_info_registry(&self) -> &ComputedValueInfoRegistry;

    /// Get all items on stack.
    fn stack_items<'a>(&'a self) -> Box<dyn Iterator<Item=LocalCall> + 'a>;

    /// Push a new stack item to execution context.
    fn push(&self, stack_item:LocalCall) -> BoxFuture<FallibleResult<()>>;

    /// Pop the last stack item from this context. It returns error when only root call remains.
    fn pop(&self) -> BoxFuture<FallibleResult<LocalCall>>;

    /// Attach a new visualization for current execution context.
    ///
    /// Returns a stream of visualization update data received from the server.
    fn attach_visualization
    (&self, visualization:Visualization)
    -> BoxFuture<FallibleResult<futures::channel::mpsc::UnboundedReceiver<VisualizationUpdateData>>>;

    /// Detach the visualization from this execution context.
    fn detach_visualization
    (&self, id:VisualizationId) -> BoxFuture<FallibleResult<Visualization>>;

    /// Dispatches the visualization update data (typically received from as LS binary notification)
    /// to the respective's visualization update channel.
    fn dispatch_visualization_update
    (&self, visualization_id:VisualizationId, data:VisualizationUpdateData) -> FallibleResult<()>;

    /// Attempt detaching all the currently active visualizations.
    ///
    /// The requests are made in parallel (not one by one). Any number of them might fail.
    /// Results for each visualization that was attempted to be removed are returned.
    fn detach_all_visualizations(&self) -> BoxFuture<Vec<FallibleResult<Visualization>>> {
        let visualizations = self.active_visualizations();
        let detach_actions = visualizations.into_iter().map(move |v| {
            self.detach_visualization(v)
        });
        futures::future::join_all(detach_actions).boxed_local()
    }
}

/// The general, shared Execution Context Model handle.
pub type ExecutionContext = Rc<dyn API>;
/// Execution Context Model which does not do anything besides storing data.
pub type Plain = plain::ExecutionContext;
/// Execution Context Model which synchronizes all changes with Language Server.
pub type Synchronized = synchronized::ExecutionContext;
