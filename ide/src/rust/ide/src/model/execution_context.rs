//! This module consists of all structures describing Execution Context.

use crate::prelude::*;

use crate::double_representation::definition::DefinitionName;
use crate::model::module::QualifiedName as ModuleQualifiedName;
use crate::notification::Publisher;

use enso_protocol::language_server;
use enso_protocol::language_server::ExpressionValueUpdate;
use enso_protocol::language_server::ExpressionValuesComputed;
use enso_protocol::language_server::MethodPointer;
use enso_protocol::language_server::VisualisationConfiguration;
use flo_stream::MessagePublisher;
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
    pub typename    : Option<ImString>,
    /// If the expression is a method call (i.e. can be entered), this points to the target method.
    pub method_call : Option<MethodPointer>,
}

impl From<ExpressionValueUpdate> for ComputedValueInfo {
    fn from(update:ExpressionValueUpdate) -> Self {
        ComputedValueInfo {
            typename    : update.typename.map(ImString::new),
            method_call : update.method_call,
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
    updates : RefCell<Publisher<ComputedValueExpressions>>,
}

impl ComputedValueInfoRegistry {
    fn emit(&self, update: ComputedValueExpressions) {
        let future = self.updates.borrow_mut().0.publish(update);
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
        self.updates.borrow_mut().subscribe()
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



// ==============
// === Errors ===
// ==============

/// Error then trying to pop stack item on ExecutionContext when there only root call remains.
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Tried to pop an entry point.")]
pub struct PopOnEmptyStack();

/// Error when using an Id that does not correspond to any known visualization.
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Tried to use incorrect visualization Id: {}.",_0)]
pub struct InvalidVisualizationId(VisualizationId);



// =================
// === StackItem ===
// =================

/// A specific function call occurring within another function's definition body.
///
/// This is a single item in ExecutionContext stack.
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct LocalCall {
    /// An expression being a call.
    pub call       : ExpressionId,
    /// A definition of function called in `call` expression.
    pub definition : DefinitionId,
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

/// Execution Context Model.
///
/// The execution context consists of the root call (which is a direct call of some function
/// definition), stack of function calls (see `StackItem` definition and docs) and a list of
/// active visualizations.
///
/// It implements internal mutability pattern, so the state may be shared between different
/// controllers.
#[derive(Debug)]
pub struct ExecutionContext {
    logger:Logger,
    /// A name of definition which is a root call of this context.
    pub entry_point:DefinitionName,
    /// Local call stack.
    stack:RefCell<Vec<LocalCall>>,
    /// Set of active visualizations.
    visualizations: RefCell<HashMap<VisualizationId,AttachedVisualization>>,
    /// Storage for information about computed values (like their types).
    pub computed_value_info_registry: ComputedValueInfoRegistry,
}

impl ExecutionContext {
    /// Create new execution context
    pub fn new(logger:impl Into<Logger>, entry_point:DefinitionName) -> Self {
        let logger                       = logger.into();
        let stack                        = default();
        let visualizations               = default();
        let computed_value_info_registry = default();
        Self {logger,entry_point,stack,visualizations, computed_value_info_registry }
    }

    /// Push a new stack item to execution context.
    pub fn push(&self, stack_item:LocalCall) {
        self.stack.borrow_mut().push(stack_item);
        self.computed_value_info_registry.clear();
    }

    /// Pop the last stack item from this context. It returns error when only root call
    /// remains.
    pub fn pop(&self) -> FallibleResult<()> {
        self.stack.borrow_mut().pop().ok_or_else(PopOnEmptyStack)?;
        self.computed_value_info_registry.clear();
        Ok(())
    }

    /// Attaches a new visualization for current execution context.
    ///
    /// Returns a stream of visualization update data received from the server.
    pub fn attach_visualization
    (&self, visualization:Visualization) -> impl Stream<Item=VisualizationUpdateData> {
        let id                       = visualization.id;
        let (update_sender,receiver) = futures::channel::mpsc::unbounded();
        let visualization            = AttachedVisualization {visualization,update_sender};
        info!(self.logger,"Inserting to the registry: {id}.");
        self.visualizations.borrow_mut().insert(id,visualization);
        receiver
    }

    /// Detaches visualization from current execution context.
    pub fn detach_visualization(&self, id:&VisualizationId) -> FallibleResult<Visualization> {
        let err = || InvalidVisualizationId(*id);
        info!(self.logger,"Removing from the registry: {id}.");
        Ok(self.visualizations.borrow_mut().remove(id).ok_or_else(err)?.visualization)
    }

    /// Get an iterator over stack items.
    ///
    /// Because this struct implements _internal mutability pattern_, the stack can actually change
    /// during iteration. It should not panic, however might give an unpredictable result.
    pub fn stack_items<'a>(&'a self) -> impl Iterator<Item=LocalCall> + 'a {
        let stack_size = self.stack.borrow().len();
        (0..stack_size).filter_map(move |i| self.stack.borrow().get(i).cloned())
    }

    /// Dispatches the visualization update data (typically received from as LS binary notification)
    /// to the respective's visualization update channel.
    pub fn dispatch_visualization_update
    (&self, visualization_id:VisualizationId, data:VisualizationUpdateData) -> FallibleResult<()> {
        if let Some(visualization) = self.visualizations.borrow_mut().get(&visualization_id) {
            // TODO [mwu] Should we consider detaching the visualization if the view has dropped the
            //   channel's receiver? Or we need to provide a way to re-establish the channel.
            let _ = visualization.update_sender.unbounded_send(data);
            debug!(self.logger,"Sending update data to the visualization {visualization_id}.");
            Ok(())
        } else {
            error!(self.logger,"Failed to dispatch update to visualization {visualization_id}. \
            Failed to found such visualization.");
            Err(InvalidVisualizationId(visualization_id).into())
        }
    }

    /// Handles the update about expressions being computed.
    pub fn handle_expression_values_computed
    (&self, notification:ExpressionValuesComputed) -> FallibleResult<()> {
        self.computed_value_info_registry.apply_update(notification);
        Ok(())
    }
}
