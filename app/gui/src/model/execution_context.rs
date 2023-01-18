//! This module consists of all structures describing Execution Context.

use crate::prelude::*;

use double_representation::identifier::Identifier;
use double_representation::name::project;
use double_representation::name::QualifiedName;
use engine_protocol::language_server;
use engine_protocol::language_server::ExpressionUpdate;
use engine_protocol::language_server::ExpressionUpdatePayload;
use engine_protocol::language_server::MethodPointer;
use engine_protocol::language_server::SuggestionId;
use engine_protocol::language_server::VisualisationConfiguration;
use ensogl::data::color;
use flo_stream::Subscriber;
use mockall::automock;
use notification::Publisher;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use uuid::Uuid;


// ==============
// === Export ===
// ==============

pub mod plain;
pub mod synchronized;



// ===============
// === Aliases ===
// ===============

/// An identifier of called definition in module.
pub type DefinitionId = double_representation::definition::Id;

/// An identifier of expression.
pub type ExpressionId = ast::Id;



// =========================
// === ComputedValueInfo ===
// =========================

/// Information about some computed value.
///
/// Contains "meta-data" like type or method pointer, not the computed value representation itself.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct ComputedValueInfo {
    /// The string representing the full qualified typename of the computed value, e.g.
    /// "Standard.Base.Number".
    pub typename:    Option<ImString>,
    pub payload:     ExpressionUpdatePayload,
    /// If the expression is a method call (i.e. can be entered), this points to the target method.
    pub method_call: Option<SuggestionId>,
}

impl From<ExpressionUpdate> for ComputedValueInfo {
    fn from(update: ExpressionUpdate) -> Self {
        ComputedValueInfo {
            typename:    update.typename.map(ImString::new),
            method_call: update.method_pointer,
            payload:     update.payload,
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
#[derive(Clone, Default, Derivative)]
#[derivative(Debug)]
pub struct ComputedValueInfoRegistry {
    map:     RefCell<HashMap<ExpressionId, Rc<ComputedValueInfo>>>,
    /// A publisher that emits an update every time a new batch of updates is received from
    /// language server.
    #[derivative(Debug = "ignore")]
    updates: Publisher<ComputedValueExpressions>,
}

impl ComputedValueInfoRegistry {
    fn emit(&self, update: ComputedValueExpressions) {
        let future = self.updates.publish(update);
        executor::global::spawn(future);
    }

    /// Store the information from the given update received from the Language Server.
    #[profile(Debug)]
    pub fn apply_updates(&self, updates: Vec<ExpressionUpdate>) {
        let updated_expressions = updates.iter().map(|update| update.expression_id).collect();
        for update in updates {
            let id = update.expression_id;
            let info = Rc::new(ComputedValueInfo::from(update));
            self.map.borrow_mut().insert(id, info);
        }
        self.emit(updated_expressions);
    }

    /// Subscribe to notifications about changes in the registry.
    pub fn subscribe(&self) -> Subscriber<ComputedValueExpressions> {
        self.updates.subscribe()
    }

    /// Look up the registry for information about given expression.
    pub fn get(&self, id: &ExpressionId) -> Option<Rc<ComputedValueInfo>> {
        self.map.borrow_mut().get(id).cloned()
    }

    /// Obtain a `Future` with data from this registry. If data is not available yet, the future
    /// will be ready once the data becomes available.
    ///
    /// The given function `F` should attempt retrieving desired information from the computed value
    /// entry. Returning `None` means that desired information is not available yet.
    ///
    /// The `Future` yields `None` only when this registry itself has been dropped.
    pub fn get_from_info<F, T>(
        self: &Rc<Self>,
        id: ExpressionId,
        mut f: F,
    ) -> StaticBoxFuture<Option<T>>
    where
        F: FnMut(Rc<ComputedValueInfo>) -> Option<T> + 'static,
        T: 'static,
    {
        let weak = Rc::downgrade(self);
        if let Some(ret) = self.get(&id).and_then(&mut f) {
            future::ready_boxed(Some(ret))
        } else {
            let info_updates = self.subscribe().filter_map(move |update| {
                let result = update.contains(&id).and_option_from(|| {
                    weak.upgrade().and_then(|this| this.get(&id)).and_then(&mut f)
                });
                futures::future::ready(result)
            });
            let ret = info_updates.into_future().map(|(head_value, _stream_tail)| head_value);
            ret.boxed_local()
        }
    }

    /// Get a future that yields a typename information for given expression as soon as it is
    /// available.
    ///
    /// The `Future` yields `None` only when this registry itself has been dropped.
    pub fn get_type(self: &Rc<Self>, id: ExpressionId) -> StaticBoxFuture<Option<ImString>> {
        self.get_from_info(id, |info| info.typename.clone())
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VisualizationUpdateData(Vec<u8>);

impl VisualizationUpdateData {
    /// Wraps given vector with binary data into a visualization update data.
    pub fn new(data: Vec<u8>) -> VisualizationUpdateData {
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
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct LocalCall {
    /// An expression being a call to a method.
    pub call:       ExpressionId,
    /// A pointer to the called method.
    pub definition: MethodPointer,
}



// ==============================
// === QualifiedMethodPointer ===
// ==============================

/// A method pointer containing the qualified module and type names.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QualifiedMethodPointer {
    /// A module name containing the method.
    pub module:          QualifiedName,
    /// A type on which the method is defined.
    pub defined_on_type: QualifiedName,
    /// A method name.
    pub name:            Identifier,
}

impl QualifiedMethodPointer {
    /// Create a method pointer representing a module method.
    pub fn module_method(module: QualifiedName, name: Identifier) -> QualifiedMethodPointer {
        QualifiedMethodPointer { module: module.clone(), defined_on_type: module, name }
    }
    /// Tries to create a new method pointer from string components.
    pub fn from_qualified_text(
        module: &str,
        defined_on_type: &str,
        name: &str,
    ) -> FallibleResult<QualifiedMethodPointer> {
        let resolved_module = module.try_into()?;
        let resolved_type = defined_on_type.try_into()?;
        let name_identifier = Identifier::from_text(name)?;
        Ok(QualifiedMethodPointer {
            module:          resolved_module,
            defined_on_type: resolved_type,
            name:            name_identifier,
        })
    }
}

impl TryFrom<MethodPointer> for QualifiedMethodPointer {
    type Error = failure::Error;

    fn try_from(method_pointer: MethodPointer) -> Result<Self, Self::Error> {
        Self::try_from(&method_pointer)
    }
}

impl TryFrom<&MethodPointer> for QualifiedMethodPointer {
    type Error = failure::Error;

    fn try_from(method_pointer: &MethodPointer) -> Result<Self, Self::Error> {
        let module = method_pointer.module.as_str().try_into()?;
        let defined_on_type = method_pointer.defined_on_type.as_str().try_into()?;
        let name = Identifier::from_text(method_pointer.name.clone())?;
        Ok(QualifiedMethodPointer { module, defined_on_type, name })
    }
}

impl From<QualifiedMethodPointer> for MethodPointer {
    fn from(qualified_method_pointer: QualifiedMethodPointer) -> Self {
        Self::from(&qualified_method_pointer)
    }
}

impl From<&QualifiedMethodPointer> for MethodPointer {
    fn from(qualified_method_pointer: &QualifiedMethodPointer) -> Self {
        let module = qualified_method_pointer.module.clone().into();
        let defined_on_type = qualified_method_pointer.defined_on_type.clone().into();
        let name = qualified_method_pointer.name.name().to_owned();
        MethodPointer { module, defined_on_type, name }
    }
}



// =====================
// === Visualization ===
// =====================

/// Unique Id for visualization.
pub type VisualizationId = Uuid;

/// Description of the visualization setup.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Visualization {
    /// Unique identifier of this visualization.
    pub id:             VisualizationId,
    /// Expression that is to be visualized.
    pub expression_id:  ExpressionId,
    /// A pointer to the enso method that will transform the data into expected format.
    pub method_pointer: QualifiedMethodPointer,
    /// Enso expressions for positional arguments
    pub arguments:      Vec<String>,
}

impl Visualization {
    /// Creates a new visualization description. The visualization will get a randomly assigned
    /// identifier.
    pub fn new(
        expression_id: ExpressionId,
        method_pointer: QualifiedMethodPointer,
        arguments: Vec<String>,
    ) -> Visualization {
        let id = VisualizationId::new_v4();
        Visualization { id, expression_id, method_pointer, arguments }
    }

    /// Creates a `VisualisationConfiguration` that is used in communication with language server.
    pub fn config(&self, execution_context_id: Uuid) -> VisualisationConfiguration {
        let expression = self.method_pointer.clone().into();
        let positional_arguments_expressions = self.arguments.clone();
        VisualisationConfiguration {
            execution_context_id,
            expression,
            positional_arguments_expressions,
        }
    }
}

/// An identifier of ExecutionContext.
pub type Id = language_server::ContextId;



// =============================
// === AttachedVisualization ===
// =============================

/// The information about active visualization. Includes the channel endpoint allowing sending
/// the visualization update's data to the visualization's attacher (presumably the view).
#[derive(Clone, Debug)]
pub struct AttachedVisualization {
    visualization: Visualization,
    update_sender: futures::channel::mpsc::UnboundedSender<VisualizationUpdateData>,
}



// ======================
// === ComponentGroup ===
// ======================

/// A named group of components which is defined in a library imported into an execution context.
///
/// Components are language elements displayed by the Component Browser. The Component Browser
/// displays them in groups defined in libraries imported into an execution context.
/// To learn more about component groups, see the [Component Browser Design
/// Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq)]
pub struct ComponentGroup {
    /// The fully qualified name of the library project.
    pub project:    project::QualifiedName,
    /// The group name without the library project name prefix. E.g. given the `Standard.Base.Group
    /// 1` group reference, the `name` field contains `Group 1`.
    pub name:       ImString,
    /// An optional color to use when displaying the component group.
    pub color:      Option<color::Rgb>,
    pub components: Vec<QualifiedName>,
}

impl ComponentGroup {
    /// Construct from a [`language_server::LibraryComponentGroup`].
    pub fn from_language_server_protocol_struct(
        group: language_server::LibraryComponentGroup,
    ) -> FallibleResult<Self> {
        let project = group.library.try_into()?;
        let name = group.name.into();
        let color = group.color.as_ref().and_then(|c| color::Rgb::from_css_hex(c));
        let components: FallibleResult<Vec<_>> =
            group.exports.into_iter().map(|e| e.name.try_into()).collect();
        Ok(ComponentGroup { project, name, color, components: components? })
    }
}

impl TryFrom<language_server::LibraryComponentGroup> for ComponentGroup {
    type Error = failure::Error;
    fn try_from(group: language_server::LibraryComponentGroup) -> FallibleResult<Self> {
        Self::from_language_server_protocol_struct(group)
    }
}



// =============
// === Model ===
// =============

/// Execution Context Model API.
#[automock]
pub trait API: Debug {
    /// Future that gets ready when execution context becomes ready (i.e. completed first
    /// evaluation).
    ///
    /// If execution context was already ready, returned future will be ready from the beginning.
    fn when_ready(&self) -> StaticBoxFuture<Option<()>>;

    /// Obtain the method pointer to the method of the call stack's top frame.
    fn current_method(&self) -> MethodPointer;

    /// Get the information about the given visualization. Fails, if there's no such visualization
    /// active.
    fn visualization_info(&self, id: VisualizationId) -> FallibleResult<Visualization>;

    /// Get the information about all the active visualizations.
    fn all_visualizations_info(&self) -> Vec<Visualization>;

    /// Returns IDs of all active visualizations.
    fn active_visualizations(&self) -> Vec<VisualizationId>;

    /// Get the component groups defined in libraries imported into the execution context, or an
    /// empty vector if component groups are not yet loaded. Component groups are loaded after the
    /// execution context becomes ready and a response from the Engine is received.
    fn component_groups(&self) -> Rc<Vec<ComponentGroup>>;

    /// Get the registry of computed values.
    fn computed_value_info_registry(&self) -> &Rc<ComputedValueInfoRegistry>;

    /// Get all items on stack.
    fn stack_items<'a>(&'a self) -> Box<dyn Iterator<Item = LocalCall> + 'a>;

    /// Push a new stack item to execution context.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn push<'a>(&'a self, stack_item: LocalCall) -> BoxFuture<'a, FallibleResult>;

    /// Pop the last stack item from this context. It returns error when only root call remains.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn pop<'a>(&'a self) -> BoxFuture<'a, FallibleResult<LocalCall>>;

    /// Attach a new visualization for current execution context.
    ///
    /// Returns a stream of visualization update data received from the server.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn attach_visualization<'a>(
        &'a self,
        visualization: Visualization,
    ) -> BoxFuture<
        'a,
        FallibleResult<futures::channel::mpsc::UnboundedReceiver<VisualizationUpdateData>>,
    >;


    /// Detach the visualization from this execution context.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn detach_visualization<'a>(
        &'a self,
        id: VisualizationId,
    ) -> BoxFuture<'a, FallibleResult<Visualization>>;

    /// Modify visualization properties. See fields in [`Visualization`] structure. Passing `None`
    /// retains the old value.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn modify_visualization<'a>(
        &'a self,
        id: VisualizationId,
        method_pointer: Option<QualifiedMethodPointer>,
        arguments: Option<Vec<String>>,
    ) -> BoxFuture<'a, FallibleResult>;

    /// Dispatches the visualization update data (typically received from as LS binary notification)
    /// to the respective's visualization update channel.
    fn dispatch_visualization_update(
        &self,
        visualization_id: VisualizationId,
        data: VisualizationUpdateData,
    ) -> FallibleResult;

    /// Attempt detaching all the currently active visualizations.
    ///
    /// The requests are made in parallel (not one by one). Any number of them might fail.
    /// Results for each visualization that was attempted to be removed are returned.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn detach_all_visualizations<'a>(
        &'a self,
    ) -> BoxFuture<'a, Vec<FallibleResult<Visualization>>> {
        let visualizations = self.active_visualizations();
        let detach_actions = visualizations.into_iter().map(move |v| self.detach_visualization(v));
        futures::future::join_all(detach_actions).boxed_local()
    }

    /// Interrupt the program execution.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn interrupt<'a>(&'a self) -> BoxFuture<'a, FallibleResult>;

    /// Restart the program execution.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn restart<'a>(&'a self) -> BoxFuture<'a, FallibleResult>;
}

// Note: Needless lifetimes
// ~~~~~~~~~~~~~~~~~~~~~~~~
// See Note: [Needless lifetimes] is `model/project.rs`.

impl Debug for MockAPI {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Mock Execution Context")
    }
}

/// The general, shared Execution Context Model handle.
pub type ExecutionContext = Rc<dyn API>;
/// Execution Context Model which does not do anything besides storing data.
pub type Plain = plain::ExecutionContext;
/// Execution Context Model which synchronizes all changes with Language Server.
pub type Synchronized = synchronized::ExecutionContext;



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use engine_protocol::language_server::types::test::value_update_with_dataflow_error;
    use engine_protocol::language_server::types::test::value_update_with_dataflow_panic;
    use engine_protocol::language_server::types::test::value_update_with_type;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use super::*;

    #[test]
    fn getting_future_type_from_registry() {
        let mut fixture = TestWithLocalPoolExecutor::set_up();

        let registry = Rc::new(ComputedValueInfoRegistry::default());
        let weak = Rc::downgrade(&registry);
        let id1 = Id::new_v4();
        let id2 = Id::new_v4();
        let mut type_future1 = registry.get_type(id1);
        let mut type_future2 = registry.get_type(id2);
        type_future1.expect_pending();
        type_future2.expect_pending();
        let typename = crate::test::mock::data::TYPE_NAME;
        let update1 = value_update_with_type(id1, typename);
        registry.apply_updates(vec![update1]);
        assert_eq!(fixture.expect_completion(type_future1), Some(typename.into()));
        type_future2.expect_pending();
        // Next attempt should return value immediately, as it is already in the registry.
        assert_eq!(fixture.expect_completion(registry.get_type(id1)), Some(typename.into()));
        // The value for other id should not be obtainable though.
        fixture.expect_pending(registry.get_type(id2));

        drop(registry);
        assert!(weak.upgrade().is_none()); // make sure we had not leaked handles to registry
        assert_eq!(fixture.expect_completion(type_future2), None);
    }

    #[test]
    fn applying_expression_update_in_registry() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        let registry = ComputedValueInfoRegistry::default();
        let mut subscriber = registry.subscribe();
        let expr1 = ExpressionId::new_v4();
        let expr2 = ExpressionId::new_v4();
        let expr3 = ExpressionId::new_v4();

        let typename1 = "Test.Typename1".to_owned();
        let typename2 = "Test.Typename2".to_owned();
        let error_msg = "Test Message".to_owned();

        // Set two values.
        let update1 = value_update_with_type(expr1, &typename1);
        let update2 = value_update_with_type(expr2, &typename2);
        registry.apply_updates(vec![update1, update2]);
        assert_eq!(registry.get(&expr1).unwrap().typename, Some(typename1.clone().into()));
        assert!(matches!(registry.get(&expr1).unwrap().payload, ExpressionUpdatePayload::Value));
        assert_eq!(registry.get(&expr2).unwrap().typename, Some(typename2.into()));
        assert!(matches!(registry.get(&expr2).unwrap().payload, ExpressionUpdatePayload::Value));
        let notification = test.expect_completion(subscriber.next()).unwrap();
        assert_eq!(notification, vec![expr1, expr2]);

        // Set two errors. One of old values is overridden
        let update1 = value_update_with_dataflow_error(expr2);
        let update2 = value_update_with_dataflow_panic(expr3, error_msg);
        registry.apply_updates(vec![update1, update2]);
        assert_eq!(registry.get(&expr1).unwrap().typename, Some(typename1.into()));
        assert!(matches!(registry.get(&expr1).unwrap().payload, ExpressionUpdatePayload::Value));
        assert!(registry.get(&expr2).unwrap().typename.is_none());
        assert!(matches!(
            registry.get(&expr2).unwrap().payload,
            ExpressionUpdatePayload::DataflowError { .. }
        ));
        assert!(registry.get(&expr3).unwrap().typename.is_none());
        assert!(matches!(
            registry.get(&expr3).unwrap().payload,
            ExpressionUpdatePayload::Panic { .. }
        ));
        let notification = test.expect_completion(subscriber.next()).unwrap();
        assert_eq!(notification, vec![expr2, expr3]);
    }
}
