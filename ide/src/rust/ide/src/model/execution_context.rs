//! This module consists of all structures describing Execution Context.

use crate::prelude::*;

use crate::double_representation::definition::DefinitionName;

use enso_protocol::language_server;
use enso_protocol::language_server::VisualisationConfiguration;

use std::collections::HashMap;
use uuid::Uuid;


// ==============
// === Errors ===
// ==============

/// Error then trying to pop stack item on ExecutionContext when there only root call remains.
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Tried to pop an entry point.")]
pub struct PopOnEmptyStack();

/// Error when using an Id that does not correspond to any known visualization.
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Tried to use incorrect visualization Id.")]
pub struct InvalidVisualizationId();



// =================
// === StackItem ===
// =================

/// An identifier of called definition in module.
pub type DefinitionId = crate::double_representation::definition::Id;
/// An identifier of expression.
pub type ExpressionId = ast::Id;

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

/// Unique Id for visualization.
pub type VisualizationId = Uuid;

/// Visualization marker for specific Ast node with preprocessing function.
#[derive(Clone,Debug)]
pub struct Visualization {
    /// Unique identifier of this visualization.
    pub id: VisualizationId,
    /// Node that is to be visualized.
    pub ast_id: ExpressionId,
    /// An enso lambda that will transform the data into expected format, i.e. `a -> a.json`.
    pub expression: String,
}

impl Visualization {
    /// Creates a `VisualisationConfiguration` that is used in communication with language server.
    pub fn config
    (&self, execution_context_id:Uuid, visualisation_module:String) -> VisualisationConfiguration {
        let expression = self.expression.clone();
        VisualisationConfiguration{execution_context_id,visualisation_module,expression}
    }
}

/// An identifier of ExecutionContext.
pub type Id  = language_server::ContextId;



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
    /// A name of definition which is a root call of this context.
    pub entry_point: DefinitionName,
    /// Local call stack.
    stack: RefCell<Vec<LocalCall>>,
    /// Set of active visualizations.
    visualizations: RefCell<HashMap<VisualizationId, Visualization>>,
}

impl ExecutionContext {
    /// Create new execution context
    pub fn new(entry_point:DefinitionName) -> Self {
        let stack          = default();
        let visualizations = default();
        Self {entry_point,stack,visualizations}
    }

    /// Push a new stack item to execution context.
    pub fn push(&self, stack_item:LocalCall) {
        self.stack.borrow_mut().push(stack_item);
    }

    /// Pop the last stack item from this context. It returns error when only root call
    /// remains.
    pub fn pop(&self) -> FallibleResult<()> {
        self.stack.borrow_mut().pop().ok_or_else(PopOnEmptyStack)?;
        Ok(())
    }

    /// Attaches a new visualization for current execution context.
    pub fn attach_visualization(&self, vis: Visualization) {
        self.visualizations.borrow_mut().insert(vis.id,vis);
    }

    /// Detaches visualization from current execution context.
    pub fn detach_visualization(&self, id:&VisualizationId) -> FallibleResult<Visualization> {
        Ok(self.visualizations.borrow_mut().remove(id).ok_or_else(InvalidVisualizationId)?)
    }

    /// Get an iterator over stack items.
    ///
    /// Because this struct implements _internal mutability pattern_, the stack can actually change
    /// during iteration. It should not panic, however might give an unpredictable result.
    pub fn stack_items<'a>(&'a self) -> impl Iterator<Item=LocalCall> + 'a {
        let stack_size = self.stack.borrow().len();
        (0..stack_size).filter_map(move |i| self.stack.borrow().get(i).cloned())
    }
}
