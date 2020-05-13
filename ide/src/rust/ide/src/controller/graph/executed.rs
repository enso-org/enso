//! A module with Executed Graph Controller.
//!
//! This controller provides operations on a specific graph with some execution context - these
//! operations usually involves retrieving values on nodes: that's are i.e. operations on
//! visualisations, retrieving types on ports, etc.
use crate::prelude::*;

use crate::model::synchronized::ExecutionContext;

/// Handle providing executed graph controller interface.
#[derive(Clone,CloneRef,Debug)]
pub struct Handle {
    /// A handle to basic graph operations.
    pub graph     : controller::Graph,
    execution_ctx : Rc<ExecutionContext>,
}

impl Handle {
    /// Create handle for given graph and execution context.
    ///
    /// This takes ownership of execution context which will be shared between all copies of this
    /// handle; when all copies will be dropped, the execution context will be dropped as well
    /// (and will then removed from LanguageServer).
    pub fn new(graph:controller::Graph, execution_ctx:ExecutionContext) -> Self {
        let execution_ctx = Rc::new(execution_ctx);
        Handle{graph,execution_ctx}
    }

    //TODO[ao] Here goes the methods requiring ContextId
}
