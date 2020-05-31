//! A module with Executed Graph Controller.
//!
//! This controller provides operations on a specific graph with some execution context - these
//! operations usually involves retrieving values on nodes: that's are i.e. operations on
//! visualisations, retrieving types on ports, etc.
use crate::prelude::*;

use crate::model::execution_context::Visualization;
use crate::model::execution_context::VisualizationId;
use crate::model::execution_context::VisualizationUpdateData;
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
    /// This takes a (shared) ownership of execution context which will be shared between all copies
    /// of this handle. It is held through `Rc` because the registry in the project controller needs
    /// to store a weak handle to the execution context as well (to be able to properly route some
    /// notifications, like visualization updates).
    ///
    /// However, in a typical setup, this controller handle (and its copies) shall be the only
    /// strong references to the execution context and it is expected that it will be dropped after
    /// the last copy of this controller is dropped.
    /// Then the context when being dropped shall remove itself from the Language Server.
    pub fn new(graph:controller::Graph, execution_ctx:Rc<ExecutionContext>) -> Self {
        Handle{graph,execution_ctx}
    }

    /// See `attach_visualization` in `ExecutionContext`.
    pub async fn attach_visualization
    (&self, visualization:Visualization)
    -> FallibleResult<impl Stream<Item=VisualizationUpdateData>> {
        self.execution_ctx.attach_visualization(visualization).await
    }

    /// See `detach_visualization` in `ExecutionContext`.
    pub async fn detach_visualization(&self, id:&VisualizationId) -> FallibleResult<Visualization> {
        self.execution_ctx.detach_visualization(id).await
    }

    // TODO [mwu] Here goes the type/short_rep value access API
}

impl Deref for Handle {
    type Target = controller::Graph;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}
