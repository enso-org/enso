use crate::prelude::*;

use crate::presenter;

use ide_view as view;

#[derive(Debug)]
struct Model {
    controller: controller::Project,
    view:       view::project::View,
    graph:      presenter::Graph,
}


#[derive(Clone, CloneRef, Debug)]
pub struct Project {
    model: Rc<Model>,
}

impl Project {
    pub fn new(
        controller: controller::Project,
        init_result: controller::project::InitializationResult,
        view: view::project::View,
    ) -> Self {
        let graph_controller = init_result.main_graph;
        let graph = presenter::Graph::new(graph_controller, view.graph().clone_ref());
        let model = Model { controller, view, graph };
        Self { model: Rc::new(model) }
    }

    pub async fn initialize(
        controller: controller::Project,
        view: view::project::View,
    ) -> FallibleResult<Self> {
        let init_result = controller.initialize().await?;
        Ok(Self::new(controller, init_result, view))
    }
}
