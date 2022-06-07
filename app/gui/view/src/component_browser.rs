use crate::prelude::*;
use enso_frp as frp;
use ensogl::application::frp::API;
use ensogl::application::Application;
use ensogl::display;
use ensogl_gui_component::component;

use crate::component_browser::component_group::prelude::StyleWatchFrp;
use crate::documentation;
pub use ide_view_component_browser::*;

const GAP: f32 = 3.0;
const DOCUMENTATION_WIDTH: f32 = 369.0;
const WIDTH: f32 = list_panel::WIDTH_INNER + GAP + DOCUMENTATION_WIDTH;
const HEIGHT: f32 = list_panel::HEIGHT_INNER;


#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    logger:            Logger,
    display_object:    display::object::Instance,
    pub list:          list_panel::ComponentBrowserPanel,
    pub documentation: documentation::View,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentBrowser"
    }

    fn new(app: &Application, logger: &Logger) -> Self {
        let logger = logger.sub("ComponentBrowser");
        let display_object = display::object::Instance::new(&logger);
        let list = app.new_view::<list_panel::ComponentBrowserPanel>();
        let documentation = documentation::View::new(&app.display.default_scene);
        app.display.default_scene.layers.node_searcher.add_exclusive(&display_object);
        display_object.add_child(&list);
        display_object.add_child(&documentation);
        documentation.visualization_frp.inputs.set_size.emit(Vector2(DOCUMENTATION_WIDTH, HEIGHT));
        list.set_position_x(list_panel::WIDTH_INNER / 2.0 - WIDTH / 2.0);
        documentation.set_position_x(WIDTH / 2.0 - DOCUMENTATION_WIDTH / 2.0);
        Self { logger, display_object, list, documentation }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

ensogl::define_endpoints_2! {
    Input {
        show(),
        hide(),
    }
    Output {
        is_visible(bool),
        size(Vector2),
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        frp_api: &<Self as API>::Private,
        app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
    ) {
        let network = &frp_api.network;
        let input = &frp_api.input;
        let out = &frp_api.output;
        frp::extend! { network
            init <- source_();
            out.is_visible <+ bool(&input.hide, &input.show);
            out.size <+ init.constant(Vector2(WIDTH, HEIGHT));
        }
        init.emit(());
    }
}

pub type View = component::ComponentView<Model, Frp>;
