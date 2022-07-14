//! A module containing the Component Browser [`View`].

// TODO[ao]: This module should be extracted to a separate crate in the future; now it shares
//   the documentation panel with old Node Searcher, that's why it must be in the view crate.

use crate::prelude::*;

use crate::component_browser::component_group::prelude::StyleWatchFrp;
use crate::documentation;

use enso_frp as frp;
use ensogl::application::frp::API;
use ensogl::application::Application;
use ensogl::display;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser as theme;


// ==============
// === Export ===
// ==============

pub use ide_view_component_browser::*;



/// The Model of Component Browser View.
#[allow(missing_docs)]
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
        expression_input_position(Vector2),
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        frp_api: &<Self as API>::Private,
        _app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
    ) {
        use crate::graph_editor::component::node::HEIGHT as NODE_HEIGHT;
        let network = &frp_api.network;
        let input = &frp_api.input;
        let out = &frp_api.output;
        let list_panel = &model.list.output;
        let documentation = &model.documentation;

        let gap = style.get_number(theme::panels_gap);
        let doc_width = style.get_number(theme::documentation::width);
        frp::extend! { network
            init <- source_();

            doc_height <- all_with(&init, &list_panel.size, |(), size| size.y);
            doc_size <- all_with(&doc_width, &doc_height, |w, h| Vector2(*w, *h));
            trace doc_size;
            eval doc_size ((size) documentation.visualization_frp.inputs.set_size.emit(*size));
            size <- all_with4(&init, &list_panel.size, &doc_size, &gap, |(), list_size, doc_size, gap| {
                let width = list_size.x + gap + doc_size.x;
                let height = max(list_size.y, doc_size.y);
                Vector2(width, height)
            });
            list_position_x <-
                all_with(&size, &list_panel.size, |sz, list_sz| list_sz.x / 2.0 - sz.x / 2.0);
            doc_position_x <- all_with(&size, &doc_size, |sz, doc_sz| sz.x / 2.0 - doc_sz.x / 2.0);
            eval list_position_x ((x) model.list.set_position_x(*x));
            eval doc_position_x ((x) model.documentation.set_position_x(*x));

            out.is_visible <+ bool(&input.hide, &input.show);
            out.size <+ size;
            out.expression_input_position <+ all_with4(
                &list_panel.size,
                &list_position_x,
                &size,
                &gap,
                |list_size, list_x, size, gap| {
                    Vector2(*list_x - list_size.x / 6.0, - size.y / 2.0 - gap - NODE_HEIGHT / 2.0)
                }
            );
        }
        init.emit(());
    }
}

/// Component Browser View.
///  
/// The Component Browser is a panel where user searches for types, variables and methods which can
/// be used to construct new nodes. The components are arranged in sections and groups, and
/// displayed in Component List Panel. The Component Browser View contains also Documentation Panel,
/// displaying documentation of currently selected or hovered component.
pub type View = component::ComponentView<Model, Frp>;
