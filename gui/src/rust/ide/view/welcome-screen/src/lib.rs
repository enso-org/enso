use ensogl::prelude::*;
use ensogl::{
    system::web,
    application::{self, Application},
    display::{self, DomSymbol, shape::StyleWatchFrp}
};
use enso_frp as frp;

const CONTENT: &str = include_str!("../assets/templates-view.html");

#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Model {
    application: Application,
    logger: Logger,
    dom: DomSymbol,
    display_object: display::object::Instance,
}

impl Model {
    pub fn new(app: &Application) -> Self {
        let application = app.clone_ref();
        let logger = Logger::new("WelcomeScreen");
        let display_object = display::object::Instance::new(&logger);
        let div = web::create_div();
        let dom = DomSymbol::new(&div);
        dom.dom().set_inner_html(&CONTENT);
        display_object.add_child(&dom);
        app.display.scene().dom.layers.front.manage(&dom);
        Self {
            application,
            logger,
            dom,
            display_object,
        }
    }
}

ensogl::define_endpoints! {
    Input {

    }

    Output {

    }
}


#[derive(Clone, CloneRef, Debug)]
pub struct View {
    model: Model,
    styles: StyleWatchFrp,
    frp: Frp,
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl View {
    pub fn new(app: &Application) -> Self {
        let model = Model::new(&app);
        let scene = app.display.scene();
        let styles = StyleWatchFrp::new(&scene.style_sheet);
        let frp = Frp::new();
        Self {
            model,
            styles,
            frp,
        }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl application::command::FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for View {
    fn label() -> &'static str {
        "WelcomeScreen"
    }

    fn new(app: &Application) -> Self {
        Self::new(app)
    }

    fn app(&self) -> &Application {
        &self.model.application
    }
}
