//! WelcomeScreen View.
//!
//! It is opened when the IDE launches without any project or entry point selected. It
//! displays a list of available projects, template cards and "new project" button.

#![warn(missing_docs)]

mod side_menu;
mod template_cards;

use ensogl::prelude::*;

use crate::side_menu::SideMenu;
use crate::template_cards::TemplateCards;

use enso_frp as frp;
use ensogl::application;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::system::web::NodeInserter;
use ensogl::system::web::StyleSetter;
use std::rc::Rc;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use web_sys::Element;
use web_sys::HtmlDivElement;
use web_sys::MouseEvent;



// =================
// === Constants ===
// =================

mod css_class {
    pub const TEMPLATES_VIEW_ROOT: &str = "enso-internal-templates-view";
    pub const CONTAINER: &str = "enso-internal-container";
    pub const SIDE_MENU: &str = "enso-internal-side-menu";
    pub const CONTENT: &str = "enso-internal-content";
    pub const CARDS: &str = "enso-internal-cards";
    pub const CARD: &str = "enso-internal-card";
    pub const ROW: &str = "enso-internal-row";
    pub const CARD_SPREADSHEETS: &str = "enso-internal-card-spreadsheets";
    pub const CARD_GEO: &str = "enso-internal-card-geo";
    pub const CARD_VISUALIZE: &str = "enso-internal-card-visualize";
}

mod css_id {
    pub const NEW_PROJECT: &str = "enso-internal-projects-list-new-project";
}



// ========================
// === ClickableElement ===
// ========================


// === ClickClosure ===

/// Type alias for "on-click" event handlers on buttons.
type ClickClosure = Closure<dyn FnMut(MouseEvent)>;


// === ClickableElement ===

/// Clickable HTML element. It has a single `click` event source that fires an event on each `click`
/// JS event.
#[derive(Debug, Clone, CloneRef)]
struct ClickableElement {
    pub element: Element,
    pub click:   frp::Source,
    pub network: frp::Network,
}

impl Deref for ClickableElement {
    type Target = Element;
    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl ClickableElement {
    pub fn new(element: Element, logger: &Logger) -> Self {
        frp::new_network! { network
            click <- source_();
        }
        let closure: ClickClosure = Closure::wrap(Box::new(f_!(click.emit(()))));
        let callback = closure.as_ref().unchecked_ref();
        if element.add_event_listener_with_callback("click", callback).is_err() {
            error!(logger, "Could not add event listener for ClickableElement.");
        }
        network.store(&Rc::new(closure));
        Self { element, network, click }
    }
}


// =============
// === Model ===
// =============


// === CSS Styles ===

static STYLESHEET: &str = include_str!("../style.css");


// === Model ===

/// Model of Welcome Screen that generates HTML DOM elements.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    application:    Application,
    logger:         Logger,
    dom:            DomSymbol,
    display_object: display::object::Instance,
    side_menu:      SideMenu,
    template_cards: TemplateCards,
}

impl Model {
    /// Constructor. `frp` is used to set up event handlers on buttons.
    pub fn new(app: &Application) -> Self {
        let application = app.clone_ref();
        let logger = Logger::new("WelcomeScreen");
        let display_object = display::object::Instance::new(&logger);

        let side_menu = SideMenu::new(&logger);
        let template_cards = TemplateCards::new(&logger);
        let dom = Self::create_dom(&logger, &side_menu, &template_cards);
        display_object.add_child(&dom);

        // Use `welcome_screen` layer to lock position when panning.
        app.display.scene().dom.layers.welcome_screen.manage(&dom);

        let style = web::create_element("style");
        style.set_inner_html(STYLESHEET);
        dom.append_or_warn(&style, &logger);

        Self { application, logger, dom, display_object, side_menu, template_cards }
    }

    fn create_dom(
        logger: &Logger,
        side_menu: &SideMenu,
        template_cards: &TemplateCards,
    ) -> DomSymbol {
        let root = web::create_div();
        root.set_class_name(css_class::TEMPLATES_VIEW_ROOT);
        // We explicitly enable pointer events for Welcome Screen elements. Pointer events are
        // disabled for all DOM layers by default. See [`DomLayers`] documentation.
        root.set_style_or_warn("pointer-events", "auto", logger);

        let container = Self::create_content_container();
        container.append_or_warn(&side_menu.model.root_dom, logger);
        container.append_or_warn(&template_cards.model.root_dom, logger);
        root.append_or_warn(&container, logger);

        DomSymbol::new(&root)
    }

    fn create_content_container() -> HtmlDivElement {
        let container = web::create_div();
        container.set_class_name(css_class::CONTAINER);

        container
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Set a displayed list of projects.
        set_projects_list(Vec<String>),
    }
    Output {
        /// Open project by name.
        open_project(String),
        /// Create a new project. Optional argument is a template name.
        create_project(Option<String>),
    }
}



// ============
// === View ===
// ============

/// View of the Welcome Screen.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct View {
    model:   Model,
    pub frp: Frp,
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let model = Model::new(app);

        frp::extend! { network
            // === Update DOM's size so CSS styles work correctly. ===

            let scene_size = app.display.scene().shape().clone_ref();
            eval scene_size ((size) model.dom.set_size(Vector2::from(*size)));
        }
        frp::extend! { network
            // === Setup event handlers for all WelcomeScreen components. ===

            model.side_menu.set_projects_list <+ frp.set_projects_list;

            let open_template = model.template_cards.output.source.open_template.clone_ref();
            frp.output.source.create_project <+ open_template.some();

            let new_project = model.side_menu.output.source.new_project.clone_ref();
            frp.output.source.create_project <+ new_project.constant(None);

            let open_project = model.side_menu.output.source.open_project.clone_ref();
            frp.output.source.open_project <+ open_project;
        }

        Self { model, frp }
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
