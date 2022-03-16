//! UI component consisting of an FRP and a Model.
//!
//! Enforces correct ownership of components: Model must not own Frp. Both need to be owned via
//! `Rc` by the parent struct, which itself acts as a smart-pointer to the FRP.
//!
//! Requires both the Frp component, and the Model to implement a trait each, which provide
//! functionality for constructing / initialising the components.

use crate::prelude::*;
use ensogl_core::display::shape::*;

use enso_frp as frp;
use ensogl_core::application;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::display;



// =============
// === Model ===
// =============

/// Model that can be used in a Component. Requires a constructor that takes an application and
/// returns `Self`. The model will be created with this constructor when constructing the
/// `Component`.
pub trait Model {
    /// Identifier of the Model. Used for initializing the component logger and
    /// to provide the label for the `command::View` implementation.
    fn label() -> &'static str;

    /// Constructor.
    fn new(app: &Application, logger: &Logger) -> Self;
}



// ===========
// === FRP ===
// ===========

/// Frp that can be used in a Component. The FRP requires an initializer that will be called during
/// the construction of the component. `Default` is usually implemented when using
/// the `ensogl_core::define_endpoints!` macro to create an FRP API.
pub trait Frp<Model>: Default + API {
    /// Frp initializer. Should set up the logic for processing inputs and generating outputs
    /// through the FRP API.
    fn init(
        frp_api: &<Self as ensogl_core::application::frp::API>::Private,
        app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
    );

    /// Set of default shortcuts to be used in the `CommandApi`. See
    /// `lib/rust/ensogl/core/src/application/command.rs` for more details.
    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        default()
    }
}



// =================
// === Component ===
// =================

/// Base struct for UI components in EnsoGL. Contains the Data/Shape model and the FPR exposing its
/// behaviour.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Component<Model, Frp> {
    /// Public FRP api of the Component.
    frp:     Rc<Frp>,
    model:   Rc<Model>,
    logger:  Logger,
    /// Reference to the application the Component belongs to. Generally required for implementing
    /// `application::View` and initialising the `Model` and `Frp` and thus provided by the
    /// `Component`.
    pub app: Application,
}

impl<M: Model, F: Frp<M>> Component<M, F> {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let logger = Logger::new(M::label());
        let model = Rc::new(M::new(&app, &logger));
        let frp = F::default();
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        F::init(frp.private(), &app, &model, &style);
        let frp = Rc::new(frp);
        Self { frp, model, app, logger }
    }
}

impl<M: display::Object, F> display::Object for Component<M, F> {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}

impl<M, F: Frp<M>> Deref for Component<M, F> {
    type Target = F::Public;
    fn deref(&self) -> &Self::Target {
        self.frp.public()
    }
}

impl<M, F: FrpNetworkProvider> FrpNetworkProvider for Component<M, F> {
    fn network(&self) -> &frp::Network {
        self.frp.network()
    }
}

impl<M: Model, F: Frp<M> + FrpNetworkProvider> application::View for Component<M, F> {
    fn label() -> &'static str {
        M::label()
    }

    fn new(app: &Application) -> Self {
        Component::new(app)
    }

    fn app(&self) -> &Application {
        &self.app
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        F::default_shortcuts()
    }
}
