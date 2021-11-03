//! UI component consisting of an FRP and a Model.
//!
//! Enforces correct ownership of components: Model must not own Frp. Both need to be owned via
//! `Rc` by the parent struct, which itself acts as a smart-pointer to the FRP.
//!
//! Requires both the Frp component, and the Model to implement a trait each, which provide
//! functionality for constructing / initialising the components.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application;
use ensogl_core::application::command::CommandApi;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::shape::*;



// =============
// === Model ===
// =============

/// Model that can be used in a Component. Requires a constructor that takes an application and
/// returns `Self`. The model will be created with this constructor when constructing the
/// `Component`.
pub trait Model {
    /// Constructor.
    fn new(app: &Application) -> Self;
}



// ===========
// === FRP ===
// ===========

/// Frp that can be used in a Component. The FRP requires an initializer that will be called during
/// the construction of the component. `Default` + `CommandApi` are usually implemented when using
/// the `ensogl_core::define_endpoints!` macro to create an FRP API.
pub trait Frp<Model>: Default + CommandApi {
    /// Frp initializer.
    fn init(&self, app: &Application, model: &Model, style: &StyleWatchFrp);
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
    pub frp: Rc<Frp>,
    model:   Rc<Model>,
    /// Reference to the application the Component belongs to. Generally required for implementing
    /// `application::View` and initialising the `Mode`l and `Frp` and thus provided by the
    /// `Component`.
    pub app: Application,
}

impl<M: Model, F: Frp<M>> Component<M, F> {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let model = Rc::new(M::new(&app));
        let frp = F::default();
        let style = StyleWatchFrp::new(&app.display.scene().style_sheet);
        frp.init(&app, &model, &style);
        let frp = Rc::new(frp);
        Self { frp, model, app }
    }
}

impl<M: display::Object, F> display::Object for Component<M, F> {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}

impl<M, F: Frp<M>> Deref for Component<M, F> {
    type Target = F;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl<M, F: application::command::FrpNetworkProvider> application::command::FrpNetworkProvider
    for Component<M, F>
{
    fn network(&self) -> &frp::Network {
        self.frp.network()
    }
}
