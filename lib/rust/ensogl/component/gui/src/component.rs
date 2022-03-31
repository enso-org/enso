//! UI Component being and [`application::View`] - the visible GUI element which also registers
//! itself in application and listens for shortcuts.
//!
//! The [`ComponentView`] is essentially a [`Widget`] where both the FRP and the Model implement a
//! trait each, which provide  functionality for constructing / initialising the component as an
//! [`application::View`].

use crate::prelude::*;
use ensogl_core::display::shape::*;

use enso_frp as frp;
use ensogl_core::application;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::gui::Widget;



// =============
// === Model ===
// =============

/// Model that can be used in a [`ComponentView`]. Requires a constructor that takes an application
/// and returns `Self`. The model will be created with this constructor when constructing the
/// [`ComponentView`].
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

/// Frp that can be used in a [`ComponentView`]. The FRP requires an initializer that will be called
/// during the construction of the component. `Default` is usually implemented when using
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

/// Base struct for visual components in EnsoGL which are [`application::View`].
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct ComponentView<Model: 'static, Frp: 'static> {
    widget: Widget<Model, Frp>,
    logger: Logger,
}

impl<M, F> ComponentView<M, F>
where
    M: Model + display::Object + 'static,
    F: Frp<M> + 'static,
{
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let logger = Logger::new(M::label());
        let model = Rc::new(M::new(app, &logger));
        let frp = F::default();
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        F::init(frp.private(), app, &model, &style);
        let display_object = model.display_object().clone_ref();
        let widget = Widget::new(app, frp, model, display_object);
        Self { widget, logger }
    }
}

impl<M: 'static, F: 'static> display::Object for ComponentView<M, F> {
    fn display_object(&self) -> &display::object::Instance {
        self.widget.display_object()
    }
}

impl<M, F: Frp<M>> Deref for ComponentView<M, F> {
    type Target = F::Public;
    fn deref(&self) -> &Self::Target {
        self.widget.frp().public()
    }
}

impl<M: 'static, F: FrpNetworkProvider + 'static> FrpNetworkProvider for ComponentView<M, F> {
    fn network(&self) -> &frp::Network {
        self.widget.frp().network()
    }
}

impl<M, F> application::View for ComponentView<M, F>
where
    M: Model + display::Object + 'static,
    F: Frp<M> + FrpNetworkProvider + 'static,
{
    fn label() -> &'static str {
        M::label()
    }

    fn new(app: &Application) -> Self {
        ComponentView::new(app)
    }

    fn app(&self) -> &Application {
        self.widget.app()
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        F::default_shortcuts()
    }
}
