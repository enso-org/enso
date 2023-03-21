//! This module defines the `Renderer` trait and related functionality.

use crate::prelude::*;
use crate::visualization::*;

use crate::data::enso;

use enso_frp as frp;
use ensogl::display;
use ensogl::display::DomSymbol;



// =================
// === Constants ===
// =================

/// A module containing the default visualization function.
pub const DEFAULT_VISUALIZATION_MODULE: &str = "Standard.Visualization.Preprocessor";
/// A name of the default visualization function.
pub const DEFAULT_VISUALIZATION_FUNCTION: &str = "default_preprocessor";
/// A list of arguments passed to the default visualization function.
const DEFAULT_VISUALIZATION_ARGUMENTS: Vec<enso::Code> = vec![];


// ====================
// === Preprocessor ===
// ====================

// === ContextModule ===

/// Designation of the module to be used as a context for preprocessor evaluation.
#[derive(Clone, CloneRef, Debug, PartialEq, Eq)]
pub enum ContextModule {
    /// Current project's `Main` module.
    ProjectMain,
    /// Specific module of known name.
    Specific(enso::Module),
}

impl Default for ContextModule {
    fn default() -> Self {
        ContextModule::ProjectMain
    }
}

impl ContextModule {
    /// Create a context from optional string with module's type.
    ///
    /// If there is no explicit module's type provided, the default (project's main) will be used.
    pub fn new(module_type: impl Into<enso::Module>) -> Self {
        Self::Specific(module_type.into())
    }
}


// === PreprocessorConfiguration ===

/// Information on how the preprocessor should be set up for the visualization.
#[derive(Clone, CloneRef, Debug, PartialEq, Eq)]
pub struct PreprocessorConfiguration {
    /// The module containing the `method`.
    pub module:    enso::Module,
    /// The method being invoked.
    pub method:    enso::Method,
    /// A list of arguments to pass to the visualization expression.
    pub arguments: Rc<Vec<enso::Code>>,
}

impl PreprocessorConfiguration {
    /// Like `new` but arguments are optional. If `None` is given, default value will be used.
    pub fn from_options(
        module: Option<impl Into<enso::Module>>,
        method: Option<impl Into<enso::Method>>,
        arguments: Option<Vec<impl Into<enso::Code>>>,
    ) -> Self {
        let mut ret = Self::default();
        if let Some(module) = module {
            ret.module = module.into();
        }
        if let Some(method) = method {
            ret.method = method.into();
        }
        if let Some(arguments) = arguments {
            ret.arguments = Rc::new(arguments.into_iter().map_into().collect());
        }
        ret
    }

    /// Create a configuration that runs the given code in the context of the given module.
    pub fn new(
        module: impl Into<enso::Module>,
        method: impl Into<enso::Method>,
        arguments: Vec<impl Into<enso::Code>>,
    ) -> PreprocessorConfiguration {
        PreprocessorConfiguration {
            module:    module.into(),
            method:    method.into(),
            arguments: Rc::new(arguments.into_iter().map_into().collect()),
        }
    }
}

impl Default for PreprocessorConfiguration {
    fn default() -> Self {
        Self {
            module:    DEFAULT_VISUALIZATION_MODULE.into(),
            method:    DEFAULT_VISUALIZATION_FUNCTION.into(),
            arguments: Rc::new(DEFAULT_VISUALIZATION_ARGUMENTS),
        }
    }
}



// ===========
// === FRP ===
// ===========

// FIXME[ao]: The FRP structures should be generated by `define_endpoints` macro, or similar.
//     However the macro generates structures owning the Network. Here we create "Visualization API"
//     in existing network (the specific visualization should manage its Network lifetime).

/// Inputs of the visualization FRP system. Please note that inputs and outputs are kept in separate
/// structures because the visualization author may want to keep the inputs in a model and allow it
/// to be clone-ref'd into FRP closures. If FRP inputs owned the network, it would cause memory
/// leak.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct FrpInputs {
    pub set_size:   frp::Source<Vector2>,
    pub send_data:  frp::Source<Data>,
    pub activate:   frp::Source,
    pub deactivate: frp::Source,
    pub set_layer:  frp::Source<Layer>,
}

/// Visualization FRP network.
#[derive(Clone, CloneRef, Debug, Deref)]
#[allow(missing_docs)]
pub struct Frp {
    #[deref]
    pub inputs: FrpInputs,

    pub on_preprocessor_change: frp::Sampler<PreprocessorConfiguration>,
    pub on_data_receive_error:  frp::Stream<Option<DataError>>,
    pub is_active:              frp::Stream<bool>,

    /// This event should be emitted when the received data are incorrect, or cause an internal
    /// error.
    pub data_receive_error:  frp::Source<Option<DataError>>,
    /// This event should be emitted to set a new code of the preprocessor. The preprocessor is
    /// a function called on the Engine side before sending data to IDE, allowing us to do some
    /// compression or filtering for the best performance. See also _Lazy Visualization_ section
    /// [here](https://enso.org/docs/developer/ide/product/visualizations.html).
    pub preprocessor_change: frp::Any<PreprocessorConfiguration>,
}

impl FrpInputs {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            set_size   <- source();
            send_data  <- source();
            activate   <- source();
            deactivate <- source();
            set_layer  <- source();
        };
        Self { set_size, send_data, activate, deactivate, set_layer }
    }
}

impl Frp {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        let inputs = FrpInputs::new(network);
        frp::extend! { network
            def preprocessor_change = any_mut();
            on_preprocessor_change  <- preprocessor_change.sampler();
            def data_receive_error  = source();
            is_active               <- bool(&inputs.deactivate, &inputs.activate);
        };
        preprocessor_change.emit(PreprocessorConfiguration::default());
        let on_data_receive_error = data_receive_error.clone_ref().into();
        Self {
            inputs,
            on_preprocessor_change,
            on_data_receive_error,
            is_active,
            data_receive_error,
            preprocessor_change,
        }
    }
}



// ================
// === Instance ===
// ================

/// Abstraction for any visualization instance.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Instance {
    display_object: display::object::Instance,
    pub frp:        Frp,
    network:        frp::Network,
    root_dom:       Immutable<Option<DomSymbol>>,
}

impl Instance {
    /// Constructor.
    pub fn new(
        display_object: impl display::Object,
        frp: impl Into<Frp>,
        network: impl Into<frp::Network>,
        root_dom: Option<DomSymbol>,
    ) -> Self {
        let display_object = display_object.display_object().clone_ref();
        let frp = frp.into();
        let network = network.into();
        let root_dom = Immutable(root_dom);
        Self { display_object, frp, network, root_dom }
    }

    /// A [`frp::Network`] getter, used to extend the instance's network, or making a bridge
    /// networks.
    pub fn network(&self) -> &frp::Network {
        &self.network
    }

    /// Get the root dom of visualization if exists.
    pub fn root_dom(&self) -> &Option<DomSymbol> {
        self.root_dom.deref()
    }
}

impl Deref for Instance {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl display::Object for Instance {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
