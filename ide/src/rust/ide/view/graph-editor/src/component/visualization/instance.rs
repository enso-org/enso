//! This module defines the `Renderer` trait and related functionality.

use crate::prelude::*;

use crate::data::enso;
use crate::visualization::*;

use enso_frp as frp;
use ensogl::display;
use ensogl::display::DomSymbol;
use ensogl::display::Scene;



// =================
// === Constants ===
// =================

/// An invocable language expression that serialize given input into JSON.
pub const DEFAULT_VISUALIZATION_EXPRESSION:&str = "x -> x.to_default_visualization_data";



// ====================
// === Preprocessor ===
// ====================

// === ContextModule ===

/// Designation of the module to be used as a context for preprocessor evaluation.
#[derive(Clone,CloneRef,Debug,PartialEq,Eq)]
pub enum ContextModule {
    /// Current project's `Main` module.
    ProjectMain,
    /// Specific module of known name.
    Specific(enso::Module)
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
    pub fn new(module_type:impl Into<enso::Module>) -> Self {
        Self::Specific(module_type.into())
    }
}


// === PreprocessorConfiguration ===

/// Information on how the preprocessor should be set up for the visualization.
#[derive(Clone,CloneRef,Debug,PartialEq,Eq)]
pub struct PreprocessorConfiguration {
    /// The code of the preprocessor. Should be a lambda that transforms node value into whatever
    /// that visualizations expect.
    pub code   : enso::Code,
    /// The module that provides context for `code` evaluation.
    pub module : ContextModule,
}

impl PreprocessorConfiguration {
    /// Like `new` but arguments are optional. If `None` is given, default value will be used.
    pub fn from_options
    (code:Option<impl Into<enso::Code>>, module:Option<impl Into<enso::Module>>) -> Self {
        let mut ret = Self::default();
        if let Some(code) = code {
            ret.code = code.into()
        }
        if let Some(module) = module {
            ret.module = ContextModule::Specific(module.into())
        }
        ret
    }

    /// Create a configuration that runs the given code in the context of the given module.
    pub fn new
    (code:impl Into<enso::Code>, module:impl Into<enso::Module>) -> PreprocessorConfiguration {
        PreprocessorConfiguration {
            module : ContextModule::Specific(module.into()),
            code   : code.into(),
        }
    }
}

impl Default for PreprocessorConfiguration {
    fn default() -> Self {
        Self {
            code   : DEFAULT_VISUALIZATION_EXPRESSION.into(),
            module : default(),
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
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct FrpInputs {
    pub set_size   : frp::Source<Vector2>,
    pub send_data  : frp::Source<Data>,
    pub activate   : frp::Source,
    pub deactivate : frp::Source,
    pub set_layer  : frp::Source<Layer>
}

/// Visualization FRP network.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
#[allow(missing_docs)]
pub struct Frp {
    #[shrinkwrap(main_field)]
    pub inputs                 : FrpInputs,

    pub on_preprocessor_change : frp::Sampler<PreprocessorConfiguration>,
    pub on_data_receive_error  : frp::Stream<Option<DataError>>,
    pub is_active              : frp::Stream<bool>,

    /// This event should be emitted when the received data are incorrect, or cause an internal
    /// error.
    pub data_receive_error    : frp::Source<Option<DataError>>,
    /// This event should be emitted to set a new code of the preprocessor. The preprocessor is
    /// a function called on the Engine side before sending data to IDE, allowing us to do some
    /// compression or filtering for the best performance. See also _Lazy Visualization_ section
    /// [here](http://dev.enso.org/docs/ide/product/visualizations.html).
    pub preprocessor_change   : frp::Source<PreprocessorConfiguration>,
}

impl FrpInputs {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            set_size   <- source();
            send_data  <- source();
            activate   <- source();
            deactivate <- source();
            set_layer  <- source();
        };
        Self {set_size,send_data,activate,deactivate,set_layer}
    }
}

impl Frp {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        let inputs = FrpInputs::new(network);
        frp::extend! { network
            def preprocessor_change = source();
            on_preprocessor_change  <- preprocessor_change.sampler();
            def data_receive_error  = source();
            is_active               <- bool(&inputs.deactivate,&inputs.activate);
        };
        preprocessor_change.emit(PreprocessorConfiguration::default());
        let on_data_receive_error  = data_receive_error.clone_ref().into();
        Self {inputs,on_preprocessor_change,on_data_receive_error,is_active,data_receive_error
             ,preprocessor_change}
    }

    /// Extend the FRP network with mechanism of passing all mouse and keyboard event to DOM when
    /// visualization is active.
    ///
    /// Used mainly in visualizations based on DOM elements (e.g. JavaScript visualization).
    pub fn pass_events_to_dom_if_active(&self, scene:&Scene, network:&frp::Network) {
        frp::extend! { network
            let mouse_up       =  scene.mouse.frp.up.clone_ref();
            let mouse_down     =  scene.mouse.frp.down.clone_ref();
            let mouse_wheel    =  scene.mouse.frp.wheel.clone_ref();
            let mouse_position =  scene.mouse.frp.position.clone_ref();
            let keyboard_up    =  scene.keyboard.frp.up.clone_ref();
            let keyboard_down  =  scene.keyboard.frp.down.clone_ref();
            caught_mouse       <- any_(mouse_up,mouse_down,mouse_wheel,mouse_position);
            caught_keyboard    <- any_(keyboard_up,keyboard_down);
            caught_event       <- any(caught_mouse,caught_keyboard);
            should_process     <- caught_event.gate(&self.is_active);
            eval_ should_process (scene.current_js_event.pass_to_dom.emit(()));
        }
    }
}



// ================
// === Instance ===
// ================

/// Abstraction for any visualization instance.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Instance {
    display_object : display::object::Instance,
    frp            : Frp,
    network        : frp::Network,
    root_dom       : Immutable<Option<DomSymbol>>,
}

impl Instance {
    /// Constructor.
    pub fn new
    ( display_object : impl display::Object
    , frp            : impl Into<Frp>
    , network        : impl Into<frp::Network>
    , root_dom       : Option<DomSymbol>
    ) -> Self {
        let display_object = display_object.display_object().clone_ref();
        let frp            = frp.into();
        let network        = network.into();
        let root_dom       = Immutable(root_dom);
        Self {display_object,frp,network,root_dom}
    }

    /// A [`frp::Network`] getter, used to extend the instance's network, or making a bridge
    /// networks.
    pub fn network(&self) -> &frp::Network { &self.network }

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
