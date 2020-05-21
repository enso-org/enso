//! This module defines the `Visualization` struct and related functionality.

use crate::prelude::*;

use crate::frp;
use crate::visualization::*;

use ensogl::display::Scene;
use ensogl::display;
use std::error::Error;


// ====================
// === Helper Types ===
// ====================

/// Type alias for a string containing enso code.
#[derive(Clone,CloneRef,Debug)]
pub struct EnsoCode {
    content: Rc<String>
}

/// Type alias for a string representing an enso type.
#[derive(Clone,CloneRef,Debug,PartialEq,Eq,Hash)]
pub struct EnsoType {
    content: Rc<String>
}

impl From<String> for EnsoType {
    fn from(source:String) -> Self {
        EnsoType { content:Rc::new(source) }
    }
}

impl From<&str> for EnsoType {
    fn from(source:&str) -> Self {
        EnsoType { content:Rc::new(source.to_string()) }
    }
}

/// Contains general information about a visualization.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct Signature {
    pub name        : String,
    pub input_types : Vec<EnsoType>,
}



// =========================
// === Visualization FRP ===
// =========================

/// Events that are used by the visualization.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Frp {
    /// Can be sent to set the data of the visualization.
    pub set_data             : frp::Source<Option<Data>>,
    /// Will be emitted if the visualization has new data (e.g., through UI interaction).
    /// Data is provides encoded as EnsoCode.
    pub on_change            : frp::Stream<Option<EnsoCode>>,
    /// Will be emitted if the visualization changes it's preprocessor code.
    pub on_preprocess_change : frp::Stream<Option<EnsoCode>>,
    /// Will be emitted if the visualization has been provided with invalid data.
    pub on_invalid_data      : frp::Stream<()>,

    // Internal sources that feed the public streams.
    change            : frp::Source<Option<EnsoCode>>,
    preprocess_change : frp::Source<Option<EnsoCode>>,
    invalid_data      : frp::Source<()>,

}

impl Frp {
    fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            def change            = source();
            def preprocess_change = source();
            def invalid_data      = source();
            def set_data          = source();

            let on_change            = change.clone_ref().into();
            let on_preprocess_change = preprocess_change.clone_ref().into();
            let on_invalid_data      = invalid_data.clone_ref().into();
        };
        Self { on_change,on_preprocess_change,set_data,on_invalid_data,change
              ,preprocess_change,invalid_data}
    }
}



// ===========================
// === Visualization Model ===
// ===========================

/// Internal data of Visualization.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct State {
    pub renderer     : Rc<dyn DataRenderer>,
    pub preprocessor : Rc<RefCell<Option<EnsoCode>>>,
}

impl display::Object for State {
    fn display_object(&self) -> &display::object::Instance {
        &self.renderer.display_object()
    }
}



// =====================
// === Visualization ===
// =====================

/// A visualization that can be rendered and interacted with. Provides an frp API.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Visualization {
    pub network : frp::Network,
    pub frp     : Frp,
        state   : State
}

impl display::Object for Visualization {
    fn display_object(&self) -> &display::object::Instance {
        &self.state.display_object()
    }
}

impl Visualization {
    /// Create a new `Visualization` with the given `DataRenderer`.
    pub fn new<T: DataRenderer + 'static>(renderer:T) -> Self {
        let preprocessor = default();
        let network      = default();
        let frp          = Frp::new(&network);
        let renderer     = Rc::new(renderer);
        let internal     = State {preprocessor,renderer};
        Visualization{frp, state: internal,network}.init()
    }

    fn init(self) -> Self {
        let network       = &self.network;
        let visualization = &self.state;
        let frp           = &self.frp;
        frp::extend! { network
            def _set_data = self.frp.set_data.map(f!([frp,visualization](data) {
                if let Some(data) = data {
                    if visualization.renderer.receive_data(data.clone_ref()).is_err() {
                        frp.invalid_data.emit(())
                    }
                }
            }));
        }

        let renderer_frp     = self.state.renderer.frp();
        let renderer_network = &renderer_frp.network;
        frp::new_bridge_network! { [network,renderer_network]
            def _on_changed = renderer_frp.on_change.map(f!([frp](data) {
                frp.change.emit(data)
            }));
           def _on_preprocess_change = renderer_frp.on_preprocess_change.map(f!([frp](data) {
                frp.preprocess_change.emit(data.as_ref().map(|code|code.clone_ref()))
            }));
        }
        self
    }

    /// Set the viewport size of the visualization.
    pub fn set_size(&self, size:Vector2<f32>) {
        self.state.renderer.set_size(size)
    }
}



// ===========================
// === Visualization Class ===
// ===========================

/// Indicates that instantiating a `Visualisation` from a `Class` has failed.
#[derive(Debug,Display)]
#[allow(missing_docs)]
pub enum InstantiationError {
    /// Indicates a problem with instantiating a class object.
    InvalidClass         { inner:Box<dyn Error> },
    /// Indicates a problem with instantiating a visualisation from a valid class object.
    InvalidVisualisation { inner:Box<dyn Error> },
}

/// Result of the attempt to instantiate a `Visualization` from a `Class`.
pub type InstantiationResult = Result<Visualization,InstantiationError>;

/// Specifies a trait that allows the instantiation of `Visualizations`.
///
/// The `Class` provides a way to implement structs that allow the instantiation of specific
/// visualizations, while already providing general information that doesn't require an
/// instantiated visualization, for example, the name or input type of the visualization.
///
/// There are two example implementations: The `JsSourceClass`, which is based on a JS snippet to
/// instantiate `JsRenderer`, and the fairly generic `NativeConstructorClass`, that only requires
/// a function that can create a InstantiationResult. The later can be used as a thin wrapper around
/// the constructor methods of native visualizations.
///
/// Example
/// --------
/// ```no_run
/// use graph_editor::component::visualization;
/// use graph_editor::component::visualization::Visualization;
/// use graph_editor::component::visualization::renderer::example::native::BubbleChart;
/// use ensogl::display::Scene;
/// use std::rc::Rc;
///
/// // Create a `visualization::Class` from a JS source code snippet.
/// let js_source_class = visualization::JsSourceClass::from_js_source_raw(r#"
///
///    class BubbleVisualization {
///         static inputTypes = ["[[Float,Float,Float]]"]
///         onDataReceived(root, data) {}
///         setSize(root, size) {}
///     }
///
///     return BubbleVisualization;
///
/// "#.into());
///
/// // Create a `visualization::Class` that instantiates a `BubbleChart`.
/// let native_bubble_vis_class = visualization::NativeConstructorClass::new(
///     visualization::Signature {
///         name        : "Bubble Visualization (native)".to_string(),
///         input_types : vec!["[[Float,Float,Float]]".into()],
///     },
///     |scene:&Scene| Ok(Visualization::new(BubbleChart::new(scene)))
/// );
/// ```
pub trait Class: Debug {
    /// Provides additional information about the `Class`, for example, which `DataType`s can be
    /// rendered by the instantiated visualization.
    fn signature(&self) -> &Signature;
    /// Create new visualization, that is initialised for the given scene. This can fail if the
    /// `visualization::Class` contains invalid data, for example, JS code that fails to execute,
    /// or if the scene is in an invalid state.
    // TODO consider not providing the scene here, but hooking the the shapes/dom elements into the
    // scene externally.
    fn instantiate(&self, scene:&Scene) -> InstantiationResult;
}

/// Wrapper for `Class` objects, so they can be passed through the FRP system.
#[derive(Clone,Debug,Default)]
#[allow(missing_docs)]
pub struct Handle {
    class : Option<Rc<dyn Class>>
}

impl Handle {
    /// Constructor.
    pub fn new<T:Class+'static>(class:T) -> Handle {
        let class = Rc::new(class);
        Handle {class:Some(class)}
    }

    /// Return the inner class.
    pub fn class(&self) -> Option<Rc<dyn Class>> {
        self.class.clone()
    }
}

impl CloneRef for Handle {}



// ================================
// === Native Constructor Class ===
// ================================

/// Type alias for a function that can create a `Visualization`.
pub trait VisualizationConstructor = Fn(&Scene) -> InstantiationResult;

/// Constructor that instantiates visualisations from a given `VisualizationConstructor`. Can be
/// used to wrap the constructor of visualizations defined in Rust.
#[derive(CloneRef,Clone,Derivative)]
#[derivative(Debug)]
#[allow(missing_docs)]
pub struct NativeConstructorClass {
    info        : Rc<Signature>,
    #[derivative(Debug="ignore")]
    constructor : Rc<dyn VisualizationConstructor>,
}

impl NativeConstructorClass {
    /// Create a visualization source from a closure that returns a `Visualization`.
    pub fn new<T>(info:Signature, constructor:T) -> Self
    where T: VisualizationConstructor + 'static {
        let info        = Rc::new(info);
        let constructor = Rc::new(constructor);
        NativeConstructorClass{info,constructor}
    }
}

impl Class for NativeConstructorClass {
    fn signature(&self) -> &Signature {
        &self.info
    }

    fn instantiate(&self, scene:&Scene) -> InstantiationResult {
        self.constructor.call((scene,))
    }
}
