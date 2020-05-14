//! This module defines the `Visualization` struct and related functionality.

use crate::prelude::*;

use crate::frp;
use crate::visualization::*;

use ensogl::display;



// ====================
// === Helper Types ===
// ====================

/// Type alias for a string containing enso code.
#[derive(Clone,CloneRef,Debug)]
pub struct EnsoCode {
    content: Rc<String>
}

/// Type alias for a string representing an enso type.
#[derive(Clone,CloneRef,Debug)]
pub struct EnsoType {
    content: Rc<String>
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
            def _set_data = self.frp.set_data.map(f!((frp,visualization)(data) {
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
            def _on_changed = renderer_frp.on_change.map(f!((frp)(data) {
                frp.change.emit(data)
            }));
           def _on_preprocess_change = renderer_frp.on_preprocess_change.map(f!((frp)(data) {
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
