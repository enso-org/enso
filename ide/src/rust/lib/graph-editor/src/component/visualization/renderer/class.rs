//! This module defines the `DataRenderer` trait and related functionality.

use crate::prelude::*;
use crate::visualization::*;

use crate::frp;

use ensogl::display;



// ===========
// === FRP ===
// ===========

/// FRP api of a `DataRenderer`.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct DataRendererFrp {
    pub network   : frp::Network,
    /// This is emitted if the state of the renderer has been changed by UI interaction.
    /// It contains the output data of this visualisation if there is some.
    pub on_change            : frp::Stream<Option<EnsoCode>>,
    /// Will be emitted if the visualization changes it's preprocessor. Transmits the new
    /// preprocessor code.
    pub on_preprocess_change : frp::Stream<Option<EnsoCode>>,
        // Internal sources that feed the public streams.
        change                : frp::Source<Option<EnsoCode>>,
        preprocess_change     : frp::Source<Option<EnsoCode>>,
}

impl Default for DataRendererFrp {
    fn default() -> Self {
        frp::new_network! { renderer_events
            def change            = source();
            def preprocess_change = source();

            let on_change            = change.clone_ref().into();
            let on_preprocess_change = preprocess_change.clone_ref().into();
        };
        let network = renderer_events;
        Self {network,on_change,on_preprocess_change,change,preprocess_change}
    }
}



// ====================
// === DataRenderer ===
// ====================

/// At the core of the visualization system sits a `DataRenderer`. The DataRenderer is in charge of
/// producing a `display::Object` that will be shown in the scene. It will create FRP events to
/// indicate updates to its output data (e.g., through user interaction).
///
/// A DataRenderer can indicate what kind of data it can use to create a visualisation through the
/// `valid_input_types` method. This serves as a hint, it will also reject invalid input in the
/// `set_data` method with a `DataError`. The owner of the `DataRenderer` is in charge of producing
/// UI feedback to indicate a problem with the data.
pub trait DataRenderer: display::Object + Debug {
    /// Indicate which `DataType`s can be rendered by this visualization.
    fn valid_input_types(&self) -> Vec<DataType> {
        // TODO this will need to be implemented by each Renderer once we get to the registry.
        unimplemented!()
    }
    /// Receive the data that should be rendered. If the data is valid, it will return the data as
    /// processed by this `DataRenderer`, if the data is of an invalid data type, it violates other
    /// assumptions of this `DataRenderer`, a `DataError` is returned.
    fn receive_data(&self, data:Data) -> Result<(), DataError>;
    /// Set the size of viewport of the visualization. The visualisation must not render outside of
    /// this viewport.
    fn set_size(&self, size:Vector2<f32>);

    /// Return a ref to the internal FRP network. This replaces a potential callback mechanism.
    ///
    /// Note: the presence of this functions imposes the requirement that a `DataRendererFrp` is
    /// owned by whoever implements this trait.
    fn frp(&self) -> &DataRendererFrp;
}
