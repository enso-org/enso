//! A module with an [`Entry`] abstraction for [`crate::GridView`]. `GridView` can be parametrized
//! by any entry with the specified API.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! { <Model: (frp::node::Data), Params: (frp::node::Data)>
    Input {
        set_model(Model),
        set_size(Vector2),
        set_params(Params),
    }
    Output {}
}

/// FRP Api of a specific Entry.
pub type EntryFrp<E> = Frp<<E as Entry>::Model, <E as Entry>::Params>;



// =============
// === Trait ===
// =============

/// The abstraction of Entry for [`crate::GridView`].
///
/// The entry may be any [`display::Object`] which can provide the [`EntryFRP`] API.
pub trait Entry: CloneRef + Debug + display::Object + 'static {
    /// The model of this entry. The entry should be a representation of data from the Model.
    /// For example, the entry being just a caption can have [`String`] as its model - the text to
    /// be displayed.
    type Model: Clone + Debug + Default;

    /// A type parametrizing the various aspects of the entry, independed of the Model (for example
    /// the text color). The parameters are set in [`crate::GridView`] and shared between all
    /// entries.
    type Params: Clone + Debug + Default;

    /// An Entry constructor.
    fn new(app: &Application, text_layer: &Option<Layer>) -> Self;

    /// FRP endpoints getter.
    fn frp(&self) -> &EntryFrp<Self>;
}
