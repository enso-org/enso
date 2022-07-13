//! A single entry in [`crate::list_view::ListView`].

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::layer::WeakLayer;
use ensogl_core::display::scene::Layer;
use ensogl_text as text;



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

pub type EntryFrp<E> = Frp<<E as Entry>::Model, <E as Entry>::Params>;


// =============
// === Trait ===
// =============

pub trait Entry: CloneRef + Debug + display::Object + 'static {
    /// The model of this entry. The entry should be a representation of data from the Model.
    /// For example, the entry being just a caption can have [`String`] as its model - the text to
    /// be displayed.
    type Model: Clone + Debug + Default;

    /// A type parametrizing the visual aspects of how the entry will be rendered in an instance of
    /// [`crate::ListView`].
    type Params: Clone + Debug + Default;

    /// An Object constructor.
    fn new(app: &Application) -> Self;

    /// FRP endpoints getter
    fn frp(&self) -> &EntryFrp<Self>;
}
