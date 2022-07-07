//! A single entry in [`crate::list_view::ListView`].

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
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



// =============
// === Trait ===
// =============

pub trait Entry: CloneRef + Debug + display::Object + 'static {
    /// The model of this entry. The entry should be a representation of data from the Model.
    /// For example, the entry being just a caption can have [`String`] as its model - the text to
    /// be displayed.
    type Model: CloneRef + Debug + Default;

    /// A type parametrizing the visual aspects of how the entry will be rendered in an instance of
    /// [`crate::ListView`].
    type Params: CloneRef + Debug + Default;

    /// An Object constructor.
    fn new(app: &Application) -> Self;

    /// FRP endpoints getter
    fn frp(&self) -> &Frp<Self::Model, Self::Params>;
}


// =======================
// === Implementations ===
// =======================

// === Label ===

#[derive(Clone, CloneRef, Debug, Default)]
pub struct LabelParams {
    font:  ImString,
    size:  Immutable<usize>,
    color: Immutable<color::Rgba>,
}

/// The [`Entry`] being a single text field displaying String.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Label {
    frp:            Frp<<Self as Entry>::Model, <Self as Entry>::Params>,
    display_object: display::object::Instance,
    pub label:      text::Area,
}

impl Entry for Label {
    type Model = ImString;
    type Params = LabelParams;

    fn new(app: &Application) -> Self {
        let logger = Logger::new("list_view::entry::Label");
        let display_object = display::object::Instance::new(logger);
        let label = app.new_view::<ensogl_text::Area>();
        let frp = Frp::new();
        let network = frp.network();
        display_object.add_child(&label);

        frp::extend! { network
            color <- frp.set_params.map(|params| params.color);
            font <- frp.set_params.map(|params| params.font.clone_ref());
            size <- frp.set_params.map(|params| *params.size);

            label.set_default_color <+ color.on_change();
            label.set_font <+ font.on_change().map(ToString::to_string);
            label.set_default_text_size <+ size.on_change();
            eval size ((size) label.set_position_y(size/2.0));

            content <- frp.set_model.map(|s| s.to_string());
            max_width_px <- frp.set_size.map(|size| size.x);
            label.set_content_truncated <+ all(&content, &max_width_px);
        }
        Self { frp, display_object, label }
    }

    fn frp(&self) -> &Frp<Self::Model, Self::Params> {
        &self.frp
    }
}

impl display::Object for Label {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
