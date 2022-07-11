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


// =======================
// === Implementations ===
// =======================

// === Label ===

#[derive(Clone, Debug)]
pub struct LabelParams {
    pub font:  ImString,
    pub size:  text::Size,
    pub color: color::Rgba,
    pub layer: Option<WeakLayer>,
}

impl LabelParams {
    pub fn new(font: impl Into<ImString>, size: text::Size, color: color::Rgba) -> Self {
        Self { font: font.into(), size, color, layer: default() }
    }

    pub fn with_layer(mut self, layer: &Layer) -> Self {
        self.layer = Some(layer.downgrade());
        self
    }
}

impl Default for LabelParams {
    fn default() -> Self {
        Self::new(text::typeface::font::DEFAULT_FONT, text::Size(16.0), default())
    }
}

/// The [`Entry`] being a single text field displaying String.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Label {
    frp:            EntryFrp<Self>,
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
        let frp = EntryFrp::<Self>::new();
        let input = &frp.private.input;
        let network = frp.network();
        let current_layer: RefCell<Option<WeakLayer>> = default();
        display_object.add_child(&label);

        frp::extend! { TRACE_ALL network
            color <- input.set_params.map(|params| params.color);
            font <- input.set_params.map(|params| params.font.clone_ref());
            size <- input.set_params.map(|params| params.size);
            layer <- input.set_params.map(|params| params.layer.clone());

            label.set_default_color <+ color.on_change();
            label.set_font <+ font.on_change().map(ToString::to_string);
            label.set_default_text_size <+ size.on_change();
            eval size ((size) label.set_position_y(size.raw / 2.0));
            eval layer ([label](layer) {
                let mut current = current_layer.borrow_mut();
                if let Some(layer) = layer.as_ref().and_then(|l| l.upgrade()) {
                    let current_id = current.as_ref().and_then(|l| l.upgrade()).map(|l| l.id());
                    if !current_id.contains(&layer.id()) {
                        *current = Some(layer.downgrade());
                        label.add_to_scene_layer(&layer)
                    }
                } else {
                    if let Some(current_upg) = current.as_ref().and_then(|l| l.upgrade()) {
                        label.remove_from_scene_layer(&current_upg);
                        *current = None
                    }
                }
            });

            content <- frp.set_model.map(|s| s.to_string());
            max_width_px <- frp.set_size.map(|size| size.x);
            label.set_content_truncated <+ all(&content, &max_width_px);
        }
        Self { frp, display_object, label }
    }

    fn frp(&self) -> &EntryFrp<Self> {
        &self.frp
    }
}

impl display::Object for Label {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
