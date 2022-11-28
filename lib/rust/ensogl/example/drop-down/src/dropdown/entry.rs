use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::display;
use ensogl_grid_view as grid_view;
use ensogl_text as text;

#[derive(Clone, CloneRef, Debug)]
pub struct View {
    frp:  grid_view::entry::EntryFrp<Self>,
    data: Data,
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.data.display_object
    }
}

#[derive(Clone, CloneRef, Debug)]
struct Data {
    label:          text::Text,
    display_object: display::object::Instance,
}

impl Data {
    fn new(
        app: &ensogl_core::application::Application,
        text_layer: Option<&display::scene::Layer>,
    ) -> Self {
        let display_object = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        display_object.add_child(&label);
        // label.set_long_text_truncation_mode(true);
        if let Some(layer) = text_layer {
            label.add_to_scene_layer(layer);
        }

        Data { label, display_object }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Model {
    pub label: ImString,
}

// #[derive(Clone, Debug, Default)]
// pub struct Params {}

impl grid_view::Entry for View {
    type Model = Model;
    type Params = ();

    fn new(
        app: &ensogl_core::application::Application,
        text_layer: Option<&display::scene::Layer>,
    ) -> Self {
        let frp = grid_view::entry::EntryFrp::<Self>::new();
        let data = Data::new(app, text_layer);
        let network = frp.network();
        let input = &frp.private().input;
        let out = &frp.private().output;

        frp::extend! { network
            out.contour <+ input.set_size.map(|size| grid_view::entry::Contour::rectangular(*size));
            data.label.set_content <+ input.set_model.map(|m| m.label.clone_ref());
            data.label.set_view_width <+ input.set_size.map(|size| Some(size.x));
            trace input.set_size;
        };
        Self { frp, data }
    }

    fn frp(&self) -> &grid_view::entry::EntryFrp<Self> {
        &self.frp
    }
}
