use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::style;
use ensogl_core::display::Scene;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::Label;


pub type Id = list_view::entry::Id;

#[derive(Clone, Debug, Default)]
pub struct Model {
    pub label: String,
}

impl From<String> for Model {
    fn from(label: String) -> Self {
        Model { label }
    }
}

impl From<&str> for Model {
    fn from(label: &str) -> Self {
        label.to_owned().into()
    }
}

#[derive(Clone, CloneRef, Debug)]
pub struct View {
    logger:         Logger,
    display_object: display::object::Instance,
    // max_width_px:   frp::Source<f32>,
    label:          Label,
}

impl list_view::Entry for View {
    type Model = Model;

    fn new(app: &Application, style_prefix: &style::Path) -> Self {
        let logger = Logger::new("component-group::Entry");
        let display_object = display::object::Instance::new(&logger);
        let label = Label::new(app, style_prefix);
        display_object.add_child(&label);

        Self { logger, display_object, label }
    }

    fn update(&self, model: &Self::Model) {
        self.label.update(&model.label);
    }

    fn set_max_width(&self, max_width_px: f32) {
        self.label.set_max_width(max_width_px);
    }

    fn set_label_layer(&self, label_layer: &Layer) {
        self.label.set_label_layer(label_layer)
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        &self.display_object
    }
}
