//! A module containing the list view entry type which represents an arbitrary icon.

use ensogl_core::prelude::*;

use crate::icon;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::style::Path;
use ensogl_list_view as list_view;



// =================
// === IconEntry ===
// =================

/// List view entry type which represents a single icon. We use list view with icons instead of
/// three separate buttons to simplify the implementation.
#[derive(Debug, Clone, CloneRef)]
pub struct Entry {
    display_object: display::object::Instance,
    logger:         Logger,
    icon:           Rc<RefCell<Option<icon::Any>>>,
    icon_id:        Rc<Cell<Option<icon::Id>>>,
    params:         Params,
}

impl display::Object for Entry {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl list_view::Entry for Entry {
    type Model = icon::Id;
    type Params = Params;

    fn new(app: &Application, _style_prefix: &Path, params: &Self::Params) -> Self {
        let logger = app.logger.sub("NavigatorIcon");
        let display_object = display::object::Instance::new();
        let icon: Rc<RefCell<Option<icon::Any>>> = default();
        let icon_id = default();
        let network = frp::Network::new("searcher_list_panel::navigator::Icon");
        frp::extend! { network
            eval params.strong_color((&c)
                icon.borrow().as_ref().map(|icon| icon.strong_color.set(c.into()))
            );
            eval params.weak_color((&c)
                icon.borrow().as_ref().map(|icon| icon.weak_color.set(c.into()))
            );
        }

        Self { display_object, logger, icon, icon_id, params: params.clone_ref() }
    }

    fn update(&self, model: &Self::Model) {
        if !self.icon_id.get().contains(model) {
            let size = Vector2(icon::SIZE, icon::SIZE);
            let icon = model.create_shape(size);
            icon.strong_color.set(self.params.strong_color.value().into());
            icon.weak_color.set(self.params.weak_color.value().into());
            self.display_object.add_child(&icon);
            *self.icon.borrow_mut() = Some(icon);
            self.icon_id.set(Some(*model));
        }
    }

    fn set_max_width(&self, _max_width_px: f32) {}

    fn set_label_layer(&self, _label_layer: &Layer) {}
}

// === IconParams ===

/// Entry parameters of the icon.
#[derive(Clone, CloneRef, Debug)]
pub struct Params {
    /// Strong (darker, or more contrasting) color parameter.
    pub strong_color: frp::Sampler<color::Rgba>,
    /// Weak (lighter, or less contrasting) color parameter.
    pub weak_color:   frp::Sampler<color::Rgba>,
}

impl Default for Params {
    fn default() -> Self {
        let network = frp::Network::new("searcher_list_panel::navigator::Params::default");
        frp::extend! { network
            default_color <- source::<color::Rgba>().sampler();
        }
        Self { strong_color: default_color.clone_ref(), weak_color: default_color.clone_ref() }
    }
}
