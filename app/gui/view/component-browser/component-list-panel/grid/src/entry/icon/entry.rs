//! A module containing the grid view entry type which represents an arbitrary icon.

use ensogl_core::prelude::*;

use crate::icon;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::style::Path;
use ensogl_grid_view as grid;


// =================
// === IconEntry ===
// =================

/// List view entry type which represents a single icon. We use grid view with icons instead of
/// multiple buttons to simplify the implementation.
#[derive(Debug, Clone, CloneRef)]
pub struct Data {
    display_object: display::object::Instance,
    icon:           Rc<RefCell<Option<icon::Any>>>,
    icon_id:        Rc<Cell<Option<icon::Id>>>,
    params:         Rc<RefCell<Params>>,
}

impl Data {
    pub fn new() -> Self {
        let display_object = display::object::Instance::new();
        let icon = default();
        let icon_id = default();
        let params = default();
        Self { display_object, icon, icon_id, params }
    }

    fn set_icon(&self, icon_id: icon::Id) {
        DEBUG!("Set icon: {icon_id:?}");
        if !self.icon_id.get().contains(&icon_id) {
            let size = Vector2(icon::SIZE, icon::SIZE);
            let icon = icon_id.create_shape(size);
            icon.strong_color.set(self.params.borrow().strong_color.clone().into());
            icon.weak_color.set(self.params.borrow().weak_color.clone().into());
            self.display_object.add_child(&icon);
            *self.icon.borrow_mut() = Some(icon);
            self.icon_id.set(Some(icon_id));
        }
    }

    fn set_size(&self, size: Vector2) {
        if let Some(icon) = self.icon.borrow().deref() {
            // TODO: cache size
            //icon.size.set(*size);
        }
    }

    fn set_strong_color(&self, color: color::Rgba) {
        self.params.borrow_mut().strong_color = color;
        if let Some(icon) = self.icon.borrow().deref() {
            icon.strong_color.set(color.into());
        }
    }

    fn set_weak_color(&self, color: color::Rgba) {
        self.params.borrow_mut().weak_color = color;
        if let Some(icon) = self.icon.borrow().deref() {
            icon.weak_color.set(color.into());
        }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.data.display_object
    }
}

#[derive(Clone, CloneRef, Debug)]
pub struct View {
    frp:  grid::entry::EntryFrp<Self>,
    data: Data,
}

impl grid::entry::Entry for View {
    type Model = icon::Id;
    type Params = Params;

    fn new(app: &Application, _text_layer: Option<&Layer>) -> Self {
        let frp = grid::entry::EntryFrp::<Self>::new();
        let data = Data::new();
        let network = frp.network();
        let input = &frp.private().input;
        let out = &frp.private().output;

        frp::extend! { network
            eval input.set_model((id) data.set_icon(*id));

            style <- input.set_params.on_change();
            strong_color <- style.map(|s| s.strong_color).on_change();
            weak_color <- style.map(|s| s.weak_color).on_change();
            selection_color <- style.map(|s| s.selection_color).on_change();
            hover_color <- style.map(|s| s.hover_color).on_change();
            eval strong_color((&c) data.set_strong_color(c));
            eval weak_color((&c) data.set_weak_color(c));

            eval input.set_size((size) data.set_size(*size));
            out.contour <+ input.set_size.map(|s| grid::entry::Contour::rectangular(*s));
            out.highlight_contour <+ out.contour.all_with(&style, |c,s| grid::entry::Contour { corners_radius: s.selection_corners_radius, ..*c });
            out.hover_highlight_color <+ hover_color;
            out.selection_highlight_color <+ selection_color;
        }

        Self { frp, data }
    }

    fn frp(&self) -> &grid::entry::EntryFrp<Self> {
        &self.frp
    }
}

// === IconParams ===

/// Entry parameters of the icon.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Params {
    /// Strong (darker, or more contrasting) color parameter.
    pub strong_color:             color::Rgba,
    /// Weak (lighter, or less contrasting) color parameter.
    pub weak_color:               color::Rgba,
    pub hover_color:              color::Lcha,
    pub selection_color:          color::Lcha,
    pub selection_size:           f32,
    pub selection_corners_radius: f32,
}
