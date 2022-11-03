use super::*;
use ensogl::prelude::*;

use crate::display;
use crate::display::DomSymbol;
use crate::web;
use crate::Application;

use ensogl_component::grid_view;
use ensogl_component::grid_view::entry::EntryFrp;



/// Model that contains the data that is required to populate the data in an `Entry`.
#[derive(Clone, Debug, Default)]
pub struct Model {
    pub text: String,
}

/// Parameters that are required to set up an `Entry`.
#[derive(Clone, Debug, Default)]
pub struct Params {
    /// DOM parent of the Entry. The text element in the `Entry` must be a child of the
    /// `parent` to appear correctly.
    pub parent:    Option<DomSymbol>,
    /// Name of the font to be used in the `Entry`.
    pub font_name: ImString,
    /// Font size in pixels.
    pub font_size: f32,
}

/// Entry for use in GridView. Contains a dom element with a text, the Entry frp, and a dummy
/// display object for compatibility with `GridView`. The `dummy_root` is not used for
/// displaying anything, all that is visible is the `text` element, which is updates through
/// the FRP.
#[derive(Clone, CloneRef, Debug)]
pub struct Entry {
    text: Rc<DomSymbol>,
    frp:  Rc<EntryFrp<Self>>,
}

impl Entry {
    fn set_model(&self, model: &Model) {
        self.text.set_inner_text(&model.text);
    }

    fn set_params(&self, params: &Params) {
        if let Some(parent) = &params.parent {
            parent.append_or_warn(self.text.dom());
        }
        self.text.set_style_or_warn("font-family", params.font_name.clone());
        self.text.set_style_or_warn("font-size", format!("{}px", params.font_size as u32));
    }

    fn set_position_and_size(&self, pos: &Vector2, size: &Vector2) {
        self.text.set_position_xy(*pos);

        self.text.set_style_or_warn("left", format!("{}px", pos.x - size.x / 2.0));
        self.text.set_style_or_warn("top", format!("{}px", -pos.y - size.y / 2.0));

        self.text.set_style_or_warn("width", format!("{}px", size.x as u32));
        self.text.set_style_or_warn("height", format!("{}px", size.y as u32));
    }
}

impl display::Object for Entry {
    fn display_object(&self) -> &display::object::Instance {
        self.text.display_object()
    }
}

impl grid_view::Entry for Entry {
    type Model = Model;
    type Params = Params;

    fn new(app: &Application, _text_layer: Option<&display::scene::Layer>) -> Self {
        let scene = &app.display.default_scene;
        let text_div = web::document.create_div_or_panic();
        let text = DomSymbol::new(&text_div);
        scene.dom.layers.front.manage(&text);
        text.set_style_or_warn("white-space", "nowrap");
        text.set_style_or_warn("pointer-events", "auto");
        text.set_style_or_warn("white-space", "pre");

        let new_entry = Self { text: Rc::new(text), frp: default() };

        let input = &new_entry.frp.private().input;
        let network = new_entry.frp.network();
        enso_frp::extend! { network
            init <- source_();
            eval input.set_model((model) new_entry.set_model(model));
            eval input.set_params((params) new_entry.set_params(params));

            pos_size <- all(&input.position_set, &input.set_size);
            eval pos_size (((pos, size)) new_entry.set_position_and_size(pos, size));
        }
        init.emit(());
        new_entry
    }

    fn frp(&self) -> &EntryFrp<Self> {
        &self.frp
    }
}
