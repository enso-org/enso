//! This module contains the `Entry` used in the `TextGrid` visualizations well as associated
//! structs.

use super::*;
use ensogl::prelude::*;

use crate::display;
use crate::web;
use crate::Application;

use ensogl_component::grid_view;
use ensogl_component::grid_view::entry::EntryFrp;
use std::fmt::Write;



// =============
// === Model ===
// =============

/// Model that contains the data that is required to populate the data in an `Entry`.
#[derive(Clone, Debug, Default)]
pub struct Model {
    pub text: String,
}



// ==============
// === Params ===
// ==============

/// Parameters that are required to set up an `Entry`.
#[derive(Clone, Debug, Default)]
pub struct Params {
    /// DOM parent of the Entry. The text element in the `Entry` must be a child of the
    /// `parent` to appear correctly.
    pub parent: Option<web::HtmlDivElement>,
}



// =============
// === Entry ===
// =============

/// Entry for use in GridView. Contains a dom element with a text, the Entry frp, and a dummy
/// display object for compatibility with `GridView`. The `dummy_root` is not used for
/// displaying anything, all that is visible is the `text` element, which is updates through
/// the FRP.
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct Entry {
    // Needed to provide a dummy display object for the `display::Object` trait. Not used, as the
    // text element is created as HTML Element and positioned manually in `set_position_and_size`.
    #[display_object]
    dummy_root: display::object::Instance,
    text:       Rc<web::HtmlDivElement>,
    frp:        Rc<EntryFrp<Self>>,
}

impl Entry {
    fn set_model(&self, model: &Model) {
        self.text.set_inner_text(&model.text);
    }

    fn set_params(&self, params: &Params) {
        if let Some(parent) = &params.parent {
            parent.append_or_warn(&self.text);
        }
    }

    fn set_position_and_size(&self, pos: &Vector2, size: &Vector2) {
        let left = pos.x - size.x / 2.0;
        let top = -pos.y - size.y / 2.0;
        let width = size.x as u32;
        let height = size.y as u32;

        let mut style = "position: absolute; white-space: pre; pointer-events: auto;".to_string();
        write!(style, "left: {left}px; top: {top}px;").ok();
        write!(style, "width: {width}px; height: {height}px;").ok();
        // The default line height in browsers is 1.2, which is great for multi-line text and
        // elements whose height is greater than the line height. In this case, however, where the
        // height and the font size are set to the same value, the default setting of 1.2 pushes
        // the text below the allotted space.
        write!(style, "line-height: 1").ok();

        self.text.set_attribute_or_warn("style", style);
    }
}

impl grid_view::Entry for Entry {
    type Model = Model;
    type Params = Params;

    fn new(_app: &Application, _text_layer: Option<&display::scene::Layer>) -> Self {
        let text = web::document.create_div_or_panic();
        let dummy_root = display::object::Instance::new();

        let new_entry = Self { dummy_root, text: Rc::new(text), frp: default() };

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
