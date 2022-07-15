//! A module defining the [`SimpleGridView`] with all helper structures.

use crate::prelude::*;
use ensogl_core::display::shape::*;

use crate::scrollable;
use crate::EntryFrp;

use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_text as text;



// ==================
// === Background ===
// ==================

/// The background of single Entry. The actually displayed rectangle is shrunk by [`PADDING_PX`]
/// from the shape size, to avoid antialiasing glitches.  
pub mod entry_background {
    use super::*;

    /// A padding added to the background rectangle to avoid antialiasing glitches.
    pub const PADDING_PX: f32 = 5.0;

    ensogl_core::define_shape_system! {
        (style:Style, color: Vector4) {
            let shape_width  : Var<Pixels> = "input_size.x".into();
            let shape_height : Var<Pixels> = "input_size.y".into();
            let width = shape_width - 2.0.px() * PADDING_PX;
            let height = shape_height - 2.0.px() * PADDING_PX;
            Rect((width, height)).fill(color).into()
        }
    }
}



// ===================
// === EntryParams ===
// ===================

/// The parameters of [`SimpleGridView`]`s entries.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct EntryParams {
    pub bg_color:    color::Rgba,
    pub bg_margin:   f32,
    pub font:        ImString,
    pub text_offset: f32,
    pub text_size:   text::Size,
    pub text_color:  color::Rgba,
}

impl Default for EntryParams {
    fn default() -> Self {
        Self {
            bg_color:    color::Rgba::transparent(),
            bg_margin:   0.0,
            font:        text::typeface::font::DEFAULT_FONT.into(),
            text_offset: 7.0,
            text_size:   text::Size(14.0),
            text_color:  default(),
        }
    }
}



// =============
// === Entry ===
// =============

// === EntryData ===

/// An internal structure of [`Entry`], which may be passed to FRP network.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct EntryData {
    display_object: display::object::Instance,
    pub label:      text::Area,
    pub background: entry_background::View,
}

impl EntryData {
    fn new(app: &Application, text_layer: &Option<Layer>) -> Self {
        let logger = Logger::new("list_view::entry::Label");
        let display_object = display::object::Instance::new(&logger);
        let label = app.new_view::<ensogl_text::Area>();
        let background = entry_background::View::new(&logger);
        display_object.add_child(&label);
        display_object.add_child(&background);
        if let Some(layer) = text_layer {
            label.add_to_scene_layer(layer);
        }
        Self { display_object, label, background }
    }

    fn update_layout(
        &self,
        entry_size: Vector2,
        bg_margin: f32,
        text_size: text::Size,
        text_offset: f32,
    ) {
        use entry_background::PADDING_PX;
        let bg_size = entry_size - Vector2(bg_margin, bg_margin) * 2.0;
        let bg_size_with_padding = bg_size + Vector2(PADDING_PX, PADDING_PX) * 2.0;
        self.background.size.set(bg_size_with_padding);
        self.label.set_position_xy(Vector2(text_offset - entry_size.x / 2.0, text_size.raw / 2.0));
    }
}


// === Entry ===

/// A [`SimpleGridView`] entry - a label with background.
#[derive(Clone, CloneRef, Debug)]
pub struct Entry {
    frp:  EntryFrp<Self>,
    data: Rc<EntryData>,
}

impl crate::Entry for Entry {
    type Model = ImString;
    type Params = EntryParams;

    fn new(app: &Application, text_layer: &Option<Layer>) -> Self {
        let data = Rc::new(EntryData::new(app, text_layer));
        let frp = EntryFrp::<Self>::new();
        let input = &frp.private().input;
        let network = frp.network();

        enso_frp::extend! { network
            bg_color <- input.set_params.map(|p| p.bg_color).on_change();
            bg_margin <- input.set_params.map(|p| p.bg_margin).on_change();
            font <- input.set_params.map(|p| p.font.clone_ref()).on_change();
            text_offset <- input.set_params.map(|p| p.text_offset).on_change();
            text_color <- input.set_params.map(|p| p.text_color).on_change();
            text_size <- input.set_params.map(|p| p.text_size).on_change();

            layout <- all(input.set_size, bg_margin, text_size, text_offset);
            eval layout ((&(es, m, ts, to)) data.update_layout(es, m, ts, to));

            eval bg_color ((color) data.background.color.set(color.into()));
            data.label.set_default_color <+ text_color.on_change();
            data.label.set_font <+ font.on_change().map(ToString::to_string);
            data.label.set_default_text_size <+ text_size.on_change();

            content <- input.set_model.map(|s| s.to_string());
            max_width_px <- input.set_size.map(|size| size.x);
            data.label.set_content_truncated <+ all(&content, &max_width_px);
        }
        Self { frp, data }
    }

    fn frp(&self) -> &EntryFrp<Self> {
        &self.frp
    }
}

impl display::Object for Entry {
    fn display_object(&self) -> &display::object::Instance {
        &self.data.display_object
    }
}



// ======================
// === SimpleGridView ===
// ======================

/// The Simple version of Grid View, where each entry is just a label with background.
pub type SimpleGridView = crate::GridView<Entry>;

/// The Simple version of Scrollable Grid View, where each entry is just a label with background.
pub type SimpleScrollableGridView = scrollable::GridView<Entry>;
