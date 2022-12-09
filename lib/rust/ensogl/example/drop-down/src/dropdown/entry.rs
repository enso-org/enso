//! A module defining the [`SimpleGridView`] with all helper structures.

use ensogl_core::display::shape::*;
use ensogl_grid_view::prelude::*;

use ensogl_grid_view::entry;
use ensogl_grid_view::entry::EntryFrp;

use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_text as text;



// ===================
// === EntryParams ===
// ===================

/// The parameters of [`Dropdown`]` grid entries.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct EntryParams {
    pub focus_color:         color::Lcha,
    pub font:                ImString,
    pub text_offset:         f32,
    pub text_size:           text::Size,
    pub text_color:          color::Lcha,
    pub selected_text_color: color::Lcha,
    pub corners_radius:      f32,
}

impl Default for EntryParams {
    fn default() -> Self {
        Self {
            focus_color:         color::Lcha::from(color::Rgba(1.0, 1.0, 1.0, 0.2)),
            font:                text::font::DEFAULT_FONT.into(),
            text_offset:         7.0,
            text_size:           text::Size(12.0),
            text_color:          color::Lcha::from(color::Rgba(1.0, 1.0, 1.0, 0.7)),
            selected_text_color: color::Lcha::from(color::Rgba(1.0, 1.0, 1.0, 1.0)),
            corners_radius:      0.0,
        }
    }
}



// ==================
// === EntryModel ===
// ==================

/// The model of [`SimpleGridView`]`s entries.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, Default)]
pub struct EntryModel {
    pub text:     ImString,
    pub selected: Immutable<bool>,
}

impl EntryModel {
    /// Create a new entry model with given text contents.
    pub fn new(text: ImString, selected: bool) -> Self {
        Self { text, selected: Immutable(selected) }
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
    pub label:      text::Text,
}

impl EntryData {
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let display_object = display::object::Instance::new();
        let label = app.new_view::<ensogl_text::Text>();
        label.set_long_text_truncation_mode(true);
        display_object.add_child(&label);
        if let Some(layer) = text_layer {
            label.add_to_scene_layer(layer);
        }
        Self { display_object, label }
    }

    fn update_layout(&self, contour: entry::Contour, text_size: text::Size, text_offset: f32) {
        self.label.set_xy(Vector2(text_offset - contour.size.x / 2.0, text_size.value / 2.0));
    }
}


// === Entry ===

/// A [`SimpleGridView`] entry - a label with background.
#[derive(Clone, CloneRef, Debug)]
pub struct Entry {
    frp:  EntryFrp<Self>,
    data: Rc<EntryData>,
}

impl ensogl_grid_view::Entry for Entry {
    type Model = EntryModel;
    type Params = EntryParams;

    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let data = Rc::new(EntryData::new(app, text_layer));
        let frp = EntryFrp::<Self>::new();
        let input = &frp.private().input;
        let out = &frp.private().output;
        let network = frp.network();

        out.hover_highlight_color.emit(color::Lcha::transparent());

        enso_frp::extend! { network
            size <- input.set_size.on_change();
            font <- input.set_params.map(|p| p.font.clone_ref()).on_change();
            text_offset <- input.set_params.map(|p| p.text_offset).on_change();
            focus_color <- input.set_params.map(|p| p.focus_color).on_change();
            text_color <- input.set_params.map(|p| p.text_color).on_change();
            text_size <- input.set_params.map(|p| p.text_size).on_change();
            corners_radius <- input.set_params.map(|p| p.corners_radius).on_change();
            selected_text_color <- input.set_params.map(|p| p.selected_text_color).on_change();


            contour <- all_with(&size, &corners_radius, |&size, &corners_radius| entry::Contour { size, corners_radius });
            layout <- all(contour, text_size, text_offset);
            eval layout ((&(c, ts, to)) data.update_layout(c, ts, to));
            selected <- input.set_model.map(|m| *m.selected);
            current_text_color <- selected.switch(&text_color, &selected_text_color);
            data.label.set_property_default <+ current_text_color.ref_into_some();
            data.label.set_font <+ font;
            data.label.set_property_default <+ text_size.ref_into_some();

            font_weight <- any(...);
            font_weight <+ selected.on_true().constant(text::Weight::Bold);
            font_weight <+ selected.on_false().constant(text::Weight::Normal);
            data.label.set_property_default <+ font_weight.ref_into_some();

            content <- input.set_model.map(|m| m.text.clone_ref());
            max_width_px <- input.set_size.map(|size| size.x);
            data.label.set_content <+ content;
            data.label.set_view_width <+ max_width_px.some();

            out.contour <+ contour;
            out.highlight_contour <+ contour;
            out.selection_highlight_color <+ focus_color;
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
