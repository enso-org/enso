//! A module defining the [`SimpleGridView`] with all helper structures.

use crate::prelude::*;
use ensogl_core::display::shape::*;

use crate::entry;
use crate::scrollable;
use crate::selectable;
use crate::EntryFrp;

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

/// The parameters of [`SimpleGridView`]`s entries.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct EntryParams {
    pub bg_color:            color::Rgba,
    pub bg_margin:           f32,
    pub hover_color:         color::Rgba,
    pub selection_color:     color::Rgba,
    pub font:                ImString,
    pub text_offset:         f32,
    pub text_size:           text::Size,
    pub text_color:          color::Rgba,
    pub disabled_text_color: color::Rgba,
}

impl Default for EntryParams {
    fn default() -> Self {
        Self {
            bg_color:            color::Rgba::transparent(),
            bg_margin:           0.0,
            hover_color:         color::Rgba(0.9, 0.9, 0.9, 1.0),
            selection_color:     color::Rgba(0.8, 0.8, 0.8, 1.0),
            font:                text::font::DEFAULT_FONT.into(),
            text_offset:         7.0,
            text_size:           text::Size(14.0),
            text_color:          color::Rgba(0.0, 0.0, 0.0, 1.0),
            disabled_text_color: color::Rgba(0.7, 0.7, 0.7, 1.0),
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
    pub text:           ImString,
    pub disabled:       Immutable<bool>,
    pub override_width: Immutable<Option<f32>>,
}

impl EntryModel {
    fn new(text: impl Into<ImString>) -> Self {
        Self { text: text.into(), ..default() }
    }
}

impl<T: Into<ImString>> From<T> for EntryModel {
    fn from(text: T) -> Self {
        Self::new(text)
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
    pub background: entry::shape::View,
}

impl EntryData {
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let display_object = display::object::Instance::new();
        let label = app.new_view::<ensogl_text::Text>();
        label.set_long_text_truncation_mode(true);
        let background = entry::shape::View::new();
        display_object.add_child(&label);
        display_object.add_child(&background);
        if let Some(layer) = text_layer {
            label.add_to_scene_layer(layer);
        }
        Self { display_object, label, background }
    }

    fn update_layout(&self, contour: entry::Contour, text_size: text::Size, text_offset: f32) {
        self.background.set_contour(contour);
        let size = contour.size;
        self.label.set_position_xy(Vector2(text_offset - size.x / 2.0, text_size.value / 2.0));
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
    type Model = EntryModel;
    type Params = EntryParams;

    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let data = Rc::new(EntryData::new(app, text_layer));
        let frp = EntryFrp::<Self>::new();
        let input = &frp.private().input;
        let out = &frp.private().output;
        let network = frp.network();

        enso_frp::extend! { network
            size <- input.set_size.on_change();
            bg_color <- input.set_params.map(|p| p.bg_color).on_change();
            bg_margin <- input.set_params.map(|p| p.bg_margin).on_change();
            hover_color <- input.set_params.map(|p| p.hover_color).on_change();
            selection_color <- input.set_params.map(|p| p.selection_color).on_change();
            font <- input.set_params.map(|p| p.font.clone_ref()).on_change();
            text_offset <- input.set_params.map(|p| p.text_offset).on_change();
            text_color <- input.set_params.map(|p| p.text_color).on_change();
            text_size <- input.set_params.map(|p| p.text_size).on_change();
            dis_text_color <- input.set_params.map(|p| p.disabled_text_color).on_change();

            contour <- all_with(&size, &bg_margin, |size, margin| entry::Contour {
                size: *size - Vector2(*margin, *margin) * 2.0,
                corners_radius: 0.0,
            });
            layout <- all(contour, text_size, text_offset);
            eval layout ((&(c, ts, to)) data.update_layout(c, ts, to));
            eval bg_color ((color) data.background.color.set(color.into()));
            disabled <- input.set_model.map(|m| *m.disabled);
            data.label.set_property_default <+ all_with3(
                &text_color,
                &dis_text_color,
                &disabled,
                |c, dc, d| if *d { *dc } else { *c }
            ).ref_into_some();
            data.label.set_font <+ font;
            data.label.set_property_default <+ text_size.ref_into_some();
            content <- input.set_model.map(|m| m.text.clone_ref());
            max_width_px <- input.set_size.map(|size| size.x);
            data.label.set_content <+ content;
            data.label.set_view_width <+ max_width_px.some();

            out.override_column_width <+ input.set_model.filter_map(|m| *m.override_width);
            out.contour <+ contour;
            out.highlight_contour <+ contour;
            out.disabled <+ disabled;
            out.hover_highlight_color <+ hover_color;
            out.selection_highlight_color <+ selection_color;
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

/// The Simple version of Selectable Grid View, where each entry is just a label with background.
pub type SimpleSelectableGridView = selectable::GridView<Entry>;

/// The Simple version of scrollable and selectable Grid View, where each entry is just a label with
/// background.
pub type SimpleScrollableSelectableGridView = scrollable::SelectableGridView<Entry>;

/// The Simple version of scrollable and selectable Grid View with headers, where each header or
/// entry is just a label with background.
pub type SimpleScrollableSelectableGridViewWithHeaders =
    scrollable::SelectableGridViewWithHeaders<Entry, Entry>;
