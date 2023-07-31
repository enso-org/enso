//! A module containing definition for Component Browser Entry [`View`] and related structures.

use crate::prelude::*;
use ensogl_core::display::shape::*;

use crate::entry::style::Colors;
use crate::entry::style::ResolvedColors;
use crate::GroupColors;
use crate::Style as GridStyle;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::entry::Contour;
use ensogl_text as text;


// ==============
// === Export ===
// ==============

pub mod icon;
pub mod style;

pub use crate::entry::style::Style;



// =================
// === Constants ===
// =================

/// The number of pixels the entries backgrounds overlap each other.
///
/// The entries need to overlap, otherwise we see artifacts at their boundaries.
const ENTRIES_OVERLAP_PX: f32 = 2.0;



// ==================
// === Background ===
// ==================

/// The background of the Component Browser Entry.
pub mod background {
    use super::*;

    ensogl_core::shape! {
        below = [grid_view::entry::overlay, grid_view::selectable::highlight::shape, icon::any];
        pointer_events = false;
        (style:Style, color:Vector4) {
            let color = Var::<color::Rgba>::from(color);
            let size: Var<Vector2<Pixels>> = "input_size".into();
            let bg = Rect(size).fill(color);
            bg.into()
        }
    }
}

// =============
// === Model ===
// =============

// === MainColor ===

/// The structure describing how entry is colored.
///
/// Each group has devined "main color" for entries, from which are derived colros of various
/// entries' elements: text, icon, background, etc. (see [`style::Colors`] documentation).
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub enum MainColor {
    /// The entry should use one variant of predefined color taken from the style sheet.
    Predefined { variant_index: usize },
    /// The entry's color is defined by library's author.
    Custom(color::Lcha),
    /// The entry should have color defined for entries outside defined groups.
    OutsideGroup,
}

impl Default for MainColor {
    fn default() -> Self {
        Self::Predefined { variant_index: 0 }
    }
}

impl MainColor {
    fn obtain(self, group_colors: &GroupColors) -> color::Lcha {
        match self {
            Self::Predefined { variant_index: variant } =>
                group_colors.variants[variant % group_colors.variants.len()],
            Self::Custom(color) => color,
            Self::OutsideGroup => group_colors.outside_group,
        }
    }
}


// === Model ===

/// The [model](grid_view::entry::Entry) for Component Browser Entry.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Model {
    pub color:       MainColor,
    pub caption:     ImString,
    pub highlighted: Rc<Vec<text::Range<text::Byte>>>,
    pub icon:        Option<icon::Id>,
}



// ==============
// === Params ===
// ==============

/// The [parameters](grid_view::entry::Entry) of Component Browser Entry.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Params {
    pub style:        Style,
    pub grid_style:   GridStyle,
    pub colors:       Colors,
    pub group_colors: GroupColors,
}



// ============
// === Data ===
// ============

// === Data ===

/// The data of Component Browser Entry [`View`], passed to its FRP nodes.
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct Data {
    display_object: display::object::Instance,
    label:          text::Text,
    background:     background::View,
    icon:           icon::any::View,
    style:          StyleWatchFrp,
}

impl Data {
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let display_object = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        let background = background::View::new();
        let icon = icon::any::View::new();
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        display_object.add_child(&background);
        display_object.add_child(&icon);
        display_object.add_child(&label);
        background.set_size((0.0, 0.0));
        icon.set_size((icon::SIZE, icon::SIZE));
        label.set_long_text_truncation_mode(true);
        if let Some(layer) = text_layer {
            layer.add(&label);
        }
        Self { display_object, label, background, icon, style }
    }

    fn update_layout(&self, style: &Style, grid_style: &GridStyle, entry_size: Vector2) {
        let overlap = Vector2(0.0, ENTRIES_OVERLAP_PX);
        self.background.set_size(entry_size + overlap);
        self.background.set_xy(-entry_size / 2.0 - overlap);
        let left = -entry_size.x / 2.0 + style.padding;
        let icon_x = left + style.icon_size / 2.0;
        self.icon.set_x(icon_x);
        let text_x = Self::text_x_position(style, grid_style);
        let text_y = style.text_y_offset;
        self.label.set_xy(Vector2(text_x, text_y));
    }

    fn contour(grid_style: &GridStyle, entry_size: Vector2) -> Contour {
        let height = entry_size.y;
        Contour::rectangular(Vector2(grid_style.content_size().x, height))
    }

    fn highlight_contour(contour: Contour, style: &Style) -> Contour {
        let corners_radius = style.selection_corners_radius;
        Contour { corners_radius, ..contour }
    }

    fn max_text_width(style: &Style, grid_style: &GridStyle) -> f32 {
        let right = grid_style.content_size().x / 2.0 - style.padding;
        let text_x = Self::text_x_position(style, grid_style);
        right - text_x
    }

    fn text_x_position(style: &Style, grid_style: &GridStyle) -> f32 {
        let left = -grid_style.content_size().x / 2.0 + style.padding;
        left + style.icon_size + style.icon_text_padding
    }
}



// ============
// === View ===
// ============

/// Component Browser Entry View.
///
/// This is a parameter of the [Grid View](grid_view) inside Component Browser Panel List,
/// parametrized by [`Params`] and whose model is [`Model`].
///
/// The entries (except [local scope entries](`Kind::LocalScopeEntry`) have width equal to the
/// column width declared in [`Style`]. Making grid column  broader can create a nice vertical gaps
/// between columns. The horizontal gaps are left by the header entries (having a bit lower height).
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct View {
    frp:  grid_view::entry::EntryFrp<Self>,
    #[display_object]
    data: Data,
}

impl grid_view::Entry for View {
    type Model = Model;
    type Params = Params;

    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let frp = grid_view::entry::EntryFrp::<Self>::new();
        let data = Data::new(app, text_layer);
        let network = frp.network();
        let input = &frp.private().input;
        let out = &frp.private().output;

        frp::extend! { network

            // === Layout ===

            style <- input.set_params.map(|p| p.style.clone()).on_change();
            colors <- input.set_params.map(|p| p.colors).on_change();
            group_colors <- input.set_params.map(|p| p.group_colors).on_change();
            grid_style <- input.set_params.map(|p| p.grid_style).on_change();
            layout_data <- all(style, grid_style, input.set_size);
            eval layout_data (((style, grid_style, entry_sz))
                data.update_layout(style, grid_style, *entry_sz)
            );
            out.contour <+ all_with(&grid_style, &input.set_size, |grid_style, entry_sz| {
                Data::contour(grid_style, *entry_sz)
            });
            out.highlight_contour <+ out.contour.map2(&style, |c, s| Data::highlight_contour(*c, s));
            out.highlight_contour_offset <+ out.contour_offset;


            // === Colors ===

            color_variant <- input.set_model.map(|m| m.color);
            color <- all_with(&color_variant, &group_colors, |c, g| c.obtain(g));
            let colors = ResolvedColors::from_main_color(network, &data.style, &color, &colors);
            eval colors.background ((c) data.background.color.set(color::Rgba::from(c).into()));
            data.label.set_property_default <+ colors.text.ref_into_some();
            eval colors.icon ((c) data.icon.r_component.set(color::Rgba::from(*c).into()));
            out.hover_highlight_color <+ colors.hover_highlight;


            // === Icon and Text ===

            max_text_width <- all_with(&style, &grid_style, Data::max_text_width);
            caption <- input.set_model.map(|m| m.caption.clone_ref());
            icon <- input.set_model.map(|m| m.icon);
            data.label.set_content <+ caption;
            data.label.set_view_width <+ max_text_width.some();
            content_changed <- data.label.content.constant(());
            style_changed <- style.constant(());
            label_updated <- all3(&input.set_model, &content_changed, &style_changed);
            highlight_range <= label_updated.map(|(m, (), ())| m.highlighted.deref().clone());
            data.label.set_property <+ highlight_range.map2(&style, |range, s| {
                (range.into(), Some(text::SdfWeight::new(s.highlight_bold).into()))
            });
            data.label.set_property_default <+ style.map(|s| text::Size::new(s.text_size)).cloned_into_some();
            eval icon ((&icon) data.icon.icon.set(icon.map_or(default(), |icon| icon.any_cached_shape_location())));
            data.label.set_font <+ style.map(|s| s.font.clone_ref()).on_change();
        }
        Self { frp, data }
    }

    fn frp(&self) -> &grid_view::entry::EntryFrp<Self> {
        &self.frp
    }
}
