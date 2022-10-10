//! A module containing definition for Component Browser Entry [`View`] and related structures.

use crate::prelude::*;
use ensogl_core::display::shape::*;

use crate::content::GroupId;
use crate::content::SectionId;
use crate::entry::style::ColorIntensities;
use crate::entry::style::Colors;
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
use ensogl_core::display::Scene;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::entry::Contour;
use ensogl_grid_view::entry::MovedHeaderPosition;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid::entry as theme;
use ensogl_shadow as shadow;
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

/// The number of pixels the entries inside one group overlap each other.
///
/// The entries need to overlap, otherwise we see artifacts at their boundaries.
const ENTRIES_OVERLAP_PX: f32 = 2.0;



// ==================
// === Background ===
// ==================

/// The background of the Component Browser Entry. It consists of a rectangle and an optional shadow
/// underneath it. The shadow is shown under the group headers which are pushed down to be visible
/// in the viewport.
pub mod background {
    use super::*;

    // We don't use the usual padding of sprites, because we clip the shadow under the background
    // and clipping the shadow shape with `*` operator causes glitches.
    // See https://www.pivotaltracker.com/story/show/182593513
    ensogl_core::define_shape_system! {
        below = [grid_view::entry::overlay, grid_view::selectable::highlight::shape];
        pointer_events = false;
        (style:Style, color:Vector4, height: f32, shadow_height_multiplier: f32) {
            let color = Var::<color::Rgba>::from(color);
            let width: Var<Pixels> = "input_size.x".into();
            let height: Var<Pixels> = height.into();
            let bg = Rect((&width, &height)).fill(color);
            // We use wider and shorter rect for the shadow because of the visual artifacts that
            // will appear otherwise:
            // 1. Rounded corners of the shadow are visible if the rect is too narrow. By widening
            //    it we keep the shadow sharp and flat for the whole width of the header.
            // 2. Visual glitching similar to z-fighting occurs on the border of the elements
            //    when the shadow rect has the exact same size as the background. We shrink the
            //    height by 1 pixel to avoid it.
            let shadow_rect = Rect((width * 2.0, height - 1.0.px()));
            let mut shadow_parameters = shadow::parameters_from_style_path(style, theme::shadow);
            shadow_parameters.size = shadow_parameters.size * shadow_height_multiplier;
            let shadow = shadow::from_shape_with_parameters(shadow_rect.into(), shadow_parameters);
            (shadow + bg).into()
        }
    }
}



// =============
// === Model ===
// =============

// === Kind ===

/// The kind of entry:
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum Kind {
    /// A standard entry with icon and caption.
    #[default]
    Entry,
    /// A group header; there is no icon, the text id stronger and there is a gap above header
    /// being a group separator.
    Header,
    /// The entry in LocalScope section which leave no gap to the left or right, and is pushed down
    /// a little to create a gap between Local Scope section and other groups.
    LocalScopeEntry {
        /// Entries in the first line will not add an (overlap)[ENTRIES_OVERLAP_PX].
        first_line: bool,
    },
}


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
    /// The entry should have color defined for the Local Scope section.
    LocalScope,
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
            Self::LocalScope => group_colors.local_scope_group,
        }
    }
}


// === Model ===

/// The [model](grid_view::entry::Entry) for Component Browser Entry.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Model {
    pub kind:        Kind,
    pub color:       MainColor,
    pub caption:     ImString,
    pub highlighted: Rc<Vec<text::Range<text::Byte>>>,
    pub icon:        Option<icon::Id>,
    pub group_id:    GroupId,
}



// ==============
// === Params ===
// ==============

// === DimmedGroups ===

/// Information about which groups shall be dimmed out.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum DimmedGroups {
    #[default]
    None,
    AllExcept(GroupId),
    AllExceptSection(SectionId),
}

impl DimmedGroups {
    fn is_group_dimmed(self, id: GroupId) -> bool {
        match self {
            Self::None => false,
            Self::AllExcept(undimmed_group) => id != undimmed_group,
            Self::AllExceptSection(undimmed_section) => id.section != undimmed_section,
        }
    }
}


// === Params ===

/// The [parameters](grid_view::entry::Entry) of Component Browser Entry.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Params {
    pub style:             Style,
    pub grid_style:        GridStyle,
    pub color_intensities: ColorIntensities,
    pub group_colors:      GroupColors,
    pub dimmed_groups:     DimmedGroups,
}



// ============
// === Data ===
// ============

// === CurrentIcon ===

/// The structure keeping a currently displayed icon in Component Group Entry [`View`], remembering
/// the position, id, and colors.
#[derive(Debug)]
struct CurrentIcon {
    display_object: display::object::Instance,
    strong_color:   color::Lcha,
    weak_color:     color::Lcha,
    shape:          Option<icon::Any>,
    id:             Option<icon::Id>,
}

impl Default for CurrentIcon {
    fn default() -> Self {
        Self {
            display_object: display::object::Instance::new(),
            strong_color:   default(),
            weak_color:     default(),
            shape:          default(),
            id:             default(),
        }
    }
}

impl CurrentIcon {
    fn update(&mut self, new_icon: Option<icon::Id>) {
        if self.id != new_icon {
            self.id = new_icon;
            if let Some(icon_id) = new_icon {
                let shape = icon_id.create_shape(Vector2(icon::SIZE, icon::SIZE));
                shape.strong_color.set(color::Rgba::from(self.strong_color).into());
                shape.weak_color.set(color::Rgba::from(self.weak_color).into());
                self.display_object.add_child(&shape);
                self.shape = Some(shape);
            } else {
                self.shape = None;
            }
        }
    }

    fn set_strong_color(&mut self, color: color::Lcha) {
        self.strong_color = color;
        if let Some(shape) = &self.shape {
            shape.strong_color.set(color::Rgba::from(color).into());
        }
    }

    fn set_weak_color(&mut self, color: color::Lcha) {
        self.weak_color = color;
        if let Some(shape) = &self.shape {
            shape.weak_color.set(color::Rgba::from(color).into());
        }
    }
}

impl display::Object for CurrentIcon {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        &self.display_object
    }
}


// === Data ===

/// The data of Component Browser Entry [`View`], passed to its FRP nodes.
#[derive(Clone, CloneRef, Debug)]
pub struct Data {
    display_object: display::object::Instance,
    label:          text::Text,
    background:     background::View,
    icon:           Rc<RefCell<CurrentIcon>>,
    style:          StyleWatchFrp,
}

impl Data {
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let display_object = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        let background = background::View::new();
        let icon = CurrentIcon::default();
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        display_object.add_child(&background);
        display_object.add_child(&icon);
        display_object.add_child(&label);
        label.set_long_text_truncation_mode(true);
        if let Some(layer) = text_layer {
            label.add_to_scene_layer(layer);
        }
        Self { display_object, label, background, icon: Rc::new(RefCell::new(icon)), style }
    }

    fn update_layout(
        &self,
        kind: Kind,
        style: &Style,
        grid_style: &GridStyle,
        entry_size: Vector2,
    ) {
        // For explanation how differend kinds of entry should behave, see documentation of
        // [`Kind`].
        let local_scope_offset = match kind {
            Kind::LocalScopeEntry { .. } => -grid_style.column_gap,
            _ => 0.0,
        };
        let overlap = match kind {
            Kind::Entry | Kind::LocalScopeEntry { first_line: false } => ENTRIES_OVERLAP_PX,
            _ => 0.0,
        };
        let gap_over_header = match kind {
            Kind::Header => grid_style.column_gap,
            _ => 0.0,
        };
        let bg_width = match kind {
            Kind::LocalScopeEntry { .. } => entry_size.x,
            _ => grid_style.column_width(),
        };
        let bg_height = entry_size.y - gap_over_header + overlap;
        // See comment in [`Self::update_shadow`] method.
        let shadow_addition = self.background.size.get().y - self.background.height.get();
        let bg_sprite_height = bg_height + shadow_addition;
        let bg_y = -gap_over_header / 2.0 + overlap / 2.0 + local_scope_offset;
        self.background.set_position_y(bg_y);
        self.background.size.set(Vector2(bg_width, bg_sprite_height));
        self.background.height.set(bg_height);
        let left = -entry_size.x / 2.0 + style.padding;
        let icon_x = left + style.icon_size / 2.0;
        let icon_y = local_scope_offset;
        self.icon.borrow().set_position_xy(Vector2(icon_x, icon_y));
        let text_x = Self::text_x_position(kind, style, grid_style);
        let text_y = style.text_size / 2.0 + local_scope_offset;
        self.label.set_position_xy(Vector2(text_x, text_y));
    }

    fn contour(kind: Kind, grid_style: &GridStyle, entry_size: Vector2) -> Contour {
        let optional_gap = if kind == Kind::Header { grid_style.column_gap } else { 0.0 };
        let height = entry_size.y - optional_gap;
        Contour::rectangular(Vector2(grid_style.column_width(), height))
    }

    fn highlight_contour(contour: Contour, style: &Style) -> Contour {
        let corners_radius = style.selection_corners_radius;
        Contour { corners_radius, ..contour }
    }

    fn contour_offset(kind: Kind, grid_style: &GridStyle) -> Vector2 {
        let y = match kind {
            Kind::Header => -grid_style.column_gap / 2.0,
            Kind::LocalScopeEntry { .. } => -grid_style.column_gap,
            _ => 0.0,
        };
        Vector2(0.0, y)
    }

    fn max_text_width(kind: Kind, style: &Style, grid_style: &GridStyle) -> f32 {
        let right = grid_style.column_width() / 2.0 - style.padding;
        let text_x = Self::text_x_position(kind, style, grid_style);
        right - text_x
    }

    fn text_x_position(kind: Kind, style: &Style, grid_style: &GridStyle) -> f32 {
        let left = -grid_style.column_width() / 2.0 + style.padding;
        if kind == Kind::Header {
            left
        } else {
            left + style.icon_size + style.icon_text_padding
        }
    }

    fn update_shadow(
        &self,
        header_position: &MovedHeaderPosition,
        style: &Style,
        entry_size: Vector2,
    ) {
        if header_position.position != Vector2::default() {
            let bg_width = self.background.size.get().x;
            let bg_height = self.background.height.get();
            let distance_to_section_top =
                header_position.y_range.end() - header_position.position.y;
            let distance_to_section_bottom =
                header_position.position.y - header_position.y_range.start();
            // We need to render both the header background and the shadow below it, so we add
            // `shadow_size` and base `height` to calculate the final sprite size of the
            // `background` shape. We use `shadow_size * 2.0`, because the shadow extends by
            // `shadow_size` in each direction around the base shape, not only down. We cap the
            // `shadow_size` by the distance to the bottom of the component group so that the shadow
            // is not visible outside the component group background.
            let shadow_size = style.header_shadow_size.min(distance_to_section_bottom);
            let bg_sprite_height = bg_height + shadow_size * 2.0;
            self.background.size.set(Vector2(bg_width, bg_sprite_height));
            let header_shadow_peak = entry_size.y / 2.0;
            let height_multiplier = (distance_to_section_top / header_shadow_peak).min(1.0);
            self.background.shadow_height_multiplier.set(height_multiplier);
        }
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
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    frp:  grid_view::entry::EntryFrp<Self>,
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

            kind <- input.set_model.map(|m| m.kind).on_change();
            style <- input.set_params.map(|p| p.style.clone()).on_change();
            color_intensities <- input.set_params.map(|p| p.color_intensities).on_change();
            group_colors <- input.set_params.map(|p| p.group_colors).on_change();
            grid_style <- input.set_params.map(|p| p.grid_style).on_change();
            kind_and_style <- all(kind, style, grid_style);
            layout_data <- all(kind_and_style, input.set_size);
            eval layout_data ((((kind, style, grid_style), entry_sz))
                data.update_layout(*kind, style, grid_style, *entry_sz)
            );
            out.contour <+ layout_data.map(|((kind, _, grid_style), entry_sz)| {
                Data::contour(*kind, grid_style, *entry_sz)
            });
            out.contour_offset <+ kind_and_style.map(|(k, _, gs)| Data::contour_offset(*k, gs));
            out.highlight_contour <+ out.contour.map2(&style, |c, s| Data::highlight_contour(*c, s));
            out.highlight_contour_offset <+ out.contour_offset;


            // === Colors ===

            color_variant <- input.set_model.map(|m| m.color);
            color <- all_with(&color_variant, &group_colors, |c, g| c.obtain(g));
            is_dimmed <- all_with(&input.set_model, &input.set_params, |m,p| {
                p.dimmed_groups.is_group_dimmed(m.group_id)
            });
            let colors = Colors::from_main_color(network, &data.style, &color, &color_intensities, &is_dimmed);
            eval colors.background ((c) data.background.color.set(color::Rgba::from(c).into()));
            data.label.set_property_default <+ colors.text.ref_into_some();
            eval colors.icon_strong ((c) data.icon.borrow_mut().set_strong_color(*c));
            eval colors.icon_weak ((c) data.icon.borrow_mut().set_weak_color(*c));
            out.hover_highlight_color <+ colors.hover_highlight;
            // We want to animate only when params changed (the different section is highlighted).
            // Other case, where entry receives new model with new section means it is reused
            // at another location, so it should not animate anything.
            colors.skip_animations <+ input.set_model.constant(());


            // === Header Shadow ===

            shadow_data <- all(input.moved_as_header, style, input.set_size);
            eval shadow_data (((p, s, e)) data.update_shadow(p, s, *e));


            // === Icon and Text ===

            max_text_width <- kind_and_style.map(|(k, s, gs)| Data::max_text_width(*k, s, gs));
            caption <- input.set_model.map(|m| m.caption.clone_ref());
            icon <- input.set_model.map(|m| m.icon);
            data.label.set_content <+ caption;
            data.label.set_view_width <+ max_text_width.some();
            content_changed <- data.label.content.constant(());
            style_changed <- style.constant(());
            highlight_range <= all_with3(
                &input.set_model,
                &content_changed,
                &style_changed,
                |m, (), ()| m.highlighted.deref().clone()
            );
            data.label.set_property <+ highlight_range.map2(&style, |range, s| {
                (range.into(), Some(text::SdfWeight::new(s.highlight_bold).into()))
            });
            data.label.set_property_default <+ style.map(|s| text::Size::new(s.text_size)).cloned_into_some();
            eval icon ((&icon) data.icon.borrow_mut().update(icon));
            data.label.set_font <+ style.map(|s| s.font.clone_ref()).on_change();
        }
        Self { frp, data }
    }

    fn frp(&self) -> &grid_view::entry::EntryFrp<Self> {
        &self.frp
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        &self.data.display_object
    }
}
