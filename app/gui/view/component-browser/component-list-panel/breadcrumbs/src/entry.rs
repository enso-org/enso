//! A module defining the special [`Entry`] type for the grid view of the breadcrumbs.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::style::FromTheme;
use ensogl_core::Animation;
use ensogl_grid_view::entry::Contour;
use ensogl_grid_view::entry::EntryFrp;
use ensogl_grid_view::Col;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::menu::breadcrumbs as theme;
use ensogl_text as text;



// ==============
// === Shapes ===
// ==============

/// A filled triangle pointing to the right. Used to separate breadcrumbs.
pub mod separator {
    use super::*;
    use std::f32::consts::PI;

    pub const ICON_WIDTH: f32 = 30.0;

    ensogl_core::shape! {
        above = [ensogl_grid_view::entry::shape];
        pointer_events = false;
        alignment = center;
        (style: Style) {
            let color = style.get_color(theme::separator::color);
            let width = style.get_number(theme::separator::width);
            let height = style.get_number(theme::separator::height);
            let triangle = Triangle(width, height).rotate((PI/2.0).radians());
            let offset_x = style.get_number(theme::separator::offset_x).px();
            let offset_y = style.get_number(theme::separator::offset_y).px();
            let triangle = triangle.translate_x(offset_x);
            let triangle = triangle.translate_y(offset_y);
            let shape = triangle.fill(color);
            shape.into()
        }
    }
}

/// A three dots icon. Used to indicate that the last module in breadcrumbs list contains more
/// ancestors.
pub mod ellipsis {
    use super::*;

    pub const ICON_WIDTH: f32 = 28.0;

    ensogl_core::shape! {
        above = [ensogl_grid_view::entry::shape];
        pointer_events = false;
        alignment = center;
        (style: Style) {
            let radius = style.get_number(theme::ellipsis::circles_radius).px();
            let gap = style.get_number(theme::ellipsis::circles_gap).px();
            let background_width = style.get_number(theme::ellipsis::background_width);
            let background_height = style.get_number(theme::ellipsis::background_height);
            let background_corners_radius = style.get_number(theme::ellipsis::background_corners_radius);
            let circles_color = style.get_color(theme::ellipsis::circles_color);
            let background_color = style.get_color(theme::ellipsis::background_color);

            let tile_size = radius.clone() * 2.0 + gap;
            let circles = Circle(radius).repeat((tile_size.clone(), tile_size.clone()));
            let mask = Rect((tile_size.clone() * 3.0, tile_size));
            let circles = circles.intersection(mask).fill(circles_color);
            let background = Rect((background_width.px(), background_height.px()));
            let background = background.corners_radius(background_corners_radius.px());
            let background = background.fill(background_color);
            let shape = background + circles;
            let offset_x = style.get_number(theme::ellipsis::offset_x).px();
            let offset_y = style.get_number(theme::ellipsis::offset_y).px();
            let shape = shape.translate_x(offset_x);
            let shape = shape.translate_y(offset_y);
            shape.into()
        }
    }
}



// =============
// === Style ===
// =============

/// Stylesheet-defined portion of the entries' parameters.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, FromTheme)]
#[base_path = "theme::entry"]
pub struct Style {
    /// The margin of the entry's [`Contour`]. The [`Contour`] specifies the size of the
    /// clickable area of the entry. If the margin is zero, the contour covers the entire entry.
    pub margin:                   f32,
    pub hover_color:              color::Rgba,
    #[theme_path = "theme::entry::font"]
    pub font_name:                ImString,
    pub text_y_offset:            f32,
    pub text_padding_left:        f32,
    pub text_size:                f32,
    pub selected_color:           color::Rgba,
    pub highlight_corners_radius: f32,
    pub greyed_out_color:         color::Rgba,
}


// =============
// === Model ===
// =============

/// A model for the entry in the breadcrumbs list.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, Default)]
pub enum Model {
    #[default]
    Ellipsis,
    Text(ImString),
    Separator,
}

#[allow(missing_docs)]
#[derive(Clone, Copy, CloneRef, Debug, Default, PartialEq)]
enum State {
    #[default]
    Ellipsis,
    Text,
    Separator,
}



// =============
// === Entry ===
// =============

// === EntryData ===

/// An internal structure of [`Entry`]. It has three visual representations: text,
/// a separator between entries, and an ellipsis. The breadcrumbs implementation selects the needed
/// representation for each entry in the grid view. For efficiency, text label and icons are
/// allocated once the entry is created.
#[allow(missing_docs)]
#[derive(Clone, Debug, display::Object)]
pub struct EntryData {
    display_object: display::object::Instance,
    text:           text::Text,
    separator:      separator::View,
    ellipsis:       ellipsis::View,
    state:          Rc<Cell<State>>,
}

impl EntryData {
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let display_object = display::object::Instance::new();
        let text = app.new_view::<ensogl_text::Text>();
        if let Some(layer) = text_layer {
            layer.add(&text);
        }
        let ellipsis = ellipsis::View::new();
        let separator = separator::View::new();
        let state = default();
        display_object.add_child(&ellipsis);
        Self { display_object, state, text, ellipsis, separator }
    }

    fn hide_current_visual_representation(&self) {
        match self.state.get() {
            State::Text => self.text.unset_parent(),
            State::Separator => self.separator.unset_parent(),
            State::Ellipsis => self.ellipsis.unset_parent(),
        }
    }

    fn set_model(&self, model: &Model) {
        match model {
            Model::Text(content) => self.switch_to_text(content),
            Model::Separator => self.switch_to_separator(),
            Model::Ellipsis => self.switch_to_ellipsis(),
        }
    }

    fn switch_to_text(&self, content: &str) {
        self.text.set_content(content);
        if self.state.get() != State::Text {
            self.hide_current_visual_representation();
            self.display_object.add_child(&self.text);
            self.state.set(State::Text);
        }
    }

    fn switch_to_separator(&self) {
        if self.state.get() != State::Separator {
            self.hide_current_visual_representation();
            self.display_object.add_child(&self.separator);
            self.state.set(State::Separator);
        }
    }

    fn switch_to_ellipsis(&self) {
        if self.state.get() != State::Ellipsis {
            self.hide_current_visual_representation();
            self.display_object.add_child(&self.ellipsis);
            self.state.set(State::Ellipsis);
        }
    }

    fn update_layout(&self, contour: Contour, text_padding: f32, text_y_offset: f32) {
        let size = contour.size;
        self.text.set_xy(Vector2(text_padding - size.x / 2.0, text_y_offset));
        self.separator.set_size(Vector2(separator::ICON_WIDTH, size.y));
        self.ellipsis.set_size(Vector2(ellipsis::ICON_WIDTH, size.y));
    }

    fn set_default_color(&self, color: color::Lcha) {
        self.text.set_property_default(color);
    }

    fn set_font(&self, font: String) {
        self.text.set_font(font);
    }

    fn set_default_text_size(&self, size: f32) {
        self.text.set_property_default(text::Size::new(size));
        self.text.set_property_default(text::Weight::Medium);
    }

    fn is_state_change(&self, model: &Model) -> bool {
        match model {
            Model::Text(new_text) => {
                let previous_state_was_not_text = self.state.get() != State::Text;
                let previous_text = String::from(self.text.content.value());
                let text_was_different = previous_text.as_str() != new_text.as_str();
                previous_state_was_not_text || text_was_different
            }
            Model::Separator => self.state.get() != State::Separator,
            Model::Ellipsis => self.state.get() != State::Ellipsis,
        }
    }

    fn is_text_displayed(&self) -> bool {
        self.state.get() == State::Text
    }

    fn width(&self, text_padding: f32) -> f32 {
        match self.state.get() {
            State::Text => self.text_width(self.text.width.value(), text_padding),
            State::Separator => separator::ICON_WIDTH,
            State::Ellipsis => ellipsis::ICON_WIDTH,
        }
    }

    /// Width of the breadcrumb column filled with text of width [`text_width`] and with margin
    /// [`text_padding`].
    fn text_width(&self, text_width: f32, text_padding: f32) -> f32 {
        text_width + text_padding * 2.0
    }
}


// === Params ===

/// The style parameters of Breadcrumbs' entries. See [`ensogl_grid_view::Frp::set_entries_params`].
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Params {
    pub style:            Style,
    /// The first greyed out column. All columns to the right will also be greyed out.
    pub greyed_out_start: Option<Col>,
}


// === Entry ===

/// A Breadcrumbs entry.
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct Entry {
    frp:  EntryFrp<Self>,
    #[display_object]
    data: Rc<EntryData>,
}

impl ensogl_grid_view::Entry for Entry {
    type Model = Model;
    type Params = Params;

    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let data = Rc::new(EntryData::new(app, text_layer));
        let frp = EntryFrp::<Self>::new();
        let input = &frp.private().input;
        let out = &frp.private().output;
        let network = frp.network();
        let color_anim = Animation::new(network);
        let appear_anim = Animation::new(network);
        fn mix(c1: &color::Lcha, c2: &color::Lcha, coefficient: &f32) -> color::Lcha {
            color::mix(*c1, *c2, *coefficient)
        }

        enso_frp::extend! { network
            init <- source_();
            size <- input.set_size.on_change();
            margin <- input.set_params.map(|p| p.style.margin).on_change();
            hover_color <- input.set_params.map(|p| p.style.hover_color).cloned_into().on_change();
            font <- input.set_params.map(|p| p.style.font_name.clone_ref()).on_change();
            text_padding <- input.set_params.map(|p| p.style.text_padding_left).on_change();
            text_color <- input.set_params.map(|p| p.style.selected_color).cloned_into().on_change();
            text_y_offset <- input.set_params.map(|p| p.style.text_y_offset).on_change();
            text_size <- input.set_params.map(|p| p.style.text_size).on_change();
            greyed_out_color <- input.set_params.map(|p| p.style.greyed_out_color).cloned_into().on_change();
            highlight_corners_radius <- input.set_params.map(|p| p.style.highlight_corners_radius).on_change();
            greyed_out_from <- input.set_params.map(|p| p.greyed_out_start).on_change();
            transparent_color <- init.constant(color::Lcha::transparent());

            col <- input.set_location._1();
            should_grey_out <- all_with(&col, &greyed_out_from,
                |col, from| from.map_or(false, |from| *col >= from)
            );
            color_anim.target <+ should_grey_out.map(|should| if *should { 1.0 } else { 0.0 });
            target_color <- all_with3(&text_color, &greyed_out_color, &color_anim.value, mix);
            appear_anim.target <+ init.constant(1.0);
            model_was_set <- input.set_model.map(f!((model) data.is_state_change(model))).on_true();
            should_appear <- any(&init, &model_was_set);
            eval_ should_appear({
                appear_anim.target.emit(0.0);
                appear_anim.skip.emit(());
                appear_anim.target.emit(1.0);
            });
            color <- all_with3(&transparent_color, &target_color, &appear_anim.value, mix);

            contour <- all_with(&size, &margin, |size, margin| Contour {
                size: *size - Vector2(*margin, *margin) * 2.0,
                corners_radius: 0.0,
            });
            layout <- all(contour, text_padding, text_y_offset);
            eval layout ((&(c, to, tyo)) data.update_layout(c, to, tyo));
            eval color((c) data.set_default_color(*c));
            eval font((f) data.set_font(f.to_string()));
            eval text_size((s) data.set_default_text_size(*s));
            is_disabled <- input.set_model.map(|m| matches!(m, Model::Separator | Model::Ellipsis));
            out.disabled <+ is_disabled;
            out.contour <+ contour;
            out.highlight_contour <+ contour.map2(&highlight_corners_radius,
                |c, r| Contour { corners_radius: *r, ..*c }
            );
            out.hover_highlight_color <+ hover_color;
            out.selection_highlight_color <+ init.constant(color::Lcha::transparent());


            // === Override column width ===

            // We need to adjust the width of the grid view column depending on the width of
            // the entry.
            out.override_column_width <+ input.set_model.map2(&text_padding,
                f!([data](model, text_padding) {
                    data.set_model(model);
                    data.width(*text_padding)
                })
            );
            // For text entries, we also listen for [`Text::width`] changes.
            text_width <- data.text.width.filter(f_!(data.is_text_displayed()));
            entry_width <- text_width.map2(&text_padding, f!((w, o) data.text_width(*w, *o)));
            out.override_column_width <+ entry_width;
        }
        init.emit(());
        Self { frp, data }
    }

    fn frp(&self) -> &EntryFrp<Self> {
        &self.frp
    }
}
