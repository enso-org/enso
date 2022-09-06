//! A module defining the special [`Entry`] type for the grid view of the breadcrumbs.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::layer::WeakLayer;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::Scene;
use ensogl_grid_view::entry::Contour;
use ensogl_grid_view::entry::EntryFrp;
use ensogl_hardcoded_theme::application::component_browser::searcher::list_panel::breadcrumbs as theme;
use ensogl_text as text;



// ==============
// === Shapes ===
// ==============

/// A filled triangle pointing to the right. Used to separate breadcrumbs.
pub mod separator {
    use super::*;
    use std::f32::consts::PI;

    pub const ICON_WIDTH: f32 = 16.0;

    ensogl_core::define_shape_system! {
        above = [ensogl_grid_view::entry::shape];
        pointer_events = false;
        (style: Style, color: Vector4) {
            let width = style.get_number(theme::separator::width);
            let height = style.get_number(theme::separator::height);
            let triangle = Triangle(width, height).rotate((PI/2.0).radians());
            let shape = triangle.fill(color);
            shape.into()
        }
    }
}

/// A three dots icon. Used to indicate that the last module in breadcrumbs list contains more
/// ancestors.
pub mod ellipsis {
    use super::*;

    pub const ICON_WIDTH: f32 = 32.0;

    ensogl_core::define_shape_system! {
        above = [ensogl_grid_view::entry::shape];
        pointer_events = false;
        (style: Style, color: Vector4) {
            let radius = style.get_number(theme::ellipsis::circles_radius).px();
            let gap = style.get_number(theme::ellipsis::circles_gap).px();
            let background_width = style.get_number(theme::ellipsis::background_width);
            let background_height = style.get_number(theme::ellipsis::background_height);
            let background_corners_radius = style.get_number(theme::ellipsis::background_corners_radius);
            let background_color = style.get_color(theme::ellipsis::background_color);

            let left = Circle(radius.clone()).fill(color.clone());
            let center = Circle(radius.clone()).fill(color.clone());
            let right = Circle(radius.clone()).fill(color);
            let circles = left.translate_x(-gap.clone()) + center + right.translate_x(gap);
            let background = Rect((background_width.px(), background_height.px()));
            let background = background.corners_radius(background_corners_radius.px());
            let background = background.fill(background_color);
            let shape = background + circles;
            shape.into()
        }
    }
}



// =============
// === Model ===
// =============

/// A model for the entry in the breadcrumbs list.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, Default)]
pub enum Model {
    Text(ImString),
    Separator,
    #[default]
    Ellipsis,
}



// =================
// === EntryType ===
// =================

/// A helper type that defines all possible types of entries in the breadcrumbs list.
#[derive(Clone, Debug, CloneRef)]
#[allow(missing_docs)]
pub enum EntryType {
    Text { label: text::Area },
    Separator { shape: separator::View },
    Ellipsis { shape: ellipsis::View },
}

impl display::Object for EntryType {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        match self {
            EntryType::Text { label } => label.display_object(),
            EntryType::Separator { shape } => shape.display_object(),
            EntryType::Ellipsis { shape } => shape.display_object(),
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
    app:            Application,
    display_object: display::object::Instance,
    logger:         Logger,
    entry:          CloneRefCell<EntryType>,
    text_layer:     Option<WeakLayer>,
}

impl EntryData {
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let logger = Logger::new("breadcrumbs::Entry");
        let display_object = display::object::Instance::new(&logger);
        let shape = ellipsis::View::new(&logger);
        let entry = EntryType::Ellipsis { shape };
        display_object.add_child(&entry);
        Self {
            display_object,
            app: app.clone_ref(),
            logger,
            entry: CloneRefCell::new(entry),
            text_layer: text_layer.map(Layer::downgrade),
        }
    }

    fn set_model(&self, model: &Model) {
        match model {
            Model::Text(content) => self.set_content(content),
            Model::Separator => self.set_separator(),
            Model::Ellipsis => self.set_ellipsis(),
        }
    }

    fn set_content(&self, content: &str) {
        if let EntryType::Text { label } = self.entry.get() {
            label.set_content(content);
        } else {
            self.switch_to_label(content);
        }
    }

    fn switch_to_label(&self, content: &str) {
        self.display_object.remove_child(&self.entry.get());
        let label = self.app.new_view::<ensogl_text::Area>();
        if let Some(weak_layer) = &self.text_layer {
            if let Some(layer) = weak_layer.upgrade() {
                label.add_to_scene_layer(&layer);
            }
        }
        label.set_content(content);
        let entry = EntryType::Text { label };
        self.display_object.add_child(&entry);
        self.entry.set(entry);
    }

    fn set_separator(&self) {
        if !matches!(self.entry.get(), EntryType::Separator { .. }) {
            self.switch_to_separator();
        }
    }

    fn set_ellipsis(&self) {
        if !matches!(self.entry.get(), EntryType::Ellipsis { .. }) {
            self.switch_to_ellipsis();
        }
    }

    fn switch_to_separator(&self) {
        let entry = EntryType::Separator { shape: separator::View::new(&self.logger) };
        self.switch_to(entry);
    }

    fn switch_to_ellipsis(&self) {
        let entry = EntryType::Ellipsis { shape: ellipsis::View::new(&self.logger) };
        self.switch_to(entry);
    }

    fn switch_to(&self, entry: EntryType) {
        self.display_object.remove_child(&self.entry.get());
        self.display_object.add_child(&entry);
        self.entry.set(entry);
    }

    fn update_layout(&self, contour: Contour, text_size: text::Size, text_offset: f32) {
        let size = contour.size;
        match self.entry.get() {
            EntryType::Text { label } => {
                label.set_position_xy(Vector2(text_offset - size.x / 2.0, text_size.raw / 2.0));
            }
            EntryType::Separator { shape } => {
                shape.size.set(size);
            }
            EntryType::Ellipsis { shape } => {
                shape.size.set(size);
            }
        }
    }

    fn set_default_color(&self, color: color::Rgba) {
        match self.entry.get() {
            EntryType::Text { label } => label.set_default_color(color),
            EntryType::Separator { shape } => shape.color.set(color.into()),
            EntryType::Ellipsis { shape } => shape.color.set(color.into()),
        }
    }

    fn set_font(&self, font: String) {
        match self.entry.get() {
            EntryType::Text { label } => label.set_font(font),
            _ => {}
        }
    }

    fn set_default_text_size(&self, size: text::Size) {
        match self.entry.get() {
            EntryType::Text { label } => label.set_default_text_size(size),
            _ => {}
        }
    }

    fn width(&self) -> f32 {
        match self.entry.get() {
            EntryType::Text { label } => label.width.value() + 14.0,
            EntryType::Separator { .. } => separator::ICON_WIDTH,
            EntryType::Ellipsis { .. } => ellipsis::ICON_WIDTH,
        }
    }
}

// === Params ===

/// The parameters of Breadcrumbs' entries.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Params {
    pub margin:                   f32,
    pub hover_color:              color::Rgba,
    pub font:                     ImString,
    pub text_offset:              f32,
    pub text_size:                text::Size,
    pub text_color:               color::Rgba,
    pub highlight_corners_radius: f32,
    pub greyed_out_color:         color::Rgba,
    pub greyed_out_start:         Option<usize>,
}

impl Default for Params {
    fn default() -> Self {
        Self {
            margin:                   1.0,
            hover_color:              color::Rgba(0.0, 0.0, 0.0, 0.0),
            font:                     text::font::DEFAULT_FONT.into(),
            text_offset:              7.0,
            text_size:                12.0.into(),
            text_color:               color::Rgba(0.0, 0.0, 0.0, 1.0),
            highlight_corners_radius: 15.0,
            greyed_out_color:         color::Rgba(0.0, 0.0, 0.0, 0.5),
            greyed_out_start:         None,
        }
    }
}

// === Entry ===

/// A Breadcrumbs entry.
#[derive(Clone, CloneRef, Debug)]
pub struct Entry {
    frp:  EntryFrp<Self>,
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

        enso_frp::extend! { network
            init <- source_();
            size <- input.set_size.on_change();
            margin <- input.set_params.map(|p| p.margin).on_change();
            hover_color <- input.set_params.map(|p| p.hover_color).on_change();
            font <- input.set_params.map(|p| p.font.clone_ref()).on_change();
            text_offset <- input.set_params.map(|p| p.text_offset).on_change();
            text_color <- input.set_params.map(|p| p.text_color).on_change();
            text_size <- input.set_params.map(|p| p.text_size).on_change();
            greyed_out_color <- input.set_params.map(|p| p.greyed_out_color).on_change();
            greyed_out_from <- input.set_params.map(|p| p.greyed_out_start).on_change();
            highlight_corners_radius <- input.set_params.map(|p| p.highlight_corners_radius).on_change();

            col <- input.set_location._1();
            should_grey_out <- all_with(&col, &greyed_out_from,
                |col, from| from.map_or(false, |from| *col >= from)
            );
            color <- switch(&should_grey_out, &text_color, &greyed_out_color);

            contour <- all_with(&size, &margin, |size, margin| Contour {
                size: *size - Vector2(*margin, *margin) * 2.0,
                corners_radius: 0.0,
            });
            layout <- all(contour, text_size, text_offset);
            eval layout ((&(c, ts, to)) data.update_layout(c, ts, to));
            text_params <- all3(&color, &font, &text_size);
            eval text_params([data](params) {
                let (color, font, size) = params;
                data.set_default_color(*color);
                data.set_default_text_size(*size);
                data.set_font(font.to_string());
            });
            is_disabled <- input.set_model.map(|m| matches!(m, Model::Separator | Model::Ellipsis));
            width <- map3(&input.set_model, &text_params, &layout,
                f!([data](model, text_params, layout) {
                    let (text_color, font, text_size) = text_params;
                    data.set_model(&model);
                    data.set_font(font.to_string());
                    data.set_default_color(*text_color);
                    data.set_default_text_size(*text_size);
                    data.update_layout(layout.0, layout.1, layout.2);
                    data.width()
                })
            );
            out.override_column_width <+ width;

            out.disabled <+ is_disabled;
            out.contour <+ contour;
            out.highlight_contour <+ contour.map2(&highlight_corners_radius,
                |c, r| Contour { corners_radius: *r, ..*c }
            );
            out.hover_highlight_color <+ hover_color;
            out.selection_highlight_color <+ init.constant(color::Rgba::transparent());
        }
        init.emit(());
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
