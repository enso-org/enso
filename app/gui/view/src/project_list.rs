//! An interactive list of projects. It used to quickly switch between projects from inside the
//! Project View.

use crate::prelude::*;
use ensogl::display::shape::*;

use enso_frp as frp;
use ensogl::application::frp::API;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::Layer;
use ensogl_component::grid_view;
use ensogl_component::shadow;
use ensogl_derive_theme::FromTheme;
use ensogl_hardcoded_theme::application::project_list as theme;
use ensogl_text as text;



// ==============
// === Styles ===
// ==============

// === Style ===

#[derive(Debug, Clone, Copy, Default, FromTheme)]
#[base_path = "theme"]
#[allow(missing_docs)]
pub struct Style {
    width:           f32,
    height:          f32,
    shadow_extent:   f32,
    corners_radius:  f32,
    paddings:        f32,
    #[theme_path = "theme::entry::height"]
    entry_height:    f32,
    #[theme_path = "theme::bar::height"]
    bar_height:      f32,
    #[theme_path = "theme::bar::label::padding"]
    label_padding:   f32,
    #[theme_path = "theme::bar::label::size"]
    bar_label_size:  f32,
    #[theme_path = "theme::bar::label::color"]
    bar_label_color: color::Rgba,
}


// === Entry Style ===

#[derive(Debug, Clone, Copy, Default, FromTheme)]
#[base_path = "theme::entry"]
#[allow(missing_docs)]
pub struct EntryStyle {
    corners_radius:      f32,
    selection_color:     color::Rgba,
    hover_color:         color::Rgba,
    #[theme_path = "theme::entry::text::padding_left"]
    text_padding_left:   f32,
    #[theme_path = "theme::entry::text::padding_bottom"]
    text_padding_bottom: f32,
    #[theme_path = "theme::entry::text::size"]
    text_size:           f32,
    #[theme_path = "theme::entry::text::color"]
    text_color:          color::Rgba,
}



// =============
// === Entry ===
// =============

// === Data ===

/// The model of the list entry. Displays the name of the project.
#[derive(Debug, Clone, CloneRef)]
struct Data {
    display_object: display::object::Instance,
    text:           text::Text,
}

impl Data {
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let display_object = display::object::Instance::new();
        let text = text::Text::new(app);
        display_object.add_child(&text);

        if let Some(text_layer) = text_layer {
            text.add_to_scene_layer(text_layer);
        }
        Self { display_object, text }
    }

    fn set_text(&self, text: ImString) {
        self.text.set_content(text);
    }
}


// === Entry ===

/// The list entry. Displays the name of the project.
#[derive(Debug, Clone, CloneRef)]
pub struct Entry {
    data: Data,
    frp:  grid_view::entry::EntryFrp<Self>,
}

impl display::Object for Entry {
    fn display_object(&self) -> &display::object::Instance {
        &self.data.display_object
    }
}

impl grid_view::Entry for Entry {
    type Model = ImString;
    type Params = ();

    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let frp = grid_view::entry::EntryFrp::<Self>::new();
        let data = Data::new(app, text_layer);

        let network = frp.network();
        let input = &frp.private().input;
        let out = &frp.private().output;

        let style_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let style = EntryStyle::from_theme(network, &style_frp);

        frp::extend! { network
            corners_radius <- style.update.map(|s| s.corners_radius);
            hover_color <- style.update.map(|s| s.hover_color.into());
            selection_color <- style.update.map(|s| s.selection_color.into());
            text_padding_left <- style.update.map(|s| s.text_padding_left);
            text_padding_bottom <- style.update.map(|s| s.text_padding_bottom);
            text_size <- style.update.map(|s| s.text_size);
            text_color <- style.update.map(|s| color::Lcha::from(s.text_color));

            eval input.set_model((text) data.set_text(text.clone_ref()));
            contour <- input.set_size.map(|s| grid_view::entry::Contour::rectangular(*s));
            out.contour <+ contour;
            out.highlight_contour <+ contour.map2(&corners_radius, |c,r| c.with_corners_radius(*r));
            out.hover_highlight_color <+ hover_color;
            out.selection_highlight_color <+ selection_color;
            width <- input.set_size.map(|s| s.x);
            _eval <- all_with(&width, &text_padding_left, f!((w, p) data.text.set_x(-w / 2.0 + p)));
            eval text_padding_bottom((p) data.text.set_y(*p));
            data.text.set_property_default <+ text_size.map(|s| text::Size(*s)).cloned_into_some();
            data.text.set_property_default <+ text_color.cloned_into_some();
        }
        style.init.emit(());
        Self { data, frp }
    }

    fn frp(&self) -> &grid_view::entry::EntryFrp<Self> {
        &self.frp
    }
}



// ==================
// === Background ===
// ==================

mod background {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style:Style) {
            let sprite_width: Var<Pixels> = "input_size.x".into();
            let sprite_height: Var<Pixels> = "input_size.y".into();
            let shadow_extent = style.get_number(theme::shadow_extent);
            let width = sprite_width - shadow_extent.px() * 2.0;
            let height = sprite_height - shadow_extent.px() * 2.0;
            let color = style.get_color(theme::background);
            let border_size = style.get_number(theme::bar::border_size);
            let bar_height = style.get_number(theme::bar::height);
            let corners_radius = style.get_number(theme::corners_radius);
            let rect = Rect((&width,&height)).corners_radius(corners_radius.px());
            let shape = rect.fill(color);

            let shadow = shadow::from_shape(rect.into(),style);

            let toolbar_border = Rect((width, border_size.px()))
                .translate_y(height / 2.0 - bar_height.px())
                .fill(style.get_color(theme::bar::border_color));

            (shadow + shape + toolbar_border).into()
        }
    }
}



// ===================
// === ProjectList ===
// ===================

/// The Project List GUI Component.
///
/// This is a list of projects in a nice frame with a title.
#[derive(Clone, CloneRef, Debug)]
pub struct ProjectList {
    network:        frp::Network,
    display_object: display::object::Instance,
    background:     background::View,
    caption:        text::Text,
    #[allow(missing_docs)]
    pub grid:       grid_view::scrollable::SelectableGridView<Entry>,
}

impl ProjectList {
    /// Create Project List Component.
    pub fn new(app: &Application) -> Self {
        let network = frp::Network::new("ProjectList");
        let display_object = display::object::Instance::new();
        let background = background::View::new();
        let caption = app.new_view::<text::Text>();
        let grid = grid_view::scrollable::SelectableGridView::new(app);
        display_object.add_child(&background);
        display_object.add_child(&caption);
        display_object.add_child(&grid);
        app.display.default_scene.layers.panel.add(&display_object);
        caption.set_content("Open Project");
        caption.add_to_scene_layer(&app.display.default_scene.layers.panel_text);

        let style_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let style = Style::from_theme(&network, &style_frp);

        frp::extend! { network
            init <- source::<()>();
            width <- style.update.map(|s| s.width);
            height <- style.update.map(|s| s.height);
            shadow_extent <- style.update.map(|s| s.shadow_extent);
            bar_height <- style.update.map(|s| s.bar_height);
            label_padding <- style.update.map(|s| s.label_padding);
            label_color <- style.update.map(|s| s.bar_label_color);
            label_size <- style.update.map(|s| s.bar_label_size);
            corners_radius <- style.update.map(|s| s.corners_radius);
            entry_height <- style.update.map(|s| s.entry_height);
            paddings <- style.update.map(|s| s.paddings);
            content_size <- all_with3(&width, &height, &init, |w,h,()| Vector2(*w,*h));
            size <- all_with3(&width, &height, &shadow_extent, |w, h, s|
                Vector2(w + s * 2.0, h + s * 2.0)
            );
            grid_size <- all_with3(&content_size, &bar_height, &paddings,
                |s, h, p| s - Vector2(0.0, *h) - Vector2(*p * 2.0, *p * 2.0)
            );
            grid_width <- grid_size.map(|s| s.x);
            caption_xy <- all_with3(&width,&height,&label_padding,
                |w,h,p| Vector2(-*w / 2.0 + *p, *h / 2.0 - p)
            );
            eval caption_xy ((xy) caption.set_xy(*xy));
            eval label_color((color) caption.set_property_default(color));
            eval label_size((size)  caption.set_property_default(text::Size(*size)));
            _eval <- all_with(&grid_width, &entry_height,
                f!((w, h) grid.set_entries_size(Vector2(*w, *h)))
            );
            eval size((size) background.set_size(*size););
            eval grid_size((size) grid.scroll_frp().resize(*size));
            eval corners_radius((r) grid.scroll_frp().set_corner_radius_bottom_left(*r));
            eval corners_radius((r) grid.scroll_frp().set_corner_radius_bottom_right(*r));
            grid_x <- grid_width.map(|width| -width / 2.0);
            grid_y <- all_with3(&content_size, &bar_height, &paddings, |s,h,p| s.y / 2.0 - *h - *p);
            _eval <- all_with(&grid_x, &grid_y, f!((x, y) grid.set_xy(Vector2(*x, *y))));
        }
        style.init.emit(());
        init.emit(());

        Self { network, display_object, background, caption, grid }
    }
}

impl display::Object for ProjectList {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
