//! This module defines a widget for displaying a list of entries of a component group and the name
//! of the component group.
//!
//! The widget is defined by the [`View`].
//!
//! To learn more about component groups, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "256"]

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::shortcut::Shortcut;
use ensogl_core::application::traits::*;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_group as theme;
use ensogl_list_view as list_view;
use ensogl_text as text;



// =================
// === Constants ===
// =================

const HEADER_FONT: &str = "DejaVuSans-Bold";



// ===============
// === Aliases ===
// ===============

type EntryId = list_view::entry::Id;


// ==========================
// === Shapes Definitions ===
// ==========================


// === Background ===

/// The background of the [`View`].
pub mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        below = [list_view::background];
        (style:Style, color:Vector4) {
            let sprite_width: Var<Pixels> = "input_size.x".into();
            let sprite_height: Var<Pixels> = "input_size.y".into();
            let color = Var::<Rgba>::from(color);
            // TODO[MC,WD]: We should use Plane here, but it has a bug - renders wrong color. See:
            //   https://github.com/enso-org/enso/pull/3373#discussion_r849054476
            let shape = Rect((&sprite_width, &sprite_height)).fill(color);
            shape.into()
        }
    }
}

/// The transparent overlay over Component Group View Header, used for capturing mouse events.
pub mod header_overlay {
    use super::*;

    ensogl_core::define_shape_system! {
        above = [background];
        () {
            let bg_color = Rgba::new(0.0,0.0,0.0,0.000_001);
            Plane().fill(bg_color).into()
        }
    }
}



// =======================
// === Header Geometry ===
// =======================

#[derive(Debug, Copy, Clone, Default)]
struct HeaderGeometry {
    height:         f32,
    padding_left:   f32,
    padding_right:  f32,
    padding_bottom: f32,
}

impl HeaderGeometry {
    fn from_style(style: &StyleWatchFrp, network: &frp::Network) -> frp::Sampler<Self> {
        let height = style.get_number(theme::header::height);
        let padding_left = style.get_number(theme::header::padding::left);
        let padding_right = style.get_number(theme::header::padding::right);
        let padding_bottom = style.get_number(theme::header::padding::bottom);

        frp::extend! { network
            init <- source_();
            theme <- all_with5(&init,&height,&padding_left,&padding_right,&padding_bottom,
                |_,&height,&padding_left,&padding_right,&padding_bottom|
                    Self{height,padding_left,padding_right,padding_bottom}
            );
            theme_sampler <- theme.sampler();
        }
        init.emit(());
        theme_sampler
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        /// Emit the `chose_header` event when it is selected
        ///
        /// The name reflects the current state of the List View. Used in debug scene, should
        /// be changed when implementing https://www.pivotaltracker.com/story/show/181414807
        /// and https://www.pivotaltracker.com/story/show/180892146.
        choose_header_if_selected(),
        set_header(String),
        set_header_selectable(bool),
        set_entries(list_view::entry::AnyModelProvider<list_view::entry::Label>),
        set_background_color(Rgba),
        set_size(Vector2),
    }
    Output {
        selected_entry(Option<EntryId>),
        chose_entry(EntryId),
        is_header_selected(bool),
        chose_header(),
        selection_size(Vector2<f32>),
        selection_position_target(Vector2<f32>),
    }
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, style: &StyleWatchFrp) {
        let network = &api.network;
        let input = &api.input;
        let out = &api.output;
        let header_text_size = style.get_number(theme::header::text::size);

        frp::extend! { network

            // === Geometry ===

            let header_geometry = HeaderGeometry::from_style(style, network);
            size_and_header_geometry <- all(&input.set_size, &header_geometry);
            eval size_and_header_geometry(((size, hdr_geom)) model.resize(*size, *hdr_geom));


            // === Header ===

            init <- source_();
            header_text_size <- all(&header_text_size, &init)._0();
            model.header.set_default_text_size <+ header_text_size.map(|v| text::Size(*v));
            _set_header <- input.set_header.map2(&size_and_header_geometry, f!(
                (text, (size, hdr_geom)) {
                    model.header_text.replace(text.clone());
                    model.update_header_width(*size, *hdr_geom);
                })
            );
            eval input.set_background_color((c)
            model.background.color.set(c.into()));


            // === Entries ===

            model.entries.set_background_color(HOVER_COLOR);
            model.entries.show_background_shadow(false);
            model.entries.set_background_corners_radius(0.0);
            model.entries.set_background_color <+ input.set_background_color;
            model.entries.set_entries <+ input.set_entries;
            out.selected_entry <+ model.entries.selected_entry;
            out.chose_entry <+ model.entries.chosen_entry.filter_map(|e| *e);


            // === Selection ===

            let is_header_selectable = &input.set_header_selectable;
            let moved_out_above = model.entries.tried_to_move_out_above.clone_ref();
            let mouse_over_header = model.header_overlay.events.mouse_over.clone_ref();
            select_header_after_moving_out <- moved_out_above.gate(is_header_selectable);
            select_header_after_hover <- mouse_over_header.gate(is_header_selectable);
            select_inside_list <- model.entries.selected_entry.filter_map(|entry| *entry);
            out.is_header_selected <+ select_header_after_moving_out.constant(true);
            out.is_header_selected <+ select_header_after_hover.constant(true);
            out.is_header_selected <+ select_inside_list.constant(false);

            out.selection_position_target <+ all_with4(
                &out.is_header_selected,
                &input.set_size,
                &header_geometry,
                &model.entries.selection_position_target,
                f!((h_sel, size, h, esp) model.selection_position(*h_sel, *size, *h, *esp))
            );


            // === Choosing Header ===

            out.chose_header <+ input.choose_header_if_selected.gate(&out.is_header_selected);
        }
        init.emit(());
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        use ensogl_core::application::shortcut::ActionType::*;
        (&[(Press, "enter", "choose_header_if_selected")])
            .iter()
            .map(|(a, b, c)| View::self_shortcut(*a, *b, *c))
            .collect()
    }
}



// =============
// === Model ===
// =============

/// The Model of the [`View`] component.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object: display::object::Instance,
    header:         text::Area,
    header_text:    Rc<RefCell<String>>,
    header_overlay: header_overlay::View,
    background:     background::View,
    entries:        list_view::ListView<list_view::entry::Label>,
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentGroup"
    }

    fn new(app: &Application, logger: &Logger) -> Self {
        let header_text = default();
        let display_object = display::object::Instance::new(&logger);
        let background = background::View::new(&logger);
        let header = text::Area::new(app);
        let header_overlay = header_overlay::View::new(&logger);
        let entries = app.new_view::<list_view::ListView<list_view::entry::Label>>();
        display_object.add_child(&background);
        display_object.add_child(&header);
        display_object.add_child(&header_overlay);
        display_object.add_child(&entries);

        header.set_font(HEADER_FONT);
        let label_layer = &app.display.default_scene.layers.label;
        header.add_to_scene_layer(label_layer);

        entries.hide_selection();

        Model { display_object, header, header_text, header_overlay, background, entries }
    }
}

impl Model {
    fn resize(&self, size: Vector2, header_geometry: HeaderGeometry) {
        // === Background ===

        self.background.size.set(size);


        // === Header Text ===

        let header_padding_left = header_geometry.padding_left;
        let header_text_x = -size.x / 2.0 + header_padding_left;
        let header_text_height = self.header.height.value();
        let header_padding_bottom = header_geometry.padding_bottom;
        let header_height = header_geometry.height;
        let header_center_y = size.y / 2.0 - header_height / 2.0;
        let header_bottom_y = header_center_y - header_height / 2.0;
        let header_text_y = header_bottom_y + header_text_height + header_padding_bottom;
        self.header.set_position_xy(Vector2(header_text_x, header_text_y));
        self.update_header_width(size, header_geometry);


        // === Header Overlay ===

        self.header_overlay.set_position_y(header_center_y);
        self.header_overlay.size.set(Vector2(size.x, header_height));


        // === Entries ===

        self.entries.resize(size - Vector2(0.0, header_height));
        self.entries.set_position_y(-header_height / 2.0);
    }

    fn update_header_width(&self, size: Vector2, header_geometry: HeaderGeometry) {
        let header_padding_left = header_geometry.padding_left;
        let header_padding_right = header_geometry.padding_right;
        let max_text_width = size.x - header_padding_left - header_padding_right;
        self.header.set_content_truncated(self.header_text.borrow().clone(), max_text_width);
    }

    fn selection_position(
        &self,
        is_header_selected: bool,
        size: Vector2,
        header_geometry: HeaderGeometry,
        entries_selection_position: Vector2,
    ) -> Vector2 {
        if is_header_selected {
            Vector2(0.0, size.y / 2.0 - header_geometry.height / 2.0)
        } else {
            self.entries.position().xy() + entries_selection_position
        }
    }
}



// ============
// === View ===
// ============

/// A widget for displaying the entries and name of a Component Group.
///
/// The widget is rendered as a header label, a list of entries below it, and a colored background.
/// It does not display the selection widget - because the selection jump between various Component
/// Groups, the parent (Component List Panel) should own the selection widget; the Component Group
/// View provides the information where the widget should be placed when it's focused.
///
/// To learn more about Component Groups, see the [Component Browser Design
/// Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
pub type View = component::ComponentView<Model, Frp>;
