//! This module defines a widget for displaying a list of entries of a component group and the name
//! of the component group.
//!
//! The widget is defined by the [`View`].
//!
//! To learn more about component groups, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

#![recursion_limit = "512"]
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

use ensogl_core::application::traits::*;
use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::shortcut::Shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_group as theme;
use ensogl_list_view as list_view;
use ensogl_text as text;


// ==============
// === Export ===
// ==============

pub mod entry;
pub mod wide;

pub use entry::View as Entry;



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
            let color = Var::<color::Rgba>::from(color);
            Plane().fill(color).into()
        }
    }
}


// === Header Overlay ===

/// The transparent overlay over Component Group View Header, used for capturing mouse events.
pub mod header_overlay {
    use super::*;

    use ensogl_core::display::shape::constants::HOVER_COLOR;

    ensogl_core::define_shape_system! {
        above = [background];
        () {
            let bg_color = HOVER_COLOR;
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
        /// Accept the currently selected suggestion. Should be bound to "Suggestion Acceptance Key"
        /// described in
        /// [Component Browser Design Doc](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#key-binding-dictionary)
        accept_suggestion(),
        set_header(String),
        set_entries(list_view::entry::AnyModelProvider<Entry>),
        set_color(color::Rgba),
        set_dimmed(bool),
        set_width(f32),
    }
    Output {
        selected_entry(Option<entry::Id>),
        suggestion_accepted(entry::Id),
        expression_accepted(entry::Id),
        is_header_selected(bool),
        header_accepted(),
        selection_size(Vector2<f32>),
        selection_position_target(Vector2<f32>),
        size(Vector2<f32>)
    }
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, app: &Application, model: &Model, style: &StyleWatchFrp) {
        let network = &api.network;
        let mouse_position = app.display.default_scene.mouse.frp.position.clone_ref();
        let input = &api.input;
        let out = &api.output;
        let header_text_font = style.get_text(theme::header::text::font);
        let header_text_size = style.get_number(theme::header::text::size);


        // === Geometry ===

        frp::extend! { network
            let header_geometry = HeaderGeometry::from_style(style, network);
            height <- all_with(&input.set_entries, &header_geometry, |entries, header_geom| {
                entries.entry_count() as f32 * list_view::entry::HEIGHT + header_geom.height
            });
            out.size <+ all_with(&input.set_width, &height, |w, h| Vector2(*w, *h));
            size_and_header_geometry <- all(&out.size, &header_geometry);
            eval size_and_header_geometry(((size, hdr_geom)) model.resize(*size, *hdr_geom));
        }


        // === Colors ===

        fn mix(colors: &(color::Rgba, color::Rgba), coefficient: &f32) -> color::Rgba {
            color::mix(colors.0, colors.1, *coefficient)
        }
        let app_bg_color = style.get_color(ensogl_hardcoded_theme::application::background);
        let header_intensity = style.get_number(theme::header::text::color_intensity);
        let bg_intensity = style.get_number(theme::background_color_intensity);
        let dimmed_intensity = style.get_number(theme::dimmed_color_intensity);
        let entry_text_color = style.get_color(theme::entries::text::color);
        frp::extend! { network
            init <- source_();
            one <- init.constant(1.0);
            intensity <- input.set_dimmed.switch(&one, &dimmed_intensity);
            app_bg_color <- all(&app_bg_color, &init)._0();
            app_bg_and_input_color <- all(&app_bg_color, &input.set_color);
            main_color <- app_bg_and_input_color.all_with(&intensity, mix);
            app_bg_and_main_color <- all(&app_bg_color, &main_color);
            header_color <- app_bg_and_main_color.all_with(&header_intensity, mix);
            bg_color <- app_bg_and_main_color.all_with(&bg_intensity, mix);
            app_bg_and_entry_text_color <- all(&app_bg_color, &entry_text_color);
            entry_color_with_intensity <- app_bg_and_entry_text_color.all_with(&intensity, mix);
            entry_color_sampler <- entry_color_with_intensity.sampler();
        }
        let params = entry::Params { color: entry_color_sampler };
        model.entries.set_entry_params_and_recreate_entries(params);


        // === Header ===

        frp::extend! { network
            header_text_font <- all(&header_text_font, &init)._0();
            model.header.set_font <+ header_text_font;
            header_text_size <- all(&header_text_size, &init)._0();
            model.header.set_default_text_size <+ header_text_size.map(|v| text::Size(*v));
            _set_header <- input.set_header.map2(&size_and_header_geometry, f!(
                (text, (size, hdr_geom)) {
                    model.header_text.replace(text.clone());
                    model.update_header_width(*size, *hdr_geom);
                })
            );
            model.header.set_default_color <+ header_color;
            eval bg_color((c) model.background.color.set(c.into()));
        }


        // === Suggestion Acceptance ===

        frp::extend! { network
            accepted_entry <- model.entries.selected_entry.sample(&input.accept_suggestion);
            out.suggestion_accepted <+ accepted_entry.filter_map(|e| *e);
            header_accepted_by_frp <- input.accept_suggestion.gate(&out.is_header_selected);
            header_accepted_by_mouse <- model.header_overlay.events.mouse_down.constant(());
            header_accepted <- any(header_accepted_by_frp, header_accepted_by_mouse);
            out.header_accepted <+ header_accepted;
            out.expression_accepted <+ model.entries.chosen_entry.filter_map(|e| *e);
        }


        // === Entries ===

        frp::extend! { network
            model.entries.set_entries <+ input.set_entries;
            out.selected_entry <+ model.entries.selected_entry;
        }


        // === Selection ===

        let overlay_events = &model.header_overlay.events;
        frp::extend! { network
            let moved_out_above = model.entries.tried_to_move_out_above.clone_ref();
            is_mouse_over <- bool(&overlay_events.mouse_out, &overlay_events.mouse_over);
            mouse_moved <- mouse_position.on_change().constant(());
            mouse_moved_over_header <- mouse_moved.gate(&is_mouse_over);

            select_header <- any(moved_out_above, mouse_moved_over_header, out.header_accepted);
            deselect_header <- model.entries.selected_entry.filter_map(|entry| *entry);
            out.is_header_selected <+ bool(&deselect_header, &select_header);
            model.entries.select_entry <+ select_header.constant(None);

            out.selection_position_target <+ all_with4(
                &out.is_header_selected,
                &out.size,
                &header_geometry,
                &model.entries.selection_position_target,
                f!((h_sel, size, h, esp) model.selection_position(*h_sel, *size, *h, *esp))
            );
        }

        init.emit(());
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        use ensogl_core::application::shortcut::ActionType::*;
        (&[(Press, "tab", "accept_suggestion")])
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
    entries:        list_view::ListView<Entry>,
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
        let entries = app.new_view::<list_view::ListView<Entry>>();
        entries.set_style_prefix(entry::STYLE_PATH);
        entries.set_background_color(HOVER_COLOR);
        entries.show_background_shadow(false);
        entries.set_background_corners_radius(0.0);
        display_object.add_child(&background);
        display_object.add_child(&header);
        display_object.add_child(&header_overlay);
        display_object.add_child(&entries);

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



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use enso_frp::future::EventOutputExt;
    use ensogl_core::control::io::mouse;
    use ensogl_list_view::entry::AnyModelProvider;

    macro_rules! expect_entry_selected {
        ($cgv:ident, $id:expr$(, $argv:tt)?) => {
            assert_eq!($cgv.selected_entry.value(), Some($id)$(, $argv)?);
            assert!(!$cgv.is_header_selected.value()$(, $argv)?);
        };
    }

    macro_rules! expect_header_selected {
        ($cgv:ident$(, $argv:tt)?) => {
            assert_eq!($cgv.selected_entry.value(), None$(, $argv)?);
            assert!($cgv.is_header_selected.value()$(, $argv)?);
        };
    }

    struct Test {
        app: Application,
        cgv: View,
    }

    impl Test {
        fn set_up() -> Self {
            let app = Application::new("root");
            ensogl_hardcoded_theme::builtin::light::register(&app);
            ensogl_hardcoded_theme::builtin::light::enable(&app);
            let cgv = View::new(&app);
            cgv.set_width(100.0);
            let entries = AnyModelProvider::<Entry>::new(vec!["Entry 1", "Entry 2", "Entry 3"]);
            cgv.set_entries(entries);
            Test { app, cgv }
        }

        fn move_mouse_over_header(&self) {
            let pos_over_header = Vector2(0.0, self.cgv.size.value().y / 2.0 - 10.0);
            let mouse_move = Vector2(5.0, 0.0);
            self.cgv.model().header_overlay.events.mouse_over.emit(());
            self.app.display.default_scene.mouse.frp.position.emit(pos_over_header);
            self.app.display.default_scene.mouse.frp.position.emit(pos_over_header + mouse_move);
        }
    }

    #[test]
    fn navigating_entries_with_keyboard() {
        let Test { app: _, cgv } = Test::set_up();
        cgv.model().entries.move_selection_down();
        expect_entry_selected!(cgv, 0);
        cgv.model().entries.move_selection_down();
        expect_entry_selected!(cgv, 1);
        cgv.model().entries.move_selection_up();
        expect_entry_selected!(cgv, 0);
        cgv.model().entries.move_selection_up();
        expect_header_selected!(cgv);
        cgv.model().entries.move_selection_up();
        expect_header_selected!(cgv);
        cgv.model().entries.move_selection_down();
        expect_entry_selected!(cgv, 0);
    }

    #[test]
    fn navigating_entries_with_keyboard_after_hovering_header() {
        let test = Test::set_up();
        let cgv = &test.cgv;
        cgv.model().entries.move_selection_down();
        expect_entry_selected!(cgv, 0);
        test.move_mouse_over_header();
        expect_header_selected!(cgv);
        cgv.model().entries.move_selection_down();
        expect_entry_selected!(cgv, 0);
        cgv.model().entries.move_selection_down();
        expect_entry_selected!(cgv, 1);
        test.move_mouse_over_header();
        expect_header_selected!(cgv);
        cgv.model().entries.move_selection_down();
        expect_entry_selected!(cgv, 0);
    }

    #[test]
    fn accepting_header() {
        let Test { app: _, cgv } = Test::set_up();
        cgv.model().entries.move_selection_down();
        expect_entry_selected!(cgv, 0);
        let expected_header_accepted = cgv.header_accepted.next_event();
        cgv.model().header_overlay.events.mouse_down.emit(mouse::Button::Button0);
        expected_header_accepted.expect();
        expect_header_selected!(cgv);

        let expected_header_accepted = cgv.header_accepted.next_event();
        cgv.accept_suggestion();
        expected_header_accepted.expect();
        expect_header_selected!(cgv);

        // When header is not selected, we should not be able to accept it with keyboard
        let expected_header_accepted = cgv.header_accepted.next_event();
        cgv.model().entries.select_entry(0);
        expect_entry_selected!(cgv, 0);
        cgv.accept_suggestion();
        expected_header_accepted.expect_not();
    }
}
