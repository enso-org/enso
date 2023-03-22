//! Definition of the `ActionBar` component for the `visualization::Container`.

use crate::prelude::*;
use ensogl::display::shape::*;

use enso_config::ARGS;
use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display;
use ensogl_component::toggle_button;
use ensogl_component::toggle_button::ColorableShape;
use ensogl_component::toggle_button::ToggleButton;
use ensogl_hardcoded_theme as theme;


// ==============
// === Export ===
// ==============

pub mod icon;



// ==================
// === Constants  ===
// ==================

const BUTTON_PADDING: f32 = 0.5;
const BUTTON_OFFSET: f32 = 0.5;
/// Grow the hover area in x direction by this amount. Used to close the gap between action
/// icons and node.
const HOVER_EXTENSION_X: f32 = 15.0;


// ===============
// === Shapes  ===
// ===============

/// Invisible rectangular area that can be hovered.
mod hover_area {
    use super::*;

    ensogl::shape! {
        (style: Style, corner_radius: f32) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let rect                 = Rect((&width,&height));
            let rect_rounded         = rect.corners_radius(corner_radius);
            let rect_filled          = rect_rounded.fill(INVISIBLE_HOVER_COLOR);
            rect_filled.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

ensogl::define_endpoints! {
    Input {
        set_size                    (Vector2),
        set_visibility              (bool),
        set_action_visibility_state (bool),
        set_action_skip_state       (bool),
        set_action_freeze_state     (bool),
        show_on_hover               (bool),
    }

    Output {
        mouse_over        (),
        mouse_out         (),
        action_visibility (bool),
        action_freeze     (bool),
        action_skip       (bool),
    }
}



// ========================
// === Action Bar Icons ===
// ========================

#[derive(Clone, CloneRef, Debug)]
struct Icons {
    display_object: display::object::Instance,
    freeze:         ToggleButton<icon::freeze::Shape>,
    visibility:     ToggleButton<icon::visibility::Shape>,
    skip:           ToggleButton<icon::skip::Shape>,
}

impl Icons {
    fn new() -> Self {
        let display_object = display::object::Instance::new();
        let freeze = ToggleButton::new();
        let visibility = ToggleButton::new();
        let skip = ToggleButton::new();
        display_object.add_child(&visibility);
        if ARGS.groups.feature_preview.options.skip_and_freeze.value {
            display_object.add_child(&freeze);
            display_object.add_child(&skip);
        }
        Self { display_object, freeze, visibility, skip }
    }

    fn set_visibility(&self, visible: bool) {
        self.freeze.frp.set_visibility(visible);
        self.skip.frp.set_visibility(visible);
        self.visibility.frp.set_visibility(visible);
    }
}

impl display::Object for Icons {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ========================
// === Action Bar Model ===
// ========================

#[derive(Clone, CloneRef, Debug)]
struct Model {
    display_object: display::object::Instance,
    hover_area:     hover_area::View,
    icons:          Icons,
    size:           Rc<Cell<Vector2>>,
    shapes:         compound::events::MouseEvents,
    styles:         StyleWatch,
}

impl Model {
    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let hover_area = hover_area::View::new();
        let icons = Icons::new();
        let shapes = compound::events::MouseEvents::default();
        let size = default();
        let styles = StyleWatch::new(&scene.style_sheet);

        shapes.add_sub_shape(&hover_area);
        shapes.add_sub_shape(&icons.freeze.view());
        shapes.add_sub_shape(&icons.visibility.view());
        shapes.add_sub_shape(&icons.skip.view());

        ensogl::shapes_order_dependencies! {
            scene => {
                hover_area -> icon::freeze;
                hover_area -> icon::visibility;
                hover_area -> icon::skip;
            }
        }

        Self { display_object, hover_area, icons, size, shapes, styles }.init()
    }

    fn init(self) -> Self {
        self.add_child(&self.hover_area);
        self.add_child(&self.icons);
        self
    }

    fn place_button_in_slot<T: ColorableShape>(&self, button: &ToggleButton<T>, index: usize) {
        let icon_size = self.icon_size();
        let index = index as f32;
        let padding = BUTTON_PADDING;
        let offset = BUTTON_OFFSET;
        button.set_x(((1.0 + padding) * index + offset) * icon_size.x);
        button.frp.set_size(icon_size);
    }

    fn icon_size(&self) -> Vector2 {
        Vector2::new(self.size.get().y, self.size.get().y)
    }

    fn layout_hover_area_to_cover_buttons(&self, button_count: usize) {
        let button_count = button_count as f32;
        let size = self.size.get();
        let padding = BUTTON_PADDING;
        let offset = BUTTON_OFFSET;
        let hover_padding = 1.0;
        let button_width = self.icon_size().x;
        let hover_width =
            button_width * (button_count + hover_padding + offset + padding) + HOVER_EXTENSION_X;
        let hover_height = button_width * 2.0;
        let hover_ara_size = Vector2::new(hover_width, hover_height);
        self.hover_area.set_size(hover_ara_size);
        let center_offset = -size.x / 2.0 + hover_ara_size.x / 2.0;
        let padding_offset = -0.5 * hover_padding * button_width - HOVER_EXTENSION_X / 2.0;
        self.hover_area.set_x(center_offset + padding_offset);
    }

    fn set_size(&self, size: Vector2) {
        self.size.set(size);
        self.icons.set_x(-size.x / 2.0);

        self.place_button_in_slot(&self.icons.visibility, 0);
        if ARGS.groups.feature_preview.options.skip_and_freeze.value {
            self.place_button_in_slot(&self.icons.skip, 1);
            self.place_button_in_slot(&self.icons.freeze, 2);
        }

        let buttons_count = if ARGS.groups.feature_preview.options.skip_and_freeze.value {
            // Toggle visualization, skip and freeze buttons.
            3
        } else {
            // Toggle visualization button only.
            1
        };
        self.layout_hover_area_to_cover_buttons(buttons_count);

        // The appears smaller than the other ones, so this is an aesthetic adjustment.
        self.icons.visibility.set_scale_xy(Vector2::new(1.2, 1.2));
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ==================
// === Action Bar ===
// ==================

/// UI for executing actions on a node.
///
/// Layout
/// ------
/// ```text
///    / ----------------------------- \
///    | <icon1> <icon2> <icon3>       |
///    \ ----------------------------- /
/// ```
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct ActionBar {
    pub frp: Frp,
    model:   Rc<Model>,
}

impl Deref for ActionBar {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl ActionBar {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app));
        let frp = Frp::new();
        ActionBar { frp, model }.init_frp()
    }

    fn init_frp(self) -> Self {
        let network = &self.frp.network;
        let frp = &self.frp;
        let model = &self.model;

        frp::extend! { network

            // === Input Processing ===

            eval frp.set_size                    ((size)  model.set_size(*size));
            eval frp.set_visibility              ((t)     model.icons.set_visibility(*t));
            eval frp.set_action_visibility_state ((state) model.icons.visibility.set_state(state));
            eval frp.set_action_skip_state ((state) model.icons.skip.set_state(state));
            eval frp.set_action_freeze_state ((state) model.icons.freeze.set_state(state));


            // === Mouse Interactions ===

            visibility_init  <- source::<bool>();
            visibility_mouse <- bool(&model.shapes.mouse_out,&model.shapes.mouse_over);
            visibility       <- any(&visibility_init,&visibility_mouse);
            visibility       <-  visibility && frp.show_on_hover;
            eval visibility ((t) model.icons.set_visibility(*t));


            // === Icon Actions ===

            frp.source.action_skip       <+ model.icons.skip.state;
            frp.source.action_freeze     <+ model.icons.freeze.state;
            frp.source.action_visibility <+ model.icons.visibility.state;
        }

        let color_scheme = toggle_button::ColorScheme {
            non_toggled: Some(
                model
                    .styles
                    .get_color(theme::graph_editor::node::actions::button::non_toggled)
                    .into(),
            ),
            toggled: Some(
                model.styles.get_color(theme::graph_editor::node::actions::button::toggled).into(),
            ),
            hovered: Some(
                model.styles.get_color(theme::graph_editor::node::actions::button::hovered).into(),
            ),
            ..default()
        };

        model.icons.freeze.frp.set_color_scheme(&color_scheme);
        model.icons.skip.frp.set_color_scheme(&color_scheme);
        model.icons.visibility.frp.set_color_scheme(&color_scheme);

        frp.show_on_hover.emit(true);
        visibility_init.emit(false);

        self
    }
}

impl display::Object for ActionBar {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}
