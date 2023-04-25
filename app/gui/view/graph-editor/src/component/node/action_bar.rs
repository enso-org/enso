//! Definition of the `ActionBar` component for the `visualization::Container`.

use crate::prelude::*;
use ensogl::display::shape::*;

use crate::ExecutionEnvironment;

use enso_config::ARGS;
use enso_frp as frp;
use ensogl::application::tooltip;
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
const VISIBILITY_TOOLTIP_LABEL: &str = "Show preview";
const DISABLE_OUTPUT_CONTEXT_TOOLTIP_LABEL: &str = "Don't write to files and databases";
const ENABLE_OUTPUT_CONTEXT_TOOLTIP_LABEL: &str = "Allow writing to files and databases";
const FREEZE_TOOLTIP_LABEL: &str = "Freeze";
const SKIP_TOOLTIP_LABEL: &str = "Skip";


// ===============
// === Shapes  ===
// ===============

/// Invisible rectangular area that can be hovered.
mod hover_area {
    use super::*;

    ensogl::shape! {
        alignment = center;
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
        set_size                        (Vector2),
        set_visibility                  (bool),
        set_action_visibility_state     (bool),
        set_action_skip_state           (bool),
        set_action_freeze_state         (bool),
        /// Set whether the output context is explicitly enabled: `Some(true/false)` for
        /// enabled/disabled; `None` for no context switch expression.
        set_action_context_switch_state (Option<bool>),
        show_on_hover                   (bool),
        set_execution_environment       (ExecutionEnvironment),
        /// Set the read-only mode for the buttons.
        set_read_only                   (bool),
    }

    Output {
        mouse_over            (),
        mouse_out             (),
        action_visibility     (bool),
        action_context_switch (bool),
        action_freeze         (bool),
        action_skip           (bool),
    }
}



// ========================
// === Action Bar Icons ===
// ========================

#[derive(Clone, CloneRef, Debug)]
struct Icons {
    display_object: display::object::Instance,
    visibility:     ToggleButton<icon::visibility::Shape>,
    context_switch: ContextSwitchButton,
    freeze:         ToggleButton<icon::freeze::Shape>,
    skip:           ToggleButton<icon::skip::Shape>,
}

impl Icons {
    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let visibility = labeled_button(app, VISIBILITY_TOOLTIP_LABEL);
        let context_switch = ContextSwitchButton::enable(app);
        let freeze = labeled_button(app, FREEZE_TOOLTIP_LABEL);
        let skip = labeled_button(app, SKIP_TOOLTIP_LABEL);
        display_object.add_child(&visibility);
        display_object.add_child(&context_switch);
        if ARGS.groups.feature_preview.options.skip_and_freeze.value {
            display_object.add_child(&freeze);
            display_object.add_child(&skip);
        }
        Self { display_object, visibility, context_switch, freeze, skip }
    }

    fn set_visibility(&self, visible: bool) {
        self.visibility.frp.set_visibility(visible);
        self.context_switch.set_visibility(visible);
        self.freeze.frp.set_visibility(visible);
        self.skip.frp.set_visibility(visible);
    }

    fn set_read_only(&self, read_only: bool) {
        self.context_switch.set_read_only(read_only);
        self.freeze.frp.set_read_only(read_only);
        self.skip.frp.set_read_only(read_only);
    }

    fn set_color_scheme(&self, color_scheme: &toggle_button::ColorScheme) {
        self.visibility.frp.set_color_scheme(color_scheme);
        self.context_switch.set_color_scheme(color_scheme);
        self.freeze.frp.set_color_scheme(color_scheme);
        self.skip.frp.set_color_scheme(color_scheme);
    }
}

impl display::Object for Icons {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

fn labeled_button<Icon: ColorableShape>(app: &Application, label: &str) -> ToggleButton<Icon> {
    let tooltip_style = tooltip::Style::set_label(label.to_owned());
    ToggleButton::new(app, tooltip_style)
}



// =============================
// === Context Switch Button ===
// =============================

/// A button to enable/disable the output context for a particular node. It holds two buttons
/// internally for each shape, but only one is shown at a time, based on the execution environment
/// which sets the global permission for the output context.
#[derive(Clone, CloneRef, Debug)]
struct ContextSwitchButton {
    globally_enabled: Rc<Cell<bool>>,
    disable_button:   ToggleButton<icon::disable_output_context::Shape>,
    enable_button:    ToggleButton<icon::enable_output_context::Shape>,
    display_object:   display::object::Instance,
}

impl ContextSwitchButton {
    fn enable(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let disable_button = labeled_button(app, DISABLE_OUTPUT_CONTEXT_TOOLTIP_LABEL);
        let enable_button = labeled_button(app, ENABLE_OUTPUT_CONTEXT_TOOLTIP_LABEL);
        disable_button.set_size((100.pc(), 100.pc()));
        enable_button.set_size((100.pc(), 100.pc()));
        display_object.add_child(&enable_button);
        let globally_enabled = Rc::new(Cell::new(false));
        Self { globally_enabled, disable_button, enable_button, display_object }
    }

    /// Set the button's on/off state based on whether the output context is explicitly enabled for
    /// this node. `output_context_enabled` is `Some(true/false)` for enabled/disabled; `None` for
    /// no context switch expression.
    fn set_state(&self, output_context_enabled: Option<bool>) {
        let disable_button_active = !output_context_enabled.unwrap_or(true);
        self.disable_button.set_state(disable_button_active);
        let enable_button_active = output_context_enabled.unwrap_or(false);
        self.enable_button.set_state(enable_button_active);
    }

    /// Swap the buttons if the execution environment changed.
    fn set_execution_environment(&self, environment: &ExecutionEnvironment) {
        if environment.output_context_enabled() != self.globally_enabled.get() {
            if environment.output_context_enabled() {
                self.remove_child(&self.enable_button);
                self.add_child(&self.disable_button);
                self.globally_enabled.set(true);
            } else {
                self.remove_child(&self.disable_button);
                self.add_child(&self.enable_button);
                self.globally_enabled.set(false);
            }
        }
    }

    fn set_visibility(&self, visible: bool) {
        self.disable_button.set_visibility(visible);
        self.enable_button.set_visibility(visible);
    }

    fn set_read_only(&self, read_only: bool) {
        self.disable_button.set_read_only(read_only);
        self.enable_button.set_read_only(read_only);
    }

    fn set_color_scheme(&self, color_scheme: &toggle_button::ColorScheme) {
        self.disable_button.set_color_scheme(color_scheme);
        self.enable_button.set_color_scheme(color_scheme);
    }
}

impl display::Object for ContextSwitchButton {
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
        let icons = Icons::new(app);
        let shapes = compound::events::MouseEvents::default();
        let size = default();
        let styles = StyleWatch::new(&scene.style_sheet);

        shapes.add_sub_shape(&hover_area);
        shapes.add_sub_shape(&icons.visibility.view());
        shapes.add_sub_shape(&icons.context_switch.disable_button.view());
        shapes.add_sub_shape(&icons.context_switch.enable_button.view());
        shapes.add_sub_shape(&icons.freeze.view());
        shapes.add_sub_shape(&icons.skip.view());

        ensogl::shapes_order_dependencies! {
            scene => {
                hover_area -> icon::visibility;
                hover_area -> icon::disable_output_context;
                hover_area -> icon::enable_output_context;
                hover_area -> icon::freeze;
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

    fn place_button_in_slot(&self, button: &dyn display::Object, index: usize) {
        let icon_size = self.icon_size();
        let index = index as f32;
        let padding = BUTTON_PADDING;
        let offset = BUTTON_OFFSET;
        button.set_x(((1.0 + padding) * index + offset) * icon_size.x);
        button.set_size(icon_size);
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
        self.place_button_in_slot(&self.icons.context_switch, 1);
        if ARGS.groups.feature_preview.options.skip_and_freeze.value {
            self.place_button_in_slot(&self.icons.skip, 2);
            self.place_button_in_slot(&self.icons.freeze, 3);
        }

        let buttons_count = if ARGS.groups.feature_preview.options.skip_and_freeze.value {
            // Toggle visualization, skip and freeze buttons.
            4
        } else {
            // Toggle visualization button only.
            2
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
            // === Read-only mode ===

            eval frp.set_read_only((read_only) model.icons.set_read_only(*read_only));


            // === Input Processing ===

            eval frp.set_size                    ((size)  model.set_size(*size));
            eval frp.set_visibility              ((t)     model.icons.set_visibility(*t));
            eval frp.set_action_visibility_state ((state) model.icons.visibility.set_state(state));
            eval frp.set_action_skip_state ((state) model.icons.skip.set_state(state));
            eval frp.set_action_freeze_state ((state) model.icons.freeze.set_state(state));
            eval frp.set_action_context_switch_state ((state)
                model.icons.context_switch.set_state(*state)
            );
            eval frp.set_execution_environment ((environment)
                model.icons.context_switch.set_execution_environment(environment)
            );


            // === Mouse Interactions ===

            visibility_init  <- source::<bool>();
            visibility_mouse <- bool(&model.shapes.mouse_out,&model.shapes.mouse_over);
            visibility       <- any(&visibility_init,&visibility_mouse);
            visibility       <-  visibility && frp.show_on_hover;
            eval visibility ((t) model.icons.set_visibility(*t));


            // === Icon Actions ===

            frp.source.action_visibility <+ model.icons.visibility.state;
            frp.source.action_skip <+ model.icons.skip.state;
            frp.source.action_freeze <+ model.icons.freeze.state;
            disable_context_button_clicked <- model.icons.context_switch.disable_button.is_pressed.on_true();
            enable_context_button_clicked <- model.icons.context_switch.enable_button.is_pressed.on_true();
            output_context_disabled <- model.icons.context_switch.disable_button.state
                .sample(&disable_context_button_clicked);
            output_context_enabled <- model.icons.context_switch.enable_button.state
                .sample(&enable_context_button_clicked);
            frp.source.action_context_switch <+ any(&output_context_disabled, &output_context_enabled);
            // Setting the state of the context switch button is necessary because e.g. toggling
            // the "enable" button when there's a "disable" expression should cause the "disable"
            // button to change state as well.
            frp.set_action_context_switch_state <+ output_context_disabled.map2(
                &model.icons.context_switch.enable_button.state,
                |disabled, enabled| {
                    match (disabled, enabled) {
                        (true, _) => Some(false),
                        (false, false) => None,
                        (false, true) => {
                            error!("Invalid node action bar button state: context switch buttons were both on.");
                            Some(true)
                        }
                    }
                }
            );
            frp.set_action_context_switch_state <+ output_context_enabled.map2(
                &model.icons.context_switch.disable_button.state,
                |enabled, disabled| {
                    match (enabled, disabled) {
                        (true, _) => Some(true),
                        (false, false) => None,
                        (false, true) => {
                            error!("Invalid node action bar button state: context switch buttons were both on.");
                            Some(false)
                        }
                    }
                }
            );
        }

        use theme::graph_editor::node::actions;
        let color_scheme = toggle_button::ColorScheme {
            non_toggled: Some(model.styles.get_color(actions::button::non_toggled).into()),
            toggled: Some(model.styles.get_color(actions::button::toggled).into()),
            hovered: Some(model.styles.get_color(actions::button::hovered).into()),
            ..default()
        };
        let context_switch_color_scheme = toggle_button::ColorScheme {
            non_toggled: Some(model.styles.get_color(actions::context_switch::non_toggled).into()),
            toggled: Some(model.styles.get_color(actions::context_switch::toggled).into()),
            hovered: Some(model.styles.get_color(actions::context_switch::hovered).into()),
            ..default()
        };
        model.icons.set_color_scheme(&color_scheme);
        model.icons.context_switch.set_color_scheme(&context_switch_color_scheme);

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



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tooltips() {
        let app = Application::new("root");
        let action_bar = ActionBar::new(&app);
        let visibility_icon = &action_bar.model.icons.visibility;

        // By default, the tooltip shouldn't be shown
        assert_eq!(app.frp.tooltip.value().content(), None);

        // Move the mouse over the visibility button
        visibility_icon.view().events_deprecated.mouse_over.emit(());

        // We expect the button to be hovered by the mouse
        assert!(visibility_icon.frp.is_hovered.value());

        // We expect the tooltip to be shown now
        assert_eq!(app.frp.tooltip.value().content(), Some("Show preview"));

        // Move the mouse away again
        visibility_icon.view().events_deprecated.mouse_out.emit(());

        // We expect the tooltip to be gone
        assert_eq!(app.frp.tooltip.value().content(), None);
    }
}
