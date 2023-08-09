//! Definition of the `ActionBar` component for the `visualization::Container`.

use crate::prelude::*;
use ensogl::display::shape::*;

use crate::layers::MainNodeLayers;

use engine_protocol::language_server::ExecutionEnvironment;
use enso_config::ARGS;
use enso_frp as frp;
use ensogl::application::tooltip;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::display;
use ensogl_component::toggle_button;
use ensogl_component::toggle_button::ColorableShape;
use ensogl_component::toggle_button::ToggleButton;
use ensogl_hardcoded_theme::graph_editor::node::actions as theme;


// ==============
// === Export ===
// ==============

pub mod icon;



// ==================
// === Constants  ===
// ==================

const BUTTON_SIZE: f32 = 15.0;
const BUTTON_GAP: f32 = BUTTON_SIZE * 0.5;
/// Grow the hover area in x direction by this amount. Used to close the gap between action
/// icons and node.
const HOVER_EXTENSION: Vector2 = Vector2(15.0, 11.0);
/// The size of additional hover area that is drawn below the node background. Necessary to prevent
/// easily losing the hover state when moving the mouse towards the action bar.
const HOVER_BRIDGE_SIZE: Vector2 = Vector2(10.0, 26.0);
const HOVER_HIDE_DELAY_MS: i32 = 20;
const VISIBILITY_TOOLTIP_LABEL: &str = "Show preview";
const DISABLE_OUTPUT_CONTEXT_TOOLTIP_LABEL: &str = "Don't write to files and databases";
const ENABLE_OUTPUT_CONTEXT_TOOLTIP_LABEL: &str = "Allow writing to files and databases";
const FREEZE_TOOLTIP_LABEL: &str = "Freeze";
const SKIP_TOOLTIP_LABEL: &str = "Skip";



// ===========
// === Frp ===
// ===========

ensogl::define_endpoints! {
    Input {
        set_visibility                  (bool),
        /// Set whether the `visibility` icon should be toggled on or off.
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
        mouse_enter            (),
        mouse_leave             (),
        action_visibility     (bool),
        /// The last visibility selection by the user. Ignores changes to the
        /// visibility chooser icon made through the input API.
        user_action_visibility (bool),
        action_context_switch (bool),
        action_freeze         (bool),
        action_skip           (bool),
    }
}



// ========================
// === Action Bar Icons ===
// ========================

#[derive(Clone, CloneRef, Debug, display::Object)]
struct Icons {
    display_object: display::object::Instance,
    visibility:     ToggleButton<icon::visibility::Shape>,
    context_switch: ContextSwitchButton,
    freeze:         ToggleButton<icon::freeze::Shape>,
    skip:           ToggleButton<icon::skip::Shape>,
}

impl Icons {
    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new_named("Icons");
        display_object
            .use_auto_layout()
            .reverse_columns()
            .set_gap((BUTTON_GAP, 0.0))
            .set_padding_xy(HOVER_EXTENSION)
            .set_children_alignment_left_center();

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

        // The visibility icon looks smaller than the other ones, so we make it bigger. This is a
        // purely aesthetic adjustment.
        visibility.set_size((BUTTON_SIZE * 1.2, BUTTON_SIZE * 1.2));
        visibility.set_margin_all(-BUTTON_SIZE * 0.2);

        Self { display_object, visibility, context_switch, freeze, skip }
    }

    fn set_visibility(&self, visible: bool) {
        self.visibility.set_visibility(visible);
        self.context_switch.set_visibility(visible);
        self.freeze.set_visibility(visible);
        self.skip.set_visibility(visible);
        let pointer_events_val = if visible { 0.0 } else { 1.0 };
        self.visibility.view().disable_pointer_events.set(pointer_events_val);
        self.freeze.view().disable_pointer_events.set(pointer_events_val);
        self.skip.view().disable_pointer_events.set(pointer_events_val);
    }

    fn set_read_only(&self, read_only: bool) {
        self.context_switch.set_read_only(read_only);
        self.freeze.set_read_only(read_only);
        self.skip.set_read_only(read_only);
    }
}

fn labeled_button<Icon: ColorableShape>(app: &Application, label: &str) -> ToggleButton<Icon> {
    let tooltip_style = tooltip::Style::set_label(label.to_owned());
    let button = ToggleButton::new(app, tooltip_style);
    button.set_size((BUTTON_SIZE, BUTTON_SIZE));
    button
}



// =============================
// === Context Switch Button ===
// =============================

/// A button to enable/disable the output context for a particular node. It holds two buttons
/// internally for each shape, but only one is shown at a time, based on the execution environment
/// which sets the global permission for the output context.
#[derive(Clone, CloneRef, Debug, display::Object)]
struct ContextSwitchButton {
    globally_enabled: Rc<Cell<bool>>,
    disable_button:   ToggleButton<icon::disable_output_context::Shape>,
    enable_button:    ToggleButton<icon::enable_output_context::Shape>,
    display_object:   display::object::Instance,
}

impl ContextSwitchButton {
    fn enable(app: &Application) -> Self {
        let display_object = display::object::Instance::new_named("ContextSwitchButton");
        display_object.use_auto_layout().set_children_alignment_left_center();

        let disable_button = labeled_button(app, DISABLE_OUTPUT_CONTEXT_TOOLTIP_LABEL);
        let enable_button = labeled_button(app, ENABLE_OUTPUT_CONTEXT_TOOLTIP_LABEL);
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
                self.replace_children(&[&self.disable_button]);
                self.globally_enabled.set(true);
            } else {
                self.replace_children(&[&self.enable_button]);
                self.globally_enabled.set(false);
            }
        }
    }

    fn set_visibility(&self, visible: bool) {
        self.disable_button.set_visibility(visible);
        self.enable_button.set_visibility(visible);
        let pointer_events_val = if visible { 0.0 } else { 1.0 };
        self.disable_button.view().disable_pointer_events.set(pointer_events_val);
        self.enable_button.view().disable_pointer_events.set(pointer_events_val);
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



// ========================
// === Action Bar Model ===
// ========================

#[derive(Clone, CloneRef, Debug, display::Object)]
struct Model {
    display_object:         display::object::Instance,
    hover_area:             Rectangle,
    /// Additional hover area that is drawn below the node background. Serves as an always active
    /// hover area, but does not block the interactions of widgets on nearby nodes. It also
    /// provides additional "bridge" between the node and the action bar, so that the hover
    /// state is not lost when moving the mouse. It is drawn below the node background to not
    /// cover it, as it can also have its own mouse interactions.
    hover_area_below_nodes: Rectangle,
    icons:                  Icons,
    styles:                 StyleWatch,
}

impl Model {
    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new_named("ActionBar");
        let hover_area = Rectangle::new();
        let hover_area_below_nodes = Rectangle::new();
        hover_area.set_color(INVISIBLE_HOVER_COLOR);
        hover_area.allow_grow().set_alignment_left_center();
        hover_area_below_nodes.set_color(INVISIBLE_HOVER_COLOR);
        hover_area_below_nodes
            .allow_grow_x()
            .set_size_y(HOVER_BRIDGE_SIZE.y)
            .set_alignment_right_center()
            .set_margin_right(-HOVER_BRIDGE_SIZE.x);

        let icons = Icons::new(app);

        display_object.add_child(&hover_area);
        display_object.add_child(&hover_area_below_nodes);
        display_object.add_child(&icons);

        let styles = StyleWatch::new(&scene.style_sheet);

        ensogl::shapes_order_dependencies! {
            scene => {
                compound::rectangle::shape -> icon::visibility;
                compound::rectangle::shape -> icon::disable_output_context;
                compound::rectangle::shape -> icon::enable_output_context;
                compound::rectangle::shape -> icon::freeze;
                compound::rectangle::shape -> icon::skip;
            }
        }

        Self { display_object, hover_area, hover_area_below_nodes, icons, styles }
    }

    fn set_visibility(&self, visible: bool) {
        self.icons.set_visibility(visible);
        self.hover_area.set_pointer_events(visible);
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
#[derive(Clone, CloneRef, Debug, Deref, display::Object)]
#[allow(missing_docs)]
pub struct ActionBar {
    #[deref]
    pub frp: Frp,
    #[display_object]
    model:   Rc<Model>,
}

impl ActionBar {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app));
        let frp = Frp::new();
        ActionBar { frp, model }.init_frp(app)
    }

    fn init_frp(self, app: &Application) -> Self {
        let network = &self.frp.network;
        let frp = &self.frp;
        let model = &self.model;

        frp::extend! { network
            // === Read-only mode ===

            eval frp.set_read_only((read_only) model.icons.set_read_only(*read_only));


            // === Input Processing ===

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

            let mouse_enter = model.display_object.on_event::<mouse::Enter>();
            let mouse_leave = model.display_object.on_event::<mouse::Leave>();
            visibility_mouse <- bool(&mouse_leave,&mouse_enter);

            let hide_delay = frp::io::timer::Timeout::new(network);
            hide_delay.restart <+ mouse_leave.constant(HOVER_HIDE_DELAY_MS);
            visibility_delay <- bool(&hide_delay.on_expired, &mouse_leave);

            visibility       <- frp.set_visibility || visibility_mouse;
            visibility       <- visibility || visibility_delay;
            visibility       <- visibility && frp.show_on_hover;
            eval visibility ((t) model.set_visibility(*t));


            // === Icon Actions ===

            frp.source.action_visibility <+ model.icons.visibility.state;
            frp.source.user_action_visibility <+ model.icons.visibility.last_user_state;
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

        let scene = &app.display.default_scene;
        let context_switch_color_scheme = toggle_button::ColorScheme {
            toggled: Some(model.styles.get_color(theme::context_switch::toggled).into()),
            ..toggle_button::default_color_scheme(&scene.style_sheet)
        };
        model.icons.context_switch.set_color_scheme(&context_switch_color_scheme);

        frp.show_on_hover.emit(true);
        visibility_init.emit(false);

        self
    }

    /// Configure this action bar to use specific node layers.
    pub fn set_layers(&self, main: &MainNodeLayers) {
        main.action_bar_icons.add(&self.model.icons);
        main.below_body_hover.add(&self.model.hover_area_below_nodes);
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
        visibility_icon.view().display_object().emit_event(mouse::Enter::default());

        // We expect the button to be hovered by the mouse
        assert!(visibility_icon.frp.is_hovered.value());

        // We expect the tooltip to be shown now
        assert_eq!(app.frp.tooltip.value().content(), Some("Show preview"));

        // Move the mouse away again
        visibility_icon.view().display_object().emit_event(mouse::Leave::default());

        // We expect the tooltip to be gone
        assert_eq!(app.frp.tooltip.value().content(), None);
    }
}
