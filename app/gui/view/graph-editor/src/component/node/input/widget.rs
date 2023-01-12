//! Definition of all hardcoded node widget variants and common widget FRP API.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::event;
use ensogl_component::drop_down::Dropdown;



// ==================
// === NodeWidget ===
// ==================

ensogl::define_endpoints_2! {
    Input {
        set_current_value(Option<ImString>),
        set_focused(bool),
    }
    Output {
        value_changed(Option<ImString>),
    }
}

/// Possible widgets for a node input.
///
/// Currently all widget types are hardcoded. This is likely to be a temporary solution. In the
/// future the set of widget types might be dynamic, similar to visualizations.
#[derive(Clone, Debug, CloneRef)]
pub enum Widget {
    /// A widget for selecting a single value from a list of available options.
    SingleChoice(SingleChoice),
}

impl Widget {
    /// Create a new node widget, selecting the appropriate widget type based on the provided
    /// argument info.
    pub fn new(
        app: &Application,
        argument_info: span_tree::ArgumentInfo,
        node_height: f32,
    ) -> Option<Self> {
        // TODO [PG] Support more widgets, use engine provided widget type.
        // https://www.pivotaltracker.com/story/show/184012753
        if !argument_info.tag_values.is_empty() {
            Some(Self::SingleChoice(SingleChoice::new(app, argument_info, node_height)))
        } else {
            None
        }
    }

    fn frp(&self) -> &Frp {
        match self {
            Self::SingleChoice(s) => &s.frp,
        }
    }
}

impl Deref for Widget {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        self.frp()
    }
}

impl display::Object for Widget {
    fn display_object(&self) -> &display::object::Instance {
        match self {
            Self::SingleChoice(s) => &s.display_object,
        }
    }
}

// =================
// === Dot Shape ===
// =================

/// Temporary dropdown activation shape definition.
pub mod dot {
    use super::*;
    ensogl::shape! {
        above = [
            crate::component::node::background,
            crate::component::node::input::port::hover
        ];
        (style:Style, color:Vector4) {
            let size   = Var::canvas_size();
            let radius = Min::min(size.x(),size.y()) / 2.0;
            let shape  = Rect(size).corners_radius(radius);
            shape.fill(color).into()
        }
    }
}



// ====================
// === SingleChoice ===
// ====================

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Clone, Debug, CloneRef)]
pub struct SingleChoice {
    display_object: display::object::Instance,
    frp:            Frp,
    #[allow(dead_code)]
    dropdown:       Dropdown<ImString>,
    /// temporary click handling
    activation_dot: dot::View,
}

impl SingleChoice {
    fn new(app: &Application, argument_info: span_tree::ArgumentInfo, node_height: f32) -> Self {
        let display_object = display::object::Instance::new();
        let dropdown = app.new_view::<Dropdown<ImString>>();
        display_object.add_child(&dropdown);
        app.display.default_scene.layers.above_nodes.add(&dropdown);
        let frp = Frp::new();
        let network = &frp.network;
        let input = &frp.input;
        let output = &frp.private.output;
        dropdown.set_y(-node_height);
        dropdown.set_max_open_size(Vector2(300.0, 500.0));

        let activation_dot = dot::View::new();
        let color: color::Rgba = color::Lcha::new(0.56708, 0.23249, 0.71372, 1.0).into();
        activation_dot.color.set(color.into());
        activation_dot.set_size((15.0, 15.0));
        activation_dot.set_y(-node_height / 2.0);
        display_object.add_child(&activation_dot);


        if !argument_info.tag_values.is_empty() {
            let entries: Vec<ImString> = argument_info.tag_values.iter().map(Into::into).collect();
            dropdown.set_all_entries(entries);
            dropdown.allow_deselect_all(true);
        } else {
            // TODO [PG]: Support dynamic entries.
            // https://www.pivotaltracker.com/story/show/184012743
            unimplemented!();
        }

        frp::extend! { network
            dropdown.set_selected_entries <+ input.set_current_value.map(|s| s.iter().cloned().collect());
            first_selected_entry <- dropdown.selected_entries.map(|e| e.iter().next().cloned());
            output.value_changed <+ first_selected_entry.on_change();

            let dot_clicked = activation_dot.events.mouse_down_primary.clone_ref();
            toggle_focus <- dot_clicked.map(f!([display_object](()) !display_object.is_focused()));
            set_focused <- any(toggle_focus, input.set_focused);
            eval set_focused([display_object](focus) match focus {
                true => display_object.focus(),
                false => display_object.blur(),
            });

            let focus_in = display_object.on_event::<event::FocusIn>();
            let focus_out = display_object.on_event::<event::FocusOut>();
            is_focused <- bool(&focus_out, &focus_in);

            dropdown.set_open <+ is_focused;
        }

        Self { display_object, frp, dropdown, activation_dot }
    }
}
