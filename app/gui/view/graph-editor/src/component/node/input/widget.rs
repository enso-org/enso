use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display;
use ensogl_component::drop_down::Dropdown;

// ==================
// === NodeWidget ===
// ==================

ensogl::define_endpoints_2! {
    Input {
        set_current_value(Option<String>),
        set_focused(bool),
    }
    Output {
        value_changed(Option<String>),
    }
}

/// Possible widgets for a node input.
#[derive(Clone, Debug)]
pub enum NodeWidget {
    /// A widget for selecting a single value from a list of available options.
    SingleChoice(SingleChoice),
}

impl NodeWidget {
    /// Create a new node widget, selecting the appropriate widget type based on the provided
    /// argument info.
    pub fn new(
        app: &Application,
        argument_info: span_tree::ArgumentInfo,
        node_height: f32,
    ) -> Option<Self> {
        // TODO: support more widgets, use engine provided widget type
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

impl Deref for NodeWidget {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        self.frp()
    }
}

impl display::Object for NodeWidget {
    fn display_object(&self) -> &display::object::Instance {
        match self {
            Self::SingleChoice(s) => &s.display_object,
        }
    }
}

// ====================
// === SingleChoice ===
// ====================

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Clone, Debug)]
pub struct SingleChoice {
    display_object: display::object::Instance,
    frp:            Frp,
    #[allow(dead_code)]
    dropdown:       Dropdown<String>,
}

impl SingleChoice {
    fn new(app: &Application, argument_info: span_tree::ArgumentInfo, node_height: f32) -> Self {
        let display_object = display::object::Instance::new();
        let dropdown = app.new_view::<Dropdown<String>>();
        display_object.add_child(&dropdown);
        let frp = Frp::new();
        let network = &frp.network;
        let input = &frp.input;
        let output = &frp.private.output;
        dropdown.set_y(-node_height);

        if !argument_info.tag_values.is_empty() {
            dropdown.set_all_entries(argument_info.tag_values.clone());
        } else {
            // TODO: support dynamic entries
            unimplemented!();
        }

        frp::extend! { network
            dropdown.set_selected_entries <+ input.set_current_value.map(|s| s.iter().cloned().collect());
            first_selected_entry <- dropdown.selected_entries.map(|e| e.iter().cloned().next());
            output.value_changed <+ first_selected_entry.on_change();
            dropdown.set_opened <+ input.set_focused;
        }

        Self { display_object, frp, dropdown }
    }
}
