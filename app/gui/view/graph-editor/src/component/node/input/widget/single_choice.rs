//! Definition of single choice widget.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::event;
use ensogl_component::drop_down::Dropdown;


use crate::component::node::input::widget::Entry;
use crate::component::node::input::widget::Metadata;

/// =================
/// === Constants ===
/// =================

pub const ACTIVATION_SHAPE_COLOR: color::Lch = color::Lch::new(0.56708, 0.23249, 0.71372);
const ACTIVATION_SHAPE_Y_OFFSET: f32 = 15.0;
pub const ACTIVATION_SHAPE_SIZE: Vector2 = Vector2(15.0, 11.0);
/// Distance between the dropdown and the bottom of the port.
const DROPDOWN_Y_OFFSET: f32 = 5.0;

const LABEL_CONFIG: Metadata =
    Metadata::always(super::label::Config { ignore_offset: true, bold: true });

// ======================
// === Triangle Shape ===
// ======================

/// Temporary dropdown activation shape definition.
pub mod triangle {
    use super::*;
    ensogl::shape! {
        above = [
            crate::component::node::background,
            crate::component::node::drag_area,
            crate::component::node::input::widget::port
        ];
        alignment = left_bottom;
        (style:Style, color:Vector4) {
            let size   = Var::canvas_size();
            let radius = 1.0.px();
            let shrink = &radius * 2.0;
            let shape  = Triangle(size.x() - &shrink, size.y() - &shrink)
                .flip_y()
                .grow(radius);
            shape.fill(color).into()
        }
    }
}



// ====================
// === SingleChoice ===
// ====================

/// SingleChoice widget configuration options.
#[derive(Debug, Clone, PartialEq)]
pub struct Config {
    /// Default label to display when no value is selected. Will use argument name if not provided.
    pub label:   Option<ImString>,
    /// Entries that should be displayed by the widget, as proposed by language server. This
    /// list is not exhaustive. The widget implementation might present additional
    /// options or allow arbitrary user input.
    pub entries: Rc<Vec<Entry>>,
}

ensogl::define_endpoints_2! {
    Input {
        set_entries(Rc<Vec<Entry>>),
        content(ImString),
        current_value(Option<ImString>),
        current_crumbs(span_tree::Crumbs),
    }
}

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Debug)]
pub struct Widget {
    config_frp:       Frp,
    display_object:   display::object::Instance,
    #[allow(dead_code)]
    content_wrapper:  display::object::Instance,
    #[allow(dead_code)]
    dropdown_wrapper: display::object::Instance,
    #[allow(dead_code)]
    dropdown:         Dropdown<Entry>,
    label_wrapper:    display::object::Instance,
    #[allow(dead_code)]
    activation_shape: triangle::View,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &display::object::Instance {
        &self.display_object
    }

    fn new(_: &Config, app: &Application, widgets_frp: &super::WidgetsFrp) -> Self {
        //  ╭─display_object───────────────╮
        //  │╭─content_wrapper────────────╮│
        //  ││╭ shape ╮ ╭─label_wrapper──╮││
        //  │││       │ │                │││
        //  │││       │ │                │││
        //  ││╰───────╯ ╰────────────────╯││
        //  │╰────────────────────────────╯│
        //  ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
        //  │ ◎ dropdown_wrapper size=0    │
        //  ╰──────────────────────────────╯

        let dot_color = color::Rgba::from(ACTIVATION_SHAPE_COLOR.with_alpha(1.0)).into();
        let activation_shape = triangle::View::new();
        activation_shape.color.set(dot_color);
        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);
        // activation_shape.set_y(-ACTIVATION_SHAPE_SIZE.y() - ACTIVATION_SHAPE_Y_OFFSET);


        let display_object = display::object::Instance::new();
        display_object
            .use_auto_layout()
            .set_column_flow()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        let content_wrapper = display_object.new_child();
        content_wrapper
            .use_auto_layout()
            .set_children_alignment_left_center()
            .justify_content_center_y();
        content_wrapper.add_child(&activation_shape);
        let label_wrapper = content_wrapper.new_child();
        label_wrapper
            .use_auto_layout()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        let dropdown_wrapper = display_object.new_child();
        dropdown_wrapper.set_size((0.0, 0.0)).set_alignment_left_top();

        let dropdown = app.new_view::<Dropdown<Entry>>();
        dropdown_wrapper.add_child(&dropdown);
        let layers = &app.display.default_scene.layers;
        layers.above_nodes.add(&dropdown);
        dropdown.set_y(-20.0);
        dropdown.set_max_open_size(Vector2(300.0, 500.0));
        dropdown.allow_deselect_all(true);


        let config_frp = Frp::new();
        let network = &config_frp.network;
        let input = &config_frp.private.input;

        let focus_receiver = display_object.clone_ref();

        frp::extend! { network
            let focus_in = focus_receiver.on_event::<event::FocusIn>();
            let focus_out = focus_receiver.on_event::<event::FocusOut>();
            is_open <- bool(&focus_out, &focus_in);
        }
        frp::extend! { network

            let dot_mouse_down = activation_shape.on_event::<mouse::Down>();
            dot_clicked <- dot_mouse_down.filter(mouse::is_primary);
            set_focused <- dot_clicked.map(f!([focus_receiver](_) !focus_receiver.is_focused()));
            eval set_focused([focus_receiver](focus) match focus {
                true => focus_receiver.focus(),
                false => focus_receiver.blur(),
            });
        }
        frp::extend! { network

            // Close the dropdown after a short delay after selection. Because the dropdown
            // value application triggers operations that can introduce a few dropped frames,
            // we want to delay the dropdown closing animation after that is handled.
            // Otherwise the animation finishes within single frame, which looks bad.
            let close_after_selection_timer = frp::io::timer::Timeout::new(&network);
            close_after_selection_timer.restart <+ dropdown.user_select_action.constant(1);
            eval close_after_selection_timer.on_expired((()) focus_receiver.blur());
        }
        frp::extend! { network
            entries_and_value <- all(&input.set_entries, &input.current_value);
            entries_and_value <- entries_and_value.debounce();
            entries_and_value <- entries_and_value.buffered_gate(&is_open);
        }
        frp::extend! { network
            dropdown.set_all_entries <+ entries_and_value.map(|(rc, _)| rc.deref().clone());
            dropdown.set_open <+ is_open.on_change();
        }
        frp::extend! { network

            selected_entry <- entries_and_value.map(|(e, v)| entry_for_current_value(e, v));
            dropdown.set_selected_entries <+ selected_entry.map(|e| e.iter().cloned().collect());

            dropdown_entry <- dropdown.selected_entries.map(|e| e.iter().next().cloned());
            // Emit the output value only after actual user action. This prevents the
            // dropdown from emitting its initial value when it is opened, which can
            // represent slightly different version of code than actually written.
            submitted_entry <- dropdown_entry.sample(&dropdown.user_select_action);
            dropdown_out_value <- submitted_entry.map(|e| e.as_ref().map(Entry::value));
            dropdown_out_import <- submitted_entry.map(|e| e.as_ref().and_then(Entry::required_import));
        }
        frp::extend! { network

            widgets_frp.request_import <+ dropdown_out_import.unwrap();
            widgets_frp.value_changed <+ dropdown_out_value.map2(&input.current_crumbs,
                move |t: &Option<ImString>, crumbs: &span_tree::Crumbs| (crumbs.clone(), t.clone())
            );
        };

        Self {
            config_frp,
            display_object,
            content_wrapper,
            dropdown_wrapper,
            dropdown,
            activation_shape,
            label_wrapper,
        }
    }

    fn configure(&mut self, config: &Config, ctx: super::ConfigContext) {
        let input = &self.config_frp.public.input;

        let is_placeholder = ctx.span_tree_node.is_expected_argument();
        let min_offset = if is_placeholder { 1.0f32 } else { 0.0 };
        let offset = min_offset.max(ctx.span_tree_node.sibling_offset.as_usize() as f32);
        self.display_object.set_margin_left(offset * super::hierarchy::SPACE_GLYPH_WIDTH);

        let has_value = !ctx.span_tree_node.is_insertion_point();
        let current_value: Option<ImString> =
            has_value.then(|| ctx.expression_at(ctx.span_tree_node.span()).into());
        let content = current_value
            .clone()
            .or_else(|| config.label.clone())
            .or_else(|| ctx.span_tree_node.kind.argument_name().map(Into::into))
            .unwrap_or_default();

        input.current_crumbs(ctx.span_tree_node.crumbs.clone());
        input.current_value(current_value);
        input.content(content);
        input.set_entries(config.entries.clone());

        // Do not increment the depth. If the dropdown is displayed, it should also display
        // its arguments.

        self.content_wrapper.remove_all_children();

        if ctx.span_tree_node.is_chained() {
            let mut chain_children = ctx.span_tree_node.chain_children_iter();
            if let Some(first_child) = chain_children.next() {
                let label =
                    ctx.builder.child_widget_of_type(first_child, ctx.depth, LABEL_CONFIG, 0);
                self.label_wrapper.replace_children(&[label]);
            }
            let content_children = chain_children
                .map(|child| ctx.builder.child_widget(child, ctx.depth))
                .collect_vec();
            self.content_wrapper.replace_children(&content_children);
        } else {
            let label =
                ctx.builder.child_widget_of_type(ctx.span_tree_node, ctx.depth, LABEL_CONFIG, 0);
            self.label_wrapper.replace_children(&[label]);
        }
    }
}


fn entry_for_current_value(
    all_entries: &[Entry],
    current_value: &Option<ImString>,
) -> Option<Entry> {
    let current_value = current_value.clone()?;
    let found_entry = all_entries.iter().find(|entry| entry.value.as_ref() == current_value);
    let with_partial_match = found_entry.or_else(|| {
        // Handle parentheses in current value. Entries with parenthesized expressions will match if
        // they start with the same expression as the current value. That way it is still matched
        // once extra arguments are added to the nested function call.
        if current_value.starts_with('(') {
            let current_value = current_value.trim_start_matches('(').trim_end_matches(')');
            all_entries.iter().find(|entry| {
                let trimmed_value = entry.value.trim_start_matches('(').trim_end_matches(')');
                current_value.starts_with(trimmed_value)
            })
        } else {
            None
        }
    });

    let with_fallback =
        with_partial_match.cloned().unwrap_or_else(|| Entry::from_value(current_value.clone()));
    Some(with_fallback)
}
