//! Definition of single choice widget.

use crate::prelude::*;

use crate::component::node::input::area::TEXT_SIZE;
use crate::component::node::input::widget::Entry;
use enso_frp as frp;
use ensogl::control::io::mouse;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::event;
use ensogl_component::drop_down::Dropdown;
use ensogl_component::text;



/// =================
/// === Constants ===
/// =================

/// Color of the activation triangle shape.
pub const ACTIVATION_SHAPE_COLOR: color::Lch = color::Lch::new(0.56708, 0.23249, 0.71372);

/// Height of the activation triangle shape.
pub const ACTIVATION_SHAPE_SIZE: Vector2 = Vector2(15.0, 11.0);

/// Distance between the top of the dropdown list and the bottom of the widget.
const DROPDOWN_Y_OFFSET: f32 = -20.0;

/// Maximum allowed size of the dropdown list. If the list needs to be longer or wider than allowed
/// by these values, it will receive a scroll bar.
const DROPDOWN_MAX_SIZE: Vector2 = Vector2(300.0, 500.0);

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
            crate::component::node::input::port::shape
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
#[allow(dead_code)]
pub struct Widget {
    config_frp:       Frp,
    display_object:   display::object::Instance,
    content_wrapper:  display::object::Instance,
    dropdown_wrapper: display::object::Instance,
    label_wrapper:    display::object::Instance,
    args_wrapper:     display::object::Instance,
    dropdown:         Dropdown<Entry>,
    label:            text::Text,
    activation_shape: triangle::View,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &display::object::Instance {
        &self.display_object
    }

    fn new(_: &Config, ctx: &super::ConfigContext) -> Self {
        let app = ctx.app();
        let widgets_frp = ctx.frp();
        //  ╭─display_object─────────────────────────────────────╮
        //  │╭─content_wrapper──────────────────────────────────╮│
        //  ││╭ shape ╮ ╭ label_wrapper ────╮ ╭ args_wrapper ─╮ ││
        //  │││       │ │                   │ │               │ ││
        //  │││       │ │                   │ │               │ ││
        //  ││╰───────╯ ╰───────────────────╯ ╰───────────────╯ ││
        //  │╰──────────────────────────────────────────────────╯│
        //  ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
        //  │ ◎ dropdown_wrapper size=0                          │
        //  ╰────────────────────────────────────────────────────╯

        let layers = &app.display.default_scene.layers;
        let dot_color = color::Rgba::from(ACTIVATION_SHAPE_COLOR.with_alpha(1.0)).into();
        let activation_shape = triangle::View::new();
        activation_shape.color.set(dot_color);
        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);

        let label = text::Text::new(app);
        layers.above_nodes_text.add(&label);
        label.set_property_default(text::Size(TEXT_SIZE));
        label.set_y(TEXT_SIZE);

        let dropdown = app.new_view::<Dropdown<Entry>>();
        layers.above_nodes.add(&dropdown);
        dropdown.set_y(DROPDOWN_Y_OFFSET);
        dropdown.set_max_open_size(DROPDOWN_MAX_SIZE);
        dropdown.allow_deselect_all(true);

        let display_object = display::object::Instance::new();
        let content_wrapper = display_object.new_child();
        content_wrapper.add_child(&activation_shape);
        let label_wrapper = content_wrapper.new_child();
        label_wrapper.add_child(&label);
        let args_wrapper = content_wrapper.new_child();
        let dropdown_wrapper = display_object.new_child();
        dropdown_wrapper.add_child(&dropdown);


        display_object
            .use_auto_layout()
            .set_column_flow()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        content_wrapper
            .use_auto_layout()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        args_wrapper
            .use_auto_layout()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        dropdown_wrapper.set_size((0.0, 0.0)).set_alignment_left_top();
        label_wrapper.set_size_y(TEXT_SIZE);



        let config_frp = Frp::new();
        let network = &config_frp.network;
        let input = &config_frp.private.input;

        let focus_receiver = display_object.clone_ref();

        frp::extend! { network
            init <- source::<()>();

            let focus_in = focus_receiver.on_event::<event::FocusIn>();
            let focus_out = focus_receiver.on_event::<event::FocusOut>();
            is_open <- bool(&focus_out, &focus_in);

            let dot_mouse_down = activation_shape.on_event::<mouse::Down>();
            dot_clicked <- dot_mouse_down.filter(mouse::is_primary);
            set_focused <- dot_clicked.map(f!([focus_receiver](_) !focus_receiver.is_focused()));
            eval set_focused([focus_receiver](focus) match focus {
                true => focus_receiver.focus(),
                false => focus_receiver.blur(),
            });

            // Close the dropdown after a short delay after selection. Because the dropdown
            // value application triggers operations that can introduce a few dropped frames,
            // we want to delay the dropdown closing animation after that is handled.
            // Otherwise the animation finishes within single frame, which looks bad.
            let close_after_selection_timer = frp::io::timer::Timeout::new(network);
            close_after_selection_timer.restart <+ dropdown.user_select_action.constant(1);
            eval close_after_selection_timer.on_expired((()) focus_receiver.blur());

            current_value     <- input.current_value.on_change();
            entries           <- input.set_entries.on_change();
            entries_and_value <- all(&entries, &current_value);
            entries_and_value <- entries_and_value.debounce();
            entries_and_value <- entries_and_value.buffered_gate(&is_open);

            dropdown.set_all_entries <+ entries_and_value.map(|(rc, _)| rc.deref().clone());
            dropdown.set_open <+ is_open.on_change();

            selected_entry <- entries_and_value.map(|(e, v)| entry_for_current_value(e, v));
            dropdown.set_selected_entries <+ selected_entry.map(|e| e.iter().cloned().collect());

            dropdown_entry <- dropdown.selected_entries.map(|e| e.iter().next().cloned());
            // Emit the output value only after actual user action. This prevents the
            // dropdown from emitting its initial value when it is opened, which can
            // represent slightly different version of code than actually written.
            submitted_entry <- dropdown_entry.sample(&dropdown.user_select_action);
            dropdown_out_value <- submitted_entry.map(|e| e.as_ref().map(Entry::value));
            dropdown_out_import <- submitted_entry.map(|e| e.as_ref().and_then(Entry::required_import));

            label.set_content <+ input.content.on_change();
            label_width <- label.width.on_change();
            eval label_width((w) { label_wrapper.set_size_x(*w); });

            has_value <- current_value.map(|v| v.is_some());
            has_value <- all(&has_value, &init)._0();
            eval has_value([label] (&has_value) {
                let color = if has_value {
                    color::Lcha::new(0.0, 0.0, 0.0, 1.0)
                } else {
                    color::Lcha::new(0.5, 0.0, 0.0, 1.0)
                };
                let weight = if has_value { text::Weight::Bold } else { text::Weight::Normal };
                label.set_property_default(color);
                label.set_property_default(weight);
            });

            widgets_frp.request_import <+ dropdown_out_import.unwrap();
            widgets_frp.value_changed <+ dropdown_out_value.map2(&input.current_crumbs,
                move |t: &Option<ImString>, crumbs: &span_tree::Crumbs| (crumbs.clone(), t.clone())
            );
        };

        init.emit(());

        Self {
            config_frp,
            display_object,
            content_wrapper,
            dropdown_wrapper,
            label_wrapper,
            args_wrapper,
            dropdown,
            activation_shape,
            label,
        }
    }

    fn configure(&mut self, config: &Config, ctx: super::ConfigContext) {
        let input = &self.config_frp.public.input;

        let has_value = !ctx.span_tree_node.is_insertion_point();
        let current_value: Option<ImString> =
            has_value.then(|| ctx.expression_at(ctx.span_tree_node.span()).into());

        input.current_crumbs(ctx.span_tree_node.crumbs.clone());
        input.current_value(current_value);
        input.set_entries(config.entries.clone());


        let mut label_expr_span = ctx.span_tree_node.span();

        if ctx.span_tree_node.is_chained() {
            let mut chain = ctx.span_tree_node.clone().chain_children_iter();
            if let Some(first_child) = chain.next() {
                label_expr_span = first_child.span();
            };

            // Do not increment the depth. If the dropdown is displayed, it should also display
            // its arguments.
            let children =
                chain.map(|child| ctx.builder.child_widget(child, ctx.state.depth)).collect_vec();
            self.args_wrapper.replace_children(&children);
        } else {
            self.args_wrapper.remove_all_children();
        }

        let content: ImString = has_value
            .then(|| ctx.expression_at(label_expr_span).into())
            .or_else(|| config.label.clone())
            .or_else(|| ctx.span_tree_node.kind.argument_name().map(Into::into))
            .unwrap_or_default();

        input.content(content);
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
