//! Definition of single choice widget.

use crate::prelude::*;

use crate::component::node::input::widget::Entry;

use enso_frp as frp;
use ensogl::control::io::mouse;
use ensogl::display;
use ensogl::display::object::event;
use ensogl_component::drop_down::Dropdown;
use ensogl_hardcoded_theme as theme;



// =================
// === Constants ===
// =================

/// Height of the activation triangle shape.
pub const ACTIVATION_SHAPE_SIZE: Vector2 = Vector2(15.0, 11.0);

/// Gap between activation shape and the dropdown widget content.
pub const ACTIVATION_SHAPE_GAP: f32 = 5.0;

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
        set_entries    (Rc<Vec<Entry>>),
        current_value  (Option<ImString>),
        current_crumbs (span_tree::Crumbs),
        is_connected   (bool),
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
    dropdown:         Rc<RefCell<LazyDropdown>>,
    activation_shape: triangle::View,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &display::object::Instance {
        &self.display_object
    }

    fn new(_: &Config, ctx: &super::ConfigContext) -> Self {
        let app = ctx.app();
        //  ╭─display_object────────────────────╮
        //  │╭─content_wrapper─────────────────╮│
        //  ││ ╭ shape ╮ ╭ label_wrapper ────╮ ││
        //  ││ │       │ │                   │ ││
        //  ││ │       │ │                   │ ││
        //  ││ ╰───────╯ ╰───────────────────╯ ││
        //  │╰─────────────────────────────────╯│
        //  ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
        //  │ ◎ dropdown_wrapper size=0         │
        //  ╰───────────────────────────────────╯

        let activation_shape = triangle::View::new();
        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);

        let layers = &app.display.default_scene.layers;
        layers.label.add(&activation_shape);

        let display_object = display::object::Instance::new_named("widget::SingleChoice");
        let content_wrapper = display_object.new_child();
        content_wrapper.add_child(&activation_shape);
        let label_wrapper = content_wrapper.new_child();
        let dropdown_wrapper = display_object.new_child();

        display_object
            .use_auto_layout()
            .set_column_flow()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        content_wrapper
            .use_auto_layout()
            .set_gap_x(ACTIVATION_SHAPE_GAP)
            .set_children_alignment_left_center()
            .justify_content_center_y();

        label_wrapper
            .use_auto_layout()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        dropdown_wrapper.set_size((0.0, 0.0)).set_alignment_left_top();

        let config_frp = Frp::new();
        let dropdown = LazyDropdown::new(app, &config_frp.network);
        let dropdown = Rc::new(RefCell::new(dropdown));

        Self {
            config_frp,
            display_object,
            content_wrapper,
            dropdown_wrapper,
            label_wrapper,
            dropdown,
            activation_shape,
        }
        .init(ctx)
    }

    fn configure(&mut self, config: &Config, mut ctx: super::ConfigContext) {
        let input = &self.config_frp.public.input;

        let has_value = !ctx.span_node.is_insertion_point();
        let current_value: Option<ImString> =
            has_value.then(|| ctx.expression_at(ctx.span_node.span()).into());

        input.current_crumbs(ctx.span_node.crumbs.clone());
        input.current_value(current_value);
        input.set_entries(config.entries.clone());
        input.is_connected(ctx.info.subtree_connection.is_some());

        if has_value {
            ctx.modify_extension::<super::label::Extension>(|ext| ext.bold = true);
        }

        let config = match ctx.span_node.children.is_empty() {
            true => super::Configuration::always(super::label::Config),
            false => super::Configuration::always(super::hierarchy::Config),
        };
        let child_level = ctx.info.nesting_level;
        let child = ctx.builder.child_widget_of_type(ctx.span_node, child_level, Some(&config));
        self.label_wrapper.replace_children(&[child.root_object]);
    }
}

impl Widget {
    fn init(self, ctx: &super::ConfigContext) -> Self {
        let is_open = self.init_dropdown_focus(ctx);
        self.init_dropdown_values(ctx, is_open);
        self.init_activation_shape(ctx);
        self
    }

    fn init_dropdown_focus(&self, ctx: &super::ConfigContext) -> frp::Stream<bool> {
        let widgets_frp = ctx.frp();
        let focus_receiver = self.display_object.clone_ref();
        let focus_in = focus_receiver.on_event::<event::FocusIn>();
        let focus_out = focus_receiver.on_event::<event::FocusOut>();
        let network = &self.config_frp.network;
        let dropdown = &self.dropdown;
        let dropdown_frp = &self.dropdown.borrow();
        let dropdown_wrapper = &self.dropdown_wrapper;
        frp::extend! { network
            eval focus_in([dropdown, dropdown_wrapper](_) {
                dropdown.borrow_mut().lazy_init(&dropdown_wrapper);
            });
            readonly_set <- widgets_frp.set_read_only.on_true();
            do_open <- focus_in.gate_not(&widgets_frp.set_read_only);
            do_close <- any_(focus_out, readonly_set);
            is_open <- bool(&do_close, &do_open);
            dropdown_frp.set_open <+ is_open.on_change();

            // Close the dropdown after a short delay after selection. Because the dropdown
            // value application triggers operations that can introduce a few dropped frames,
            // we want to delay the dropdown closing animation after that is handled.
            // Otherwise the animation finishes within single frame, which looks bad.
            let close_after_selection_timer = frp::io::timer::Timeout::new(network);
            close_after_selection_timer.restart <+ dropdown_frp.user_select_action.constant(1);
            eval close_after_selection_timer.on_expired((()) focus_receiver.blur());

        }
        is_open
    }


    fn init_dropdown_values(&self, ctx: &super::ConfigContext, is_open: frp::Stream<bool>) {
        let network = &self.config_frp.network;
        let dropdown_frp = &self.dropdown.borrow();
        let config_frp = &self.config_frp;
        let widgets_frp = ctx.frp();

        frp::extend! { network
            current_value <- config_frp.current_value.on_change();
            entries <- config_frp.set_entries.on_change();
            entries_and_value <- all(&entries, &current_value);
            entries_and_value <- entries_and_value.debounce();
            dropdown_frp.set_all_entries <+ entries_and_value.map(|(e, _)| e.deref().clone());
            entries_and_value <- entries_and_value.buffered_gate(&is_open);

            selected_entry <- entries_and_value.map(|(e, v)| entry_for_current_value(e, v));
            dropdown_frp.set_selected_entries <+ selected_entry.map(|e| e.iter().cloned().collect());

            dropdown_entry <- dropdown_frp.selected_entries
                .map(|e: &HashSet<Entry>| e.iter().next().cloned());

            // Emit the output value only after actual user action. This prevents the
            // dropdown from emitting its initial value when it is opened, which can
            // represent slightly different version of code than actually written.
            submitted_entry <- dropdown_entry.sample(&dropdown_frp.user_select_action);
            dropdown_out_value <- submitted_entry.map(|e| e.as_ref().map(Entry::value));
            dropdown_out_import <- submitted_entry.map(|e| e.as_ref().and_then(Entry::required_import));

            widgets_frp.request_import <+ dropdown_out_import.unwrap();
            widgets_frp.value_changed <+ dropdown_out_value.map2(&config_frp.current_crumbs,
                move |t: &Option<ImString>, crumbs: &span_tree::Crumbs| (crumbs.clone(), t.clone())
            );

        };
    }

    fn init_activation_shape(&self, ctx: &super::ConfigContext) {
        let network = &self.config_frp.network;
        let config_frp = &self.config_frp;
        let widgets_frp = ctx.frp();
        let styles = ctx.styles();
        let activation_shape = &self.activation_shape;
        let focus_receiver = &self.display_object;
        frp::extend! { network
            is_hovered <- widgets_frp.on_port_hover.map2(&config_frp.current_crumbs, |h, crumbs| {
                h.on().map_or(false, |h| crumbs.starts_with(h))
            });
            is_connected_or_hovered <- config_frp.is_connected || is_hovered;
            activation_shape_theme <- is_connected_or_hovered.map(|is_connected_or_hovered| {
                if *is_connected_or_hovered {
                    Some(theme::widget::activation_shape::connected)
                } else {
                    Some(theme::widget::activation_shape::base)
                }
            });
            activation_shape_theme <- activation_shape_theme.on_change();
            eval activation_shape_theme([styles, activation_shape](path) {
                if let Some(path) = path {
                    let color = styles.get_color(path);
                    activation_shape.color.set(color.into());
                }
            });

            let dot_mouse_down = activation_shape.on_event::<mouse::Down>();
            dot_clicked <- dot_mouse_down.filter(mouse::is_primary);
            set_focused <- dot_clicked.map(f!([focus_receiver](_) !focus_receiver.is_focused()));
            eval set_focused([focus_receiver](focus) match focus {
                true => focus_receiver.focus(),
                false => focus_receiver.blur(),
            });

        };
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
        current_value.starts_with('(').and_option_from(|| {
            let current_value = current_value.trim_start_matches('(').trim_end_matches(')');
            all_entries.iter().find(|entry| {
                let trimmed_value = entry.value.trim_start_matches('(').trim_end_matches(')');
                current_value.starts_with(trimmed_value)
            })
        })
    });

    let with_fallback =
        with_partial_match.cloned().unwrap_or_else(|| Entry::from_value(current_value.clone()));
    Some(with_fallback)
}



// ====================
// === LazyDropdown ===
// ====================

/// A wrapper for dropdown that can be initialized lazily, with all required FRP endpoints to drive
/// it as if was just an ordinary view. Before calling `lazy_init` for the first time, the overhead
/// is minimal, as the actual dropdown view is not created.
#[derive(Debug)]
struct LazyDropdown {
    app: ensogl::application::Application,
    set_all_entries: frp::Any<Vec<Entry>>,
    set_selected_entries: frp::Any<HashSet<Entry>>,
    set_open: frp::Any<bool>,
    sampled_set_all_entries: frp::Sampler<Vec<Entry>>,
    sampled_set_selected_entries: frp::Sampler<HashSet<Entry>>,
    sampled_set_open: frp::Sampler<bool>,
    selected_entries: frp::Any<HashSet<Entry>>,
    user_select_action: frp::Any<()>,
    dropdown: Option<Dropdown<Entry>>,
}

impl LazyDropdown {
    fn new(app: &ensogl::application::Application, network: &frp::Network) -> Self {
        frp::extend! { network
            set_all_entries <- any(...);
            set_selected_entries <- any(...);
            set_open <- any(...);
            selected_entries <- any(...);
            user_select_action <- any(...);
            sampled_set_all_entries <- set_all_entries.sampler();
            sampled_set_selected_entries <- set_selected_entries.sampler();
            sampled_set_open <- set_open.sampler();
        }

        Self {
            app: app.clone_ref(),
            set_all_entries,
            set_selected_entries,
            set_open,
            selected_entries,
            user_select_action,
            sampled_set_all_entries,
            sampled_set_selected_entries,
            sampled_set_open,
            dropdown: None,
        }
    }

    /// Perform initialization that actually creates the dropdown. Should be done only once there is
    /// a request to open the dropdown.
    fn lazy_init(&mut self, parent: &display::object::Instance) {
        if self.dropdown.is_some() {
            return;
        }

        let dropdown = self.dropdown.insert(self.app.new_view::<Dropdown<Entry>>());
        parent.add_child(dropdown);
        self.app.display.default_scene.layers.above_nodes.add(&*dropdown);

        frp::extend! { _network
            dropdown.set_all_entries <+ self.sampled_set_all_entries;
            dropdown.set_selected_entries <+ self.sampled_set_selected_entries;
            dropdown.set_open <+ self.sampled_set_open;
            self.selected_entries <+ dropdown.selected_entries;
            self.user_select_action <+ dropdown.user_select_action;
        }

        dropdown.set_y(DROPDOWN_Y_OFFSET);
        dropdown.set_max_open_size(DROPDOWN_MAX_SIZE);
        dropdown.allow_deselect_all(true);
        dropdown.set_all_entries(self.sampled_set_all_entries.value());
        dropdown.set_selected_entries(self.sampled_set_selected_entries.value());
        dropdown.set_open(self.sampled_set_open.value());
    }
}
