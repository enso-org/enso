//! Definition of single choice widget.

use crate::component::node::input::widget::prelude::*;
use crate::prelude::*;

use crate::component::node::input::widget::label;

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
        above = [display::shape::compound::rectangle::shape];
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
    pub label:     Option<ImString>,
    /// Entries that should be displayed by the widget, as proposed by language server. This
    /// list is not exhaustive. The widget implementation might present additional
    /// options or allow arbitrary user input.
    pub choices:   Rc<Vec<Choice>>,
    /// Optional widget configurations for arguments for method or constructor applications within
    /// choices' expressions. This list is always ordered by `choice_index`.
    ///
    /// Note: The list of arguments could in theory be a part of a `Choice` struct, but doing so
    /// would add a requirement that all widget `Configuration`s would need to implement `Eq` trait
    /// and in effect could not use `f32` values. Additionally, the actual dropdown component
    /// doesn't require the arguments to be present in its entries, so having to clone them would
    /// be a waste. By separating the argument configurations and the choices into separate lists,
    /// we can avoid both of these issues.
    pub arguments: Vec<ChoiceArgConfig>,
}

/// An optional configuration override for an argument nested within a given dropdown choice. Only
/// applies to the arguments of an application directly nested within the choice's expression. That
/// includes method calls, standalone functions or constructors.
#[derive(Debug, Clone, PartialEq)]
pub struct ChoiceArgConfig {
    /// The index of an associated choice in [`Config.choices`] vector.
    pub choice_index:  usize,
    /// The name of the function argument for which the configuration is specified.
    pub name:          ImString,
    /// The widget configuration to apply for the argument node (or placeholder) matching the
    /// specified name.
    pub configuration: Configuration,
}

ensogl::define_endpoints_2! {
    Input {
        set_entries    (Rc<Vec<Choice>>),
        selected_entry  (Option<Choice>),
        current_crumbs (span_tree::Crumbs),
        is_connected   (bool),
    }
}

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Debug, display::Object)]
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

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(ctx: &ConfigContext) -> Score {
        let tags = ctx.span_node.kind.tag_values().unwrap_or_default();
        let has_tags = !tags.is_empty();
        has_tags.then_val_or_default(Score::Perfect)
    }

    fn default_config(ctx: &ConfigContext) -> Configuration<Self::Config> {
        let kind = &ctx.span_node.kind;
        let label = kind.name().map(Into::into);
        let tags = kind.tag_values().unwrap_or_default();
        let choices = Rc::new(tags.iter().map(Choice::from).collect());
        let default_config = Config { label, choices, arguments: default() };
        Configuration::always(default_config)
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
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

    fn configure(&mut self, config: &Config, mut ctx: ConfigContext) {
        let input = &self.config_frp.public.input;

        let has_value = !ctx.span_node.is_insertion_point();
        let current_value = has_value.then(|| ctx.span_expression());
        let (entry_idx, selected_entry) =
            entry_for_current_value(&config.choices, current_value).unzip();

        input.current_crumbs(ctx.span_node.crumbs.clone());
        input.set_entries(config.choices.clone());
        input.selected_entry(selected_entry);
        input.is_connected(ctx.info.subtree_connection.is_some());

        if !config.arguments.is_empty() && let Some(index) = entry_idx.filter(|i| *i < usize::MAX) {
            let start = config.arguments.partition_point(|arg| arg.choice_index < index);
            let end = config.arguments.partition_point(|arg| arg.choice_index <= index);
            let choice_args = &config.arguments[start..end];
            if !choice_args.is_empty() && let Some(call_id) = find_call_id(&ctx.span_node) {
                for arg in choice_args {
                    let key = OverrideKey { call_id, argument_name: arg.name.clone() };
                    ctx.builder.set_local_override(key, arg.configuration.clone())
                }
            }
        }

        if has_value {
            ctx.modify_extension::<label::Extension>(|ext| ext.bold = true);
        }

        let child = ctx.builder.child_widget(ctx.span_node, ctx.info.nesting_level);
        self.label_wrapper.replace_children(&[child.root_object]);
    }
}

/// Find an unambiguous single node with attached application within a given subtree and return its
/// AST ID. It represents the call ID that can have its arguments's configuration overridden
/// depending on currently selected dropdown choice.
///
/// Currently handled cases:
/// - `foo` - where `foo` is a resolved application with potential placeholders arguments.
/// - `foo bar` - the node itself is an application with specified arguments.
/// - `(<any previous case>)` - the node is a group with an singular child with an unambiguous
///   application.
fn find_call_id(node: &span_tree::Node) -> Option<ast::Id> {
    if node.application.is_some() {
        node.ast_id.or(node.extended_ast_id)
    } else if let Some(ast::TreeType::Group) = &node.tree_type
           && let [a, b, c] = &node.children[..] && a.is_token() && c.is_token()
    {
        find_call_id(&b.node)
    } else {
        None
    }
}

impl Widget {
    fn init(self, ctx: &ConfigContext) -> Self {
        let is_open = self.init_dropdown_focus(ctx);
        self.init_dropdown_values(ctx, is_open);
        self.init_activation_shape(ctx);
        self
    }

    fn init_dropdown_focus(&self, ctx: &ConfigContext) -> frp::Stream<bool> {
        let widgets_frp = ctx.frp();
        let focus_receiver = &self.dropdown_wrapper;
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
            eval_ close_after_selection_timer.on_expired(focus_receiver.blur());

        }
        is_open
    }


    fn init_dropdown_values(&self, ctx: &ConfigContext, is_open: frp::Stream<bool>) {
        let network = &self.config_frp.network;
        let dropdown_frp = &self.dropdown.borrow();
        let config_frp = &self.config_frp;
        let widgets_frp = ctx.frp();

        frp::extend! { network
            selected_entry <- config_frp.selected_entry.buffered_gate(&is_open).on_change();
            all_entries <- config_frp.set_entries.buffered_gate(&is_open).on_change();
            dropdown_frp.set_all_entries <+ all_entries.map(|e| e.deref().clone());
            dropdown_frp.set_selected_entries <+ selected_entry.map(|e| e.iter().cloned().collect());

            dropdown_entry <- dropdown_frp.selected_entries
                .map(|e: &HashSet<Choice>| e.iter().next().cloned());

            // Emit the output value only after actual user action. This prevents the
            // dropdown from emitting its initial value when it is opened, which can
            // represent slightly different version of code than actually written.
            submitted_entry <- dropdown_entry.sample(&dropdown_frp.user_select_action);
            dropdown_out_value <- submitted_entry.map(|e| e.as_ref().map(Choice::value));
            dropdown_out_import <- submitted_entry.map(|e| e.as_ref().and_then(Choice::required_import));

            widgets_frp.request_import <+ dropdown_out_import.unwrap();
            widgets_frp.value_changed <+ dropdown_out_value.map2(&config_frp.current_crumbs,
                move |t: &Option<ImString>, crumbs: &span_tree::Crumbs| (crumbs.clone(), t.clone())
            );

        };
    }

    fn init_activation_shape(&self, ctx: &ConfigContext) {
        let network = &self.config_frp.network;
        let config_frp = &self.config_frp;
        let widgets_frp = ctx.frp();
        let styles = ctx.styles();
        let activation_shape = &self.activation_shape;
        let focus_receiver = &self.dropdown_wrapper;
        frp::extend! { network
            let id = ctx.info.identity;
            parent_port_hovered <- widgets_frp.hovered_port_children.map(move |h| h.contains(&id));
            is_connected_or_hovered <- config_frp.is_connected || parent_port_hovered;
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
    all_entries: &[Choice],
    current_value: Option<&str>,
) -> Option<(usize, Choice)> {
    let current_value = current_value?;
    let found_entry =
        all_entries.iter().enumerate().find(|(_, entry)| entry.value == current_value);
    let with_partial_match = found_entry.or_else(|| {
        // Handle parentheses in current value. Entries with parenthesized expressions will match if
        // they start with the same expression as the current value. That way it is still matched
        // once extra arguments are added to the nested function call.
        current_value.starts_with('(').and_option_from(|| {
            let current_value = current_value.trim_start_matches('(').trim_end_matches(')');
            all_entries.iter().enumerate().find(|(_, entry)| {
                let trimmed_value = entry.value.trim_start_matches('(').trim_end_matches(')');
                current_value.starts_with(trimmed_value)
            })
        })
    });

    let with_fallback = with_partial_match
        .map(|(index, entry)| (index, entry.clone()))
        .unwrap_or_else(|| (usize::MAX, Choice::from_value(current_value.into())));
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
    // Required for lazy initialization
    app: ensogl::application::Application,
    set_all_entries: frp::Any<Vec<Choice>>,
    set_selected_entries: frp::Any<HashSet<Choice>>,
    set_open: frp::Any<bool>,
    sampled_set_all_entries: frp::Sampler<Vec<Choice>>,
    sampled_set_selected_entries: frp::Sampler<HashSet<Choice>>,
    sampled_set_open: frp::Sampler<bool>,
    selected_entries: frp::Any<HashSet<Choice>>,
    user_select_action: frp::Any<()>,
    dropdown: Option<Dropdown<Choice>>,
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

        let dropdown = self.dropdown.insert(self.app.new_view::<Dropdown<Choice>>());
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
