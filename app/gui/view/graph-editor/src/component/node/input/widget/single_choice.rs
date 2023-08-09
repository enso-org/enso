//! Definition of single choice widget.

use super::prelude::*;
use crate::prelude::*;

use crate::component::node;
use crate::component::node::input::widget::label;

use ensogl::display::object::event;
use ensogl::display::shape::SimpleTriangle;
use ensogl_component::button::prelude::INVISIBLE_HOVER_COLOR;
use ensogl_component::drop_down::Dropdown;



/// =============
/// === Style ===
/// =============

#[derive(Clone, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::widget::single_choice"]
struct Style {
    triangle_base:      color::Lcha,
    triangle_connected: color::Lcha,
    triangle_size:      Vector2,
    triangle_offset:    Vector2,
    /// Offset between the top of the dropdown list and the bottom left corner of the widget.
    dropdown_offset:    Vector2,
    /// Maximum allowed size of the dropdown list. If the list needs to be longer or wider than
    /// allowed by these values, it will receive a scroll bar.
    dropdown_max_size:  Vector2,
    dropdown_tint:      color::Lcha,
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
    display_object:   object::Instance,
    hover_area:       Rectangle,
    content_wrapper:  object::Instance,
    dropdown_wrapper: object::Instance,
    triangle_wrapper: object::Instance,
    dropdown:         Rc<RefCell<LazyDropdown>>,
    triangle:         SimpleTriangle,
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
        let label = kind.argument_name().map(Into::into);
        let tags = kind.tag_values().unwrap_or_default();
        let choices = Rc::new(tags.iter().map(Choice::from).collect());
        let default_config = Config { label, choices, arguments: default() };
        Configuration::always(default_config)
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        let app = ctx.app();
        //  ╭─display_object────────────────────╮
        //  │╭─content_wrapper─────────────────╮│
        //  ││                                 ││
        //  ││                                 ││
        //  │╰─────────────────────────────────╯│
        //  ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
        //  │ size=0        ◎ triangle_wrapper  │
        //  │ ◎ dropdown_wrapper                │
        //  ╰───────────────────────────────────╯

        let display_object = object::Instance::new_named("widget::SingleChoice");
        let hover_area = Rectangle();
        hover_area
            .set_color(INVISIBLE_HOVER_COLOR)
            .allow_grow_x()
            .set_alignment_center()
            .set_margin_xy((0.0, -node::HEIGHT / 2.0))
            .set_size_y(node::HEIGHT);
        display_object.add_child(&hover_area);
        let triangle_wrapper = display_object.new_child();
        let triangle = SimpleTriangle();
        triangle_wrapper.add_child(&triangle);
        let dropdown_wrapper = display_object.new_child();
        let content_wrapper = display_object.new_child();

        content_wrapper
            .use_auto_layout()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        triangle_wrapper.set_size((0.0, 0.0)).set_alignment_center_bottom();
        dropdown_wrapper.set_size((0.0, 0.0)).allow_grow_x().set_alignment_left_bottom();

        let config_frp = Frp::new();
        let dropdown = LazyDropdown::new(app, &config_frp.network);
        let dropdown = Rc::new(RefCell::new(dropdown));

        Self {
            config_frp,
            display_object,
            hover_area,
            content_wrapper,
            triangle_wrapper,
            dropdown_wrapper,
            dropdown,
            triangle,
        }
        .init(ctx)
    }

    fn configure(&mut self, config: &Config, mut ctx: ConfigContext) {
        let input = &self.config_frp.public.input;
        ctx.layers.hover.add(&self.hover_area);

        let has_value = !ctx.span_node.is_insertion_point();
        let current_value = has_value.then(|| ctx.span_expression());
        let (entry_idx, selected_entry) =
            entry_for_current_value(&config.choices[..], current_value).unzip();

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
        self.content_wrapper.replace_children(&[child.root_object]);
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
        let style = ctx.cached_style::<Style>(&self.config_frp.network);
        let is_open = self.init_dropdown_focus(ctx, &style);
        self.init_dropdown_values(ctx, is_open);
        self.init_triangle(ctx, &style);
        self
    }

    fn init_dropdown_focus(
        &self,
        ctx: &ConfigContext,
        style: &frp::Stream<Style>,
    ) -> frp::Stream<bool> {
        let widgets_frp = ctx.frp();
        let focus_receiver = &self.dropdown_wrapper;
        let focus_in = focus_receiver.on_event::<event::FocusIn>();
        let focus_out = focus_receiver.on_event::<event::FocusOut>();
        let network = &self.config_frp.network;
        let dropdown = &self.dropdown;
        let dropdown_frp = &self.dropdown.borrow();
        let dropdown_wrapper = &self.dropdown_wrapper;
        frp::extend! { network
            _eval <- focus_in.map2(style, f!([dropdown, dropdown_wrapper, style] (_, style_value) {
                dropdown.borrow_mut().lazy_init(&dropdown_wrapper, &style, style_value);
            }));
            readonly_set <- widgets_frp.set_read_only.on_true();
            do_open <- focus_in.gate_not(&widgets_frp.set_read_only);
            do_close <- any_(focus_out, readonly_set);
            is_open <- bool(&do_close, &do_open);
            dropdown_frp.set_open <+ is_open.on_change();
            dropdown_frp.set_color <+ all_with(style, &widgets_frp.node_base_color,
                |style, color| style.dropdown_tint.over(*color)
            );

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
        }
    }

    fn init_triangle(&self, ctx: &ConfigContext, style: &frp::Stream<Style>) {
        let network = &self.config_frp.network;
        let config_frp = &self.config_frp;
        let widgets_frp = ctx.frp();
        let display_object = &self.display_object;
        let triangle = &self.triangle;
        let focus_receiver = &self.dropdown_wrapper;

        frp::extend! { network
            let id = ctx.info.identity;
            parent_port_hovered <- widgets_frp.hovered_port_children.map(move |h| h.contains(&id));
            is_connected <- config_frp.is_connected || parent_port_hovered;
            eval *style([triangle] (style) {
                let size = style.triangle_size;
                triangle.set_xy(style.triangle_offset - Vector2(size.x * 0.5, -size.y));
                triangle.set_base_and_altitude(size.x, -size.y);
            });

            let mouse_down = display_object.on_event::<mouse::Down>();
            let mouse_dropdown_down = focus_receiver.on_event::<mouse::Down>();
            let mouse_enter = display_object.on_event::<mouse::Enter>();
            let mouse_leave = display_object.on_event::<mouse::Leave>();

            mouse_dropdown_down_delayed <- mouse_dropdown_down.debounce();
            handling_dropdown_down <- bool(&mouse_dropdown_down_delayed, &mouse_dropdown_down);
            mouse_down <- mouse_down.gate(&widgets_frp.allow_interaction);
            clicked <- mouse_down.filter(mouse::is_primary);
            eval clicked([] (event) event.stop_propagation());
            clicked <- clicked.gate_not(&handling_dropdown_down);

            is_hovered <- bool(&mouse_leave, &mouse_enter).and(&widgets_frp.allow_interaction);

            let triangle_color = color::Animation::new(network);
            triangle_color.target <+ is_connected.all_with3(style, &is_hovered,
                |connected, s, hovered| {
                let color = if *connected { s.triangle_connected } else { s.triangle_base };
                color.multiply_alpha(if *hovered { 1.0 } else { 0.0 })
            }).on_change();
            eval triangle_color.value((color) triangle.set_color(color.into()););

            set_focused <- clicked.map(f!([focus_receiver](_) !focus_receiver.is_focused()));
            eval set_focused([focus_receiver](focus) match focus {
                true => focus_receiver.focus(),
                false => focus_receiver.blur(),
            });
        }
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
    set_color: frp::Any<color::Lcha>,
    sampled_set_all_entries: frp::Sampler<Vec<Choice>>,
    sampled_set_selected_entries: frp::Sampler<HashSet<Choice>>,
    sampled_set_open: frp::Sampler<bool>,
    sampled_set_color: frp::Sampler<color::Lcha>,
    selected_entries: frp::Any<HashSet<Choice>>,
    user_select_action: frp::Any<()>,
    network: frp::Network,
    dropdown: Option<Dropdown<Choice>>,
}

impl LazyDropdown {
    fn new(app: &ensogl::application::Application, network: &frp::Network) -> Self {
        frp::extend! { network
            set_all_entries <- any(...);
            set_selected_entries <- any(...);
            set_open <- any(...);
            set_color <- any(...);
            selected_entries <- any(...);
            user_select_action <- any(...);
            sampled_set_all_entries <- set_all_entries.sampler();
            sampled_set_selected_entries <- set_selected_entries.sampler();
            sampled_set_open <- set_open.sampler();
            sampled_set_color <- set_color.sampler();
        }

        Self {
            app: app.clone_ref(),
            set_all_entries,
            set_selected_entries,
            set_open,
            set_color,
            selected_entries,
            user_select_action,
            sampled_set_all_entries,
            sampled_set_selected_entries,
            sampled_set_open,
            sampled_set_color,
            dropdown: None,
            network: frp::Network::new("LazyDropdown"),
        }
    }

    /// Perform initialization that actually creates the dropdown. Should be done only once there is
    /// a request to open the dropdown.
    fn lazy_init(
        &mut self,
        parent: &object::Instance,
        style: &frp::Stream<Style>,
        current_style: &Style,
    ) {
        if self.dropdown.is_some() {
            return;
        }

        let dropdown = self.dropdown.insert(self.app.new_view::<Dropdown<Choice>>());
        parent.add_child(dropdown);
        self.app.display.default_scene.layers.above_nodes.add(&*dropdown);
        let network = &self.network;

        frp::extend! { network
            dropdown.set_all_entries <+ self.sampled_set_all_entries;
            dropdown.set_selected_entries <+ self.sampled_set_selected_entries;
            dropdown.set_open <+ self.sampled_set_open;
            dropdown.set_color <+ self.sampled_set_color;
            self.selected_entries <+ dropdown.selected_entries;
            self.user_select_action <+ dropdown.user_select_action;
            eval* style([dropdown] (style) {
                dropdown.set_xy(style.dropdown_offset);
                dropdown.set_max_open_size(style.dropdown_max_size);
            });
            eval_ parent.on_transformed([dropdown, parent] {
                dropdown.set_min_open_width(parent.computed_size().x())
            });
        }

        dropdown.set_xy(current_style.dropdown_offset);
        dropdown.set_max_open_size(current_style.dropdown_max_size);
        dropdown.set_min_open_width(parent.computed_size().x());
        dropdown.allow_deselect_all(true);
        dropdown.set_all_entries(self.sampled_set_all_entries.value());
        dropdown.set_selected_entries(self.sampled_set_selected_entries.value());
        dropdown.set_open(self.sampled_set_open.value());
        dropdown.set_color(self.sampled_set_color.value());
    }
}
