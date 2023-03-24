//! Definition of single choice widget.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::event;
use ensogl_component::drop_down::Dropdown;
use ensogl_component::text;

use crate::component::node::input::area::TEXT_SIZE;
use crate::component::node::input::widget::Entry;

/// =================
/// === Constants ===
/// =================

const ACTIVATION_SHAPE_COLOR: color::Lch = color::Lch::new(0.56708, 0.23249, 0.71372);
const ACTIVATION_SHAPE_Y_OFFSET: f32 = 15.0;
const ACTIVATION_SHAPE_SIZE: Vector2 = Vector2(15.0, 11.0);
/// Distance between the dropdown and the bottom of the port.
const DROPDOWN_Y_OFFSET: f32 = 5.0;



// ======================
// === Triangle Shape ===
// ======================

/// Temporary dropdown activation shape definition.
pub mod triangle {
    use super::*;
    ensogl::shape! {
        above = [
            crate::component::node::background,
            crate::component::node::input::port::hover
        ];
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
    /// Entries that should be displayed by the widget, as proposed by language server. This
    /// list is not exhaustive. The widget implementation might present additional
    /// options or allow arbitrary user input.
    pub entries: Rc<Vec<Entry>>,
}

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Debug)]
pub struct Widget {
    #[allow(dead_code)]
    network:           frp::Network,
    set_current_value: frp::Source<Option<ImString>>,
    current_crumbs:    frp::Any<span_tree::Crumbs>,
    dropdown:          Rc<RefCell<LazyDropdown>>,
    display_object:    display::object::Instance,
    label:             text::Text,
    /// temporary click handling
    _activation_shape: triangle::View,
}

impl super::SpanWidget for Widget {
    type Config = Config;
    fn new(config: &Config, ctx: super::ConfigContext) -> Self {
        let app = ctx.app();
        let frp = ctx.frp();

        let display_object = display::object::Instance::new();
        display_object.use_auto_layout();
        let activation_shape = triangle::View::new();
        let dot_color = color::Rgba::from(ACTIVATION_SHAPE_COLOR.with_alpha(1.0)).into();
        activation_shape.color.set(dot_color);
        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);
        activation_shape.set_alignment_center_x();
        activation_shape.set_y(-ACTIVATION_SHAPE_SIZE.y() - ACTIVATION_SHAPE_Y_OFFSET);

        let label = text::Text::new(app);
        label.set_property_default(text::Size(TEXT_SIZE));
        display_object.add_child(&label);
        display_object.add_child(&activation_shape);

        frp::new_network! { network
            init <- source_();
            set_current_value <- source();
            value_changed <- any(...);

            let focus_in = display_object.on_event::<event::FocusIn>();
            let focus_out = display_object.on_event::<event::FocusOut>();
            is_focused <- bool(&focus_out, &focus_in);
            is_open <- is_focused.sampler();
            current_value_sampler <- set_current_value.sampler();
            current_crumbs <- any(...);
            frp.out_value_changed <+ value_changed.map2(&current_crumbs,
                move |t: &Option<ImString>, crumbs: &span_tree::Crumbs| (crumbs.clone(), t.clone())
            );
        };

        let request_import = frp.out_request_import.clone_ref();
        let dropdown = LazyDropdown::new(
            app,
            &display_object,
            current_value_sampler,
            is_open,
            value_changed,
            request_import,
        );
        let dropdown = Rc::new(RefCell::new(dropdown));

        frp::extend! { network
            let dot_clicked = activation_shape.events.mouse_down_primary.clone_ref();
            set_focused <- dot_clicked.map(f!([display_object](()) !display_object.is_focused()));
            eval set_focused([display_object](focus) match focus {
                true => display_object.focus(),
                false => display_object.blur(),
            });

            eval focus_in((_) dropdown.borrow_mut().initialize_on_open());
        }

        init.emit(());

        let mut this = Self {
            display_object,
            network,
            set_current_value,
            current_crumbs,
            dropdown,
            _activation_shape: activation_shape,
            label,
        };
        this.configure(config, ctx);
        this
    }

    fn configure(&mut self, config: &Config, ctx: super::ConfigContext) {
        self.display_object.set_parent(ctx.parent_instance);

        let mut dropdown = self.dropdown.borrow_mut();
        let content: ImString = ctx.expression_at(ctx.span_tree_node.span()).into();
        dropdown.set_entries(config.entries.clone());
        self.set_current_value.emit(content.clone());
        self.label.set_content(content);
        self.current_crumbs.emit(ctx.span_tree_node.crumbs.clone());

        // Do not increment the depth. If the dropdown is displayed, it should also display
        // its arguments.
        for child in ctx.span_tree_node.children_iter() {
            ctx.builder.child_widget(&self.display_object, child, ctx.depth);
        }
    }
}

// ====================
// === LazyDropdown ===
// ====================

/// A lazy dropdown that is only initialized when it is opened for the first time. This prevents
/// very long initialization time, as dropdown view creation is currently a very slow process.
///
/// FIXME [PG]: Improve grid-view creation performance, so that this is no longer needed.
/// https://www.pivotaltracker.com/story/show/184223891
///
/// Once grid-view creation is reasonably fast, this might be replaced by direct dropdown
/// initialization on widget creation.
#[derive(Debug)]
enum LazyDropdown {
    NotInitialized {
        app:               Application,
        display_object:    display::object::Instance,
        entries:           Rc<Vec<Entry>>,
        set_current_value: frp::Sampler<Option<ImString>>,
        is_open:           frp::Sampler<bool>,
        output_value:      frp::Any<Option<ImString>>,
        request_import:    frp::Any<ImString>,
    },
    Initialized {
        _network:    frp::Network,
        _dropdown:   Dropdown<Entry>,
        set_entries: frp::Any<Vec<Entry>>,
    },
}

impl LazyDropdown {
    fn new(
        app: &Application,
        display_object: &display::object::Instance,
        set_current_value: frp::Sampler<Option<ImString>>,
        is_open: frp::Sampler<bool>,
        output_value: frp::Any<Option<ImString>>,
        request_import: frp::Any<ImString>,
    ) -> Self {
        let app = app.clone_ref();
        let display_object = display_object.clone_ref();
        let entries = default();
        LazyDropdown::NotInitialized {
            app,
            display_object,
            entries,
            set_current_value,
            is_open,
            output_value,
            request_import,
        }
    }

    fn set_entries(&mut self, new_entries: Rc<Vec<Entry>>) {
        match self {
            LazyDropdown::Initialized { set_entries, .. } => {
                let new_entries = Rc::try_unwrap(new_entries).unwrap_or_else(|rc| (*rc).clone());
                set_entries.emit(new_entries);
            }
            LazyDropdown::NotInitialized { entries, .. } => {
                *entries = new_entries;
            }
        }
    }

    #[profile(Detail)]
    fn initialize_on_open(&mut self) {
        match self {
            LazyDropdown::Initialized { .. } => {}
            LazyDropdown::NotInitialized {
                app,
                display_object,
                entries,
                is_open,
                set_current_value,
                output_value,
                request_import,
            } => {
                let dropdown = app.new_view::<Dropdown<Entry>>();
                display_object.add_child(&dropdown);
                app.display.default_scene.layers.above_nodes.add(&dropdown);
                dropdown.set_y(20.0);
                dropdown.set_max_open_size(Vector2(300.0, 500.0));
                dropdown.allow_deselect_all(true);

                frp::new_network! { network
                    init <- source_();
                    set_entries <- any(...);

                    dropdown.set_all_entries <+ set_entries;
                    entries_and_value <- all(&set_entries, set_current_value);
                    entries_and_value <- entries_and_value.debounce();

                    selected_entry <- entries_and_value.map(|(e, v)| entry_for_current_value(e, v));
                    dropdown.set_selected_entries <+ selected_entry.map(|e| e.iter().cloned().collect());

                    dropdown_entry <- dropdown.selected_entries.map(|e| e.iter().next().cloned());
                    // Emit the output value only after actual user action. This prevents the
                    // dropdown from emitting its initial value when it is opened, which can
                    // represent slightly different version of code than actually written.
                    submitted_entry <- dropdown_entry.sample(&dropdown.user_select_action);
                    dropdown_out_value <- submitted_entry.map(|e| e.as_ref().map(Entry::value));
                    dropdown_out_import <- submitted_entry.map(|e| e.as_ref().and_then(Entry::required_import));
                    request_import <+ dropdown_out_import.unwrap();
                    output_value <+ dropdown_out_value.sample(&dropdown.user_select_action);

                    is_open <- all(is_open, &init)._0();
                    dropdown.set_open <+ is_open.on_change();

                    // Close the dropdown after a short delay after selection. Because the dropdown
                    // value application triggers operations that can introduce a few dropped frames,
                    // we want to delay the dropdown closing animation after that is handled.
                    // Otherwise the animation finishes within single frame, which looks bad.
                    let close_after_selection_timer = frp::io::timer::Timeout::new(&network);
                    close_after_selection_timer.restart <+ dropdown.user_select_action.constant(1);
                    eval close_after_selection_timer.on_expired((()) display_object.blur());
                }

                let entries = std::mem::take(Rc::make_mut(entries));
                set_entries.emit(entries);
                init.emit(());
                *self = LazyDropdown::Initialized {
                    _network: network,
                    _dropdown: dropdown,
                    set_entries,
                };
            }
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
