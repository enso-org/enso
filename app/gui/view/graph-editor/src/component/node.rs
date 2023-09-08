//! Definition of the Node component.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use crate::component::visualization;
use crate::selection::BoundingBox;
use crate::tooltip;
use crate::view;
use crate::CallWidgetsConfig;
use crate::GraphLayers;
use crate::Type;

use engine_protocol::language_server::ExecutionEnvironment;
use enso_frp as frp;
use enso_frp;
use ensogl::animation::delayed::DelayedAnimation;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::style::FromTheme;
use ensogl::gui;
use ensogl::Animation;
use ensogl_component::text;
use ensogl_hardcoded_theme as theme;


// ==============
// === Export ===
// ==============

#[deny(missing_docs)]
pub mod action_bar;
#[warn(missing_docs)]
pub mod error;
pub mod expression;
pub mod growth_animation;
pub mod input;
pub mod output;
#[deny(missing_docs)]
pub mod vcs;

pub use error::Error;
pub use expression::Expression;



// =================
// === Constants ===
// =================

/// Base height of a single-line node.
pub const HEIGHT: f32 = 32.0;
/// The additional reserved space around base background shape to allow for drawing node backdrop
/// shapes, such as selection or status.
pub const BACKDROP_INSET: f32 = 25.0;
/// Size of error indicator border around node.
pub const ERROR_BORDER_WIDTH: f32 = 3.0;
/// Distance between node and error indicator border.
pub const ERROR_BORDER_DISTANCE: f32 = 3.0;
/// Radius of rounded corners of base node background shape. Node also contains other shapes, such
/// as selection, that will also received rounded corners by growing the background shape.
pub const CORNER_RADIUS: f32 = HEIGHT / 2.0;

/// Space between the documentation comment and the node.
pub const COMMENT_MARGIN: f32 = 10.0;

const ERROR_VISUALIZATION_SIZE: Vector2 = visualization::container::DEFAULT_SIZE;

/// Distance between the origin of the node and the top of the visualization.
const VISUALIZATION_OFFSET_Y: f32 = 25.0;
const VISUALIZATION_OFFSET: Vector2 = Vector2(0.0, -VISUALIZATION_OFFSET_Y);

const ENABLE_VIS_PREVIEW: bool = false;
const VIS_PREVIEW_ONSET_MS: f32 = 4000.0;
const ERROR_PREVIEW_ONSET_MS: f32 = 0000.0;
/// A type of unresolved methods. We filter them out, because we don't want to treat them as types
/// for ports and edges coloring (due to bad UX otherwise).
const UNRESOLVED_SYMBOL_TYPE: &str = "Builtins.Main.Unresolved_Symbol";



// ==============
// === Shapes ===
// ==============

/// A node's background area and selection.
#[derive(Debug, Clone, display::Object)]
pub struct Background {
    _network:            frp::Network,
    #[display_object]
    shape:               Rectangle,
    selection_shape:     Rectangle,
    selection_animation: Animation<f32>,
    color_animation:     color::Animation,
    node_is_hovered:     frp::Any<bool>,
    size_and_center:     frp::Source<(Vector2, Vector2)>,
}

#[derive(Debug, Clone, Default, FromTheme)]
struct BackgroundStyle {
    #[theme_path = "theme::graph_editor::node::selection::opacity"]
    selection_opacity:       f32,
    #[theme_path = "theme::graph_editor::node::selection::hover_opacity"]
    selection_hover_opacity: f32,
    #[theme_path = "theme::graph_editor::node::selection::size"]
    selection_size:          f32,
}

impl Background {
    fn new(style: &StyleWatchFrp) -> Self {
        let network = frp::Network::new("Background");
        let style = BackgroundStyle::from_theme(&network, style);
        let shape = Rectangle();
        let selection_shape = Rectangle();
        let color_animation = color::Animation::new(&network);
        let selection_animation = Animation::new(&network);
        let hover_animation = Animation::new(&network);
        shape.add_child(&selection_shape);

        let selection_enter = selection_shape.on_event::<mouse::Enter>();
        let selection_leave = selection_shape.on_event::<mouse::Leave>();

        frp::extend! { network
            selection_is_hovered <- bool(&selection_leave, &selection_enter);
            node_is_hovered <- any(...);
            is_hovered <- selection_is_hovered || node_is_hovered;
            hover_animation.target <+ is_hovered.switch_constant(INVISIBLE_HOVER_COLOR.alpha, 1.0);
            selection_colors <- color_animation.value.all_with3(
                &hover_animation.value, &style,
                |color, hover, style| (
                    color.multiply_alpha(style.selection_opacity),
                    color.multiply_alpha(style.selection_hover_opacity * hover),
                )
            );
            selection_border <- selection_animation.value.all_with(&style,
                |selection, style| style.selection_size * (1.0 - selection)
            );

            size_and_center <- source();
            eval size_and_center([shape] (size_and_center) {
                let (size, center) = *size_and_center;
                shape.set_xy(center - size / 2.0);
                shape.set_size(size);
                shape.set_corner_radius(CORNER_RADIUS);
            });

            size_and_selection <- size_and_center.all_with(&style,
                |(size, _), style| (*size, style.selection_size)
            ).on_change();

            eval size_and_selection([selection_shape] (size_and_selection) {
                let (size, total_selection_size) = *size_and_selection;
                let total_selection_vec = Vector2::from_element(total_selection_size);
                // selection shape is positioned relative to the background shape.
                selection_shape.set_xy(-total_selection_vec);
                selection_shape.set_size(size + total_selection_vec * 2.0);
                selection_shape.set_corner_radius(CORNER_RADIUS + total_selection_size);
            });

            eval color_animation.value((color) shape.set_color(color.into()););
            eval selection_border((border) selection_shape.set_border_and_inset(*border););
            eval selection_colors([selection_shape] ((selected_color, unselected_color)) {
                selection_shape.set_color(selected_color.into());
                selection_shape.set_border_color(unselected_color.into());
            });
        }

        selection_animation.target.emit(0.0);
        hover_animation.precision.emit(0.01);
        hover_animation.target.emit(INVISIBLE_HOVER_COLOR.alpha);

        Self {
            _network: network,
            shape,
            selection_shape,
            selection_animation,
            node_is_hovered,
            color_animation,
            size_and_center,
        }
    }

    fn set_selected(&self, selected: bool) {
        self.selection_animation.target.emit(if selected { 1.0 } else { 0.0 });
    }

    fn set_color(&self, color: color::Lcha) {
        self.color_animation.target.emit(color);
    }

    fn set_size_and_center_xy(&self, size: Vector2<f32>, center: Vector2<f32>) {
        self.size_and_center.emit((size, center));
    }
}



// ============
// === Node ===
// ============

ensogl::define_endpoints_2! {
    Input {
        select                (),
        deselect              (),
        enable_visualization  (),
        enable_fullscreen_visualization  (),
        disable_visualization (),
        set_visualization     (Option<visualization::Definition>),
        set_disabled          (bool),
        set_pending           (bool),
        set_connections       (HashMap<span_tree::PortId, color::Lcha>),
        set_expression        (Expression),
        edit_expression       (text::Range<text::Byte>, ImString),
        set_skip_macro        (bool),
        set_freeze_macro      (bool),
        /// Set whether the output context is explicitly enabled: `Some(true/false)` for
        /// enabled/disabled; `None` for no context switch expression.
        set_context_switch    (Option<bool>),
        set_comment           (ImString),
        set_error             (Option<Error>),
        /// Set the expression USAGE type. This is not the definition type, which can be set with
        /// `set_expression` instead. In case the usage type is set to None, ports still may be
        /// colored if the definition type was present.
        set_expression_usage_type         (ast::Id, Option<Type>),
        update_widgets                    (CallWidgetsConfig),
        set_output_expression_visibility  (bool),
        set_vcs_status                    (Option<vcs::Status>),
        /// Show visualization preview until either editing of the node is finished or the
        /// visualization state is explicitly changed by the user. The preview looks the same as
        /// normal visualization, but its state is not persisted in the node's metadata.
        show_preview                      (),
        /// Indicate whether preview visualizations should be delayed or immediate.
        quick_preview_vis                 (bool),
        set_view_mode                     (view::Mode),
        set_profiling_min_global_duration (f32),
        set_profiling_max_global_duration (f32),
        /// Indicate whether on hover the quick action icons should appear.
        show_quick_action_bar_on_hover    (bool),
        set_execution_environment         (ExecutionEnvironment),

        /// Set read-only mode for input ports.
        set_read_only                     (bool),

        /// Set the mode in which the cursor will indicate that editing of the node is possible.
        set_edit_ready_mode (bool),
    }
    Output {
        /// Press event. Emitted when user clicks on non-active part of the node, like its
        /// background. In edit mode, the whole node area is considered non-active.
        background_press         (),
        /// This event occurs when the user modifies an expression, either by typing directly or
        /// using a widget. It includes information about the specific part of the expression that
        /// was changed and where it fits within the larger expression.
        ///
        /// Note: Node component is not able to perform the actual modification of the expression,
        /// as that requires rebuilding the span-tree, which in turn requires access to the
        /// execution context. It is the responsibility of the parent component to apply the changes
        /// and update the node with new expression tree using `set_expression`.
        on_expression_modified   (span_tree::Crumbs, ImString),
        comment                  (ImString),
        context_switch           (bool),
        skip                     (bool),
        freeze                   (bool),
        hover                    (bool),
        error                    (Option<Error>),
        /// The [`display::object::Model::position`] of the Node. Emitted when the Display Object
        /// hierarchy is updated (see: [`ensogl_core::display::object::Instance::update`]).
        position                 (Vector2),
        /// The bounding box of the Node. Contains the bounding box of the visualization if the
        /// visualization is enabled and visible.
        ///
        /// Updated after any of [`position`], [`expression`], [`visualization_enabled`], or
        /// [`visualization_visible`] is updated. Please remember, that the [`position`] is not
        /// immediately updated, only during the Display Object hierarchy update
        bounding_box             (BoundingBox),
        /// The bounding box of the node without the visualization.
        inner_bounding_box       (BoundingBox),
        /// A set of widgets attached to a method requires metadata to be queried. The tuple
        /// contains the ID of the call expression the widget is attached to, and the ID of that
        /// call's target expression (`self` or first argument).
        requested_widgets        (ast::Id, ast::Id),
        request_import           (ImString),

        base_color               (color::Lcha),
        port_color               (color::Lcha),
    }
}

/// The visual node representation.
///
/// ## Origin
/// Please note that the origin of the node is on its left side, centered vertically. This decision
/// was made to both optimise performance and make the origin point more meaningful. When editing
/// the node, its width changes, while its left border remains still. When expanding the node, its
/// height changes, while its top remains at the same place. Thus, while editing or expanding the
/// node, there is no need to update its position. Moreover, the chosen origin point is more natural
/// than origin placed in other possible places, including the upper-left corner of its bounding
/// box. The `x` symbolises the origin on the following drawing:
///
/// ```text
///   ╭─────────────────╮
///  x│                 │
///   ╰─────────────────╯
/// ```
///
/// ## FRP Event Architecture.
/// Nodes FRP architecture is designed for efficiency. Event with millions nodes on the stage, only
/// small amount of events will be passed around on user action. This is not always simple, and it
/// required a careful, well thought architecture.
///
/// Take for example the `edit_mode` event. It is emitted when user presses the `cmd` button. The
/// following requirements should be hold:
///
/// 1. If the mouse is not over a node, nothing happens.
/// 2. If the mouse traverses over the node with `cmd` being hold, the mouse cursor should change to
///    text cursor to indicate that editing of the expression is possible.
/// 3. If the mouse was over the node when pressing `cmd`, the mouse cursor should change to text
///    cursor as well.
///
/// The points 1 and 2 are pretty easy to be done. We can discover mouse hover from inside of the
/// node and react in the right way. The point 3 is tricky. There are several possible solutions
/// out there:
///
/// A. After pressing / releasing `cmd` we should send an event to every node on the stage to
///    indicate that the "edit mode" is on. This is a simple solution, but also very inefficient
///    with a lot of nodes on the stage.
///
/// B. We could pass a special FRP output to node constructor, like
///    `is_edit_mode_on:frp::Sampler<bool>`, which could be sampled by the node whenever the mouse
///    hovers it. This will solve the requirement 2, but will not work with requirement 3.
///
/// C. We could discover inside of node when mouse hovers it (either the drag area, or ports, or
///    anything else that we consider part of the node), and emit it as an output event. Then we
///    can capture the event in the graph editor and tag it with the node id. Having the information
///    in place, we can send events to the currently hovered node whenever we need, directly from
///    the graph editor. This solves all issues in a very efficient and elegant way, but is somehow
///    complex logically (the events are emitted from node to graph, then processed there and
///    emitted back to the right node).
///
/// Currently, the solution "C" (nearest to optimal) is implemented here.
#[derive(Clone, CloneRef, Debug, display::Object)]
#[allow(missing_docs)]
pub struct Node {
    widget: gui::Widget<NodeModel, Frp>,
}

impl AsRef<Node> for Node {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl AsRef<gui::Widget<NodeModel, Frp>> for Node {
    fn as_ref(&self) -> &gui::Widget<NodeModel, Frp> {
        &self.widget
    }
}


impl Deref for Node {
    type Target = gui::Widget<NodeModel, Frp>;
    fn deref(&self) -> &Self::Target {
        &self.widget
    }
}

/// Internal data of `Node`
#[derive(Clone, Debug, display::Object)]
#[allow(missing_docs)]
pub struct NodeModel {
    pub layers:              GraphLayers,
    pub display_object:      display::object::Instance,
    pub background:          Background,
    pub error_indicator:     Rectangle,
    pub input:               input::Area,
    pub output:              output::Area,
    pub visualization:       visualization::Container,
    pub error_visualization: error::Container,
    pub action_bar_wrapper:  display::object::Instance,
    pub action_bar:          action_bar::ActionBar,
    pub vcs_indicator:       vcs::StatusIndicator,
    pub style:               StyleWatchFrp,
    pub comment:             text::Text,
    pub interaction_state:   Cell<InteractionState>,
}

impl NodeModel {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application, layers: &GraphLayers, registry: visualization::Registry) -> Self {
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);

        let error_indicator = Rectangle();
        error_indicator
            .set_corner_radius_max()
            .set_pointer_events(false)
            .set_color(color::Rgba::transparent())
            .set_border_and_inset(ERROR_BORDER_WIDTH);
        let background = Background::new(&style);
        let vcs_indicator = vcs::StatusIndicator::new(app);
        let display_object = display::object::Instance::new_named("Node");

        display_object.add_child(&background);
        display_object.add_child(&vcs_indicator);

        let input = input::Area::new(app, layers);
        let visualization = visualization::Container::new(app, registry);

        display_object.add_child(&visualization);
        display_object.add_child(&input);

        let error_visualization = error::Container::new(app);
        error_visualization.frp.set_size.emit(ERROR_VISUALIZATION_SIZE);

        let action_bar = action_bar::ActionBar::new(app);
        let action_bar_wrapper = display::object::Instance::new_named("action_bar_wrapper");
        action_bar_wrapper.set_size((0.0, 0.0)).set_xy((0.0, 0.0));
        action_bar_wrapper.add_child(&action_bar);
        action_bar.set_alignment_right_center();
        display_object.add_child(&action_bar_wrapper);

        let output = output::Area::new(app);
        display_object.add_child(&output);

        let comment = text::Text::new(app);
        display_object.add_child(&comment);

        let interaction_state = default();

        Self {
            layers: layers.clone(),
            display_object,
            background,
            error_indicator,
            input,
            output,
            visualization,
            error_visualization,
            action_bar_wrapper,
            action_bar,
            vcs_indicator,
            style,
            comment,
            interaction_state,
        }
        .init()
    }

    #[profile(Debug)]
    fn init(self) -> Self {
        self.set_expression(Expression::new_plain("empty"));
        self.set_layers_for_state(self.interaction_state.get());
        self
    }

    /// Set whether the node is being edited. This is used to adjust the camera.
    pub fn set_editing_expression(&self, editing: bool) {
        let new_state = self.interaction_state.update(|state| state.editing_expression(editing));
        self.set_layers_for_state(new_state);
    }

    fn set_layers_for_state(&self, new_state: InteractionState) {
        let (below, main) = match new_state {
            InteractionState::Normal => (&self.layers.main_backdrop, &self.layers.main_nodes),
            InteractionState::EditingExpression =>
                (&self.layers.edited_backdrop, &self.layers.edited_nodes),
        };

        main.body.add(&self.display_object);
        below.backdrop.add(&self.background.selection_shape);
        below.backdrop.add(&self.error_indicator);
        main.action_bar.add(&self.action_bar_wrapper);
        main.below_body.add(&self.output);
        main.output_hover.add(self.output.hover_root());
        self.action_bar.set_layers(main);
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn width(&self) -> f32 {
        self.input.width.value()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn height(&self) -> f32 {
        HEIGHT
    }

    #[profile(Debug)]
    fn set_expression(&self, expr: impl Into<Expression>) {
        let expr = expr.into();
        self.output.set_expression(&expr);
        self.input.set_expression(&expr);
    }

    #[profile(Debug)]
    fn set_expression_usage_type(&self, id: ast::Id, tp: &Option<Type>) {
        self.input.set_expression_usage_type(id, tp);
        self.output.set_expression_usage_type(id, tp);
    }

    #[profile(Debug)]
    fn set_width(&self, width: f32) -> Vector2 {
        let height = self.height();
        let size = Vector2(width, height);
        let padded_size = size + Vector2(BACKDROP_INSET, BACKDROP_INSET) * 2.0;
        let error_padding = ERROR_BORDER_WIDTH + ERROR_BORDER_DISTANCE;
        let error_size = size + Vector2(error_padding, error_padding) * 2.0;
        self.output.frp.set_size(size);
        self.error_indicator.set_size(error_size);
        self.vcs_indicator.frp.set_size(padded_size);
        let x_offset_to_node_center = x_offset_to_node_center(width);
        let background_origin = Vector2(x_offset_to_node_center, 0.0);
        self.background.set_size_and_center_xy(size, background_origin);
        self.error_indicator.set_xy((-error_padding, -height / 2.0 - error_padding));
        self.vcs_indicator.set_x(x_offset_to_node_center);

        self.visualization.set_xy(VISUALIZATION_OFFSET);
        // Error visualization has origin in the center, while regular visualization has it at the
        // top left corner.
        let error_vis_offset_y = -ERROR_VISUALIZATION_SIZE.y / 2.0;
        let error_vis_offset_x = ERROR_VISUALIZATION_SIZE.x / 2.0;
        let error_vis_offset = Vector2(error_vis_offset_x, error_vis_offset_y);
        let error_vis_pos = VISUALIZATION_OFFSET + error_vis_offset;
        self.error_visualization.set_xy(error_vis_pos);
        self.visualization.frp.set_width(width);

        size
    }

    #[profile(Debug)]
    fn set_error(&self, error: Option<&Error>) {
        if let Some(error) = error {
            self.error_visualization.display_kind(*error.kind);
            if let Some(error_data) = error.visualization_data() {
                self.error_visualization.set_data(error_data);
            }
            if error.should_display() {
                self.display_object.add_child(&self.error_visualization);
            }
        } else {
            self.error_visualization.unset_parent();
        }
    }

    #[profile(Debug)]
    fn set_error_color(&self, color: &color::Lcha) {
        if color.alpha < f32::EPSILON {
            self.error_indicator.unset_parent();
        } else {
            self.error_indicator.set_border_color(color.into());
            self.display_object.add_child(&self.error_indicator);
        }
    }

    #[profile(Debug)]
    fn update_colors(&self, color: color::Lcha, port_color: color::Lcha) {
        self.background.set_color(color);
        self.output.set_port_color(port_color);
    }

    fn set_selected(&self, selected: bool) {
        self.background.set_selected(selected);
    }
}

impl Node {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    #[profile(Debug)]
    pub fn new(app: &Application, layers: &GraphLayers, registry: visualization::Registry) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let out = &frp.private.output;
        let input = &frp.private.input;
        let model = Rc::new(NodeModel::new(app, layers, registry));
        let display_object = &model.display_object;

        // TODO[ao] The comment color should be animated, but this is currently slow. Will be fixed
        //      in https://github.com/enso-org/ide/issues/1031
        // let comment_color    = color::Animation::new(network);
        let error_color_anim = color::Animation::new(network);
        let style = StyleWatch::new(&app.display.default_scene.style_sheet);
        let style_frp = &model.style;
        let action_bar = &model.action_bar.frp;

        frp::extend! { network
            init <- source::<()>();
            // Hook up the display object position updates to the node's FRP. Required to calculate
            // the bounding box.
            out.position <+ display_object.on_transformed.map(f_!(display_object.position().xy()));
        }

        frp::extend! { network
            // === Hover ===

            // The hover discovery of a node is an interesting process. First, we discover whether
            // ths user hovers the background. The input port manager merges this information with
            // port hover events and outputs the final hover event for any part inside of the node.

            let background_enter = model.background.on_event::<mouse::Enter>();
            let background_leave = model.background.on_event::<mouse::Leave>();
            background_hover <- bool(&background_leave, &background_enter);
            let input_enter = model.input.on_event::<mouse::Enter>();
            let input_leave = model.input.on_event::<mouse::Leave>();
            input_hover <- bool(&input_leave, &input_enter);
            node_hover <- background_hover || input_hover;
            node_hover <- node_hover.debounce().on_change();
            model.input.set_hover <+ node_hover;
            model.output.set_hover <+ model.input.body_hover;
            out.hover <+ model.output.body_hover;
            model.background.node_is_hovered <+ out.hover;
        }

        frp::extend! { network
            // === Background Press ===

            let background_press = model.background.on_event::<mouse::Down>();
            let input_press = model.input.on_event::<mouse::Down>();
            input_as_background_press <- input_press.gate(&input.set_edit_ready_mode);
            background_press <- background_press.gate_not(&model.input.editing);
            any_background_press <- any(&background_press, &input_as_background_press);
            any_primary_press <- any_background_press.filter(mouse::event::is_primary);
            out.background_press <+_ any_primary_press;
        }

        frp::extend! { network
            // === Selection ===

            selected <- bool(&input.deselect, &input.select);
            eval selected ((selected) model.set_selected(*selected));
        }

        frp::extend! { network
            // === Expression ===

            let unresolved_symbol_type = Some(Type(ImString::new(UNRESOLVED_SYMBOL_TYPE)));
            filtered_usage_type <- input.set_expression_usage_type.filter(
                move |(_,tp)| *tp != unresolved_symbol_type
            );
            eval filtered_usage_type(((a,b)) model.set_expression_usage_type(*a,b));
            eval input.set_expression((a) model.set_expression(a));
            model.input.edit_expression <+ input.edit_expression;
            out.on_expression_modified <+ model.input.frp.on_port_code_update;
            out.requested_widgets <+ model.input.frp.requested_widgets;
            out.request_import <+ model.input.frp.request_import;

            model.input.set_connections <+ input.set_connections;
            model.input.set_disabled <+ input.set_disabled;
            model.input.set_pending <+ input.set_pending;
            model.input.update_widgets <+ input.update_widgets;
            model.output.set_expression_visibility <+ input.set_output_expression_visibility;

        }

        frp::extend! { network
            // === Comment ===


            let comment_base_color = style_frp.get_color(theme::graph_editor::node::text);
            comment_color <- all_with(
                &comment_base_color, &model.output.expression_label_visibility,
                |&base_color,&expression_visible| {
                    let mut color = color::Lcha::from(base_color);
                    color.mod_alpha(|alpha| {
                        // Comment is hidden when output expression (i.e. node name) is visible.
                        if expression_visible { *alpha = 0.0 }
                    });
                    color
            });
            eval comment_color ((value) model.comment.set_property(.., color::Rgba::from(value)));

            eval model.comment.width ([model](width)
                model.comment.set_x(-*width - COMMENT_MARGIN));
            eval model.comment.height ([model](height)
                model.comment.set_y(*height / 2.0));
            model.comment.set_content <+ input.set_comment;
            out.comment <+ model.comment.content.map(|text| text.to_im_string());
        }

        frp::extend! { network
            // === Size ===

            input_width <- all(&model.input.frp.width, &init)._0();
            new_size <- input_width.map(f!((w) model.set_width(*w)));
        }

        frp::extend! { network
            // === Action Bar ===

            out.context_switch <+ action_bar.action_context_switch;
            out.skip   <+ action_bar.action_skip;
            out.freeze <+ action_bar.action_freeze;
            show_action_bar <- node_hover && input.show_quick_action_bar_on_hover;
            eval show_action_bar ((t) action_bar.set_visibility(t));
            eval input.show_quick_action_bar_on_hover((value) action_bar.show_on_hover(value));
            action_bar.set_action_freeze_state <+ input.set_freeze_macro;
            action_bar.set_action_skip_state <+ input.set_skip_macro;
            action_bar.set_action_context_switch_state <+ input.set_context_switch;
            action_bar.set_execution_environment <+ input.set_execution_environment;
        }

        frp::extend! { network
            // === View Mode ===

            model.input.set_view_mode <+ input.set_view_mode;
            model.input.set_edit_ready_mode <+ input.set_edit_ready_mode;
            model.vcs_indicator.set_visibility  <+ input.set_view_mode.map(|&mode| {
                !matches!(mode,view::Mode::Profiling {..})
            });
        }

        frp::extend! { network
            // === Read-only mode ===

            action_bar.set_read_only <+ input.set_read_only;
            model.input.set_read_only <+ input.set_read_only;
        }


        // === Visualizations & Errors ===

        let hover_onset_delay = DelayedAnimation::new(network);
        hover_onset_delay.set_delay(VIS_PREVIEW_ONSET_MS);
        hover_onset_delay.set_duration(0.0);

        let visualization = &model.visualization.frp;

        frp::extend! { network
            enabled <- bool(&input.disable_visualization, &input.enable_visualization);

            out.error <+ input.set_error;
            is_error_set <- input.set_error.map(
                |err| err.as_ref().map_or(false, Error::should_display)
            );
            no_error_set <- not(&is_error_set);
            error_color_anim.target <+ all_with(&input.set_error,&input.set_view_mode,
                f!([style](error,&mode)
                    let error_color = Self::error_color(error,&style);
                    match mode {
                        view::Mode::Normal    => error_color,
                        view::Mode::Profiling => error_color.to_grayscale(),
                    }
                ));

            viz_enabled <- enabled && no_error_set;
            visualization.set_view_state <+ viz_enabled.on_true().constant(visualization::ViewState::Enabled { has_error: false });
            visualization.set_view_state <+ viz_enabled.on_false().constant(visualization::ViewState::Disabled);
        }
        frp::extend! { network
            // Integration between visualization and action bar.
            visualization.set_visualization <+ input.set_visualization;
            is_enabled <- visualization.view_state.map(|state|{
                matches!(state,visualization::ViewState::Enabled { has_error: false })
            });
            action_bar.set_action_visibility_state <+ is_enabled;
            button_set_to_true <- action_bar.user_action_visibility.on_true();
            button_set_to_true_without_error <- button_set_to_true.gate_not(&is_error_set);
            button_set_to_true_with_error <- button_set_to_true.gate(&is_error_set);
            visualization.set_view_state <+ button_set_to_true_without_error.constant(visualization::ViewState::Enabled { has_error: false });
            action_bar.set_action_visibility_state <+ button_set_to_true_with_error.constant(false);

            visualization.set_view_state <+ action_bar.user_action_visibility.on_false().constant(visualization::ViewState::Disabled);
        }
        frp::extend! { network
            // Show preview visualization after some delay, depending on whether we show an error
            // or are in quick preview mode. Also, omit the preview if we don't have an
            // expression.
            has_tooltip    <- model.output.frp.tooltip.map(|tt| tt.has_content());
            has_expression <- input.set_expression.map(|expr| *expr != Expression::default());

            preview_show_delay <- all(&input.quick_preview_vis,&is_error_set);
            preview_show_delay <- preview_show_delay.map(|(quick_preview,is_error)| {
                match(is_error,quick_preview) {
                    (true,_)      => ERROR_PREVIEW_ONSET_MS,
                    (false,false) => if ENABLE_VIS_PREVIEW {VIS_PREVIEW_ONSET_MS} else {f32::MAX},
                    (false,true)  => 0.0
                }
            });
            hover_onset_delay.set_delay <+ preview_show_delay;
            hide_tooltip                <- preview_show_delay.map(|&delay| delay <= f32::EPSILON);
        }
        frp::extend! { network
            output_hover <- model.output.on_port_hover.map(|s| s.is_on());
            visualization_hover <- bool(&model.visualization.on_event::<mouse::Out>(), &model.visualization.on_event::<mouse::Over>());
            hovered_for_preview <- output_hover || visualization_hover;
            // The debounce is needed for a case where user moves mouse cursor from output port to
            // visualization preview. Moving out of the port make `output_hover` emit `false`,
            // making `hovered_for_preview` false for a brief moment - and that moment would cause
            // preview to be hidden, if not debounced.
            hovered_for_preview <- hovered_for_preview.debounce();
            hover_onset_delay.start <+ hovered_for_preview.on_true();
            hover_onset_delay.reset <+ hovered_for_preview.on_false();
            hover_onset_active <- bool(&hover_onset_delay.on_reset, &hover_onset_delay.on_end);
            hover_preview_visible <- has_expression && hover_onset_active;
            hover_preview_visible <- hover_preview_visible.on_change();
            hide_preview <- any(...);
            editing_finished <- model.input.frp.editing.filter(|e| !*e).constant(());
            hide_preview <+ editing_finished;
            preview_enabled <- bool(&hide_preview, &input.show_preview);
            preview_visible <- hover_preview_visible || preview_enabled;
            vis_preview_visible <- preview_visible && no_error_set;
            vis_preview_visible <- vis_preview_visible.on_change();
            visualization.set_view_state <+ vis_preview_visible.on_true().constant(visualization::ViewState::Preview { has_error: false });
            visualization.set_view_state <+ vis_preview_visible.on_false().constant(visualization::ViewState::Disabled);
        }
        frp::extend! { network
            update_error <- all(input.set_error, preview_visible);
            eval update_error([model]((error, visible)){
                if *visible {
                     model.set_error(error.as_ref());
                } else {
                     model.set_error(None);
                }
            });

            eval error_color_anim.value ((value) model.set_error_color(value));
            visualization.set_view_state <+ input.set_error.is_some().map2(&visualization.view_state, |&has_error, state| state.error_status_updated(has_error));

            enable_fullscreen <- frp.enable_fullscreen_visualization.gate(&no_error_set);
            visualization.set_view_state <+ enable_fullscreen.constant(visualization::ViewState::Fullscreen);

        }

        frp::extend! { network
            // === Tooltip ===

            // Hide tooltip if we show the preview vis.
            app.frp.set_tooltip <+ preview_visible.on_true().constant(tooltip::Style::unset_label());
            // Propagate output tooltip. Only if it is not hidden, or to disable it.
            block_tooltip      <- hide_tooltip && has_tooltip;
            app.frp.set_tooltip <+ model.output.frp.tooltip.gate_not(&block_tooltip);
        }

        frp::extend! { network
            // === Type Labels ===`

            model.output.set_type_label_visibility <+ no_error_set.and_not(&visualization.visible);
        }

        frp::extend! { network
            // === Bounding Box ===

            let visualization_size = &model.visualization.frp.size;
            bbox_input <- all4(
                &out.position,&new_size,&visualization.visible,visualization_size);
            out.bounding_box <+ bbox_input.map(|(a,b,c,d)| bounding_box(*a,*b,c.then(|| *d)));

            inner_bbox_input <- all2(&out.position,&new_size);
            out.inner_bounding_box <+ inner_bbox_input.map(|(a,b)| bounding_box(*a,*b,None));
        }

        frp::extend! { network
            // === VCS Handling ===

            model.vcs_indicator.frp.set_status <+ input.set_vcs_status;
        }

        frp::extend! { network
            // === Colors ===

            let port_color_tint = style_frp.get_color_lcha(theme::graph_editor::node::port_color_tint);
            let editing_color = style_frp.get_color_lcha(theme::graph_editor::node::background);
            let pending_alpha_factor =
                style_frp.get_number(theme::graph_editor::node::pending::alpha_factor);
            base_color_source <- source();
            adjusted_base_color <- all_with3(
                &base_color_source, &frp.set_pending, &pending_alpha_factor,
                |c: &color::Lcha, pending, factor| {
                    match *pending {
                        true => c.multiply_alpha(*factor),
                        false => *c,
                    }
                }
            );
            out.base_color <+ adjusted_base_color;
            out.port_color <+ out.base_color.all_with(&port_color_tint, |c, tint| tint.over(*c));
            background_color <- model.input.frp.editing.switch(&frp.base_color, &editing_color);
            node_colors <- all(background_color, frp.port_color);
            eval node_colors(((base, port)) model.update_colors(*base, *port));
            model.input.set_node_colors <+ node_colors;
        }

        base_color_source.emit(color::Lcha(0.4911, 0.3390, 0.72658, 1.0));


        // Init defaults.
        init.emit(());
        model.error_visualization.set_layer(visualization::Layer::Front);
        frp.set_error.emit(None);
        frp.set_disabled.emit(false);
        frp.set_pending.emit(false);
        frp.show_quick_action_bar_on_hover.emit(true);

        let widget = gui::Widget::new(app, frp, model);
        Node { widget }
    }

    #[profile(Debug)]
    fn error_color(error: &Option<Error>, style: &StyleWatch) -> color::Lcha {
        use theme::graph_editor::node::error as error_theme;

        if let Some(error) = error {
            let path = match *error.kind {
                error::Kind::Panic => error_theme::panic,
                error::Kind::Dataflow => error_theme::dataflow,
                error::Kind::Warning => error_theme::warning,
            };
            style.get_color(path).into()
        } else {
            color::Lcha::transparent()
        }
    }

    /// FRP API of the visualization container attached to this node.
    pub fn visualization(&self) -> &visualization::container::Frp {
        &self.model().visualization.frp
    }
}



// === Positioning ===

fn x_offset_to_node_center(node_width: f32) -> f32 {
    node_width / 2.0
}

#[profile(Debug)]
fn bounding_box(
    node_position: Vector2,
    node_size: Vector2,
    visualization_size: Option<Vector2>,
) -> BoundingBox {
    let x_offset_to_node_center = x_offset_to_node_center(node_size.x);
    let node_bbox_pos = node_position + Vector2(x_offset_to_node_center, 0.0) - node_size / 2.0;
    let node_bbox = BoundingBox::from_bottom_left_position_and_size(node_bbox_pos, node_size);
    if let Some(visualization_size) = visualization_size {
        let visualization_pos = node_position + VISUALIZATION_OFFSET;
        let visualization_bbox =
            BoundingBox::from_top_left_position_and_size(visualization_pos, visualization_size);
        node_bbox.concat_ref(visualization_bbox)
    } else {
        node_bbox
    }
}


// === Interaction state ===

/// Information about how the node is being interacted with.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum InteractionState {
    /// The node is being edited (with the cursor / Component Browser).
    EditingExpression,
    /// The node is not being interacted with.
    #[default]
    Normal,
}

impl InteractionState {
    fn editing_expression(self, editing: bool) -> Self {
        match editing {
            true => Self::EditingExpression,
            false => Self::Normal,
        }
    }
}



// ==================
// === Test Utils ===
// ==================

/// Test-specific API.
pub mod test_utils {
    use super::*;

    /// Addional [`NodeModel`] API for tests.
    pub trait NodeModelExt {
        /// Return the `Shape` used for mouse handling of the first output port of the node. Returns
        /// `None` if there are no output ports.
        fn output_port_hover_shape(&self) -> Option<Rectangle>;

        /// Return the `Shape` used for mouse handling of the first input port of the node. Returns
        /// `None` if there are no input ports.
        fn input_port_hover_shape(&self) -> Option<Rectangle>;
    }

    impl NodeModelExt for NodeModel {
        fn output_port_hover_shape(&self) -> Option<Rectangle> {
            let shapes = self.output.model.port_hover_shapes();
            shapes.into_iter().next()
        }

        fn input_port_hover_shape(&self) -> Option<Rectangle> {
            let shapes = self.input.model.port_hover_shapes();
            shapes.into_iter().next()
        }
    }
}
