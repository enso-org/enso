//! Definition of the Node component.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use crate::component::node::profiling::ProfilingLabel;
use crate::component::visualization;
use crate::selection::BoundingBox;
use crate::tooltip;
use crate::view;
use crate::CallWidgetsConfig;
use crate::Type;

use engine_protocol::language_server::ExecutionEnvironment;
use enso_frp as frp;
use enso_frp;
use ensogl::animation::delayed::DelayedAnimation;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::shape::compound::rectangle;
use ensogl::gui;
use ensogl::Animation;
use ensogl_component::text;
use ensogl_hardcoded_theme as theme;
use ensogl_hardcoded_theme;


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
#[warn(missing_docs)]
pub mod profiling;
#[deny(missing_docs)]
pub mod vcs;

pub use error::Error;
pub use expression::Expression;



// =================
// === Constants ===
// =================

#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub const ACTION_BAR_WIDTH: f32 = 180.0;
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub const ACTION_BAR_HEIGHT: f32 = 15.0;
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub const CORNER_RADIUS: f32 = 14.0;
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub const HEIGHT: f32 = 28.0;
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub const PADDING: f32 = 40.0;
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub const RADIUS: f32 = 14.0;

/// Space between the documentation comment and the node.
pub const COMMENT_MARGIN: f32 = 10.0;

const INFINITE: f32 = 99999.0;
const ERROR_VISUALIZATION_SIZE: Vector2 = visualization::container::DEFAULT_SIZE;

const VISUALIZATION_OFFSET_Y: f32 = -20.0;
const VISUALIZATION_OFFSET: Vector2 = Vector2(0.0, VISUALIZATION_OFFSET_Y);

const ENABLE_VIS_PREVIEW: bool = false;
const VIS_PREVIEW_ONSET_MS: f32 = 4000.0;
const ERROR_PREVIEW_ONSET_MS: f32 = 0000.0;
/// A type of unresolved methods. We filter them out, because we don't want to treat them as types
/// for ports and edges coloring (due to bad UX otherwise).
const UNRESOLVED_SYMBOL_TYPE: &str = "Builtins.Main.Unresolved_Symbol";



// ===============
// === Comment ===
// ===============

/// String with documentation comment text for this node.
///
/// This is just a plain string, as this is what text area expects and node just redirects this
/// value,
pub type Comment = ImString;



// ==============
// === Shapes ===
// ==============

/// A node's background area and selection.
#[derive(Debug, Clone, CloneRef)]
pub struct Background {
    shape:           Rectangle,
    inset:           Immutable<f32>,
    selection_color: Immutable<color::Rgba>,
}

impl Background {
    fn new(style: &StyleWatchFrp) -> Self {
        let selection_color =
            style.get_color(ensogl_hardcoded_theme::graph_editor::node::selection).value();
        let selection_size =
            style.get_number(ensogl_hardcoded_theme::graph_editor::node::selection::size).value();
        let selection_offset =
            style.get_number(ensogl_hardcoded_theme::graph_editor::node::selection::offset).value();
        let inset = selection_size + selection_offset;
        let shape = Rectangle();
        shape.set_corner_radius(RADIUS);
        shape.set_frame_border(selection_size);
        shape.set_border_color(color::Rgba::transparent());
        shape.set_inset(inset);
        Self { shape, inset: Immutable(inset), selection_color: Immutable(selection_color) }
    }

    fn set_selected(&self, degree: f32) {
        let selected = self.selection_color;
        let blended = color::Rgba(selected.red, selected.green, selected.blue, degree);
        self.shape.set_border_color(blended);
    }

    fn set_size_and_center_xy(&self, size: Vector2<f32>, center: Vector2<f32>) {
        let size_with_inset = size + 2.0 * Vector2(*self.inset, *self.inset);
        let origin = center - size_with_inset / 2.0;
        self.shape.set_size(size_with_inset);
        self.shape.set_xy(origin);
    }
}

impl display::Object for Background {
    fn display_object(&self) -> &display::object::Instance {
        self.shape.display_object()
    }
}



// =======================
// === Error Indicator ===
// =======================

#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub mod error_shape {
    use super::*;

    ensogl::shape! {
        below = [rectangle];
        alignment = center;
        (style:Style,color_rgba:Vector4<f32>) {
            use ensogl_hardcoded_theme::graph_editor::node as node_theme;

            let width  = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let zoom   = Var::<f32>::from("1.0/zoom()");
            let width  = width  - PADDING.px() * 2.0;
            let height = height - PADDING.px() * 2.0;
            let radius = RADIUS.px();

            let error_width         = style.get_number(node_theme::error::width).px();
            let repeat_x            = style.get_number(node_theme::error::repeat_x).px();
            let repeat_y            = style.get_number(node_theme::error::repeat_y).px();
            let stripe_width        = style.get_number(node_theme::error::stripe_width);
            let stripe_angle        = style.get_number(node_theme::error::stripe_angle);
            let repeat              = Var::<Vector2<Pixels>>::from((repeat_x,repeat_y));
            let stripe_width        = Var::<Pixels>::from(zoom * stripe_width);
            let stripe_red          = Rect((&stripe_width,INFINITE.px()));
            let stripe_angle_rad    = stripe_angle.radians();
            let pattern             = stripe_red.repeat(repeat).rotate(stripe_angle_rad);
            let mask                = Rect((&width,&height)).corners_radius(radius);
            let mask                = mask.grow(error_width);
            let pattern             = mask.intersection(pattern).fill(color_rgba);

            pattern.into()
        }
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
        set_connections       (HashMap<span_tree::PortId, color::Lcha>),
        set_expression        (Expression),
        edit_expression       (text::Range<text::Byte>, ImString),
        set_skip_macro        (bool),
        set_freeze_macro      (bool),
        /// Set whether the output context is explicitly enabled: `Some(true/false)` for
        /// enabled/disabled; `None` for no context switch expression.
        set_context_switch    (Option<bool>),
        set_comment           (Comment),
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
        set_profiling_status              (profiling::Status),
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
        comment                  (Comment),
        context_switch           (bool),
        skip                     (bool),
        freeze                   (bool),
        hover                    (bool),
        error                    (Option<Error>),
        expression_label_visible (bool),
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
#[derive(Clone, CloneRef, Debug)]
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
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct NodeModel {
    // Required for switching the node to a different layer
    pub app:                 Application,
    pub display_object:      display::object::Instance,
    pub background:          Background,
    pub error_indicator:     error_shape::View,
    pub profiling_label:     ProfilingLabel,
    pub input:               input::Area,
    pub output:              output::Area,
    pub visualization:       visualization::Container,
    pub error_visualization: error::Container,
    pub action_bar:          action_bar::ActionBar,
    pub vcs_indicator:       vcs::StatusIndicator,
    pub style:               StyleWatchFrp,
    pub comment:             text::Text,
    pub interaction_state:   Rc<Cell<InteractionState>>,
}

impl NodeModel {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application, registry: visualization::Registry) -> Self {
        use display::shape::compound::rectangle;
        ensogl::shapes_order_dependencies! {
            app.display.default_scene => {
                error_shape               -> output::port::single_port;
                error_shape               -> output::port::multi_port;
                output::port::single_port -> rectangle;
                output::port::multi_port  -> rectangle;
            }
        }

        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);

        let error_indicator = error_shape::View::new();
        let profiling_label = ProfilingLabel::new(app);
        let background = Background::new(&style);
        let vcs_indicator = vcs::StatusIndicator::new(app);
        let display_object = display::object::Instance::new_named("Node");

        display_object.add_child(&profiling_label);
        display_object.add_child(&background);
        display_object.add_child(&vcs_indicator);

        let input = input::Area::new(app);
        let visualization = visualization::Container::new(app, registry);

        display_object.add_child(&visualization);
        display_object.add_child(&input);

        let error_visualization = error::Container::new(app);
        error_visualization.frp.set_size.emit(ERROR_VISUALIZATION_SIZE);

        let action_bar = action_bar::ActionBar::new(app);
        display_object.add_child(&action_bar);

        let output = output::Area::new(app);
        display_object.add_child(&output);

        let comment = text::Text::new(app);
        display_object.add_child(&comment);

        let interaction_state = default();

        let app = app.clone_ref();
        Self {
            app,
            display_object,
            background,
            error_indicator,
            profiling_label,
            input,
            output,
            visualization,
            error_visualization,
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

    /// Set whether the node is being interacted with by moving an edge with the mouse.
    pub fn set_editing_edge(&self, editing: bool) {
        let new_state = self.interaction_state.update(|state| state.editing_edge(editing));
        self.set_layers_for_state(new_state);
    }

    fn set_layers_for_state(&self, new_state: InteractionState) {
        let scene = &self.app.display.default_scene;
        let main_layer;
        let background_layer;
        let text_layer;
        let action_bar_layer;
        match new_state {
            InteractionState::EditingExpression => {
                // Move all sub-components to `edited_node` layer.
                //
                // `action_bar` is moved to the `edited_node` layer as well, though normally it
                // lives on a separate `above_nodes` layer, unlike every other node component.
                main_layer = scene.layers.edited_node.default_partition();
                background_layer = main_layer.clone();
                text_layer = &scene.layers.edited_node_text;
                action_bar_layer = &scene.layers.edited_node;
            }
            InteractionState::EditingEdge => {
                main_layer = scene.layers.main_nodes_level.clone();
                background_layer = scene.layers.main_active_nodes_level.clone();
                text_layer = &scene.layers.label;
                action_bar_layer = &scene.layers.above_nodes;
            }
            InteractionState::Normal => {
                main_layer = scene.layers.main_nodes_level.clone();
                background_layer = main_layer.clone();
                text_layer = &scene.layers.label;
                action_bar_layer = &scene.layers.above_nodes;
            }
        }
        main_layer.add(&self.display_object);
        background_layer.add(&self.background);
        action_bar_layer.add(&self.action_bar);
        // For the text layer, [`Layer::add`] can't be used because text rendering currently uses a
        // separate layer management API.
        self.output.set_label_layer(text_layer);
        self.input.set_label_layer(text_layer);
        self.profiling_label.set_label_layer(text_layer);
        self.comment.add_to_scene_layer(text_layer);
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
        let padded_size = size + Vector2(PADDING, PADDING) * 2.0;
        self.error_indicator.set_size(padded_size);
        self.vcs_indicator.frp.set_size(padded_size);
        let x_offset_to_node_center = x_offset_to_node_center(width);
        let background_origin = Vector2(x_offset_to_node_center, 0.0);
        self.background.set_size_and_center_xy(size, background_origin);
        self.error_indicator.set_x(x_offset_to_node_center);
        self.vcs_indicator.set_x(x_offset_to_node_center);

        let action_bar_width = ACTION_BAR_WIDTH;
        self.action_bar
            .set_x(x_offset_to_node_center + width / 2.0 + CORNER_RADIUS + action_bar_width / 2.0);
        self.action_bar.frp.set_size(Vector2::new(action_bar_width, ACTION_BAR_HEIGHT));

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
        self.error_indicator.color_rgba.set(color::Rgba::from(color).into());
        if color.alpha < f32::EPSILON {
            self.error_indicator.unset_parent();
        } else {
            self.display_object.add_child(&self.error_indicator);
        }
    }

    fn set_selected(&self, degree: f32) {
        self.background.set_selected(degree);
    }
}

impl Node {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    #[profile(Debug)]
    pub fn new(app: &Application, registry: visualization::Registry) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let out = &frp.private.output;
        let input = &frp.private.input;
        let model = Rc::new(NodeModel::new(app, registry));
        let selection = Animation::<f32>::new(network);
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


            // === Background Press ===

            let background_press = model.background.on_event::<mouse::Down>();
            let input_press = model.input.on_event::<mouse::Down>();
            input_as_background_press <- input_press.gate(&input.set_edit_ready_mode);
            any_background_press <- any(&background_press, &input_as_background_press);
            any_primary_press <- any_background_press.filter(mouse::event::is_primary);
            out.background_press <+ any_primary_press.constant(());


            // === Selection ===

            deselect_target  <- input.deselect.constant(0.0);
            select_target    <- input.select.constant(1.0);
            selection.target <+ any(&deselect_target, &select_target);
            eval selection.value ((t) model.set_selected(*t));


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
            model.input.update_widgets <+ input.update_widgets;
            model.output.set_expression_visibility <+ input.set_output_expression_visibility;


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


            // === Size ===

            input_width <- all(&model.input.frp.width, &init)._0();
            new_size <- input_width.map(f!((w) model.set_width(*w)));
            model.output.frp.set_size <+ new_size;


            // === Action Bar ===

            out.context_switch <+ action_bar.action_context_switch;
            out.skip   <+ action_bar.action_skip;
            out.freeze <+ action_bar.action_freeze;
            show_action_bar <- out.hover  && input.show_quick_action_bar_on_hover;
            eval show_action_bar ((t) action_bar.set_visibility(t));
            eval input.show_quick_action_bar_on_hover((value) action_bar.show_on_hover(value));
            action_bar.set_action_freeze_state <+ input.set_freeze_macro;
            action_bar.set_action_skip_state <+ input.set_skip_macro;
            action_bar.set_action_context_switch_state <+ input.set_context_switch;
            action_bar.set_execution_environment <+ input.set_execution_environment;


            // === View Mode ===

            model.input.set_view_mode <+ input.set_view_mode;
            model.input.set_edit_ready_mode <+ input.set_edit_ready_mode;
            model.profiling_label.set_view_mode <+ input.set_view_mode;
            model.vcs_indicator.set_visibility  <+ input.set_view_mode.map(|&mode| {
                !matches!(mode,view::Mode::Profiling {..})
            });


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
            visualization.set_view_state <+ viz_enabled.on_true().constant(visualization::ViewState::Enabled);
            visualization.set_view_state <+ viz_enabled.on_false().constant(visualization::ViewState::Disabled);

            // Integration between visualization and action bar.
            visualization.set_visualization <+ input.set_visualization;
            is_enabled <- visualization.view_state.map(|state|{
                matches!(state,visualization::ViewState::Enabled)
            });
            action_bar.set_action_visibility_state <+ is_enabled;
            button_set_to_true <- action_bar.user_action_visibility.on_true();
            button_set_to_true_without_error <- button_set_to_true.gate_not(&is_error_set);
            button_set_to_true_with_error <- button_set_to_true.gate(&is_error_set);
            visualization.set_view_state <+ button_set_to_true_without_error.constant(visualization::ViewState::Enabled);
            action_bar.set_action_visibility_state <+ button_set_to_true_with_error.constant(false);

            visualization.set_view_state <+ action_bar.user_action_visibility.on_false().constant(visualization::ViewState::Disabled);

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
            visualization.set_view_state <+ vis_preview_visible.on_true().constant(visualization::ViewState::Preview);
            visualization.set_view_state <+ vis_preview_visible.on_false().constant(visualization::ViewState::Disabled);

            update_error <- all(input.set_error,preview_visible);
            eval update_error([model]((error,visible)){
                if *visible {
                     model.set_error(error.as_ref());
                } else {
                     model.set_error(None);
                }
            });

            eval error_color_anim.value ((value) model.set_error_color(value));
            visualization.set_view_state <+ input.set_error.is_some().constant(visualization::ViewState::Disabled);

            enable_fullscreen <- frp.enable_fullscreen_visualization.gate(&no_error_set);
            visualization.set_view_state <+ enable_fullscreen.constant(visualization::ViewState::Fullscreen);

        }

        // === Profiling Indicator ===

        frp::extend! { network
            model.profiling_label.set_min_global_duration
                <+ input.set_profiling_min_global_duration;
            model.profiling_label.set_max_global_duration
                <+ input.set_profiling_max_global_duration;
            model.profiling_label.set_status <+ input.set_profiling_status;
            model.input.set_profiling_status <+ input.set_profiling_status;
        }

        let bg_color_anim = color::Animation::new(network);

        frp::extend! { network

            // === Color Handling ===

            let bgg = style_frp.get_color(ensogl_hardcoded_theme::graph_editor::node::background);
            let profiling_theme = profiling::Theme::from_styles(style_frp,network);

            profiling_color <- all_with5
                (&input.set_profiling_status,&input.set_profiling_min_global_duration,
                &input.set_profiling_max_global_duration,&profiling_theme,&bgg,
                |&status,&min,&max,&theme,&bgg| {
                    if status.is_finished() {
                        status.display_color(min,max,theme).with_alpha(1.0)
                    } else {
                        color::Lcha::from(bgg)
                    }
                });

            bg_color_anim.target <+ all_with3(&bgg,&input.set_view_mode,&profiling_color,
                |bgg,&mode,&profiling_color| {
                    match mode {
                        view::Mode::Normal    => color::Lcha::from(*bgg),
                        view::Mode::Profiling => profiling_color,
                    }
                });

            // FIXME [WD]: Uncomment when implementing disabled icon.
            // bg_color <- frp.set_disabled.map(f!([model,style](disabled) {
            //     model.input.frp.set_disabled(*disabled);
            //     let bg_color_path = ensogl_hardcoded_theme::graph_editor::node::background;
            //     if *disabled { style.get_color_dim(bg_color_path) }
            //     else         { style.get_color(bg_color_path) }
            // }));
            // bg_color_anim.target <+ bg_color;

            eval bg_color_anim.value ((c) model.background.shape.set_color(c.into()););


            // === Tooltip ===

            // Hide tooltip if we show the preview vis.
            app.frp.set_tooltip <+ preview_visible.on_true().constant(tooltip::Style::unset_label());
            // Propagate output tooltip. Only if it is not hidden, or to disable it.
            block_tooltip      <- hide_tooltip && has_tooltip;
            app.frp.set_tooltip <+ model.output.frp.tooltip.gate_not(&block_tooltip);


            // === Type Labels ===

            model.output.set_type_label_visibility
                <+ visualization.visible.not().and(&no_error_set);


            // === Bounding Box ===

            let visualization_size = &model.visualization.frp.size;
            bbox_input <- all4(
                &out.position,&new_size,&visualization.visible,visualization_size);
            out.bounding_box <+ bbox_input.map(|(a,b,c,d)| bounding_box(*a,*b,c.then(|| *d)));

            inner_bbox_input <- all2(&out.position,&new_size);
            out.inner_bounding_box <+ inner_bbox_input.map(|(a,b)| bounding_box(*a,*b,None));


            // === VCS Handling ===

            model.vcs_indicator.frp.set_status <+ input.set_vcs_status;
        }

        // Init defaults.
        init.emit(());
        model.error_visualization.set_layer(visualization::Layer::Front);
        frp.set_error.emit(None);
        frp.set_disabled.emit(false);
        frp.show_quick_action_bar_on_hover.emit(true);

        let display_object = model.display_object.clone_ref();
        let widget = gui::Widget::new(app, frp, model, display_object);
        Node { widget }
    }

    #[profile(Debug)]
    fn error_color(error: &Option<Error>, style: &StyleWatch) -> color::Lcha {
        use ensogl_hardcoded_theme::graph_editor::node::error as error_theme;

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

impl display::Object for Node {
    fn display_object(&self) -> &display::object::Instance {
        self.deref().display_object()
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
    let node_bbox = BoundingBox::from_position_and_size(node_bbox_pos, node_size);
    if let Some(visualization_size) = visualization_size {
        let visualization_pos = node_position + VISUALIZATION_OFFSET;
        let visualization_bbox_pos = visualization_pos - visualization_size / 2.0;
        let visualization_bbox =
            BoundingBox::from_position_and_size(visualization_bbox_pos, visualization_size);
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
    /// An edge of the node is being interacted with.
    EditingEdge,
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

    fn editing_edge(self, active: bool) -> Self {
        match (self, active) {
            (Self::EditingExpression, _) => self,
            (_, true) => Self::EditingEdge,
            (_, false) => Self::Normal,
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
        /// Return the `SinglePortView` of the first output port of the node.
        ///
        /// Returns `None`:
        /// 1. If there are no output ports.
        /// 2. If the port does not have a `PortShapeView`. Some port models does not initialize
        ///    the `PortShapeView`, see [`output::port::Model::init_shape`].
        /// 3. If the output port is [`MultiPortView`].
        fn output_port_shape(&self) -> Option<output::port::SinglePortView>;

        /// Return the `Shape` of the first input port of the node. Returns `None` if there are no
        /// input ports.
        fn input_port_hover_shape(&self) -> Option<input::port::HoverShape>;
    }

    impl NodeModelExt for NodeModel {
        fn output_port_shape(&self) -> Option<output::port::SinglePortView> {
            let ports = self.output.model.ports();
            let port = ports.first()?;
            let shape = port.shape.as_ref()?;
            use output::port::PortShapeView::Single;
            match shape {
                Single(shape) => Some(shape.clone_ref()),
                _ => None,
            }
        }

        fn input_port_hover_shape(&self) -> Option<input::port::HoverShape> {
            let shapes = self.input.model.port_hover_shapes();
            shapes.into_iter().next()
        }
    }
}
