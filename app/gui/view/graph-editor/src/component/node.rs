//! Definition of the Node component.

// WARNING! UNDER HEAVY DEVELOPMENT. EXPECT DRASTIC CHANGES.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use crate::component::node::profiling::ProfilingLabel;
use crate::component::visualization;
use crate::selection::BoundingBox;
use crate::tooltip;
use crate::view;
use crate::Type;

use super::edge;
use enso_frp as frp;
use enso_frp;
use ensogl::animation::delayed::DelayedAnimation;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::Layer;
use ensogl::gui;
use ensogl::Animation;
use ensogl_component::shadow;
use ensogl_component::text;
use ensogl_hardcoded_theme as theme;
use ensogl_hardcoded_theme;
use std::f32::EPSILON;


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
const ERROR_VISUALIZATION_SIZE: (f32, f32) = visualization::container::DEFAULT_SIZE;

const VISUALIZATION_OFFSET_Y: f32 = -120.0;

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



// =============
// === Shape ===
// =============

/// Node background definition.
pub mod background {
    use super::*;

    ensogl::shape! {
        (style:Style, bg_color:Vector4) {
            let bg_color = Var::<color::Rgba>::from(bg_color);
            let width    = Var::<Pixels>::from("input_size.x");
            let height   = Var::<Pixels>::from("input_size.y");
            let width    = width  - PADDING.px() * 2.0;
            let height   = height - PADDING.px() * 2.0;
            let radius   = RADIUS.px();
            let shape    = Rect((&width,&height)).corners_radius(&radius);
            let shape    = shape.fill(bg_color);
            shape.into()
        }
    }
}

/// Node backdrop. Contains shadow and selection.
pub mod backdrop {
    use super::*;

    ensogl::shape! {
        // Disable to allow interaction with the output port.
        pointer_events = false;
        (style:Style, selection:f32) {

            let width  = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let width  = width  - PADDING.px() * 2.0;
            let height = height - PADDING.px() * 2.0;

            // === Shadow ===

            let shadow_radius = &height / 2.0;
            let shadow_base   = Rect((&width,&height)).corners_radius(shadow_radius);
            let shadow        = shadow::from_shape(shadow_base.into(),style);


            // === Selection ===

            let sel_color  = style.get_color(ensogl_hardcoded_theme::graph_editor::node::selection);
            let sel_size   = style.get_number(ensogl_hardcoded_theme::graph_editor::node::selection::size);
            let sel_offset = style.get_number(ensogl_hardcoded_theme::graph_editor::node::selection::offset);

            let sel_width   = &width  - 2.px() + &sel_offset.px() * 2.0 * &selection;
            let sel_height  = &height - 2.px() + &sel_offset.px() * 2.0 * &selection;
            let sel_radius  = &sel_height / 2.0;
            let select      = Rect((&sel_width,&sel_height)).corners_radius(&sel_radius);

            let sel2_width  = &width  - 2.px() + &(sel_size + sel_offset).px() * 2.0 * &selection;
            let sel2_height = &height - 2.px() + &(sel_size + sel_offset).px() * 2.0 * &selection;
            let sel2_radius = &sel2_height / 2.0;
            let select2     = Rect((&sel2_width,&sel2_height)).corners_radius(&sel2_radius);

            let select = select2 - select;
            let select = select.fill(sel_color);


             // === Error Pattern  Alternative ===
             // TODO: Remove once the error indicator design is finalised.
             // let repeat      =  Var::<Vector2<Pixels>>::from((10.px(), 10.px()));
             // let error_width =  Var::<Pixels>::from(5.px());
             //
             // let stripe_red   = Rect((error_width, 99999.px()));
             // let pattern = stripe_red.repeat(repeat).rotate(45.0.radians());
             // let mask    = Rect((&width,&height)).corners_radius(&radius);
             // let pattern1 = mask.intersection(pattern).fill(color::Rgba::red());

             // let out =  select + shadow + shape + pattern1;

            // === Final Shape ===

            let out = select + shadow;
            out.into()
        }
    }
}

#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub mod drag_area {
    use super::*;

    ensogl::shape! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let width  = width  - PADDING.px() * 2.0;
            let height = height - PADDING.px() * 2.0;
            let radius = 14.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            let shape  = shape.fill(color::Rgba::new(0.0,0.0,0.0,0.000_001));

            let out = shape;
            out.into()
        }
    }
}



// =======================
// === Error Indicator ===
// =======================

#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub mod error_shape {
    use super::*;

    ensogl::shape! {
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
            let mask                = Rect((&width,&height)).corners_radius(&radius);
            let mask                = mask.grow(error_width);
            let pattern             = mask.intersection(pattern).fill(color_rgba);

            pattern.into()
        }
    }
}



// ==============
// === Crumbs ===
// ==============

#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub enum Endpoint {
    Input,
    Output,
}

#[derive(Clone, Debug)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct Crumbs {
    pub endpoint: Endpoint,
    pub crumbs:   span_tree::Crumbs,
}

impl Crumbs {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn input(crumbs: span_tree::Crumbs) -> Self {
        let endpoint = Endpoint::Input;
        Self { endpoint, crumbs }
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn output(crumbs: span_tree::Crumbs) -> Self {
        let endpoint = Endpoint::Output;
        Self { endpoint, crumbs }
    }
}

impl Default for Crumbs {
    fn default() -> Self {
        Self::output(default())
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
        disable_visualization (),
        set_visualization     (Option<visualization::Definition>),
        set_disabled          (bool),
        set_input_connected   (span_tree::Crumbs,Option<Type>,bool),
        set_expression        (Expression),
        set_comment           (Comment),
        set_error             (Option<Error>),
        /// Set the expression USAGE type. This is not the definition type, which can be set with
        /// `set_expression` instead. In case the usage type is set to None, ports still may be
        /// colored if the definition type was present.
        set_expression_usage_type         (Crumbs,Option<Type>),
        set_output_expression_visibility  (bool),
        set_vcs_status                    (Option<vcs::Status>),
        /// Show visualization preview until either editing of the node is finished or the
        /// visualization state is explicitly changed by the user. The preview looks the same as
        /// normal visualization, but its state is not persisted in the node's metadata.
        show_preview                      (),
        /// Indicate whether preview visualisations should be delayed or immediate.
        quick_preview_vis                 (bool),
        set_view_mode                     (view::Mode),
        set_profiling_min_global_duration (f32),
        set_profiling_max_global_duration (f32),
        set_profiling_status              (profiling::Status),
        /// Indicate whether on hover the quick action icons should appear.
        show_quick_action_bar_on_hover    (bool)
    }
    Output {
        /// Press event. Emitted when user clicks on non-active part of the node, like its
        /// background. In edit mode, the whole node area is considered non-active.
        background_press         (),
        expression               (ImString),
        comment                  (Comment),
        skip                     (bool),
        freeze                   (bool),
        hover                    (bool),
        error                    (Option<Error>),
        /// Whether visualization was permanently enabled (e.g. by pressing the button).
        visualization_enabled    (bool),
        /// Visualization can be visible even when it is not enabled, e.g. when showing preview.
        /// Visualization can be invisible even when enabled, e.g. when the node has an error.
        visualization_visible    (bool),
        visualization_path       (Option<visualization::Path>),
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
    pub app:                 Application,
    pub display_object:      display::object::Instance,
    pub logger:              Logger,
    pub backdrop:            backdrop::View,
    pub background:          background::View,
    pub drag_area:           drag_area::View,
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
}

impl NodeModel {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application, registry: visualization::Registry) -> Self {
        ensogl::shapes_order_dependencies! {
            app.display.default_scene => {
                //TODO[ao] The two lines below should not be needed - the ordering should be
                //    transitive. But removing them causes a visual glitches described in
                //    https://github.com/enso-org/ide/issues/1624
                //    The matter should be further investigated.
                edge::back::corner        -> backdrop;
                edge::back::line          -> backdrop;
                edge::back::corner        -> error_shape;
                edge::back::line          -> error_shape;
                error_shape               -> backdrop;
                backdrop                  -> output::port::single_port;
                backdrop                  -> output::port::multi_port;
                output::port::single_port -> background;
                output::port::multi_port  -> background;
                background                -> drag_area;
                drag_area                 -> edge::front::corner;
                drag_area                 -> edge::front::line;
                edge::front::corner       -> input::port::hover;
                edge::front::line         -> input::port::hover;
                input::port::hover        -> input::port::viz;
            }
        }

        let scene = &app.display.default_scene;
        let logger = Logger::new("node");

        let error_indicator = error_shape::View::new();
        let profiling_label = ProfilingLabel::new(app);
        let backdrop = backdrop::View::new();
        let background = background::View::new();
        let drag_area = drag_area::View::new();
        let vcs_indicator = vcs::StatusIndicator::new(app);
        let display_object = display::object::Instance::new();

        display_object.add_child(&profiling_label);
        display_object.add_child(&drag_area);
        display_object.add_child(&backdrop);
        display_object.add_child(&background);
        display_object.add_child(&vcs_indicator);

        let input = input::Area::new(app);
        let visualization = visualization::Container::new(&logger, app, registry);

        display_object.add_child(&visualization);
        display_object.add_child(&input);

        let error_visualization = error::Container::new(scene);
        let (x, y) = ERROR_VISUALIZATION_SIZE;
        error_visualization.set_size.emit(Vector2(x, y));

        let action_bar = action_bar::ActionBar::new(app);
        display_object.add_child(&action_bar);
        scene.layers.above_nodes.add(&action_bar);

        let output = output::Area::new(app);
        display_object.add_child(&output);

        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);

        let comment = text::Text::new(app);
        display_object.add_child(&comment);

        let app = app.clone_ref();
        Self {
            app,
            display_object,
            logger,
            backdrop,
            background,
            drag_area,
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
        }
        .init()
    }

    #[profile(Debug)]
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn get_crumbs_by_id(&self, id: ast::Id) -> Option<Crumbs> {
        let input_crumbs = self.input.get_crumbs_by_id(id).map(Crumbs::input);
        input_crumbs.or_else(|| self.output.get_crumbs_by_id(id).map(Crumbs::output))
    }

    #[profile(Debug)]
    fn init(self) -> Self {
        self.set_expression(Expression::new_plain("empty"));
        self
    }

    #[profile(Debug)]
    fn set_layers(&self, layer: &Layer, text_layer: &Layer, action_bar_layer: &Layer) {
        layer.add(&self.display_object);
        action_bar_layer.add(&self.action_bar);
        self.output.set_label_layer(text_layer);
        self.input.set_label_layer(text_layer);
        self.profiling_label.set_label_layer(text_layer);
        self.comment.add_to_scene_layer(text_layer);
    }

    /// Move all sub-components to `edited_node` layer.
    ///
    /// A simple [`Layer::add`] wouldn't work because text rendering in ensogl uses a
    /// separate layer management API.
    ///
    /// `action_bar` is moved to the `edited_node` layer as well, though normally it lives on a
    /// separate `above_nodes` layer, unlike every other node component.
    #[profile(Debug)]
    pub fn move_to_edited_node_layer(&self) {
        let scene = &self.app.display.default_scene;
        let layer = &scene.layers.edited_node;
        let text_layer = &scene.layers.edited_node_text;
        let action_bar_layer = &scene.layers.edited_node;
        self.set_layers(layer, text_layer, action_bar_layer);
    }

    /// Move all sub-components to `main` layer.
    ///
    /// A simple [`Layer::add`] wouldn't work because text rendering in ensogl uses a
    /// separate layer management API.
    ///
    /// `action_bar` is handled separately, as it uses `above_nodes` scene layer unlike any other
    /// node component.
    #[profile(Debug)]
    pub fn move_to_main_layer(&self) {
        let scene = &self.app.display.default_scene;
        let layer = &scene.layers.main;
        let text_layer = &scene.layers.label;
        let action_bar_layer = &scene.layers.above_nodes;
        self.set_layers(layer, text_layer, action_bar_layer);
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
    fn set_expression_usage_type(&self, crumbs: &Crumbs, tp: &Option<Type>) {
        match crumbs.endpoint {
            Endpoint::Input => self.input.set_expression_usage_type(&crumbs.crumbs, tp),
            Endpoint::Output => self.output.set_expression_usage_type(&crumbs.crumbs, tp),
        }
    }

    #[profile(Debug)]
    fn set_width(&self, width: f32) -> Vector2 {
        let height = self.height();
        let size = Vector2(width, height);
        let padded_size = size + Vector2(PADDING, PADDING) * 2.0;
        self.backdrop.size.set(padded_size);
        self.background.size.set(padded_size);
        self.drag_area.size.set(padded_size);
        self.error_indicator.size.set(padded_size);
        self.vcs_indicator.set_size(padded_size);
        let x_offset_to_node_center = x_offset_to_node_center(width);
        self.backdrop.set_position_x(x_offset_to_node_center);
        self.background.set_position_x(x_offset_to_node_center);
        self.drag_area.set_position_x(x_offset_to_node_center);
        self.error_indicator.set_position_x(x_offset_to_node_center);
        self.vcs_indicator.set_position_x(x_offset_to_node_center);

        let action_bar_width = ACTION_BAR_WIDTH;
        self.action_bar.mod_position(|t| {
            t.x = x_offset_to_node_center + width / 2.0 + CORNER_RADIUS + action_bar_width / 2.0;
        });
        self.action_bar.frp.set_size(Vector2::new(action_bar_width, ACTION_BAR_HEIGHT));

        let visualization_offset = visualization_offset(width);
        self.error_visualization.set_position_xy(visualization_offset);
        self.visualization.set_position_xy(visualization_offset);

        size
    }

    #[profile(Debug)]
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn visualization(&self) -> &visualization::Container {
        &self.visualization
    }

    #[profile(Debug)]
    fn set_error(&self, error: Option<&Error>) {
        if let Some(error) = error {
            self.error_visualization.display_kind(*error.kind);
            if let Some(error_data) = error.visualization_data() {
                self.error_visualization.set_data(&error_data);
            }
            self.display_object.add_child(&self.error_visualization);
        } else {
            self.error_visualization.unset_parent();
        }
    }

    #[profile(Debug)]
    fn set_error_color(&self, color: &color::Lcha) {
        self.error_indicator.color_rgba.set(color::Rgba::from(color).into());
        if color.alpha < EPSILON {
            self.error_indicator.unset_parent();
        } else {
            self.display_object.add_child(&self.error_indicator);
        }
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

        // TODO[ao] The comment color should be animated, but this is currently slow. Will be fixed
        //      in https://github.com/enso-org/ide/issues/1031
        // let comment_color    = color::Animation::new(network);
        let error_color_anim = color::Animation::new(network);
        let style = StyleWatch::new(&app.display.default_scene.style_sheet);
        let style_frp = &model.style;
        let action_bar = &model.action_bar.frp;
        // Hook up the display object position updates to the node's FRP. Required to calculate the
        // bounding box.
        model.display_object.set_on_updated(f!((p) out.position.emit(p.position().xy())));

        frp::extend! { network

            // === Hover ===
            // The hover discovery of a node is an interesting process. First, we discover whether
            // ths user hovers the drag area. The input port manager merges this information with
            // port hover events and outputs the final hover event for any part inside of the node.

            let drag_area = &model.drag_area.events;
            drag_area_hover <- bool(&drag_area.mouse_out,&drag_area.mouse_over);
            model.input.set_hover  <+ drag_area_hover;
            model.output.set_hover <+ model.input.body_hover;
            out.hover <+ model.output.body_hover;


            // === Background Press ===

            out.background_press <+ model.drag_area.events.mouse_down_primary;
            out.background_press <+ model.input.on_background_press;


            // === Selection ===

            deselect_target  <- input.deselect.constant(0.0);
            select_target    <- input.select.constant(1.0);
            selection.target <+ any(&deselect_target,&select_target);
            eval selection.value ((t) model.backdrop.selection.set(*t));


            // === Expression ===

            let unresolved_symbol_type = Some(Type(ImString::new(UNRESOLVED_SYMBOL_TYPE)));
            filtered_usage_type <- input.set_expression_usage_type.filter(
                move |(_,tp)| *tp != unresolved_symbol_type
            );
            eval filtered_usage_type (((a,b)) model.set_expression_usage_type(a,b));
            eval input.set_expression  ((a)     model.set_expression(a));
            out.expression                  <+ model.input.frp.expression;
            model.input.set_connected              <+ input.set_input_connected;
            model.input.set_disabled               <+ input.set_disabled;
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
                model.comment.set_position_x(-*width - COMMENT_MARGIN));
            eval model.comment.height ([model](height)
                model.comment.set_position_y(*height / 2.0));
            model.comment.set_content <+ input.set_comment;
            out.comment <+ model.comment.content.map(|text| text.to_im_string());


            // === Size ===

            new_size <- model.input.frp.width.map(f!((w) model.set_width(*w)));
            model.output.frp.set_size <+ new_size;


            // === Action Bar ===

            let visualization_button_state = action_bar.action_visibility.clone_ref();
            out.skip   <+ action_bar.action_skip;
            out.freeze <+ action_bar.action_freeze;
            show_action_bar <- out.hover  && input.show_quick_action_bar_on_hover;
            eval show_action_bar ((t) action_bar.set_visibility(t));
            eval input.show_quick_action_bar_on_hover((value) action_bar.show_on_hover(value));


            // === View Mode ===

            model.input.set_view_mode <+ input.set_view_mode;
            model.output.set_view_mode <+ input.set_view_mode;
            model.profiling_label.set_view_mode <+ input.set_view_mode;
            model.vcs_indicator.set_visibility  <+ input.set_view_mode.map(|&mode| {
                !matches!(mode,view::Mode::Profiling {..})
            });
        }


        // === Visualizations & Errors ===

        let hover_onset_delay = DelayedAnimation::new(network);
        hover_onset_delay.set_delay(VIS_PREVIEW_ONSET_MS);
        hover_onset_delay.set_duration(0.0);

        frp::extend! { network

            out.error <+ input.set_error;
            is_error_set <- input.set_error.map(|err| err.is_some());
            no_error_set <- not(&is_error_set);
            error_color_anim.target <+ all_with(&input.set_error,&input.set_view_mode,
                f!([style](error,&mode)
                    let error_color = Self::error_color(error,&style);
                    match mode {
                        view::Mode::Normal    => error_color,
                        view::Mode::Profiling => error_color.to_grayscale(),
                    }
                ));

            eval input.set_visualization ((t) model.visualization.frp.set_visualization.emit(t));
            visualization_enabled_frp <- bool(&input.disable_visualization,&input.enable_visualization);
            eval visualization_enabled_frp ((enabled)
                model.action_bar.set_action_visibility_state(enabled)
            );

            // Show preview visualisation after some delay, depending on whether we show an error
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
            hide_tooltip                <- preview_show_delay.map(|&delay| delay <= EPSILON);

            outout_hover            <- model.output.on_port_hover.map(|s| s.is_on());
            hover_onset_delay.start <+ outout_hover.on_true();
            hover_onset_delay.reset <+ outout_hover.on_false();
            hover_onset_active <- bool(&hover_onset_delay.on_reset, &hover_onset_delay.on_end);
            hover_preview_visible <- has_expression && hover_onset_active;
            hover_preview_visible <- hover_preview_visible.on_change();
            hide_preview <- any(...);
            editing_finished <- model.input.frp.editing.filter(|e| !*e).constant(());
            hide_preview <+ editing_finished;
            preview_enabled <- bool(&hide_preview, &input.show_preview);
            preview_visible <- hover_preview_visible || preview_enabled;
            preview_visible <- preview_visible.on_change();

            // If the preview is visible while the visualization button is disabled, clicking the
            // visualization button hides the preview and keeps the visualization button disabled.
            vis_button_on <- visualization_button_state.filter(|e| *e).constant(());
            vis_button_off <- visualization_button_state.filter(|e| !*e).constant(());
            visualization_on <- vis_button_on.gate_not(&preview_visible);
            vis_button_on_while_preview_visible <- vis_button_on.gate(&preview_visible);
            hide_preview <+ vis_button_on_while_preview_visible;
            hide_preview <+ vis_button_off;
            action_bar.set_action_visibility_state <+
                vis_button_on_while_preview_visible.constant(false);
            visualization_enabled <- bool(&vis_button_off, &visualization_on);

            visualization_visible            <- visualization_enabled || preview_visible;
            visualization_visible            <- visualization_visible && no_error_set;
            visualization_visible_on_change  <- visualization_visible.on_change();
            out.visualization_visible <+ visualization_visible_on_change;
            out.visualization_enabled <+ visualization_enabled;
            eval visualization_visible_on_change ((is_visible)
                model.visualization.frp.set_visibility(is_visible)
            );
            init <- source::<()>();
            out.visualization_path <+ model.visualization.frp.visualisation.all_with(&init,|def_opt,_| {
                def_opt.as_ref().map(|def| def.signature.path.clone_ref())
            });

            // Ensure the preview is visible above all other elements, but the normal visualisation
            // is below nodes.
            layer_on_hover     <- hover_preview_visible.on_false().map(|_| visualization::Layer::Default);
            layer_on_not_hover <- hover_preview_visible.on_true().map(|_| visualization::Layer::Front);
            layer              <- any(layer_on_hover,layer_on_not_hover);
            model.visualization.frp.set_layer <+ layer;

            update_error <- all(input.set_error,preview_visible);
            eval update_error([model]((error,visible)){
                if *visible {
                     model.set_error(error.as_ref());
                } else {
                     model.set_error(None);
                }
            });

            eval error_color_anim.value ((value) model.set_error_color(value));

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
            // eval bg_color_anim.value ((c)
            //     model.background.bg_color.set(color::Rgba::from(c).into())
            // );

            eval bg_color_anim.value ((c)
                model.background.bg_color.set(color::Rgba::from(c).into()));


            // === Tooltip ===

            // Hide tooltip if we show the preview vis.
            app.frp.set_tooltip <+ preview_visible.on_true().constant(tooltip::Style::unset_label());
            // Propagate output tooltip. Only if it is not hidden, or to disable it.
            block_tooltip      <- hide_tooltip && has_tooltip;
            app.frp.set_tooltip <+ model.output.frp.tooltip.gate_not(&block_tooltip);


            // === Type Labels ===

            model.output.set_type_label_visibility
                <+ visualization_visible.not().and(&no_error_set);


            // === Bounding Box ===

            let visualization_size = &model.visualization.frp.size;
            // Visualization can be enabled and not visible when the node has an error.
            visualization_enabled_and_visible <- visualization_enabled && visualization_visible;
            bbox_input <- all4(
                &out.position,&new_size,&visualization_enabled_and_visible,visualization_size);
            out.bounding_box <+ bbox_input.map(|(a,b,c,d)| bounding_box(*a,*b,*c,*d));


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
            };
            style.get_color(path).into()
        } else {
            color::Lcha::transparent()
        }
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

/// Calculate a position where to render the [`visualization::Container`] of a node, relative to
/// the node's origin.
fn visualization_offset(node_width: f32) -> Vector2 {
    Vector2(x_offset_to_node_center(node_width), VISUALIZATION_OFFSET_Y)
}

#[profile(Debug)]
fn bounding_box(
    node_position: Vector2,
    node_size: Vector2,
    visualization_enabled_and_visible: bool,
    visualization_size: Vector2,
) -> BoundingBox {
    let x_offset_to_node_center = x_offset_to_node_center(node_size.x);
    let node_bbox_pos = node_position + Vector2(x_offset_to_node_center, 0.0) - node_size / 2.0;
    let node_bbox = BoundingBox::from_position_and_size(node_bbox_pos, node_size);
    if visualization_enabled_and_visible {
        let visualization_offset = visualization_offset(node_size.x);
        let visualization_pos = node_position + visualization_offset;
        let visualization_bbox_pos = visualization_pos - visualization_size / 2.0;
        let visualization_bbox =
            BoundingBox::from_position_and_size(visualization_bbox_pos, visualization_size);
        node_bbox.concat_ref(visualization_bbox)
    } else {
        node_bbox
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

        /// Return the `Shape` of the first input port of the node.
        ///
        /// Returns `None`:
        /// 1. If there are no input ports.
        /// 2. If the port does not have a `Shape`. Some port models does not initialize the
        ///    `Shape`, see [`input::port::Model::init_shape`].
        fn input_port_shape(&self) -> Option<input::port::Shape>;
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

        fn input_port_shape(&self) -> Option<input::port::Shape> {
            let ports = self.input.model.ports();
            let port = ports.first()?;
            port.shape.as_ref().map(CloneRef::clone_ref)
        }
    }
}
