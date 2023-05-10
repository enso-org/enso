//! Definition of the Edge component.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl_hardcoded_theme as theme;
use nalgebra::Rotation2;



// =================
// === Constants ===
// =================

const LINE_WIDTH: f32 = 4.0;
const ARROW_SIZE_X: f32 = 20.0;
const ARROW_SIZE_Y: f32 = 20.0;

const HOVER_EXTENSION: f32 = 10.0;

const RIGHT_ANGLE: f32 = std::f32::consts::PI / 2.0;

/// The threshold for the y-distance between nodes at which we switch from using the y-distance
/// only to determine the closest port to using the full cartesian distance.
const MIN_SOURCE_TARGET_DIFFERENCE_FOR_Y_VALUE_DISCRIMINATION: f32 = 45.0;

const HOVER_COLOR: color::Rgba = color::Rgba::new(1.0, 0.0, 0.0, 0.000_001);



// ===================
// === Vector Math ===
// ===================

fn up() -> Vector2<f32> {
    Vector2(1.0, 0.0)
}

fn point_rotation(point: Vector2<f32>) -> Rotation2<f32> {
    Rotation2::rotation_between(&point, &up())
}



// ==================
// === SnapTarget ===
// ==================

/// `SnapTarget` is the result value of snapping operations on `AnyEdgeShape`. It holds the
/// shape that a hover position was snapped to and the snapped position on the shape. The snapped
/// position lies (a) on the visible part of the shape and (b) is the closes position on the shape
/// to the source position that was used to compute the snapped position.
#[derive(Clone, Debug)]
struct SnapTarget {
    position:        Vector2<f32>,
    target_shape_id: display::object::Id,
}

impl SnapTarget {
    fn new(position: Vector2<f32>, target_shape_id: display::object::Id) -> Self {
        SnapTarget { position, target_shape_id }
    }
}



// =========================
// === Shape Definitions ===
// =========================

macro_rules! define_arrow { () => {
    /// Shape definition.
    pub mod arrow {
        use super::*;
        ensogl::shape! {
            above = [joint];
            alignment = center;
            (
                style: Style,
                focus_split_center: Vector2<f32>,
                focus_split_angle: f32,
                color_rgba: Vector4<f32>,
                focus_color_rgba: Vector4<f32>
            ) {
                let width  : Var<Pixels> = "input_size.x".into();
                let height : Var<Pixels> = "input_size.y".into();
                let color                = Var::<color::Rgba>::from(color_rgba);
                let focus_color          = Var::<color::Rgba>::from(focus_color_rgba);
                let focus_split_angle    = focus_split_angle.into();
                let focus_split_center   = focus_split_center.px();

                let shape_padding = -1.px();
                let shape         = Triangle(width+&shape_padding,height+&shape_padding);
                let shape         = FocusedEdge::new(shape,&focus_split_center,&focus_split_angle);
                let shape         = shape.fill(&color, &focus_color);
                shape.into()
            }
        }

        impl EdgeShape for View {
            fn set_focus_split_center_local(&self, center:Vector2<f32>) {
                // We don't want the arrow to be half-focused. The focus split point is set to the
                // closest edge (all or nothing).
                let min = -Vector2(ARROW_SIZE_X,ARROW_SIZE_Y);
                let max =  Vector2(ARROW_SIZE_X,ARROW_SIZE_Y);
                let mid =  Vector2::<f32>::zero();
                let x   = if center.x < mid.x { min.x } else { max.x };
                let y   = if center.y < mid.y { min.y } else { max.y };
                self.focus_split_center.set(Vector2(x,y));
            }

            fn set_focus_split_angle(&self, angle:f32) {
                 self.focus_split_angle.set(angle);
            }

            fn events(&self) -> &PointerTarget_DEPRECATED {
                &self.events_deprecated
            }

            fn set_color(&self, color:color::Rgba) {
                self.color_rgba.set(Vector4(color.red,color.green,color.blue,color.alpha));
            }

            fn set_color_focus(&self, color:color::Rgba) {
                self.focus_color_rgba.set(Vector4(color.red,color.green,color.blue,color.alpha));
            }

            fn normal_local(&self, _:Vector2<f32>) -> Rotation2<f32> {
                Rotation2::new(0.0)
            }

            fn normal(&self, _point:Vector2<f32>) -> Rotation2<f32> {
                 Rotation2::new(-RIGHT_ANGLE)
            }

            fn snap_local(&self, point:Vector2<f32>) -> Option<Vector2<f32>> {
                Some(Vector2(0.0, point.y))
            }
        }
    }
}}

const NODE_CORNER_RADIUS: f32 = 14.0;

const MIN_RADIUS: f32 = 20.0;
const MAX_RADIUS: f32 = 30.0;



// ===========
// === FRP ===
// ===========

/// FRP endpoints of the edge.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Frp {
    pub source_width:    frp::Source<f32>,
    pub source_height:   frp::Source<f32>,
    pub target_position: frp::Source<Vector2<f32>>,
    pub target_attached: frp::Source<bool>,
    pub source_attached: frp::Source<bool>,
    pub redraw:          frp::Source,
    pub set_disabled:    frp::Source<bool>,
    pub set_color:       frp::Source<color::Lcha>,

    pub hover_position: frp::Source<Option<Vector2<f32>>>,
    pub shape_events:   PointerTargetProxy,
}

impl Frp {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            def source_width    = source();
            def source_height   = source();
            def target_position = source();
            def target_attached = source();
            def source_attached = source();
            def redraw          = source();
            def hover_position  = source();
            def set_disabled    = source();
            def set_color       = source();
        }
        let shape_events = PointerTargetProxy::new(network);
        Self {
            source_width,
            source_height,
            target_position,
            target_attached,
            source_attached,
            redraw,
            set_disabled,
            set_color,
            hover_position,
            shape_events,
        }
    }
}


// === Pointer Target Proxy ===

/// FRP system that is used to collect and aggregate shape view events from the sub-shapes of an
/// `Edge`. The Edge exposes the `mouse_down`/`mouse_over`/`mouse_out` streams, while the sub-shapes
/// emit events via the internal `on_mouse_down`/`on_mouse_over`/`on_mouse_out` sources.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct PointerTargetProxy {
    pub mouse_down_primary: frp::Stream,
    pub mouse_over:         frp::Stream,
    pub mouse_out:          frp::Stream,

    on_mouse_down: frp::Source<display::object::Id>,
    on_mouse_over: frp::Source<display::object::Id>,
    on_mouse_out:  frp::Source<display::object::Id>,
}

#[allow(missing_docs)]
impl PointerTargetProxy {
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            on_mouse_over <- source();
            on_mouse_out  <- source();
            on_mouse_down <- source();

            mouse_down_primary <- on_mouse_down.constant(());
            mouse_over <- on_mouse_over.constant(());
            mouse_out  <- on_mouse_out.constant(());
        }

        Self {
            mouse_down_primary,
            mouse_over,
            mouse_out,
            on_mouse_down,
            on_mouse_over,
            on_mouse_out,
        }
    }
}



// ==================
// === Math Utils ===
// ==================

/// For the given radius of the first circle (`r1`), radius of the second circle (`r2`), and the
/// x-axis position of the second circle (`x`), computes the y-axis position of the second circle in
/// such a way, that the borders of the circle cross at the right angle. It also computes the angle
/// of the intersection. Please note, that the center of the first circle is in the origin.
///
/// ```text
///       r1
///      ◄───►                (1) x^2 + y^2 = r1^2 + r2^2
///    _____                  (1) => y = sqrt((r1^2 + r2^2)/x^2)
///  .'     `.
/// /   _.-"""B-._     ▲
/// | .'0┼    |   `.   │      angle1 = A-XY-0
/// \/   │    /     \  │ r2   angle2 = 0-XY-B
/// |`._ │__.'       | │      alpha  = B-XY-X_AXIS
/// |   A└───┼─      | ▼
/// |      (x,y)     |        tg(angle1) = y  / x
///  \              /         tg(angle2) = r1 / r2
///   `._        _.'          alpha      = PI - angle1 - angle2
///      `-....-'
/// ```
fn circle_intersection(x: f32, r1: f32, r2: f32) -> (f32, f32) {
    let x_norm = x.clamp(-r2, r1);
    let y = (r1 * r1 + r2 * r2 - x_norm * x_norm).sqrt();
    let angle1 = f32::atan2(y, x_norm);
    let angle2 = f32::atan2(r1, r2);
    let angle = std::f32::consts::PI - angle1 - angle2;
    (y, angle)
}



// ============
// === Edge ===
// ============

/// Edge definition.
#[derive(AsRef, Clone, CloneRef, Debug, Deref)]
pub struct Edge {
    #[deref]
    model:   EdgeModel,
    network: frp::Network,
}

impl AsRef<Edge> for Edge {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl display::Object for EdgeModelData {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl Edge {
    /// Constructor.
    #[profile(Detail)]
    pub fn new(app: &Application) -> Self {
        let network = frp::Network::new("node_edge");
        let data = Rc::new(EdgeModelData::new(&app.display.default_scene, &network));
        let model = EdgeModel { data };
        Self { model, network }.init(app)
    }

    fn init(self, app: &Application) -> Self {
        let network = &self.network;
        let input = &self.frp;

        let model = &self.model;
        let shape_events = &self.frp.shape_events;
        let edge_color = color::Animation::new(network);
        let edge_focus_color = color::Animation::new(network);
        let _style = StyleWatch::new(&app.display.default_scene.style_sheet);

        //model.data.front.register_proxy_frp(network, &input.shape_events);
        //model.data.back.register_proxy_frp(network, &input.shape_events);

        frp::extend! { network
            eval input.target_position ((t) model.set_target_position(*t));
            eval input.source_attached ((t) model.set_source_attached(*t));
            eval input.target_attached ((t) model.set_target_attached(*t));
            eval input.source_width    ((t) model.set_source_width(*t));
            eval input.source_height   ((t) model.set_source_height(*t));
            eval input.hover_position  ((t) model.set_hover_position(*t));

            //eval  shape_events.on_mouse_over ((id) hover_target.set(Some(*id)));
            //eval_ shape_events.on_mouse_out       (hover_target.set(None));
            eval_ input.redraw                    (model.redraw());


            // === Colors ===

            is_hovered      <- input.hover_position.map(|t| t.is_some());
            new_color       <- all_with(&input.set_color,&input.set_disabled,
                f!((c,t)model.base_color(*c,*t)));
            new_focus_color <- new_color.map(f!((color) model.focus_color(*color)));
            focus_color     <- switch(&is_hovered,&new_color,&new_focus_color);

            edge_color.target       <+ new_color;
            edge_focus_color.target <+ focus_color;

            eval edge_color.value       ((color) model.set_color(color.into()));
            eval edge_focus_color.value ((color) model.set_focus_color(color.into()));
        }
        self
    }
}

impl display::Object for Edge {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =================
// === EdgeModel ===
// =================

/// Indicates the type of end connection of the Edge.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum PortType {
    InputPort,
    OutputPort,
}

/// Edge definition.
#[derive(AsRef, Clone, CloneRef, Debug, Deref)]
pub struct EdgeModel {
    data: Rc<EdgeModelData>,
}

/// Internal data of `Edge`
#[derive(Debug)]
pub struct EdgeModelData {
    /// The parent display object of all the edge's parts.
    display_object:      display::object::Instance,
    /// The edge's FRP endpoints.
    pub frp:             Frp,
    /// The width of the node that originates the edge. The edge may begin anywhere around the
    /// bottom half of the node.
    pub source_width:    Cell<f32>,
    /// The height of the node that originates the edge. The edge may begin anywhere around the
    /// bottom half of the node.
    pub source_height:   Cell<f32>,
    /// The coordinates of the node input the edge connects to. The edge enters the node from
    /// above.
    pub target_position: Cell<Vector2>,
    /// Whether the edge is connected to a node input.
    target_attached:     Cell<bool>,
    /// Whether the edge is connected to a node output.
    source_attached:     Cell<bool>,
    /// The location of the mouse over the edge.
    hover_position:      Cell<Option<Vector2<f32>>>,
    color:               Cell<color::Rgba>,

    sections: RefCell<Vec<Rectangle>>,
    hover_sections: RefCell<Vec<Rectangle>>,

    scene: Scene,
}

impl EdgeModelData {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(scene: &Scene, network: &frp::Network) -> Self {
        let display_object = display::object::Instance::new_named("Edge");

        let frp = Frp::new(network);
        let source_height = default();
        let source_width = default();
        let target_position = default();
        let target_attached = default();
        let source_attached = Cell::new(true);
        let hover_position = default();
        let color = default();
        let sections = default();
        let hover_sections = default();

        let scene = scene.into();
        Self {
            display_object,
            frp,
            source_width,
            source_height,
            target_position,
            target_attached,
            source_attached,
            hover_position,
            color,
            sections,
            hover_sections,
            scene,
        }
    }

    /// Set the color of the edge.
    fn set_color(&self, color: color::Lcha) {
        // We must never use alpha in edges, as it will show artifacts with overlapping sub-parts.
        // XXX: This should no longer be necessary.
        let color: color::Lcha = color.opaque.into();
        let color_rgba = color::Rgba::from(color);
        self.color.set(color_rgba);
        for shape in &*self.sections.borrow() {
            shape.set_border_color(color_rgba);
        }
    }

    fn set_focus_color(&self, color: color::Lcha) {
        let color: color::Lcha = color.opaque.into();
        //self.shapes().iter().for_each(|shape| shape.set_color_focus(color.into()));
    }

    fn base_color(&self, color: color::Lcha, is_disabled: bool) -> color::Lcha {
        let color: color::Lcha = color.opaque.into();
        if !is_disabled {
            color
        } else {
            let styles = StyleWatch::new(&self.scene.style_sheet);
            styles.get_color(theme::code::syntax::disabled).into()
        }
    }

    fn focus_color(&self, color: color::Lcha) -> color::Lcha {
        // We must never use alpha in edges, as it will show artifacts with overlapping sub-parts.
        let color: color::Lcha = color.opaque.into();
        let styles = StyleWatch::new(&self.scene.style_sheet);
        let bg_color = styles.get_color(theme::application::background).into();
        color::mix(bg_color, color, 0.25)
    }

    fn set_source_width(&self, width: f32) {
        self.source_width.set(width);
    }

    fn set_source_height(&self, height: f32) {
        self.source_height.set(height);
    }

    fn set_target_position(&self, position: Vector2<f32>) {
        self.target_position.set(position);
    }

    fn set_target_attached(&self, attached: bool) {
        self.target_attached.set(attached);
    }

    fn set_source_attached(&self, attached: bool) {
        self.source_attached.set(attached);
    }

    fn set_hover_position(&self, position: Option<Vector2<f32>>) {
        self.hover_position.set(position);
    }

    /// Redraws the connection.
    #[profile(Detail)]
    pub fn redraw(&self) {
        let new_shape = || {
            let new = Rectangle::new();
            new.set_corner_radius_max();
            new.set_inset_border(LINE_WIDTH);
            new.set_color(color::Rgba::transparent());
            new.set_border_color(self.color.get());
            new.set_pointer_events(false);
            self.display_object.add_child(&new);
            new
        };
        let hover_width = LINE_WIDTH + HOVER_EXTENSION;
        let new_hover = || {
            let new = Rectangle::new();
            new.set_corner_radius_max();
            new.set_inset_border(hover_width);
            new.set_color(color::Rgba::transparent());
            new.set_border_color(color::Rgba::black());
            new.set_pointer_events(true);
            self.display_object.add_child(&new);
            new
        };
        let mut shapes = self.sections.take().into_iter().chain(iter::repeat_with(new_shape));
        let mut hovers = self.hover_sections.take().into_iter().chain(iter::repeat_with(new_hover));

        let source_half_width = (self.source_width.get() / 2.0 - NODE_CORNER_RADIUS).max(0.0);
        let target_offset = self.target_position.get() - self.display_object.xy();
        let (target_x, target_y) = (target_offset.x(), target_offset.y());
        if target_y < -MAX_RADIUS
            || (target_y <= 0.0 && target_x.abs() <= source_half_width + 3.0 * MAX_RADIUS)
        {
            // === One corner ===

            // The edge can originate anywhere along the length of the node.
            let source_x = target_offset.x().clamp(-source_half_width, source_half_width);
            let shape = hovers.next().unwrap();
            corner(&shape, Vector2(source_x, 0.0), Vector2(target_x, target_y), hover_width);
            self.hover_sections.borrow_mut().push(shape);
            let shape = shapes.next().unwrap();
            corner(&shape, Vector2(source_x, 0.0), Vector2(target_x, target_y), LINE_WIDTH);
            self.sections.borrow_mut().push(shape);
        } else {
            // === Three corners ===

            // The edge originates from either side of the node.
            let source_x = source_half_width.copysign(target_x);
            let distance_x = (target_x - source_x).abs();
            let top = target_y + MAX_RADIUS + NODE_CORNER_RADIUS;
            let (j0_x, j1_x);
            if distance_x > 3.0 * MIN_RADIUS && target_x.abs() > source_x.abs() {
                // Junctions (J0, J1) are in between source and target.
                let source_side_sections_extra_x = (distance_x / 3.0).min(MAX_RADIUS);
                j0_x = source_x + source_side_sections_extra_x.copysign(target_x);
                j1_x = source_x + 2.0 * source_side_sections_extra_x.copysign(target_x);
            } else {
                // J0 > source; J0 > J1; J1 > target.
                j1_x = target_x + MAX_RADIUS.copysign(target_x);
                let j0_beyond_target = target_x.abs() + MAX_RADIUS * 2.0;
                let j0_beyond_source = source_x.abs() + MAX_RADIUS;
                j0_x = j0_beyond_source.max(j0_beyond_target).copysign(target_x);
            }
            let source = Vector2(source_x, 0.0);
            let j0 = Vector2(j0_x, top / 2.0);
            let j1 = Vector2(j1_x, top);
            let target = Vector2(target_x, target_y + NODE_CORNER_RADIUS);
            let corners = &[(source, j0), (j1, j0), (j1, target)];
            let new_hovers = corners
                .iter()
                .map(|&(horizontal, vertical)| {
                    let shape = hovers.next().unwrap();
                    corner(&shape, horizontal, vertical, hover_width);
                    shape
                })
                .collect_vec();
            self.hover_sections.replace(new_hovers);
            let new_sections = corners
                .iter()
                .map(|&(horizontal, vertical)| {
                    let shape = shapes.next().unwrap();
                    corner(&shape, horizontal, vertical, LINE_WIDTH);
                    shape
                })
                .collect_vec();
            self.sections.replace(new_sections);
        }
    }
}

/// Set the given [`Rectangle`]'s geometry to draw a corner shape, from the two specified line end
/// points.
fn corner(shape: &Rectangle, horizontal: Vector2<f32>, vertical: Vector2<f32>, line_width: f32) {
    let offset = horizontal - vertical;
    let (dx, dy) = (-offset.x(), offset.y());
    let (x_clip, y_clip) = (0.5f32.copysign(dx), 0.5f32.copysign(dy));
    shape.set_clip(Vector2(x_clip, y_clip));
    let width = (dx.abs() + line_width / 2.0).max(line_width);
    let height = (dy.abs() + line_width / 2.0).max(line_width);
    shape.set_size(Vector2(width, height));
    let x = horizontal.x().min(vertical.x() - line_width / 2.0);
    let y = vertical.y().min(horizontal.y() - line_width / 2.0);
    shape.set_xy(Vector2(x, y));
}


// === Edge Splitting ===

impl EdgeModelData {
    /// Return whether the point is in the upper half of the overall edge shape.
    fn is_in_upper_half(&self, point: Vector2<f32>) -> bool {
        let world_space_source = self.position().y;
        let world_space_target = self.target_position.get().y;
        let mid_y = (world_space_source + world_space_target) / 2.0;
        point.y > mid_y
    }

    /// Returns whether the given position should detach the the `Input` or `Output` part of the
    /// edge.
    ///
    /// We determine the target port primarily based y-position. We only use the y distance to the
    /// start/end of the edge and whichever is closer, is the target. However, this becomes
    /// problematic if the start and end of the edge have the same y-position or even if they are
    /// almost level. That is why, we then switch to using the euclidean distance instead.
    pub fn port_to_detach_for_position(&self, point: Vector2<f32>) -> PortType {
        //if self.input_and_output_y_too_close() {
        //    return self.closest_end_for_point(point);
        //}
        /*
        let input_port_is_in_upper_half = self.layout_state.get().is_input_above_output();
        let point_is_in_upper_half = self.is_in_upper_half(point);

        // We always detach the port that is on the opposite side of the cursor.
        if point_is_in_upper_half != input_port_is_in_upper_half {
            PortType::InputPort
        } else {
            PortType::OutputPort
        }
         */
        PortType::OutputPort
    }

    /// Return the `EndDesignation` for the closest end of the edge for the given point. Uses
    /// euclidean distance between point and `Input`/`Output`.
    fn closest_end_for_point(&self, point: Vector2<f32>) -> PortType {
        let target_position = self.target_position.get().xy();
        let source_position = self.position().xy() - Vector2(0.0, self.source_height.get() / 2.0);
        let target_distance = (point - target_position).norm();
        let source_distance = (point - source_position).norm();
        if source_distance > target_distance {
            PortType::OutputPort
        } else {
            PortType::InputPort
        }
    }
}
