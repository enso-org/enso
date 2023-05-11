//! Definition of the Edge component.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::bounding_box::BoundingBox;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl_hardcoded_theme as theme;



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
            def set_disabled    = source();
            def set_color       = source();
        }
        Self {
            source_width,
            source_height,
            target_position,
            target_attached,
            source_attached,
            redraw,
            set_disabled,
            set_color,
        }
    }
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
        let edge_color = color::Animation::new(network);
        let edge_focus_color = color::Animation::new(network);
        let _style = StyleWatch::new(&app.display.default_scene.style_sheet);

        use ensogl::control::io::mouse;
        let display_object = &self.display_object;
        let mouse_enter = display_object.on_event::<mouse::Enter>();
        let mouse_leave = display_object.on_event::<mouse::Leave>();
        let mouse_move = display_object.on_event::<mouse::Move>();
        let mouse_down = display_object.on_event::<mouse::Down>();

        frp::extend! { network
            eval input.target_position ((t) model.set_target_position(*t));
            eval input.source_attached ((t) model.set_source_attached(*t));
            eval input.target_attached ((t) model.set_target_attached(*t));
            eval input.source_width    ((t) model.set_source_width(*t));
            eval input.source_height   ((t) model.set_source_height(*t));
            eval mouse_enter           ((e) model.on_mouse_enter(e.client_centered()));
            eval mouse_move            ((e) model.on_mouse_move(e.client_centered()));
            eval_ mouse_leave           (model.on_mouse_leave());
            eval mouse_down            ((e) model.on_mouse_down(e.client_centered()));

            eval_ input.redraw                    (model.redraw());


            // === Colors ===

            is_hovered      <- bool(&mouse_leave, &mouse_enter);
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

    sections:       RefCell<Vec<Rectangle>>,
    hover_sections: RefCell<Vec<Rectangle>>,
    focus_sections: RefCell<Vec<Rectangle>>,
    corner_points:  RefCell<Vec<Vector2<f32>>>,

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
        let focus_sections = default();
        let corner_points = default();

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
            focus_sections,
            corner_points,
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

    fn on_mouse_enter(&self, pos: Vector2<f32>) {
        warn!("on_mouse_enter: {pos:?}");
    }

    fn on_mouse_leave(&self) {
        warn!("on_mouse_leave");
    }

    fn on_mouse_down(&self, screen_pos: Vector2<f32>) {
        // Convert point to local coordinates.
        let screen_pos_3d = Vector3(screen_pos.x(), screen_pos.y(), 0.0);
        let scene_pos = self.scene.screen_to_scene_coordinates(screen_pos_3d).xy();
        let pos = scene_pos - self.display_object.xy();

        let corners = corners(&*self.corner_points.borrow()).collect_vec();
        let (_, closer_end) = find_position(pos, &corners);
        warn!("on_mouse_down: {pos:?}; closer_end: {closer_end:?}");
    }

    fn on_mouse_move(&self, screen_pos: Vector2<f32>) {
        // Convert point to local coordinates.
        let screen_pos_3d = Vector3(screen_pos.x(), screen_pos.y(), 0.0);
        let scene_pos = self.scene.screen_to_scene_coordinates(screen_pos_3d).xy();
        let pos = scene_pos - self.display_object.xy();

        // Make a shape factory that recycles any previous focus shapes.
        let mut shapes = self
            .focus_sections
            .take()
            .into_iter()
            .chain(iter::repeat_with(|| self.new_focus_section()));

        // Find the hovered position on the edge.
        let corners = corners(&*self.corner_points.borrow()).collect_vec();
        let (partial_corner, closer_end) = find_position(pos, &corners);

        // Draw the shapes.
        // Focus from the mouse to whichever end is *farther*.
        let fully_focused = match closer_end {
            EndPoint::Target => 0..partial_corner,
            EndPoint::Source => (partial_corner + 1)..corners.len(),
        };
        let partially_focused_count = 1;
        let mut new_shapes = Vec::with_capacity(fully_focused.len() + partially_focused_count);
        for &corner in &corners[fully_focused] {
            let shape = shapes.next().unwrap();
            corner.draw(&shape, LINE_WIDTH);
            new_shapes.push(shape);
        }
        let shape = shapes.next().unwrap();
        let partial = corners[partial_corner];
        let hover_width = LINE_WIDTH + HOVER_EXTENSION;
        // Focus from the mouse toward whichever end is *farther*.
        match closer_end {
            EndPoint::Target => partial.draw_to(&shape, pos, hover_width, LINE_WIDTH),
            EndPoint::Source => partial.draw_from(&shape, pos, hover_width, LINE_WIDTH),
        }
        new_shapes.push(shape);
        self.focus_sections.replace(new_shapes);
    }

    fn set_hover_position(&self, position: Option<Vector2<f32>>) {
        warn!("set_hover_position: {position:?}");
        self.hover_position.set(position);
    }

    fn new_section(&self) -> Rectangle {
        let new = Rectangle::new();
        new.set_corner_radius_max();
        new.set_inset_border(LINE_WIDTH);
        new.set_color(color::Rgba::transparent());
        new.set_border_color(self.color.get());
        new.set_pointer_events(false);
        self.display_object.add_child(&new);
        new
    }

    fn new_hover_section(&self) -> Rectangle {
        let hover_width = LINE_WIDTH + HOVER_EXTENSION;
        let new = Rectangle::new();
        new.set_corner_radius_max();
        new.set_inset_border(hover_width);
        new.set_color(color::Rgba::transparent());
        new.set_border_color(color::Rgba::black());
        self.display_object.add_child(&new);
        new
    }

    fn new_focus_section(&self) -> Rectangle {
        let styles = StyleWatch::new(&self.scene.style_sheet);
        let bg_color = styles.get_color(theme::application::background).into();
        let color = color::mix(bg_color, self.color.get(), 0.25);
        let new = Rectangle::new();
        new.set_corner_radius_max();
        new.set_inset_border(LINE_WIDTH);
        new.set_color(color::Rgba::transparent());
        new.set_border_color(color);
        new.set_pointer_events(false);
        self.display_object.add_child(&new);
        new
    }

    /// Redraws the connection.
    #[profile(Detail)]
    pub fn redraw(&self) {
        // Calculate the current geometry.
        let target_offset = self.target_position.get() - self.display_object.xy();
        let target = match self.target_attached.get() {
            // If the target is a node, connect to a point on its top edge. If the radius is small,
            // this looks better than connecting to a vertically-centered point.
            true => target_offset + Vector2(0.0, NODE_CORNER_RADIUS),
            // If the target is the cursor, connect all the way to it.
            false => target_offset,
        };
        let new_corner_points = self.corner_points_to(target);
        let new_corners = corners(&new_corner_points).collect_vec();
        self.corner_points.replace(new_corner_points);

        // Clear the old sections and hover-sections, and create factories that recycle the old
        // objects as available.
        let mut section_factory =
            self.sections.take().into_iter().chain(iter::repeat_with(|| self.new_section()));
        let mut hover_factory = self
            .hover_sections
            .take()
            .into_iter()
            .chain(iter::repeat_with(|| self.new_hover_section()));

        // Create shape objects for the current geometry.
        if self.target_attached.get() && self.source_attached.get() {
            let hover_width = LINE_WIDTH + HOVER_EXTENSION;
            let new_hovers = new_corners
                .iter()
                .zip(hover_factory)
                .map(|(&corner, shape)| {
                    corner.draw(&shape, hover_width);
                    shape
                })
                .collect_vec();
            self.hover_sections.replace(new_hovers);
        }
        let new_sections = new_corners
            .iter()
            .zip(section_factory)
            .map(|(&corner, shape)| {
                corner.draw(&shape, LINE_WIDTH);
                shape
            })
            .collect_vec();
        self.sections.replace(new_sections);
    }

    /// Calculate the start and end positions of each 1-corner section composing an edge to the
    /// given offset from this object's `display_object`.
    fn corner_points_to(&self, target: Vector2<f32>) -> Vec<Vector2<f32>> {
        let source_half_width = (self.source_width.get() / 2.0 - NODE_CORNER_RADIUS).max(0.0);
        let (target_x, target_y) = (target.x(), target.y());
        if target_y < -MIN_RADIUS
            || (target_y <= 0.0 && target_x.abs() <= source_half_width + 3.0 * MAX_RADIUS)
        {
            // === One corner ===

            // The edge can originate anywhere along the length of the node.
            let source_x = target_x.clamp(-source_half_width, source_half_width);
            let source = Vector2(source_x, 0.0);
            vec![source, target]
        } else {
            // === Three corners ===

            // The edge originates from either side of the node.
            let source_x = source_half_width.copysign(target_x);
            let distance_x = (target_x - source_x).abs();
            let top = target_y + MAX_RADIUS;
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
            vec![source, j0, j1, target]
        }
    }
}

fn find_position(pos: Vector2<f32>, corners: &[Oriented<Corner>]) -> (usize, EndPoint) {
    let hover_width = LINE_WIDTH + HOVER_EXTENSION;
    let total_length = corners.iter().map(|&corner| corner.snake_length()).sum();
    let mut distance_from_source = 0.0;
    let mut partial_corner = corners.len() - 1;
    for (i, &corner) in corners.iter().enumerate() {
        if corner.bounding_box(hover_width).contains_inclusive(pos) {
            let offset = pos - corner.source_end();
            distance_from_source += offset.x().abs() + offset.y().abs();
            partial_corner = i;
            break;
        }
        distance_from_source += corner.snake_length();
    }
    let closer_end = match distance_from_source * 2.0 < total_length {
        true => EndPoint::Source,
        false => EndPoint::Target,
    };
    (partial_corner, closer_end)
}

#[derive(Debug, Copy, Clone)]
enum EndPoint {
    Source,
    Target,
}

#[derive(Debug, Copy, Clone)]
enum CornerDirection {
    HorizontalToVertical,
    VerticalToHorizontal,
}

impl CornerDirection {
    fn reverse(self) -> Self {
        match self {
            CornerDirection::HorizontalToVertical => CornerDirection::VerticalToHorizontal,
            CornerDirection::VerticalToHorizontal => CornerDirection::HorizontalToVertical,
        }
    }
}

fn corners(points: &[Vector2<f32>]) -> impl Iterator<Item = Oriented<Corner>> + '_ {
    let mut next_direction = CornerDirection::HorizontalToVertical;
    points.array_windows().map(move |&[p0, p1]| {
        let direction = next_direction;
        next_direction = next_direction.reverse();
        let corner = match direction {
            CornerDirection::HorizontalToVertical => Corner { horizontal: p0, vertical: p1 },
            CornerDirection::VerticalToHorizontal => Corner { horizontal: p1, vertical: p0 },
        };
        Oriented::new(corner, direction)
    })
}

#[derive(Debug, Copy, Clone, Deref)]
struct Oriented<T> {
    #[deref]
    value:     T,
    direction: CornerDirection,
}

impl<T> Oriented<T> {
    fn new(value: T, direction: CornerDirection) -> Self {
        Self { value, direction }
    }
}

impl Oriented<Corner> {
    fn source_end(self) -> Vector2<f32> {
        match self.direction {
            CornerDirection::VerticalToHorizontal => self.value.vertical,
            CornerDirection::HorizontalToVertical => self.value.horizontal,
        }
    }

    fn target_end(self) -> Vector2<f32> {
        match self.direction {
            CornerDirection::VerticalToHorizontal => self.value.horizontal,
            CornerDirection::HorizontalToVertical => self.value.vertical,
        }
    }

    fn with_target_end(mut self, value: Vector2<f32>) -> Self {
        *(match self.direction {
            CornerDirection::VerticalToHorizontal => &mut self.value.horizontal,
            CornerDirection::HorizontalToVertical => &mut self.value.vertical,
        }) = value;
        self
    }

    fn reverse(self) -> Self {
        let Self { value, direction } = self;
        let direction = direction.reverse();
        Self { value, direction }
    }

    fn draw_to(
        self,
        shape: &Rectangle,
        end_point: Vector2<f32>,
        snap_line_width: f32,
        line_width: f32,
    ) {
        let Corner { horizontal, vertical } = self.value;
        let hv_offset = horizontal - vertical;
        let (dx, dy) = (hv_offset.x().abs(), hv_offset.y().abs());
        let radius = min(dx, dy);
        let (linear_x, linear_y) = (dx - radius, dy - radius);
        let snap_distance = snap_line_width / 2.0;

        let y_along_vertical = (self.vertical.y() - end_point.y()).abs() <= linear_y;
        let x_along_horizontal = (self.horizontal.x() - end_point.x()).abs() <= linear_x;
        let y_near_horizontal = (self.horizontal.y() - end_point.y()).abs() <= snap_distance;
        let x_near_vertical = (self.vertical.x() - end_point.x()).abs() <= snap_distance;

        // FIXME: This logic sometimes reaches the "arc" case when the corner is supposed to be a
        //  straight line.
        if y_near_horizontal && x_along_horizontal {
            // The point is along the horizontal line. Snap its y-value, and draw a corner to it.
            let snapped = Vector2(end_point.x(), self.horizontal.y());
            self.with_target_end(snapped).draw(shape, line_width);
        } else if x_near_vertical && y_along_vertical {
            // The point is along the vertical line. Snap its x-value, and draw a corner to it.
            let snapped = Vector2(self.vertical.x(), end_point.y());
            self.with_target_end(snapped).draw(shape, line_width);
        } else {
            // The point is along the arc.
            // Snap it to the arc:
            // - Find its angle from the arc's center,
            // - Then find the arc point at that angle.
            let point_outside_arc = Vector2(self.vertical.x(), self.horizontal.y());
            let point_inside_arc = Vector2(self.horizontal.x(), self.vertical.y());
            let outside_to_inside = point_inside_arc - point_outside_arc;
            let outside_to_origin = Vector2(
                radius.copysign(outside_to_inside.x()),
                radius.copysign(outside_to_inside.y()),
            );
            let origin = point_outside_arc + outside_to_origin;
            const DEBUG_ARC_SNAP_CIRCLES: bool = false;
            if DEBUG_ARC_SNAP_CIRCLES {
                // Draw the circle that is being used for the arc-snap computation. It should line
                // up exactly with the visible arc (although inputs may lie anywhere on the larger
                // hover-extended arc).
                let adjusted_radius = radius + line_width / 2.0;
                shape.set_xy(origin - Vector2(adjusted_radius, adjusted_radius));
                shape.set_clip(Vector2(0.0, 0.0));
                shape.set_size(Vector2(radius * 2.0 + line_width, radius * 2.0 + line_width));
            }
            let offset_from_origin = end_point - origin;
            let angle = offset_from_origin.y().atan2(offset_from_origin.x());
            let unitized_angle = angle / (std::f32::consts::PI / 2.0);
            warn!("angle: {unitized_angle}");
        }
    }

    fn draw_from(
        self,
        shape: &Rectangle,
        start_point: Vector2<f32>,
        snap_line_width: f32,
        line_width: f32,
    ) {
        self.reverse().draw_to(shape, start_point, snap_line_width, line_width);
    }
}

#[derive(Debug, Copy, Clone)]
struct Corner {
    horizontal: Vector2<f32>,
    vertical:   Vector2<f32>,
}

impl Corner {
    /// Set the given [`Rectangle`]'s geometry to draw this corner shape.
    ///
    /// Note that the shape's `inset` and `border` should be set to the same value as the provided
    /// [`line_width`]. This is not done here as an optimization: When shapes are reused, the value
    /// only needs to be set once, avoiding extra buffer mutation.
    fn draw(self, shape: &Rectangle, line_width: f32) {
        shape.set_clip(self.clip());
        shape.set_size(self.size(line_width));
        shape.set_xy(self.origin(line_width));
    }

    #[inline]
    fn clip(self) -> Vector2 {
        let Corner { horizontal, vertical } = self;
        let (dx, dy) = (vertical.x() - horizontal.x(), horizontal.y() - vertical.y());
        let (x_clip, y_clip) = (0.5f32.copysign(dx), 0.5f32.copysign(dy));
        Vector2(x_clip, y_clip)
    }

    #[inline]
    fn origin(self, line_width: f32) -> Vector2 {
        let Corner { horizontal, vertical } = self;
        let x = horizontal.x().min(vertical.x() - line_width / 2.0);
        let y = vertical.y().min(horizontal.y() - line_width / 2.0);
        Vector2(x, y)
    }

    #[inline]
    fn size(self, line_width: f32) -> Vector2 {
        let Corner { horizontal, vertical } = self;
        let offset = horizontal - vertical;
        let width = (offset.x().abs() + line_width / 2.0).max(line_width);
        let height = (offset.y().abs() + line_width / 2.0).max(line_width);
        Vector2(width, height)
    }

    fn bounding_box(self, line_width: f32) -> BoundingBox {
        let origin = self.origin(line_width);
        let size = self.size(line_width);
        BoundingBox::from_position_and_size_unchecked(origin, size)
    }

    fn euclidean_length(self) -> f32 {
        let Corner { horizontal, vertical } = self;
        let offset = horizontal - vertical;
        let (dx, dy) = (offset.x().abs(), offset.y().abs());
        let radius = dx.min(dy);
        let linear_x = dx - radius;
        let linear_y = dy - radius;
        let arc = std::f32::consts::PI * radius / 2.0;
        arc + linear_x + linear_y
    }

    fn snake_length(self) -> f32 {
        let Corner { horizontal, vertical } = self;
        let offset = horizontal - vertical;
        offset.x().abs() + offset.y().abs()
    }
}
