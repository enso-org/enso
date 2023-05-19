//! Definition of the Edge component.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::bounding_box::BoundingBox;
use ensogl::data::color;
use ensogl::define_endpoints_2;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl_hardcoded_theme as theme;



// =================
// === Constants ===
// =================

const LINE_WIDTH: f32 = 4.0;
const HOVER_EXTENSION: f32 = 10.0;
const HOVER_WIDTH: f32 = LINE_WIDTH + HOVER_EXTENSION;
const NODE_CORNER_RADIUS: f32 = crate::component::node::CORNER_RADIUS;
const NODE_HEIGHT: f32 = crate::component::node::HEIGHT;
const MIN_RADIUS: f32 = 20.0;
const MAX_RADIUS: f32 = 30.0;
const TARGET_ATTACHMENT_LENGTH: f32 = 6.0;
const BACKWARD_EDGE_ARROW_THRESHOLD: f32 = 400.0;
const ARROW_ARM_LENGTH: f32 = 12.0;
const ARROW_ARM_WIDTH: f32 = LINE_WIDTH;



// =========================
// === Shape Definitions ===
// =========================

/// An arc around the origin. `outer_radius` determines the distance from the origin to the outer
/// edge of the arc, `stroke_width` the width of the arc. The arc starts at `start_angle`, relative
/// to the origin. Its radial size is `sector_angle`. The ends are flat, not rounded as in
/// [`RoundedArc`].
mod arc {
    use super::*;
    ensogl::shape! {
        pointer_events = false;
        (
            style: Style,
            color: Vector4,
            outer_radius: f32,
            stroke_width: f32,
            start_angle: f32,
            sector_angle: f32,
        ) {
            let circle = Circle(outer_radius.px()) - Circle((outer_radius - stroke_width).px());
            let angle_adjust = Var::<f32>::from(std::f32::consts::FRAC_PI_2);
            let rotate_angle = -start_angle + angle_adjust - &sector_angle / 2.0;
            let angle = PlaneAngleFast(sector_angle).rotate(rotate_angle);
            let angle = angle.grow(0.5.px());
            let shape = circle * angle;
            let shape = shape.fill(color);
            shape.into()
        }
    }
}



// ===========
// === FRP ===
// ===========

define_endpoints_2! {
    Input {
        /// The width and height of the source node in pixels.
        source_size(Vector2),
        /// The location of the center of the target node's input port.
        target_position(Vector2),
        /// Whether the target end of the edge is attached to a node (If `false`, it is being
        /// dragged by the mouse.)
        target_attached(bool),
        /// Whether the source end of the edge is attached to a node (If `false`, it is being
        /// dragged by the mouse.)
        source_attached(bool),
        set_disabled(bool),
        /// The typical color of the node; also used to derive the focus color.
        set_color(color::Lcha),
    }
    Output {
        /// The mouse has clicked to detach the source end of the edge.
        source_click(),
        /// The mouse has clicked to detach the target end of the edge.
        target_click(),
    }
}



// ============
// === Edge ===
// ============

/// Edge definition.
#[derive(AsRef, Clone, CloneRef, Debug, Deref)]
pub struct Edge {
    #[deref]
    frp:   Frp,
    model: Rc<EdgeModel>,
}

impl AsRef<Edge> for Edge {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl Edge {
    /// Constructor.
    #[profile(Detail)]
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(EdgeModel::new(&app.display.default_scene));

        let network = &frp.network;

        let edge_color = color::Animation::new(network);

        let display_object = &model.display_object;
        let mouse_move = display_object.on_event::<mouse::Move>();
        let mouse_down = display_object.on_event::<mouse::Down>();
        let mouse_out = display_object.on_event::<mouse::Out>();

        let output = &frp.private.output;
        frp::extend! { network
            // Setters.
            eval frp.target_position ((t) model.inputs.set_target_position(ParentCoords(*t)));
            eval frp.source_attached ((t) model.inputs.set_source_attached(*t));
            eval frp.target_attached ((t) model.inputs.set_target_attached(*t));
            eval frp.source_size ((t) model.inputs.set_source_size(*t));
            eval frp.set_disabled ((t) model.inputs.set_disabled(*t));

            // Mouse events.
            eval mouse_move ([model] (e) {
                let pos = model.screen_pos_to_scene_pos(e.client_centered());
                model.inputs.set_mouse_position(pos);
            });
            eval_ mouse_out (model.inputs.clear_focus.set(true));
            eval mouse_down ([model, output] (e) {
                let pos = model.screen_pos_to_scene_pos(e.client_centered());
                let pos = model.scene_pos_to_parent_pos(pos);
                match model.closer_end(pos) {
                    Some(EndPoint::Source) => output.target_click.emit(()),
                    Some(EndPoint::Target) => output.source_click.emit(()),
                    // Ignore click events that were delivered to our display object inaccurately.
                    None => (),
                }
            });

            // Colors.
            edge_color.target <+ frp.set_color;
            eval edge_color.value ((color) model.inputs.set_color(color.into()));

            // Invalidation.
            redraw_needed <- any(...);
            redraw_needed <+ frp.target_position.constant(());
            redraw_needed <+ frp.source_attached.constant(());
            redraw_needed <+ frp.target_attached.constant(());
            redraw_needed <+ frp.source_size.constant(());
            redraw_needed <+ frp.set_disabled.constant(());
            redraw_needed <+ mouse_move.constant(());
            redraw_needed <+ mouse_out.constant(());
            redraw_needed <+ edge_color.value.constant(());
            redraw_needed <+ display_object.on_transformed.constant(());
            redraw <- redraw_needed.debounce();
            eval_ redraw (model.redraw());
        }
        Self { model, frp }
    }

    /// Return the FRP network.
    pub fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl display::Object for Edge {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}



// ==========================
// === Coordinate systems ===
// ==========================

/// Marker for coordinates relative to the origin of the parent display object.
#[derive(Debug, Copy, Clone, PartialEq, Default)]
struct ParentOrigin;

/// Marker for coordinates relative to the origin of the scene.
#[derive(Debug, Copy, Clone, PartialEq, Default)]
struct SceneOrigin;

/// Coordinates marked to identify different coordinate spaces.
#[derive(Debug, Copy, Clone, PartialEq, Default)]
struct Coords<Space, Number: Copy + Debug + PartialEq + 'static=f32> {
    coords: Vector2<Number>,
    space: PhantomData<*const Space>,
}

type ParentCoords = Coords<ParentOrigin>;
type SceneCoords = Coords<SceneOrigin>;

#[allow(non_snake_case)]
fn ParentCoords(coords: Vector2) -> ParentCoords {
    Coords { coords, space: default() }
}
#[allow(non_snake_case)]
fn SceneCoords(coords: Vector2) -> SceneCoords {
    Coords { coords, space: default() }
}



// =================
// === EdgeModel ===
// =================

/// Internal data of `Edge`
#[derive(Debug)]
struct EdgeModel {
    /// The parent display object of all the edge's parts.
    display_object: display::object::Instance,
    scene:          Scene,
    inputs:         EdgeInputs,
    shapes:         EdgeShapes,

    // === Cached state ===
    /// The endpoints of the individual [`Corner`]s making up the edge.
    corner_points: RefCell<Vec<Vector2>>,

    // === Change detection ===
    previous_target:          Cell<Option<Vector2>>,
    previous_is_hoverable:    Cell<Option<bool>>,
    previous_hover_split:     Cell<Option<EdgeSplit>>,
    previous_color:           Cell<Option<color::Rgba>>,
    previous_target_attached: Cell<Option<bool>>,
}

impl EdgeModel {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(scene: &Scene) -> Self {
        let display_object = display::object::Instance::new_named("Edge");
        let scene = scene.clone_ref();
        let layer = &scene.layers.main_edges_level;
        layer.add(&display_object);
        Self {
            display_object,
            scene,
            inputs: default(),
            shapes: default(),
            corner_points: default(),
            previous_target: default(),
            previous_is_hoverable: default(),
            previous_hover_split: default(),
            previous_color: default(),
            previous_target_attached: default(),
        }
    }

    /// Return the maximum x-distance from the source (our local coordinate origin) for the point
    /// where the edge will begin.
    fn source_max_x_offset(&self) -> f32 {
        (self.inputs.source_size.get().x() / 2.0 - NODE_CORNER_RADIUS).max(0.0)
    }

    fn screen_pos_to_scene_pos(&self, screen_pos: Vector2) -> SceneCoords {
        let screen_pos_3d = Vector3(screen_pos.x(), screen_pos.y(), 0.0);
        SceneCoords(self.scene.screen_to_scene_coordinates(screen_pos_3d).xy())
    }

    fn scene_pos_to_parent_pos(&self, scene_pos: SceneCoords) -> ParentCoords {
        ParentCoords(scene_pos.coords - self.display_object.xy())
    }

    fn closer_end(&self, pos: ParentCoords) -> Option<EndPoint> {
        let corners = corners(&self.corner_points.borrow()).collect_vec();
        let source_height = self.inputs.source_size.get().y();
        find_position(pos, &corners, source_height).map(|split| split.closer_end)
    }

    fn target_offset(&self) -> Vector2 {
        let target_offset = self.inputs.target_position.get().coords - self.display_object.xy();
        match self.inputs.target_attached.get() {
            // If the target is a node, connect to a point on its top edge. If the radius is small,
            // this looks better than connecting to a vertically-centered point.
            true => target_offset + Vector2(0.0, NODE_HEIGHT / 2.0),
            // If the target is the cursor, connect all the way to it.
            false => target_offset,
        }
    }

    /// Calculate the start and end positions of each 1-corner section composing an edge to the
    /// given offset from this object's `display_object`.
    fn corner_points_to(&self, target: Vector2) -> Vec<Vector2> {
        let source_half_width = self.source_max_x_offset();
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

    /// Redraws the connection.
    #[profile(Detail)]
    pub fn redraw(&self) {
        // Calculate the current geometry.
        let target = self.target_offset();
        let target_changed = update_and_compare(&self.previous_target, target);
        if target_changed {
            let new_corner_points = self.corner_points_to(target);
            self.corner_points.replace(new_corner_points);
        }
        let corners = corners(&self.corner_points.borrow()).collect_vec();
        let target_attached = self.inputs.target_attached.get();
        let target_attachment_changed =
            update_and_compare(&self.previous_target_attached, target_attached);
        let source_attached = self.inputs.source_attached.get();
        let is_hoverable = target_attached && source_attached;
        let is_hoverable_changed = update_and_compare(&self.previous_is_hoverable, is_hoverable);
        if self.inputs.clear_focus.take() {
            self.inputs.hover_position.take();
        }
        let hover_split = is_hoverable
            .then(|| {
                // Pointer targets are updated by an asynchronous process, independent of pointer
                // movement detection. As a result, we can receive mouse events when the pointer is
                // not within the bounding box of any of our shapes, in which case `find_position`
                // here will return `None`. We treat it the same way as a
                // `mouse::Out` event.
                self.inputs.hover_position.get().and_then(|position| {
                    let position = self.scene_pos_to_parent_pos(position);
                    find_position(position, &corners, self.inputs.source_size.get().y())
                })
            })
            .flatten();
        let hover_split_changed = update_and_compare(&self.previous_hover_split, hover_split);
        let styles = StyleWatch::new(&self.scene.style_sheet);
        let normal_color = if self.inputs.disabled.get() {
            styles.get_color(theme::code::syntax::disabled)
        } else {
            self.inputs.color.get()
        };
        let color_changed = update_and_compare(&self.previous_color, normal_color);
        let bg_color = styles.get_color(theme::application::background);
        let focused_color = color::mix(bg_color, normal_color, 0.25);
        let (source_color, target_color) = match hover_split.map(|split| split.closer_end) {
            Some(EndPoint::Target) => (focused_color, normal_color),
            Some(EndPoint::Source) => (normal_color, focused_color),
            None => (normal_color, normal_color),
        };

        // Create shape objects for the current geometry.
        let mut layout_dirty = false;
        if target_changed || is_hoverable_changed {
            self.redraw_hover_sections(is_hoverable.then_some(&corners[..]).unwrap_or_default());
            layout_dirty = true;
        }
        if target_changed || color_changed || hover_split_changed {
            self.redraw_sections(&corners, source_color, target_color, hover_split);
            self.redraw_dataflow_arrow(target, source_color, target_color, hover_split);
            layout_dirty = true;
        }
        if target_changed || color_changed || hover_split_changed || target_attachment_changed {
            self.redraw_target_attachment(target, target_color);
            layout_dirty = true;
        }
        if layout_dirty {
            // Force layout update of this object's children. Because edge positions are computed
            // based on node positions, `redraw` must be run after the layout has been updated.
            // Updating the layouts of modified edges a second time later in the frame avoids
            // latency when edge children are modified.
            //
            // FIXME: Find a better solution to fix this issue. We either need a layout that can
            //  depend on other arbitrary position, or we need the layout update to be multi-stage.
            self.display_object.update(&self.scene);
        }
    }

    fn redraw_dataflow_arrow(
        &self,
        offset: Vector2,
        source_color: color::Rgba,
        target_color: color::Rgba,
        hover_split: Option<EdgeSplit>,
    ) {
        let shape = self.shapes.dataflow_arrow.take();
        let points = self.corner_points.borrow();
        let long_backward_edge = (offset.y() > BACKWARD_EDGE_ARROW_THRESHOLD)
            || (offset.y() + offset.x().abs() / 2.0 > BACKWARD_EDGE_ARROW_THRESHOLD
                && offset.y() > 3.0 * ARROW_ARM_LENGTH);
        let multicorner_layout = points.len() > 2;
        if long_backward_edge && multicorner_layout {
            // The points are ordered from source end to destination, and are alternately horizontal
            // and vertical junctions. The arrow must be in a vertical part of the edge. Place it at
            // the first vertical junction.
            let arrow_origin = points[1];
            let arrow_origin_to_center_of_tip =
                Vector2(0.0, -(ARROW_ARM_LENGTH - ARROW_ARM_WIDTH) * std::f32::consts::SQRT_2);
            // The arrow will have the same color as the target-end of the first corner from the
            // source (this is the `arrow_origin` point).
            let color = match hover_split.map(|split| split.corner_index) {
                Some(0) => target_color,
                _ => source_color,
            };
            let shape = shape.unwrap_or_else(|| self.new_dataflow_arrow());
            shape.set_xy(arrow_origin + arrow_origin_to_center_of_tip);
            shape.set_border_color(color);
            self.shapes.dataflow_arrow.set(shape);
        }
    }

    fn redraw_hover_sections(&self, corners: &[Oriented<Corner>]) {
        let hover_factory = self
            .shapes
            .hover_sections
            .take()
            .into_iter()
            .chain(iter::repeat_with(|| self.new_hover_section()));
        *self.shapes.hover_sections.borrow_mut() = corners
            .iter()
            .zip(hover_factory)
            .map(|(corner, shape)| draw_corner(shape, **corner, HOVER_WIDTH))
            .collect();
    }

    fn redraw_sections(
        &self,
        corners: &[Oriented<Corner>],
        source_color: color::Rgba,
        target_color: color::Rgba,
        hover_split: Option<EdgeSplit>,
    ) {
        let corner_index =
            hover_split.map(|split| split.corner_index).unwrap_or_else(|| corners.len());
        let split_corner = hover_split.map(|split| split.split_corner);
        let mut section_factory =
            self.shapes.sections.take().into_iter().chain(iter::repeat_with(|| self.new_section()));
        let mut new_sections = self.redraw_complete_sections(
            &mut section_factory,
            corners,
            corner_index,
            source_color,
            target_color,
        );
        let arc_shapes = self.shapes.split_arc.take();
        if let Some(split_corner) = split_corner {
            let source_side = split_corner.source_end.to_rectangle_geometry(LINE_WIDTH);
            let target_side = split_corner.target_end.to_rectangle_geometry(LINE_WIDTH);
            let split_arc = split_corner.split_arc;
            if let Some(split_arc) = split_arc {
                let arc_shapes = arc_shapes.unwrap_or_else(|| [self.new_arc(), self.new_arc()]);
                let arc_shapes = draw_split_arc(arc_shapes, split_arc);
                arc_shapes[0].color.set(source_color.into());
                arc_shapes[1].color.set(target_color.into());
                self.shapes.split_arc.set(arc_shapes);
            }
            let (source_shape, target_shape) =
                (section_factory.next().unwrap(), section_factory.next().unwrap());
            source_shape.set_border_color(source_color);
            target_shape.set_border_color(target_color);
            new_sections.push(draw_geometry(source_shape, source_side));
            new_sections.push(draw_geometry(target_shape, target_side));
        }
        *self.shapes.sections.borrow_mut() = new_sections;
    }

    fn redraw_complete_sections(
        &self,
        section_factory: impl Iterator<Item = Rectangle>,
        corners: &[Oriented<Corner>],
        corner_index: usize,
        source_color: color::Rgba,
        target_color: color::Rgba,
    ) -> Vec<Rectangle> {
        corners
            .iter()
            .enumerate()
            .filter_map(|(i, corner)| {
                if i == corner_index {
                    None
                } else {
                    let color = match i < corner_index {
                        true => source_color,
                        false => target_color,
                    };
                    Some((color, corner))
                }
            })
            .zip(section_factory)
            .map(|((color, corner), shape)| {
                let shape = draw_corner(shape, **corner, LINE_WIDTH);
                shape.set_border_color(color);
                shape
            })
            .collect()
    }

    fn redraw_target_attachment(&self, target: Vector2, color: color::Rgba) {
        let shape = self.shapes.target_attachment.take();
        if self.inputs.target_attached.get() {
            let shape = shape.unwrap_or_else(|| self.new_target_attachment());
            shape.set_xy(target + Vector2(-LINE_WIDTH / 2.0, 0.5 - TARGET_ATTACHMENT_LENGTH));
            shape.set_color(color);
            self.shapes.target_attachment.set(shape);
        }
    }
}


// === Inputs ===

/// The inputs to the edge state computation. These values are all set orthogonally, so that the
/// order of events that set different properties doesn't affect the outcome.
#[derive(Debug, Default)]
pub struct EdgeInputs {
    /// The width and height of the node that originates the edge. The edge may begin anywhere
    /// around the bottom half of the node.
    source_size:     Cell<Vector2>,
    /// The coordinates of the node input the edge connects to. The edge enters the node from
    /// above.
    target_position: Cell<ParentCoords>,
    /// Whether the edge is connected to a node input.
    target_attached: Cell<bool>,
    /// Whether the edge is connected to a node output.
    source_attached: Cell<bool>,
    color:           Cell<color::Rgba>,
    /// The location of the mouse over the edge.
    hover_position:  Cell<Option<Coords<SceneOrigin>>>,
    disabled:        Cell<bool>,
    /// Reset the hover position at next redraw.
    clear_focus:     Cell<bool>,
}

impl EdgeInputs {
    /// Set the color of the edge.
    fn set_color(&self, color: color::Lcha) {
        // We must never use alpha in edges, as it will show artifacts with overlapping sub-parts.
        let color: color::Lcha = color.opaque.into();
        let color_rgba = color::Rgba::from(color);
        self.color.set(color_rgba);
    }

    fn set_source_size(&self, size: Vector2) {
        self.source_size.set(size);
    }

    fn set_disabled(&self, disabled: bool) {
        self.disabled.set(disabled);
    }

    fn set_target_position(&self, position: ParentCoords) {
        self.target_position.set(position);
    }

    fn set_target_attached(&self, attached: bool) {
        self.target_attached.set(attached);
        self.clear_focus.set(true);
    }

    fn set_source_attached(&self, attached: bool) {
        self.source_attached.set(attached);
        self.clear_focus.set(true);
    }

    fn set_mouse_position(&self, pos: SceneCoords) {
        self.hover_position.set(Some(pos));
    }
}


// === Shapes ===

/// The shapes used to render an edge.
#[derive(Debug, Default)]
struct EdgeShapes {
    /// The individual [`Corner`]s making up the edge. Each is drawn in the focused or unfocused
    /// color.
    sections:          RefCell<Vec<Rectangle>>,
    /// A pair of [`arc`] shapes used when the mouse is over the rounded corner, and the edge must
    /// must be split into focused and unfocused sides at a certain angle along the arc.
    split_arc:         RefCell<Option<[arc::View; 2]>>,
    /// Wider versions of the [`sections`], for receiving mouse events.
    hover_sections:    RefCell<Vec<Rectangle>>,
    /// The end of the edge that is drawn on top of the node and connects to the target node's
    /// input port.
    target_attachment: RefCell<Option<Rectangle>>,
    /// Arrow drawn on long backward edges to indicate data flow direction.
    dataflow_arrow:    RefCell<Option<Rectangle>>,
}

impl EdgeModel {
    fn new_section(&self) -> Rectangle {
        let new = Rectangle::new();
        new.set_corner_radius_max();
        new.set_inset_border(LINE_WIDTH);
        new.set_color(color::Rgba::transparent());
        new.set_pointer_events(false);
        self.display_object.add_child(&new);
        new
    }

    fn new_hover_section(&self) -> Rectangle {
        let new = Rectangle::new();
        new.set_corner_radius_max();
        new.set_inset_border(HOVER_WIDTH);
        new.set_color(color::Rgba::transparent());
        new.set_border_color(INVISIBLE_HOVER_COLOR);
        self.display_object.add_child(&new);
        new
    }

    fn new_arc(&self) -> arc::View {
        let arc = arc::View::new();
        arc.stroke_width.set(LINE_WIDTH);
        self.display_object.add_child(&arc);
        self.scene.layers.below_main.add(&arc);
        arc
    }

    fn new_target_attachment(&self) -> Rectangle {
        let new = Rectangle::new();
        new.set_size(Vector2(LINE_WIDTH, TARGET_ATTACHMENT_LENGTH));
        new.set_border_color(color::Rgba::transparent());
        new.set_pointer_events(false);
        self.display_object.add_child(&new);
        self.scene.layers.main_edge_port_attachments_level.add(&new);
        new
    }

    fn new_dataflow_arrow(&self) -> Rectangle {
        let new = Rectangle::new();
        new.set_size(Vector2(ARROW_ARM_LENGTH, ARROW_ARM_LENGTH));
        new.set_inset_border(ARROW_ARM_WIDTH);
        new.set_color(color::Rgba::transparent());
        new.set_border_color(color::Rgba::transparent());
        new.set_pointer_events(false);
        new.set_rotation_z(std::f32::consts::FRAC_PI_4);
        new.set_clip(Vector2(0.5, 0.5));
        self.display_object.add_child(&new);
        new
    }
}


// === Change detection ===

/// Set the given [`Cell`] to a new value.
///
/// Returns `true` if the new value is different from the old value.
fn update_and_compare<T: Copy + PartialEq<T>, U: Into<Option<T>>>(
    value: &Cell<Option<T>>,
    new: U,
) -> bool {
    let new = new.into();
    let old = value.replace(new);
    old != new
}


// === Splitting edges ===

#[derive(Debug, Copy, Clone, PartialEq)]
struct EdgeSplit {
    corner_index: usize,
    closer_end:   EndPoint,
    split_corner: SplitCorner,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum EndPoint {
    Source,
    Target,
}

/// Find a point along the edge. Return the index of the corner the point occurs in, and which end
/// is closer to the point, and information about how the corner under the point has been split.
///
/// Returns [`None`] if the point is not on the edge.
fn find_position(
    position: ParentCoords,
    corners: &[Oriented<Corner>],
    source_height: f32,
) -> Option<EdgeSplit> {
    let position = position.coords;
    let corner_index = corners
        .iter()
        .position(|&corner| corner.bounding_box(HOVER_WIDTH).contains_inclusive(position))?;
    let split_corner = corners[corner_index].split(position, HOVER_WIDTH)?;
    let (full_corners, following_corners) = corners.split_at(corner_index);
    let full_corners_distance: f32 =
        full_corners.iter().map(|&corner| corner.rectilinear_length()).sum();
    let following_distance: f32 =
        following_corners.iter().map(|&corner| corner.rectilinear_length()).sum();
    let target_attachment_distance = TARGET_ATTACHMENT_LENGTH;
    // The source end of the edge is on a horizontal line through the center of the source node
    // (this gives nice behavior when the edge exits the end at an angle). To accurately determine
    // which end a point appears closer to, we must exclude the portion of the edge that is hidden
    // under the source node.
    let hidden_source_distance = source_height / 2.0;
    let total_distance = full_corners_distance + following_distance - hidden_source_distance
        + target_attachment_distance;
    let offset_from_partial_corner = position - corners[corner_index].source_end();
    let partial_corner_distance =
        offset_from_partial_corner.x().abs() + offset_from_partial_corner.y().abs();
    let distance_from_source =
        full_corners_distance + partial_corner_distance - hidden_source_distance;
    let closer_end = match distance_from_source * 2.0 < total_distance {
        true => EndPoint::Source,
        false => EndPoint::Target,
    };
    Some(EdgeSplit { corner_index, closer_end, split_corner })
}


// === Connecting points with corners ===

fn corners(points: &[Vector2]) -> impl Iterator<Item = Oriented<Corner>> + '_ {
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


// === Applying geometry parameters to shapes ===

/// Set the given [`Rectangle`]'s geometry to draw this corner shape.
///
/// Note that the shape's `inset` and `border` should be the same value as the provided
/// [`line_width`]. They are not set here as an optimization: When shapes are reused, the value does
/// not need to be set again, reducing needed GPU uploads.
fn draw_corner(shape: Rectangle, corner: Corner, line_width: f32) -> Rectangle {
    draw_geometry(shape, corner.to_rectangle_geometry(line_width))
}

fn draw_geometry(shape: Rectangle, geometry: RectangleGeometry) -> Rectangle {
    shape.set_clip(geometry.clip);
    shape.set_size(geometry.size);
    shape.set_xy(geometry.xy);
    shape
}

/// Apply the specified arc-splitting parameters to the given arc shapes.
fn draw_split_arc(arc_shapes: [arc::View; 2], split_arc: SplitArc) -> [arc::View; 2] {
    let outer_radius = split_arc.radius + LINE_WIDTH / 2.0;
    let arc_box = Vector2(outer_radius * 2.0, outer_radius * 2.0);
    let arc_offset = Vector2(-outer_radius, -outer_radius);
    for shape in &arc_shapes {
        shape.set_xy(split_arc.origin + arc_offset);
        shape.set_size(arc_box);
        shape.outer_radius.set(split_arc.radius + LINE_WIDTH / 2.0);
    }
    let (source, target) = (&arc_shapes[0], &arc_shapes[1]);
    source.start_angle.set(
        split_arc
            .split_angle
            .rem_euclid(std::f32::consts::TAU)
            .min(split_arc.source_end_angle.rem_euclid(std::f32::consts::TAU)),
    );
    source.sector_angle.set((split_arc.split_angle.abs() - split_arc.source_end_angle.abs()).abs());
    target.start_angle.set(
        split_arc
            .split_angle
            .rem_euclid(std::f32::consts::TAU)
            .min(split_arc.target_end_angle.rem_euclid(std::f32::consts::TAU)),
    );
    target.sector_angle.set((split_arc.target_end_angle.abs() - split_arc.split_angle.abs()).abs());
    arc_shapes
}



// ==============
// === Corner ===
// ==============

#[derive(Debug, Copy, Clone, PartialEq)]
struct Corner {
    horizontal: Vector2,
    vertical:   Vector2,
}

impl Corner {
    /// Return [`Rectangle`] geometry parameters to draw this corner shape.
    fn to_rectangle_geometry(self, line_width: f32) -> RectangleGeometry {
        RectangleGeometry {
            clip: self.clip(),
            size: self.size(line_width),
            xy:   self.origin(line_width),
        }
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

    #[allow(unused)]
    fn euclidean_length(self) -> f32 {
        let Corner { horizontal, vertical } = self;
        let offset = horizontal - vertical;
        let (dx, dy) = (offset.x().abs(), offset.y().abs());
        let radius = dx.min(dy);
        let linear_x = dx - radius;
        let linear_y = dy - radius;
        let arc = std::f32::consts::FRAC_PI_2 * radius;
        arc + linear_x + linear_y
    }

    fn rectilinear_length(self) -> f32 {
        let Corner { horizontal, vertical } = self;
        let offset = horizontal - vertical;
        offset.x().abs() + offset.y().abs()
    }

    #[allow(unused)]
    fn transpose(self) -> Self {
        let Corner { horizontal, vertical } = self;
        Corner { horizontal: vertical.yx(), vertical: horizontal.yx() }
    }

    fn vertical_end_angle(self) -> f32 {
        match self.vertical.x() > self.horizontal.x() {
            true => 0.0,
            false => std::f32::consts::PI.copysign(self.horizontal.y() - self.vertical.y()),
        }
    }

    fn horizontal_end_angle(self) -> f32 {
        std::f32::consts::FRAC_PI_2.copysign(self.horizontal.y() - self.vertical.y())
    }
}


// === Rectangle geometry describing a corner ===

#[derive(Debug, Copy, Clone, Default)]
struct RectangleGeometry {
    clip: Vector2,
    size: Vector2,
    xy:   Vector2,
}


// === Parameters for drawing the arc portion of a corner in two parts ===

#[derive(Debug, Copy, Clone, Default, PartialEq)]
struct SplitArc {
    origin:           Vector2,
    radius:           f32,
    source_end_angle: f32,
    split_angle:      f32,
    target_end_angle: f32,
}



// ========================
// === Oriented corners ===
// ========================

#[derive(Debug, Copy, Clone, Deref, PartialEq)]
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
    fn source_end(self) -> Vector2 {
        match self.direction {
            CornerDirection::VerticalToHorizontal => self.value.vertical,
            CornerDirection::HorizontalToVertical => self.value.horizontal,
        }
    }

    #[allow(unused)]
    fn target_end(self) -> Vector2 {
        match self.direction {
            CornerDirection::VerticalToHorizontal => self.value.horizontal,
            CornerDirection::HorizontalToVertical => self.value.vertical,
        }
    }

    fn with_target_end(mut self, value: Vector2) -> Self {
        *(match self.direction {
            CornerDirection::VerticalToHorizontal => &mut self.value.horizontal,
            CornerDirection::HorizontalToVertical => &mut self.value.vertical,
        }) = value;
        self
    }

    fn with_source_end(mut self, value: Vector2) -> Self {
        *(match self.direction {
            CornerDirection::VerticalToHorizontal => &mut self.value.vertical,
            CornerDirection::HorizontalToVertical => &mut self.value.horizontal,
        }) = value;
        self
    }

    fn reverse(self) -> Self {
        let Self { value, direction } = self;
        let direction = direction.reverse();
        Self { value, direction }
    }

    /// Split the shape at the given point, if the point is within the tolerance specified by
    /// `snap_line_width` of the shape.
    fn split(self, split_point: Vector2, snap_line_width: f32) -> Option<SplitCorner> {
        let Corner { horizontal, vertical } = self.value;
        let hv_offset = horizontal - vertical;
        let (dx, dy) = (hv_offset.x().abs(), hv_offset.y().abs());
        let radius = min(dx, dy);

        // Calculate closeness to the straight segments.
        let (linear_x, linear_y) = (dx - radius, dy - radius);
        let snap_distance = snap_line_width / 2.0;
        let y_along_vertical = (self.vertical.y() - split_point.y()).abs() <= linear_y;
        let x_along_horizontal = (self.horizontal.x() - split_point.x()).abs() <= linear_x;
        let y_near_horizontal = (self.horizontal.y() - split_point.y()).abs() <= snap_distance;
        let x_near_vertical = (self.vertical.x() - split_point.x()).abs() <= snap_distance;

        // Calculate closeness to the arc.
        // 1. Find the origin of the circle the arc is part of.
        // The corner of our bounding box that is immediately outside the arc.
        let point_outside_arc = Vector2(self.vertical.x(), self.horizontal.y());
        // The opposite corner of our bounding box, far inside the arc.
        // Used to find the direction from outside the arc to the origin of the arc's circle.
        let point_inside_arc = Vector2(self.horizontal.x(), self.vertical.y());
        let outside_to_inside = point_inside_arc - point_outside_arc;
        let outside_to_origin =
            Vector2(radius.copysign(outside_to_inside.x()), radius.copysign(outside_to_inside.y()));
        let origin = point_outside_arc + outside_to_origin;
        // 2. Check if the point is on the arc.
        let input_to_origin = split_point - origin;
        let distance_squared_from_origin =
            input_to_origin.x().powi(2) + input_to_origin.y().powi(2);
        let min_radius = radius - snap_line_width / 2.0;
        let max_radius = radius + snap_line_width / 2.0;
        let too_close = distance_squared_from_origin < min_radius.powi(2);
        let too_far = distance_squared_from_origin > max_radius.powi(2);
        let on_arc = !(too_close || too_far);

        if y_near_horizontal && x_along_horizontal {
            // The point is along the horizontal line. Snap its y-value, and draw a corner to it.
            let snapped = Vector2(split_point.x(), self.horizontal.y());
            let source_end = self.with_target_end(snapped);
            let target_end = self.with_source_end(snapped);
            Some(SplitCorner { source_end, target_end, split_arc: None })
        } else if x_near_vertical && y_along_vertical {
            // The point is along the vertical line. Snap its x-value, and draw a corner to it.
            let snapped = Vector2(self.vertical.x(), split_point.y());
            let source_end = self.with_target_end(snapped);
            let target_end = self.with_source_end(snapped);
            Some(SplitCorner { source_end, target_end, split_arc: None })
        } else if on_arc {
            // Find the input point's angle along the arc.
            let offset_from_origin = split_point - origin;
            let split_angle = offset_from_origin.y().atan2(offset_from_origin.x());
            // Split the arc on the angle.
            let arc_horizontal_end = origin - Vector2(0.0, radius.copysign(outside_to_inside.y()));
            let arc_vertical_end = origin - Vector2(radius.copysign(outside_to_inside.x()), 0.0);
            let (arc_begin, arc_end) = match self.direction {
                CornerDirection::HorizontalToVertical => (arc_horizontal_end, arc_vertical_end),
                CornerDirection::VerticalToHorizontal => (arc_vertical_end, arc_horizontal_end),
            };
            let source_end = self.with_target_end(arc_begin);
            let target_end = self.with_source_end(arc_end);
            let source_end_angle = self.source_end_angle();
            let target_end_angle = self.target_end_angle();
            // Snap the angle to the arc's quadrant, so that the signs don't come out wrong if we
            // handle an event slightly outside the expected bounds.
            let (low, high) = match source_end_angle < target_end_angle {
                true => (source_end_angle, target_end_angle),
                false => (target_end_angle, source_end_angle),
            };
            let split_angle = split_angle.clamp(low, high);
            let split =
                SplitArc { origin, radius, source_end_angle, split_angle, target_end_angle };
            Some(SplitCorner { source_end, target_end, split_arc: Some(split) })
        } else {
            None
        }
    }

    fn source_end_angle(self) -> f32 {
        match self.direction {
            CornerDirection::HorizontalToVertical => self.horizontal_end_angle(),
            CornerDirection::VerticalToHorizontal => self.vertical_end_angle(),
        }
    }

    fn target_end_angle(self) -> f32 {
        self.reverse().source_end_angle()
    }
}


// === Corner direction ===

#[derive(Debug, Copy, Clone, PartialEq)]
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


// === Split (oriented) corners ====

#[derive(Debug, Copy, Clone, PartialEq)]
struct SplitCorner {
    source_end: Oriented<Corner>,
    target_end: Oriented<Corner>,
    split_arc:  Option<SplitArc>,
}
