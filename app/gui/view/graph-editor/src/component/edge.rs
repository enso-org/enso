//! Definition of the Edge component.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
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
const ARROW_SIZE_X: f32 = 20.0;
const ARROW_SIZE_Y: f32 = 20.0;

const HOVER_EXTENSION: f32 = 10.0;
const HOVER_WIDTH: f32 = LINE_WIDTH + HOVER_EXTENSION;



// =========================
// === Shape Definitions ===
// =========================

/// An arc around the origin. `outer_radius` determines the distance from the origin to the outer
/// edge of the arc, `stroke_width` the width of the arc. The arc starts at `start_angle`, relative
/// to the origin and ends at `end_angle`. The ends are flat not rounded, as in [`RoundedArc`].
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

define_endpoints_2! {
    Input {
        source_width(f32),
        source_height(f32),
        target_position(Vector2<f32>),
        target_attached(bool),
        source_attached(bool),
        redraw(),
        set_disabled(bool),
        set_color(color::Lcha),
    }
    Output {
        source_click(),
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
    model:   Rc<EdgeModel>,
    pub frp: Frp,
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
        let edge_focus_color = color::Animation::new(network);

        use ensogl::control::io::mouse;
        let display_object = &model.display_object;
        let mouse_move = display_object.on_event::<mouse::Move>();
        let mouse_down = display_object.on_event::<mouse::Down>();

        // These events never fire.
        //let mouse_enter = display_object.on_event::<mouse::Enter>();
        //let mouse_leave = display_object.on_event::<mouse::Leave>();

        let mouse_over = display_object.on_event::<mouse::Over>();
        let mouse_out = display_object.on_event::<mouse::Out>();

        //app.display.default_scene.layers.main.add(display_object);

        let output = &frp.private.output;
        frp::extend! { network
            // Setter inputs.
            eval frp.target_position ((t) model.set_target_position(*t));
            eval frp.source_attached ((t) model.set_source_attached(*t));
            eval frp.target_attached ((t) model.set_target_attached(*t));
            eval frp.source_width    ((t) model.set_source_width(*t));
            eval frp.source_height   ((t) model.set_source_height(*t));

            // Mouse events.
            eval mouse_move            ((e) model.set_mouse_position_and_redraw(e.client_centered()));
            eval_ mouse_out            (model.clear_focus_and_redraw());
            eval mouse_down            ([model, output] (e) {
                match model.closer_end_to_screen_pos(e.client_centered()) {
                    Some(EndPoint::Source) => output.target_click.emit(()),
                    Some(EndPoint::Target) => output.source_click.emit(()),
                    // Ignore click events that were delivered to our display object inaccurately.
                    None => (),
                }
            });

            // Redraw event.
            eval_ frp.redraw                    (model.redraw());


            // === Colors ===

            edge_color.target       <+ frp.set_color;
            eval edge_color.value       ((color) model.set_color(color.into()));

            /*
            new_color       <- all_with(&frp.set_color,&frp.set_disabled,
                f!((c,t)model.base_color(*c,*t)));
            new_focus_color <- new_color.map(f!((color) model.focus_color(*color)));
            focus_color     <- switch(&is_hovered,&new_color,&new_focus_color);

            edge_focus_color.target <+ focus_color;

            eval edge_focus_color.value ((color) model.set_focus_color(color.into()));
             */
        }
        Self { model, frp }
    }
}

impl display::Object for Edge {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}



// =================
// === EdgeModel ===
// =================

/// Internal data of `Edge`
#[derive(Debug)]
pub struct EdgeModel {
    /// The parent display object of all the edge's parts.
    display_object: display::object::Instance,
    scene:          Scene,

    // === Inputs ===
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
    color:               Cell<color::Rgba>,
    /// The location of the mouse over the edge.
    hover_position:      Cell<Option<Vector2<f32>>>,

    // === Cached state ===
    /// The endpoints of the individual [`Corner`]s making up the edge.
    corner_points: RefCell<Vec<Vector2<f32>>>,

    // === Shapes ===
    /// The individual [`Corner`]s making up the edge. Each is drawn in the focused or unfocused
    /// color.
    sections:       RefCell<Vec<Rectangle>>,
    /// A pair of [`arc`] shapes used when the mouse is over the rounded corner, and the edge must
    /// must be split into focused and unfocused sides at a certain angle along the arc.
    split_arc:      RefCell<Option<[arc::View; 2]>>,
    /// Wider versions of the [`sections`], for receiving mouse events.
    hover_sections: RefCell<Vec<Rectangle>>,

    // === Change detection ===
    previous_target:       Cell<Option<Vector2<f32>>>,
    previous_is_hoverable: Cell<Option<bool>>,
    previous_hover_split:  Cell<Option<EdgeSplit>>,
    previous_color:        Cell<Option<color::Rgba>>,
}

fn update_and_compare<T: Copy + PartialEq<T>, U: Into<Option<T>>>(
    value: &Cell<Option<T>>,
    new: U,
) -> bool {
    let new = new.into();
    let old = value.replace(new);
    old != new
}

impl EdgeModel {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(scene: &Scene) -> Self {
        let display_object = display::object::Instance::new_named("Edge");
        let source_attached = Cell::new(true);
        let scene = scene.into();
        Self {
            display_object,
            source_attached,
            scene,
            source_width: default(),
            source_height: default(),
            target_position: default(),
            target_attached: default(),
            color: default(),
            sections: default(),
            split_arc: default(),
            hover_sections: default(),
            corner_points: default(),
            hover_position: default(),
            previous_target: default(),
            previous_is_hoverable: default(),
            previous_hover_split: default(),
            previous_color: default(),
        }
    }

    /// Set the color of the edge.
    fn set_color(&self, color: color::Lcha) {
        // We must never use alpha in edges, as it will show artifacts with overlapping sub-parts.
        // XXX: This should no longer be necessary.
        let color: color::Lcha = color.opaque.into();
        let color_rgba = color::Rgba::from(color);
        self.color.set(color_rgba);
        self.redraw();
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

    fn source_x_range(&self) -> f32 {
        match self.source_attached.get() {
            // When attached to a node, our origination point can be anywhere along the length of
            // the node, excluding the rounded edges.
            true => (self.source_width.get() / 2.0 - NODE_CORNER_RADIUS).max(0.0),
            // When attached to the cursor, our origination point is fixed at the cursor.
            false => 0.0,
        }
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

    fn clear_focus_and_redraw(&self) {
        self.hover_position.set(None);
        self.redraw();
    }

    fn closer_end_to_screen_pos(&self, screen_pos: Vector2<f32>) -> Option<EndPoint> {
        // Convert point to local coordinates.
        let screen_pos_3d = Vector3(screen_pos.x(), screen_pos.y(), 0.0);
        let scene_pos = self.scene.screen_to_scene_coordinates(screen_pos_3d).xy();
        let pos = scene_pos - self.display_object.xy();

        let corners = corners(&*self.corner_points.borrow()).collect_vec();
        find_position(pos, &corners).map(|split| split.closer_end)
    }

    fn set_mouse_position_and_redraw(&self, screen_pos: Vector2<f32>) {
        // Convert point to local coordinates.
        let screen_pos_3d = Vector3(screen_pos.x(), screen_pos.y(), 0.0);
        let scene_pos = self.scene.screen_to_scene_coordinates(screen_pos_3d).xy();
        let pos = scene_pos - self.display_object.xy();

        self.hover_position.set(Some(pos));
        self.redraw();
    }

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

    fn target_offset(&self) -> Vector2<f32> {
        let target_offset = self.target_position.get() - self.display_object.xy();
        match self.target_attached.get() {
            // If the target is a node, connect to a point on its top edge. If the radius is small,
            // this looks better than connecting to a vertically-centered point.
            true => target_offset + Vector2(0.0, NODE_CORNER_RADIUS),
            // If the target is the cursor, connect all the way to it.
            false => target_offset,
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
        let is_hoverable = self.target_attached.get() && self.source_attached.get();
        let is_hoverable_changed = update_and_compare(&self.previous_is_hoverable, is_hoverable);
        let hover_split = is_hoverable
            .then(|| {
                // Pointer targets are updated by an asynchronous process, independent of pointer
                // movement detection. As a result, we can receive mouse events when the pointer is
                // not within the bounding box of any of our shapes, in which case `find_position`
                // here will return `None`. We treat it the same way as a
                // `mouse::Out` event.
                self.hover_position.get().and_then(|position| find_position(position, &corners))
            })
            .flatten();
        let hover_split_changed = update_and_compare(&self.previous_hover_split, hover_split);
        let normal_color = self.color.get();
        let color_changed = update_and_compare(&self.previous_color, normal_color);

        // Create shape objects for the current geometry.
        if target_changed || is_hoverable_changed {
            let hover_factory = self
                .hover_sections
                .take()
                .into_iter()
                .chain(iter::repeat_with(|| self.new_hover_section()));
            if is_hoverable {
                *self.hover_sections.borrow_mut() = corners
                    .iter()
                    .zip(hover_factory)
                    .map(|(corner, shape)| draw_corner(shape, **corner, HOVER_WIDTH))
                    .collect();
            }
        }
        if target_changed || hover_split_changed || color_changed {
            let styles = StyleWatch::new(&self.scene.style_sheet);
            let bg_color = styles.get_color(theme::application::background).into();
            let focused_color = color::mix(bg_color, normal_color, 0.25);
            let EdgeSplit { corner_index, closer_end, position } =
                hover_split.unwrap_or_else(|| EdgeSplit {
                    corner_index: corners.len(),
                    closer_end:   EndPoint::Source,
                    position:     default(),
                });
            let mut section_factory =
                self.sections.take().into_iter().chain(iter::repeat_with(|| self.new_section()));
            let mut new_sections = corners
                .iter()
                .enumerate()
                .filter_map(|(i, corner)| {
                    if i == corner_index {
                        None
                    } else {
                        let is_focused = match closer_end {
                            EndPoint::Source => i > corner_index,
                            EndPoint::Target => i < corner_index,
                        };
                        let color = match is_focused {
                            false => normal_color,
                            true => focused_color,
                        };
                        Some((color, corner))
                    }
                })
                .zip(&mut section_factory)
                .map(|((color, corner), shape)| {
                    let shape = draw_corner(shape, **corner, LINE_WIDTH);
                    shape.set_border_color(color);
                    shape
                })
                .collect_vec();
            let arc_shapes = self.split_arc.take();
            if let Some(corner) = corners.get(corner_index)
                    && let Some(pos) = self.hover_position.get() {
                let (source_side, target_side, split_arc) =
                    corner.split_geometry(pos, HOVER_WIDTH, LINE_WIDTH);
                let (source_color, target_color) = match closer_end {
                    EndPoint::Source => (normal_color, focused_color),
                    EndPoint::Target => (focused_color, normal_color),
                };
                if let Some(split_arc) = split_arc {
                    let arc_shapes = arc_shapes.unwrap_or_else(|| [self.new_arc(), self.new_arc()]);
                    let outer_radius = split_arc.radius + LINE_WIDTH / 2.0;
                    let arc_box = Vector2(outer_radius * 2.0, outer_radius * 2.0);
                    let arc_offset = Vector2(-outer_radius, -outer_radius);
                    for shape in &arc_shapes {
                        shape.set_xy(split_arc.origin + arc_offset);
                        shape.set_size(arc_box);
                        shape.outer_radius.set(split_arc.radius + LINE_WIDTH / 2.0);
                    }
                    let (source, target) = (&arc_shapes[0], &arc_shapes[1]);
                    source.start_angle.set(split_arc.split_angle.rem_euclid(std::f32::consts::TAU).min(split_arc.source_end_angle.rem_euclid(std::f32::consts::TAU)));
                    source.sector_angle.set((split_arc.split_angle.abs() - split_arc.source_end_angle.abs()).abs());
                    target.start_angle.set(split_arc.split_angle.rem_euclid(std::f32::consts::TAU).min(split_arc.target_end_angle.rem_euclid(std::f32::consts::TAU)));
                    target.sector_angle.set((split_arc.target_end_angle.abs() - split_arc.split_angle.abs()).abs());
                    source.color.set(source_color.into());
                    target.color.set(target_color.into());
                    self.split_arc.set(arc_shapes);
                }
                let (source_shape, target_shape) = (section_factory.next().unwrap(), section_factory.next().unwrap());
                source_shape.set_border_color(source_color);
                target_shape.set_border_color(target_color);
                new_sections.push(draw_geometry(source_shape, source_side));
                new_sections.push(draw_geometry(target_shape, target_side));
            }
            *self.sections.borrow_mut() = new_sections;
        }
    }

    fn new_arc(&self) -> arc::View {
        let arc = arc::View::new();
        arc.stroke_width.set(LINE_WIDTH);
        self.display_object.add_child(&arc);
        arc
    }

    /// Calculate the start and end positions of each 1-corner section composing an edge to the
    /// given offset from this object's `display_object`.
    fn corner_points_to(&self, target: Vector2<f32>) -> Vec<Vector2<f32>> {
        let source_half_width = self.source_x_range();
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

#[derive(Debug, Copy, Clone, PartialEq)]
struct EdgeSplit {
    corner_index: usize,
    closer_end:   EndPoint,
    position:     Vector2<f32>,
}

/// Find a point along the edge. Return the index of the corner the point occurs in, and which end
/// is closer to the point.
///
/// Returns [`None`] if the point is not on the edge.
fn find_position(position: Vector2<f32>, corners: &[Oriented<Corner>]) -> Option<EdgeSplit> {
    corners
        .iter()
        .position(|&corner| corner.bounding_box(HOVER_WIDTH).contains_inclusive(position))
        .map(|corner_index| {
            let (full_corners, following_corners) = corners.split_at(corner_index);
            let full_corners_distance: f32 =
                full_corners.iter().map(|&corner| corner.snake_length()).sum();
            let following_distance: f32 =
                following_corners.iter().map(|&corner| corner.snake_length()).sum();
            let total_distance = full_corners_distance + following_distance;
            let offset_from_partial_corner = position - corners[corner_index].source_end();
            let partial_corner_distance =
                offset_from_partial_corner.x().abs() + offset_from_partial_corner.y().abs();
            let distance_from_source = full_corners_distance + partial_corner_distance;
            let closer_end = match distance_from_source * 2.0 < total_distance {
                true => EndPoint::Source,
                false => EndPoint::Target,
            };
            EdgeSplit { corner_index, closer_end, position }
        })
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum EndPoint {
    Source,
    Target,
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



// ==============
// === Corner ===
// ==============

#[derive(Debug, Copy, Clone)]
struct Corner {
    horizontal: Vector2<f32>,
    vertical:   Vector2<f32>,
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

    fn snake_length(self) -> f32 {
        let Corner { horizontal, vertical } = self;
        let offset = horizontal - vertical;
        offset.x().abs() + offset.y().abs()
    }

    fn transpose(self) -> Self {
        let Corner { horizontal, vertical } = self;
        Corner { horizontal: vertical.yx().into(), vertical: horizontal.yx().into() }
    }

    fn vertical_end_angle(self) -> f32 {
        match self.vertical.x() > self.horizontal.x() {
            true => 0.0,
            false => std::f32::consts::PI.copysign(self.horizontal.y()-self.vertical.y())
        }
    }

    fn horizontal_end_angle(self) -> f32 {
        std::f32::consts::FRAC_PI_2.copysign(self.horizontal.y()-self.vertical.y())
    }
}


// === Rectangle geometry describing a corner ===

#[derive(Debug, Copy, Clone, Default)]
struct RectangleGeometry {
    clip: Vector2<f32>,
    size: Vector2<f32>,
    xy:   Vector2<f32>,
}


// === Parameters for drawing the arc portion of a corner in two parts ===

#[derive(Debug, Copy, Clone, Default)]
struct SplitArcGeometry {
    origin:           Vector2<f32>,
    radius:           f32,
    source_end_angle: f32,
    split_angle:      f32,
    target_end_angle: f32,
}



// ========================
// === Oriented corners ===
// ========================

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

    fn with_source_end(mut self, value: Vector2<f32>) -> Self {
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

    fn split_geometry(
        self,
        end_point: Vector2<f32>,
        snap_line_width: f32,
        line_width: f32,
    ) -> (RectangleGeometry, RectangleGeometry, Option<SplitArcGeometry>) {
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

        if y_near_horizontal && x_along_horizontal {
            // The point is along the horizontal line. Snap its y-value, and draw a corner to it.
            let snapped = Vector2(end_point.x(), self.horizontal.y());
            let from_source = self.with_target_end(snapped).to_rectangle_geometry(line_width);
            let to_target = self.with_source_end(snapped).to_rectangle_geometry(line_width);
            (from_source, to_target, default())
        } else if x_near_vertical && y_along_vertical {
            // The point is along the vertical line. Snap its x-value, and draw a corner to it.
            let snapped = Vector2(self.vertical.x(), end_point.y());
            let from_source = self.with_target_end(snapped).to_rectangle_geometry(line_width);
            let to_target = self.with_source_end(snapped).to_rectangle_geometry(line_width);
            (from_source, to_target, default())
        } else {
            // The point is along the arc. Snap it to the arc.
            // 1. Find the origin of the circle the arc is part of.
            // The corner of our bounding box that is immediately outside the arc.
            let point_outside_arc = Vector2(self.vertical.x(), self.horizontal.y());
            // The opposite corner of our bounding box, far inside the arc.
            // Used to find the direction from outside the arc to the origin of the arc's circle.
            let point_inside_arc = Vector2(self.horizontal.x(), self.vertical.y());
            let outside_to_inside = point_inside_arc - point_outside_arc;
            let outside_to_origin = Vector2(
                radius.copysign(outside_to_inside.x()),
                radius.copysign(outside_to_inside.y()),
            );
            let origin = point_outside_arc + outside_to_origin;
            // 2. Find the input point's angle along the arc.
            let offset_from_origin = end_point - origin;
            let split_angle = offset_from_origin.y().atan2(offset_from_origin.x());
            // 3. Find the true arc point at the input point's angle.
            let (sin, cos) = split_angle.sin_cos();
            let snapped_offset = Vector2(cos * radius, sin * radius);
            let snapped = origin + snapped_offset;
            const DEBUG_ARC_SNAP: bool = false;
            if DEBUG_ARC_SNAP {
                // Draw the circle that is being used for the arc-snap computation. It should line
                // up exactly with the visible arc (although inputs may lie anywhere on the larger
                // hover-extended arc).
                let adjusted_radius = radius + line_width / 2.0;
                let snap_circle = RectangleGeometry {
                    xy:   origin - Vector2(adjusted_radius, adjusted_radius),
                    clip: Vector2(0.0, 0.0),
                    size: Vector2(radius * 2.0 + line_width, radius * 2.0 + line_width),
                };
                // Draw a corner from the snapped point calculated from the circle above to the
                // target side.
                let corner_from_snapped_point =
                    self.with_source_end(snapped).to_rectangle_geometry(line_width);
                return (snap_circle, corner_from_snapped_point, default());
            }
            let arc_horizontal_end = origin - Vector2(0.0, radius.copysign(outside_to_inside.y()));
            let arc_vertical_end = origin - Vector2(radius.copysign(outside_to_inside.x()), 0.0);
            let (arc_begin, arc_end) = match self.direction {
                CornerDirection::HorizontalToVertical => (arc_horizontal_end, arc_vertical_end),
                CornerDirection::VerticalToHorizontal => (arc_vertical_end, arc_horizontal_end),
            };
            let source_to_arc = self.with_target_end(arc_begin).to_rectangle_geometry(line_width);
            let target_from_arc = self.with_source_end(arc_end).to_rectangle_geometry(line_width);
            let source_end_angle = self.source_end_angle();
            let target_end_angle = self.target_end_angle();
            let split = SplitArcGeometry {
                origin,
                radius,
                source_end_angle,
                split_angle,
                target_end_angle,
            };
            (source_to_arc, target_from_arc, Some(split))
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
