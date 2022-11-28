//! Definition of the Edge component.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use crate::component::node;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl::gui::component::PointerTarget;
use ensogl_hardcoded_theme as theme;
use nalgebra::Rotation2;



// =================
// === Constants ===
// =================

const LINE_SHAPE_WIDTH: f32 = LINE_WIDTH + 2.0 * PADDING;
const LINE_SIDE_OVERLAP: f32 = 1.0;
const LINE_SIDES_OVERLAP: f32 = 2.0 * LINE_SIDE_OVERLAP;
const LINE_WIDTH: f32 = 4.0;
const ARROW_SIZE_X: f32 = 20.0;
const ARROW_SIZE_Y: f32 = 20.0;

const HOVER_EXTENSION: f32 = 10.0;

const MOUSE_OFFSET: f32 = 2.0;

// It was node::SHADOW_SIZE; Should be moved to theme manager and linked to node::shadow.
const NODE_PADDING: f32 = 10.0;

// The padding needs to be large enough to accommodate the extended hover area without clipping it.
const PADDING: f32 = 4.0 + HOVER_EXTENSION;
const RIGHT_ANGLE: f32 = std::f32::consts::PI / 2.0;
const INFINITE: f32 = 99999.0;

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



// =================
// === EdgeShape ===
// =================

/// Abstraction for all sub-shapes the edge shape is build from.
trait EdgeShape: display::Object {
    // === Info ===

    fn id(&self) -> display::object::Id {
        self.display_object().id()
    }
    fn events(&self) -> &PointerTarget;
    fn set_color(&self, color: color::Rgba);
    fn set_color_focus(&self, color: color::Rgba);


    // === Hover ===

    /// Set the center of the shape split on this shape. The coordinates must be in the shape local
    /// coordinate system.
    fn set_focus_split_center_local(&self, center: Vector2<f32>);

    /// Set the angle of the half plane that will be focused. Rotation starts with the plane
    /// focusing the left half plane.
    fn set_focus_split_angle(&self, angle: f32);

    /// Set the focus split for this shape. The `split` indicates where the shape should be
    /// split and how the split should be rotated.
    fn set_focus_split(&self, split: FocusSplit) {
        let angle = self.global_to_local_rotation(split.angle);
        let center = self.global_to_local_position(split.position);
        self.set_focus_split_angle(angle);
        self.set_focus_split_center_local(center);
    }

    /// Focus the whole edge.
    fn focus_none(&self) {
        // Set the focus split in the top right corner and focus everything to the right of it.
        self.set_focus_split_center_local(Vector2(INFINITE, INFINITE));
        self.set_focus_split_angle(RIGHT_ANGLE);
    }

    /// Do not focus any part of the edge.
    fn focus_all(&self) {
        // Set the focus split in the top right corner and focus everything below it.
        self.set_focus_split_center_local(Vector2(INFINITE, INFINITE));
        self.set_focus_split_angle(2.0 * RIGHT_ANGLE);
    }


    // === Snapping ===

    /// Snaps the provided point to the closest location on the shape.
    fn snap_local(&self, point: Vector2<f32>) -> Option<Vector2<f32>>;

    /// Snaps the provided point to the closest location on the shape.
    fn snap(&self, point: Vector2<f32>) -> Option<Vector2<f32>> {
        let local = self.global_to_local_position(point);
        let local_snapped = self.snap_local(local)?;
        Some(self.local_to_global_position(local_snapped))
    }


    // === Shape Analysis ===

    /// Return the angle perpendicular to the shape at the point given in the shapes local
    /// coordinate system . Defaults to zero, if not implemented.
    fn normal_local(&self, _point: Vector2<f32>) -> Rotation2<f32>;

    /// Return the angle perpendicular to the shape at the given point.
    fn normal(&self, point: Vector2<f32>) -> Rotation2<f32> {
        let local = self.global_to_local_position(point);
        self.normal_local(local)
    }


    // === Metrics ===

    /// Convert the angle to the local coordinate system.
    fn global_to_local_rotation(&self, angle: f32) -> f32 {
        angle + self.display_object().rotation().z
    }

    /// Convert the global position to the local coordinate system.
    fn global_to_local_position(&self, point: Vector2<f32>) -> Vector2<f32> {
        let base_rotation = self.display_object().rotation().z;
        let local_unrotated = point - self.display_object().global_position().xy();
        Rotation2::new(-base_rotation) * local_unrotated
    }

    /// Convert the local position to the global coordinate system.
    fn local_to_global_position(&self, point: Vector2<f32>) -> Vector2<f32> {
        let base_rotation = self.display_object().rotation().z;
        let local_unrotated = Rotation2::new(base_rotation) * point;
        local_unrotated + self.display_object().global_position().xy()
    }
}



// ====================
// === AnyEdgeShape ===
// ====================

/// The AnyEdgeShape trait allows operations on a collection of `EdgeShape`.
trait AnyEdgeShape {
    /// Return references to all `EdgeShape`s in this `AnyEdgeShape`.
    fn shapes(&self) -> Vec<&dyn EdgeShape>;

    /// Connect the given `PointerTargetProxy` to the mouse events of all sub-shapes.
    fn register_proxy_frp(&self, network: &frp::Network, frp: &PointerTargetProxy) {
        for shape in &self.shapes() {
            let event = shape.events();
            let id = shape.id();
            frp::extend! { network
                eval_ event.mouse_down_primary (frp.on_mouse_down.emit(id));
                eval_ event.mouse_over (frp.on_mouse_over.emit(id));
                eval_ event.mouse_out (frp.on_mouse_out.emit(id));
            }
        }
    }
}



// =======================
// === Hover Extension ===
// =======================

/// Add an invisible hover area to the provided shape. The base shape should already be colored
/// otherwise coloring it later will also color the hover area.
fn hover_area(base_shape: AnyShape, size: Var<Pixels>) -> AnyShape {
    let hover_area = base_shape.grow(size).fill(HOVER_COLOR);
    (hover_area + base_shape).into()
}



// ==================
// === FocusSplit ===
// ==================

/// Holds the data required to split a shape into two focus visual groups.
#[derive(Clone, Copy, Debug)]
struct FocusSplit {
    position: Vector2<f32>,
    angle:    f32,
}

impl FocusSplit {
    fn new(position: Vector2<f32>, angle: f32) -> Self {
        FocusSplit { position, angle }
    }
}



// ===================
// === FocusedEdge ===
// ===================

/// An edge split into two parts - focused and unfocused one.
struct FocusedEdge {
    focused:   AnyShape,
    unfocused: AnyShape,
}

impl FocusedEdge {
    /// Splits the shape in two at the line given by the `split_center` and `split_angle`.
    fn new(
        base_shape: impl Into<AnyShape>,
        split_center: &Var<Vector2<Pixels>>,
        split_angle: &Var<Radians>,
    ) -> Self {
        let base_shape = base_shape.into();
        let split_mask = HalfPlane().rotate(split_angle).translate(split_center);
        let focused = (&base_shape * &split_mask).into();
        let unfocused = (&base_shape - &split_mask).into();
        FocusedEdge { focused, unfocused }
    }

    /// Color the focused and unfocused parts with the provided colors.
    fn fill<C: Into<Var<color::Rgba>>>(&self, focused_color: C, unfocused_color: C) -> AnyShape {
        let focused_color = focused_color.into();
        let unfocused_color = unfocused_color.into();
        let focused = self.focused.fill(&focused_color);
        let unfocused = self.unfocused.fill(&unfocused_color);
        (focused + unfocused).into()
    }
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

/// Joint definition.
pub mod joint {
    use super::*;

    ensogl::shape! {
        pointer_events = false;
        (style: Style, color_rgba: Vector4<f32>) {
            let radius        = Var::<Pixels>::from("input_size.y");
            let joint         = Circle((radius-PADDING.px())/2.0);
            let joint_color   = Var::<color::Rgba>::from(color_rgba);
            let joint_colored = joint.fill(joint_color);
            joint_colored.into()
        }
    }
}

fn corner_base_shape(
    radius: &Var<f32>,
    width: &Var<Pixels>,
    angle: &Var<f32>,
    start_angle: &Var<f32>,
) -> AnyShape {
    let radius = 1.px() * radius;
    let width2 = width / 2.0;
    let radius_outer = &radius + &width2;
    let radius_inner = &radius - &width2;
    let ring = Circle(radius_outer) - Circle(radius_inner);
    let right: Var<f32> = RIGHT_ANGLE.into();
    let rot = right - angle / 2.0 + start_angle;
    let mask = Plane().cut_angle_fast(angle.clone()).rotate(rot);
    let shape = ring * mask;
    shape.into()
}

// FIXME [WD]: The 2 following impls are almost the same. Should be merged. This task should be
//             handled by Wojciech.
macro_rules! define_corner_start {
    () => {
        /// Shape definition.
        pub mod corner {
            use super::*;

            ensogl::shape! {
                below = [joint];
                ( style:               Style
                , radius             : f32
                , angle              : f32
                , start_angle        : f32
                , pos                : Vector2<f32>
                , dim                : Vector2<f32>
                , focus_split_center : Vector2<f32>
                , focus_split_angle  : f32
                , color_rgba:Vector4<f32>
                , focus_color_rgba:Vector4<f32>
                ) {
                    let width       = &LINE_WIDTH.px();
                    let shape       = corner_base_shape(&radius,width,&angle,&start_angle);
                    let color       = Var::<color::Rgba>::from(color_rgba);
                    let focus_color = Var::<color::Rgba>::from(focus_color_rgba);


                    let shadow_size = 10.px();
                    let node_radius = &shadow_size + 1.px() * dim.y();
                    let node_width  = &shadow_size*2.0 + 2.px() * dim.x();
                    let node_heigt  = &node_radius*2.0;

                    let node_shape  = Rect((node_width,node_heigt)).corners_radius(node_radius);
                    let node_shape  = node_shape.fill(color::Rgba::new(1.0,0.0,0.0,1.0));
                    let tx          = - 1.px() * pos.x();
                    let ty          = - 1.px() * pos.y();
                    let node_shape  = node_shape.translate((tx,ty));

                    let shape    = shape.difference(node_shape);

                    let split_shape = FocusedEdge::new(
                        shape,&focus_split_center.px(),&focus_split_angle.into());
                    let shape       = split_shape.fill(&color, &focus_color);

                    let hover_width = width + HOVER_EXTENSION.px() * 2.0;
                    let hover_area  = corner_base_shape(&radius,&hover_width,&angle,&start_angle);
                    let hover_area  = hover_area.fill(HOVER_COLOR);
                    (hover_area + shape).into()
                }
            }

            impl EdgeShape for View {
                fn set_focus_split_center_local(&self, center: Vector2<f32>) {
                    self.focus_split_center.set(center);
                }

                fn set_focus_split_angle(&self, angle: f32) {
                    self.focus_split_angle.set(angle);
                }

                fn events(&self) -> &PointerTarget {
                    &self.events
                }

                fn set_color(&self, color: color::Rgba) {
                    self.color_rgba.set(Vector4(color.red, color.green, color.blue, color.alpha));
                }

                fn set_color_focus(&self, color: color::Rgba) {
                    let color_vec = Vector4(color.red, color.green, color.blue, color.alpha);
                    self.focus_color_rgba.set(color_vec);
                }

                fn normal_local(&self, point: Vector2<f32>) -> Rotation2<f32> {
                    point_rotation(point)
                }

                fn snap_local(&self, point: Vector2<f32>) -> Option<Vector2<f32>> {
                    // FIXME: These bounds check should not be required and should be removed once
                    // issue #689 is resolved.
                    let radius = self.radius.get();
                    let center = Vector2::zero();
                    let point_to_center = point.xy() - center;

                    let closest_point =
                        center + point_to_center / point_to_center.magnitude() * radius;
                    let vector_angle =
                        -Rotation2::rotation_between(&Vector2(0.0, 1.0), &closest_point).angle();
                    let start_angle = self.start_angle.get();
                    let end_angle = start_angle + self.angle.get();
                    let upper_bound = start_angle.max(end_angle);
                    let lower_bound = start_angle.min(end_angle);

                    let correct_quadrant = lower_bound < vector_angle && upper_bound > vector_angle;
                    correct_quadrant.as_some(Vector2(closest_point.x, closest_point.y))
                }
            }
        }
    };
}


macro_rules! define_corner_end {
    () => {
        /// Shape definition.
        pub mod corner {
            use super::*;
            ensogl::shape! {
                below = [joint];
                (
                    style: Style,
                    radius: f32,
                    angle: f32,
                    start_angle: f32,
                    pos: Vector2<f32>,
                    dim: Vector2<f32>,
                    focus_split_center: Vector2<f32>,
                    focus_split_angle: f32,
                    color_rgba: Vector4<f32>,
                    focus_color_rgba: Vector4<f32>,
                ) {
                    let width       = &LINE_WIDTH.px();
                    let shape       = corner_base_shape(&radius,width,&angle,&start_angle);
                    let color       = Var::<color::Rgba>::from(color_rgba);
                    let focus_color = Var::<color::Rgba>::from(focus_color_rgba);

                    let shadow_size = 10.px() + 1.px();
                    let node_radius = &shadow_size + 1.px() * dim.y();
                    let node_shape  = Rect((&shadow_size*2.0 + 2.px() * dim.x(),&node_radius*2.0));
                    let node_shape  = node_shape.corners_radius(node_radius);
                    let node_shape  = node_shape.fill(color::Rgba::new(1.0,0.0,0.0,1.0));
                    let tx       = - 1.px() * pos.x();
                    let ty       = - 1.px() * pos.y();
                    let node_shape  = node_shape.translate((tx,ty));

                    let shape = shape.intersection(node_shape);

                    let split_shape = FocusedEdge::new(
                    shape,&focus_split_center.px(),&focus_split_angle.into());
                    let shape       = split_shape.fill(&color,&focus_color);

                    let hover_width = width + HOVER_EXTENSION.px() * 2.0;
                    let hover_area  = corner_base_shape(&radius,&hover_width,&angle,&start_angle);
                    let hover_area  = hover_area.fill(HOVER_COLOR);
                    (hover_area + shape).into()
                }
            }

            impl EdgeShape for View {
                fn set_focus_split_center_local(&self, center: Vector2<f32>) {
                    self.focus_split_center.set(center);
                }

                fn set_focus_split_angle(&self, angle: f32) {
                    self.focus_split_angle.set(angle);
                }

                fn events(&self) -> &PointerTarget {
                    &self.events
                }

                fn set_color(&self, color: color::Rgba) {
                    self.color_rgba.set(Vector4(color.red, color.green, color.blue, color.alpha));
                }

                fn set_color_focus(&self, color: color::Rgba) {
                    self.focus_color_rgba.set(Vector4(
                        color.red,
                        color.green,
                        color.blue,
                        color.alpha,
                    ));
                }

                fn normal_local(&self, point: Vector2<f32>) -> Rotation2<f32> {
                    point_rotation(point)
                }

                fn snap_local(&self, point: Vector2<f32>) -> Option<Vector2<f32>> {
                    // FIXME: These bounds check should not be required and should be removed once
                    // issue #689 is resolved.
                    let radius = self.radius.get();
                    let center = Vector2::zero();
                    let point_to_center = point.xy() - center;

                    let closest_point =
                        center + point_to_center / point_to_center.magnitude() * radius;
                    let vector_angle =
                        -Rotation2::rotation_between(&Vector2(0.0, 1.0), &closest_point).angle();
                    let start_angle = self.start_angle.get();
                    let end_angle = start_angle + self.angle.get();
                    let upper_bound = start_angle.max(end_angle);
                    let lower_bound = start_angle.min(end_angle);

                    let correct_quadrant = lower_bound < vector_angle && upper_bound > vector_angle;
                    if correct_quadrant {
                        Some(Vector2(closest_point.x, closest_point.y))
                    } else {
                        None
                    }
                }
            }
        }
    };
}

macro_rules! define_line {
    () => {
        /// Shape definition.
        pub mod line {
            use super::*;
            ensogl::shape! {
                below = [joint];
                (
                    style: Style,
                    focus_split_center: Vector2<f32>,
                    focus_split_angle: f32,
                    color_rgba: Vector4<f32>,
                    focus_color_rgba: Vector4<f32>
                ) {
                    let width       = LINE_WIDTH.px();
                    let height      = Var::<Pixels>::from("input_size.y");
                    let shape       = Rect((width.clone(),height));
                    let color       = Var::<color::Rgba>::from(color_rgba);
                    let focus_color = Var::<color::Rgba>::from(focus_color_rgba);

                    let split_shape = FocusedEdge::new(
                        shape,&focus_split_center.px(),&focus_split_angle.into());
                    let shape       = split_shape.fill(&color,&focus_color);
                    hover_area(shape,HOVER_EXTENSION.px()).into()
                }
            }

            impl EdgeShape for View {
                fn set_focus_split_center_local(&self, center: Vector2<f32>) {
                    self.focus_split_center.set(center);
                }

                fn set_focus_split_angle(&self, angle: f32) {
                    self.focus_split_angle.set(angle);
                }

                fn events(&self) -> &PointerTarget {
                    &self.events
                }

                fn set_color(&self, color: color::Rgba) {
                    self.color_rgba.set(Vector4(color.red, color.green, color.blue, color.alpha));
                }

                fn set_color_focus(&self, color: color::Rgba) {
                    self.focus_color_rgba.set(Vector4(
                        color.red,
                        color.green,
                        color.blue,
                        color.alpha,
                    ));
                }

                fn normal_local(&self, _: Vector2<f32>) -> Rotation2<f32> {
                    Rotation2::new(0.0)
                }

                fn snap_local(&self, point: Vector2<f32>) -> Option<Vector2<f32>> {
                    // FIXME: These bounds check should not be required and should be removed once
                    // issue #689 is resolved.
                    let height = self.size.get().y;
                    let y = point.y.clamp(-height / 2.0, height / 2.0);
                    Some(Vector2(0.0, y))
                }
            }
        }
    };
}

macro_rules! define_arrow { () => {
    /// Shape definition.
    pub mod arrow {
        use super::*;
        ensogl::shape! {
            above = [joint];
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

            fn events(&self) -> &PointerTarget {
                &self.events
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



// ========================
// === Shape Operations ===
// ========================

trait LayoutLine {
    fn layout_v(&self, start: Vector2<f32>, len: f32);
    fn layout_h(&self, start: Vector2<f32>, len: f32);
    fn layout_v_no_overlap(&self, start: Vector2<f32>, len: f32);
    fn layout_h_no_overlap(&self, start: Vector2<f32>, len: f32);
}

impl LayoutLine for front::line::View {
    fn layout_v(&self, start: Vector2<f32>, len: f32) {
        let pos = Vector2(start.x, start.y + len / 2.0);
        let size = Vector2(LINE_SHAPE_WIDTH, len.abs() + LINE_SIDES_OVERLAP);
        self.size.set(size);
        self.set_xy(pos);
    }
    fn layout_h(&self, start: Vector2<f32>, len: f32) {
        let pos = Vector2(start.x + len / 2.0, start.y);
        let size = Vector2(LINE_SHAPE_WIDTH, len.abs() + LINE_SIDES_OVERLAP);
        self.size.set(size);
        self.set_xy(pos);
    }
    fn layout_v_no_overlap(&self, start: Vector2<f32>, len: f32) {
        let pos = Vector2(start.x, start.y + len / 2.0);
        let size = Vector2(LINE_SHAPE_WIDTH, len.abs());
        self.size.set(size);
        self.set_xy(pos);
    }
    fn layout_h_no_overlap(&self, start: Vector2<f32>, len: f32) {
        let pos = Vector2(start.x + len / 2.0, start.y);
        let size = Vector2(LINE_SHAPE_WIDTH, len.abs());
        self.size.set(size);
        self.set_xy(pos);
    }
}

impl LayoutLine for back::line::View {
    fn layout_v(&self, start: Vector2<f32>, len: f32) {
        let pos = Vector2(start.x, start.y + len / 2.0);
        let size = Vector2(LINE_SHAPE_WIDTH, len.abs() + LINE_SIDES_OVERLAP);
        self.size.set(size);
        self.set_xy(pos);
    }
    fn layout_h(&self, start: Vector2<f32>, len: f32) {
        let pos = Vector2(start.x + len / 2.0, start.y);
        let size = Vector2(LINE_SHAPE_WIDTH, len.abs() + LINE_SIDES_OVERLAP);
        self.size.set(size);
        self.set_xy(pos);
    }
    fn layout_v_no_overlap(&self, start: Vector2<f32>, len: f32) {
        let pos = Vector2(start.x, start.y + len / 2.0);
        let size = Vector2(LINE_SHAPE_WIDTH, len.abs());
        self.size.set(size);
        self.set_xy(pos);
    }
    fn layout_h_no_overlap(&self, start: Vector2<f32>, len: f32) {
        let pos = Vector2(start.x + len / 2.0, start.y);
        let size = Vector2(LINE_SHAPE_WIDTH, len.abs());
        self.size.set(size);
        self.set_xy(pos);
    }
}



// ===========================
// === Front / Back Shapes ===
// ===========================

/// Shape definitions which will be rendered in the front layer (on top of nodes).
pub mod front {
    use super::*;
    define_corner_start!();
    define_line!();
    define_arrow!();
}

/// Shape definitions which will be rendered in the bottom layer (below nodes).
pub mod back {
    use super::*;
    define_corner_end!();
    define_line!();
    define_arrow!();
}



// ===========================
// === Front / Back Layers ===
// ===========================

macro_rules! define_components {
    ($name:ident {
        $($field:ident : ($field_type:ty,  $field_shape_type:expr)),* $(,)?
    }) => {
        #[derive(Debug,Clone,CloneRef)]
        #[allow(missing_docs)]
        pub struct $name {
            pub logger            : Logger,
            pub display_object    : display::object::Instance,
            pub shape_view_events : Rc<Vec<PointerTarget>>,
            shape_type_map        : Rc<HashMap<display::object::Id,ShapeRole>>,
            $(pub $field : $field_type),*
        }

        impl $name {
            /// Constructor.
            #[allow(clippy::vec_init_then_push)]
            pub fn new(logger:Logger) -> Self {
                let display_object = display::object::Instance::new();
                $(let $field = <$field_type>::new();)*
                $(display_object.add_child(&$field);)*
                let mut shape_view_events:Vec<PointerTarget> = Vec::default();
                $(shape_view_events.push($field.events.clone_ref());)*
                let shape_view_events = Rc::new(shape_view_events);

                let mut shape_type_map:HashMap<display::object::Id,ShapeRole> = default();
                $(shape_type_map.insert(EdgeShape::id(&$field), $field_shape_type);)*
                let shape_type_map = Rc::new(shape_type_map);

                Self {logger,display_object,shape_view_events,shape_type_map,$($field),*}
            }

            fn get_shape(&self, id:display::object::Id) -> Option<&dyn EdgeShape> {
                match id {
                    $(id if id == EdgeShape::id(&self.$field) => Some(&self.$field),)*
                    _ => None,
                }
            }

            fn get_shape_type(&self, id:display::object::Id) -> Option<ShapeRole> {
                self.shape_type_map.get(&id).cloned()
            }
        }

        impl display::Object for $name {
            fn display_object(&self) -> &display::object::Instance {
                &self.display_object
            }
        }

        impl AnyEdgeShape for $name {
            #[allow(clippy::vec_init_then_push)]
            fn shapes(&self) -> Vec<&dyn EdgeShape> {
                let mut output = Vec::<&dyn EdgeShape>::default();
                $(output.push(&self.$field);)*
                output
            }
        }
    }
}

define_components! {
    Front {
        corner     : (front::corner::View, ShapeRole::Corner),
        corner2    : (front::corner::View, ShapeRole::Corner2),
        corner3    : (front::corner::View, ShapeRole::Corner3),
        side_line  : (front::line::View,   ShapeRole::SideLine),
        side_line2 : (front::line::View,   ShapeRole::SideLine2),
        main_line  : (front::line::View,   ShapeRole::MainLine),
        port_line  : (front::line::View,   ShapeRole::PortLine),
        arrow      : (front::arrow::View,  ShapeRole::Arrow),
    }
}

define_components! {
    Back {
        corner     : (back::corner::View, ShapeRole::Corner),
        corner2    : (back::corner::View, ShapeRole::Corner2),
        corner3    : (back::corner::View, ShapeRole::Corner3),
        side_line  : (back::line::View,   ShapeRole::SideLine),
        side_line2 : (back::line::View,   ShapeRole::SideLine2),
        main_line  : (back::line::View,   ShapeRole::MainLine),
        arrow      : (back::arrow::View,  ShapeRole::Arrow),
    }
}

impl AnyEdgeShape for EdgeModelData {
    fn shapes(&self) -> Vec<&dyn EdgeShape> {
        let mut shapes_back = self.back.shapes();
        let mut shapes_front = self.front.shapes();
        shapes_front.append(&mut shapes_back);
        shapes_front
    }
}



// ===========================
// === Shape & State Enums ===
// ===========================

/// Indicates which role a shape plays within the overall edge.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ShapeRole {
    SideLine,
    Corner,
    MainLine,
    Corner2,
    SideLine2,
    Corner3,
    PortLine,
    Arrow,
}

/// Indicates the state the shape layout is in. Can be used to adjust behaviour based on state
/// to address edge cases for specific layouts. The terms are used to follow the direction of the
/// edge from `Output` to `Input`.
///
/// Each state represents a unique layout in terms of: adjacency of shapes (some shapes may
/// disappear in some layout), or the relative geometric position of shapes. For example, the
/// `TopCenterRightLoop` has the main line leaving the node right to left, while corner2 and
/// corner3 are left to right relative to each other. Compare the `UpRight`, which is almost the
/// same, but has the main line leave the source node left to right.
///
/// This list is not exhaustive and new constellations should be added as needed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LayoutState {
    UpLeft,
    UpRight,
    DownLeft,
    DownRight,
    /// The edge goes right / up / left / down.
    TopCenterRightLoop,
    /// The edge goes left / up / right / down.
    TopCenterLeftLoop,
}

impl LayoutState {
    /// Indicates whether the `OutputPort` is below the `InputPort` in the current layout
    /// configuration.
    fn is_output_above_input(self) -> bool {
        match self {
            LayoutState::UpLeft => false,
            LayoutState::UpRight => false,
            LayoutState::TopCenterRightLoop => false,
            LayoutState::TopCenterLeftLoop => false,
            LayoutState::DownLeft => true,
            LayoutState::DownRight => true,
        }
    }

    fn is_input_above_output(self) -> bool {
        !self.is_output_above_input()
    }
}



// =====================
// === SemanticSplit ===
// =====================

/// The semantic split, splits the sub-shapes according to their relative position from `OutputPort`
/// to `InputPort` and allows access to the three different groups of shapes:
///   - shapes that are input side of the split;
///   - shapes that are at the split;
///   - shapes that are output side of the split.
///
/// This allows us to apply special handling to these groups. This is required as a simple geometric
/// split based on a line, can lead to double intersections with the edge. Thus we avoid the
/// geometric intersections, for shapes away from the intersection point, and instead color them
/// based on their position within the edge.
///
/// We have seven "slots" of shapes within the edge that can be ordered from output port to input
/// port: `SideLine` `Corner`, `MainLine`/`Arrow`, `Corner2`, `SideLine2`, `Corner3`, `PortLine`.
/// Note that it does not matter, if we have multiple adjacent shapes in the same bucket (as can
/// be the case with back/front shapes), as long as no self-intersection is possible for these
/// shapes.
///
/// Example: We need to split on the `SideLine2` and focus the shapes closer to the
/// output port. That means we need to do the geometric split on  `Corner2`, `SideLine2`, `Corner3`,
/// which we can access via `split_shapes` and apply the focusing to `SideLine` `Corner` and
/// `MainLine`/`Arrow`, which we can access via `output_side_shapes`. The remaining shapes that must
/// not be focused can be accessed via `input_side_shapes`.
#[derive(Clone, Debug)]
struct SemanticSplit {
    /// Ids of the shapes in the order they appear in the edge. Shapes that fill the same "slot"
    /// are binned into a sub-vector and can be handled together.
    ordered_part_ids: Vec<Vec<display::object::Id>>,
    /// The index the shape where the edge split occurs in the `ordered_part_ids`.
    split_index:      usize,
}

impl SemanticSplit {
    fn new(edge_data: &EdgeModelData, split_shape: display::object::Id) -> Option<Self> {
        let ordered_part_ids = Self::semantically_binned_edges(edge_data);

        // Find the object id in our `ordered_part_ids`
        let mut split_index = None;
        for (index, shape_ids) in ordered_part_ids.iter().enumerate() {
            if shape_ids.contains(&split_shape) {
                split_index = Some(index);
                break;
            }
        }
        let split_index = split_index?;

        Some(SemanticSplit { ordered_part_ids, split_index })
    }

    /// Return an ordered vector that contains the ids of the shapes in the order they appear in the
    /// edge. Shapes that are to be handled as in the same place, are binned into a sub-vector.
    /// This enables us to infer which parts are next to each other, and which ones are
    /// "source-side"/"target-side".
    ///
    /// In general, we treat the equivalent shape from front and back as the same, as they tend to
    /// occupy the same space within the shape and thus need to be handled together. But,
    /// for example, also the arrow needs to be handled together with the main line.
    ///
    /// The important information we create here is the rough adjacency of shapes. This is used to
    /// determine which shapes are adjacent to avoid rendering a split on a shape that can be all
    /// focus on or all focus off.
    fn semantically_binned_edges(edge_data: &EdgeModelData) -> Vec<Vec<display::object::Id>> {
        let front = &edge_data.front;
        let back = &edge_data.back;
        vec![
            vec![EdgeShape::id(&front.side_line), EdgeShape::id(&back.side_line)],
            vec![EdgeShape::id(&front.corner), EdgeShape::id(&back.corner)],
            vec![
                EdgeShape::id(&front.main_line),
                EdgeShape::id(&back.main_line),
                EdgeShape::id(&front.arrow),
                EdgeShape::id(&back.arrow),
            ],
            vec![EdgeShape::id(&front.corner2), EdgeShape::id(&back.corner2)],
            vec![EdgeShape::id(&front.side_line2), EdgeShape::id(&back.side_line2)],
            vec![EdgeShape::id(&front.corner3), EdgeShape::id(&back.corner3)],
            vec![EdgeShape::id(&front.port_line)],
        ]
    }

    /// Return `Id`s that match the given index condition `cond`.
    fn index_filtered_shapes<F: Fn(i32) -> bool>(&self, cond: F) -> Vec<display::object::Id> {
        self.ordered_part_ids
            .iter()
            .enumerate()
            .filter(|(index, _)| cond(*index as i32))
            .flat_map(|(_index, ids)| ids.clone())
            .collect()
    }

    /// Shapes that are output side of the split.
    fn output_side_shapes(&self) -> Vec<display::object::Id> {
        self.index_filtered_shapes(move |index| (index) < self.split_index as i32)
    }

    /// Shapes that are input side of the split.
    fn input_side_shapes(&self) -> Vec<display::object::Id> {
        self.index_filtered_shapes(move |index| index > (self.split_index as i32))
    }

    /// Shapes that are at the split location.
    fn split_shapes(&self) -> Vec<display::object::Id> {
        self.index_filtered_shapes(move |index| (index == self.split_index as i32))
    }
}



// ===========
// === FRP ===
// ===========

/// FRP system that is used to collect and aggregate shape view events from the sub-shapes of an
/// `Edge`. The Edge exposes the `mouse_down`/`mouse_over`/`mouse_out` streams, while the sub-shapes
/// emit events via th internal `on_mouse_down`/`on_mouse_over`/`on_mouse_out` sources.
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
    model:   Rc<EdgeModel>,
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
        let model = Rc::new(EdgeModel { data });
        Self { model, network }.init(app)
    }

    fn init(self, app: &Application) -> Self {
        let network = &self.network;
        let input = &self.frp;
        let target_position = &self.target_position;
        let target_attached = &self.target_attached;
        // FIXME This should be used for #672 (Edges created from Input Ports do not overlay nodes)
        let _source_attached = &self.source_attached;
        let source_width = &self.source_width;
        let source_height = &self.source_height;
        let hover_position = &self.hover_position;
        let hover_target = &self.hover_target;

        let model = &self.model;
        let shape_events = &self.frp.shape_events;
        let edge_color = color::Animation::new(network);
        let edge_focus_color = color::Animation::new(network);
        let _style = StyleWatch::new(&app.display.default_scene.style_sheet);

        model.data.front.register_proxy_frp(network, &input.shape_events);
        model.data.back.register_proxy_frp(network, &input.shape_events);

        frp::extend! { network
            eval input.target_position ((t) target_position.set(*t));
            // FIXME This should be enabled for #672 (Edges created from Input Ports do not overlay
            //       nodes)
            // eval input.source_attached ((t) source_attached.set(*t));
            eval input.target_attached ((t) target_attached.set(*t));
            eval input.source_width    ((t) source_width.set(*t));
            eval input.source_height   ((t) source_height.set(*t));
            eval input.hover_position  ((t) hover_position.set(*t));

            eval  shape_events.on_mouse_over ((id) hover_target.set(Some(*id)));
            eval_ shape_events.on_mouse_out       (hover_target.set(None));
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
#[allow(missing_docs)]
pub struct EdgeModelData {
    pub display_object:  display::object::Instance,
    pub logger:          Logger,
    pub frp:             Frp,
    pub front:           Front,
    pub back:            Back,
    pub joint:           joint::View,
    pub source_width:    Rc<Cell<f32>>,
    pub source_height:   Rc<Cell<f32>>,
    pub target_position: Rc<Cell<Vector2>>,
    pub target_attached: Rc<Cell<bool>>,
    pub source_attached: Rc<Cell<bool>>,

    layout_state:   Rc<Cell<LayoutState>>,
    hover_position: Rc<Cell<Option<Vector2<f32>>>>,
    hover_target:   Rc<Cell<Option<display::object::Id>>>,
    scene:          Scene,
}

impl EdgeModelData {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(scene: &Scene, network: &frp::Network) -> Self {
        let logger = Logger::new("edge");
        let display_object = display::object::Instance::new();
        let front = Front::new(Logger::new_sub(&logger, "front"));
        let back = Back::new(Logger::new_sub(&logger, "back"));
        let joint = joint::View::new();

        display_object.add_child(&front);
        display_object.add_child(&back);
        display_object.add_child(&joint);

        front.side_line.mod_rotation(|r| r.z = RIGHT_ANGLE);
        back.side_line.mod_rotation(|r| r.z = RIGHT_ANGLE);
        front.side_line2.mod_rotation(|r| r.z = RIGHT_ANGLE);
        back.side_line2.mod_rotation(|r| r.z = RIGHT_ANGLE);

        let frp = Frp::new(network);
        let source_height = default();
        let source_width = default();
        let target_position = default();
        let target_attached = Rc::new(Cell::new(false));
        let source_attached = Rc::new(Cell::new(true));
        let hover_position = default();
        let layout_state = Rc::new(Cell::new(LayoutState::UpLeft));
        let hover_target = default();

        let scene = scene.into();
        Self {
            display_object,
            logger,
            frp,
            front,
            back,
            joint,
            source_width,
            source_height,
            target_position,
            target_attached,
            source_attached,
            layout_state,
            hover_position,
            hover_target,
            scene,
        }
    }

    /// Set the color of the edge.
    fn set_color(&self, color: color::Lcha) {
        // We must never use alpha in edges, as it will show artifacts with overlapping sub-parts.
        let color: color::Lcha = color.opaque.into();
        let color_rgba = color::Rgba::from(color);
        self.shapes().iter().for_each(|shape| shape.set_color(color_rgba));
        self.joint.color_rgba.set(color_rgba.into());
    }

    fn set_focus_color(&self, color: color::Lcha) {
        let color: color::Lcha = color.opaque.into();
        self.shapes().iter().for_each(|shape| shape.set_color_focus(color.into()));
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

    /// Redraws the connection.
    #[allow(clippy::cognitive_complexity)]
    pub fn redraw(&self) {
        // === Variables ===

        let fg = &self.front;
        let bg = &self.back;
        // FIXME This should be enabled for #672
        // let fully_attached   = self.target_attached.get() && self.source_attached.get();
        let fully_attached = self.target_attached.get();
        let node_half_width = self.source_width.get() / 2.0;
        let target_node_half_height = node::HEIGHT / 2.0;
        let source_node_half_height = self.source_height.get() / 2.0;
        let source_node_circle = Vector2(node_half_width - source_node_half_height, 0.0);
        let source_node_radius = source_node_half_height;


        // === Update Highlights ===

        match (fully_attached, self.hover_position.get(), self.hover_target.get()) {
            (true, Some(hover_position), Some(hover_target)) => {
                let focus_part = self.port_to_detach_for_position(hover_position);
                let focus_split_result =
                    self.try_enable_focus_split(hover_position, hover_target, focus_part);
                if let Ok(snap_data) = focus_split_result {
                    let joint_position = snap_data.position - self.display_object.position().xy();
                    self.joint.set_xy(joint_position);
                    let joint_size = LINE_WIDTH + PADDING;
                    self.joint.size.set(Vector2(joint_size, joint_size));
                }
            }
            _ => {
                self.focus_none();
                self.joint.size.set(Vector2::zero());
            }
        }


        // === Target ===
        //
        // Target is the end position of the connection in local node space (the origin is placed in
        // the center of the node). We handle lines drawing in special way when target is below the
        // node (for example, not to draw the port line above source node).
        //
        // ╭──────────────╮
        // │      ┼ (0,0) │
        // ╰──────────────╯────╮
        //                     │
        //                     ▢ target

        let world_space_target = self.target_position.get();
        let target_x = world_space_target.x - self.position().x;
        let target_y = world_space_target.y - self.position().y;
        let side = target_x.signum();
        let target_x = target_x.abs();
        let target = Vector2(target_x, target_y);
        let target_is_below_node_x = target.x < node_half_width;
        let target_is_below_node_y = target.y < (-source_node_half_height);
        let target_is_below_node = target_is_below_node_x && target_is_below_node_y;
        let port_line_len_max = target_node_half_height + NODE_PADDING;
        let side_right_angle = side * RIGHT_ANGLE;


        // === Upward Discovery ===
        //
        // Discovers when the connection should go upwards. The `upward_corner_radius` defines the
        // preferred radius for every corner in this scenario.
        //
        // ╭─────╮    ╭─╮
        // ╰─────╯────╯ │
        //              ▢

        let upward_corner_radius = 20.0;
        let min_len_for_non_curved_line = upward_corner_radius + port_line_len_max;
        let upward_distance = target.y + min_len_for_non_curved_line;


        // === Flat side ===
        //
        // Maximum side distance before connection is curved up.
        //
        // ╭─────╮◄──►    ╭─────╮◄──►╭─╮
        // ╰─────╯───╮    ╰─────╯────╯ │
        //           ▢                 ▢

        let flat_side_size = 40.0;
        let is_flat_side = target.x < node_half_width + flat_side_size;
        let downward_flat =
            if target_is_below_node_x { target_is_below_node_y } else { target.y < 0.0 };
        let downward_far = -target.y > min_len_for_non_curved_line || target_is_below_node;
        let is_down = if is_flat_side { downward_flat } else { downward_far };

        // === Layout State ===
        // Initial guess at our layout. Can still be changed further down in the layout code in
        // case we encounter a layout where the corners need to loop back.
        let state = match (is_down, (side < 0.0)) {
            (true, true) => LayoutState::DownLeft,
            (true, false) => LayoutState::DownRight,
            (false, true) => LayoutState::UpLeft,
            (false, false) => LayoutState::UpRight,
        };
        self.layout_state.set(state);

        // === Port Line Length ===
        //
        // ╭──╮
        // ╰──╯───╮
        //        ╵
        //     ╭──┼──╮ ▲  Port line covers the area above target node and the area of target node
        //     │  ▢  │ ▼  shadow. It can be shorter if the target position is below the node or the
        //     ╰─────╯    connection is being dragged, in order not to overlap with the source node.

        let port_line_start = Vector2(side * target.x, target.y + MOUSE_OFFSET);
        let space_attached = -port_line_start.y - target_node_half_height - LINE_SIDE_OVERLAP;
        let space = space_attached - NODE_PADDING;
        let len_below_free = max(0.0, min(port_line_len_max, space));
        let len_below_attached = max(0.0, min(port_line_len_max, space_attached));
        let len_below = if fully_attached { len_below_attached } else { len_below_free };
        let far_side_len = if target_is_below_node { len_below } else { port_line_len_max };
        let flat_side_len = min(far_side_len, -port_line_start.y);
        let mut port_line_len = if is_flat_side && is_down { flat_side_len } else { far_side_len };
        let port_line_end = Vector2(target.x, port_line_start.y + port_line_len);


        // === Corner1 ===
        //
        // The first corner on the line. It is always placed at the right angle to the tangent of
        // the node border. In case the edge is in the drag mode, the curve is divided into two
        // parts. The first part is placed under the source node shadow, while the second part is
        // placed on the top layer.
        //
        // ╭─────╮        ╭─────╮ 2╭───╮3
        // ╰─────╯──╮1    ╰─────╯──╯1  ▢
        //          ▢

        let mut corner1_target = port_line_end;
        if !is_down {
            corner1_target.x = if is_flat_side {
                let radius_grow = max(0.0, target.x - node_half_width + upward_corner_radius);
                node_half_width + upward_corner_radius + radius_grow
            } else {
                let radius1 = node_half_width + (target.x - node_half_width) / 2.0;
                let radius2 = node_half_width + 2.0 * upward_corner_radius;
                min(radius1, radius2)
            };
            corner1_target.y = min(upward_corner_radius, upward_distance / 2.0);
        }

        let corner1_grow = ((corner1_target.x - node_half_width) * 0.6).max(0.0);
        let corner1_radius = 20.0 + corner1_grow;
        let corner1_radius = corner1_radius.min(corner1_target.y.abs());
        let corner1_x = corner1_target.x - corner1_radius;

        let corner1_x_loc = corner1_x - source_node_circle.x;
        let (y, angle) = circle_intersection(corner1_x_loc, source_node_radius, corner1_radius);
        let corner1_y = if is_down { -y } else { y };
        let corner1 = Vector2(corner1_x * side, corner1_y);
        let angle_overlap = if corner1_x > node_half_width { 0.0 } else { 0.1 };
        let corner1_side = (corner1_radius + PADDING) * 2.0;
        let corner1_size = Vector2(corner1_side, corner1_side);
        let corner1_start_angle = if is_down { 0.0 } else { side_right_angle };
        let corner1_angle = (angle + angle_overlap) * side;
        let corner1_angle = if is_down { corner1_angle } else { side_right_angle };

        bg.corner.size.set(corner1_size);
        bg.corner.start_angle.set(corner1_start_angle);
        bg.corner.angle.set(corner1_angle);
        bg.corner.radius.set(corner1_radius);
        bg.corner.pos.set(corner1);
        bg.corner.set_xy(corner1);
        if !fully_attached {
            bg.corner.dim.set(Vector2(node_half_width, source_node_half_height));
            fg.corner.size.set(corner1_size);
            fg.corner.start_angle.set(corner1_start_angle);
            fg.corner.angle.set(corner1_angle);
            fg.corner.radius.set(corner1_radius);
            fg.corner.pos.set(corner1);
            fg.corner.dim.set(Vector2(node_half_width, source_node_half_height));
            fg.corner.set_xy(corner1);
        } else {
            fg.corner.size.set(zero());
            bg.corner.dim.set(Vector2(INFINITE, INFINITE));
        }


        // === Side Line ===
        //
        // Side line is the first horizontal line. In case the edge is in drag mode, the line is
        // divided into two segments. The first is placed below the shadow of the source node, while
        // the second is placed on the top layer. The side line placement is the same in case of
        // upwards connections - it is then placed between node and corenr 1.
        //
        // ╭─────╮       ╭─────╮ 2╭───╮3
        // ╰─────╯╴──╮   ╰─────╯╴─╯1  ▢
        //           ▢

        let side_line_shift = LINE_SIDES_OVERLAP;
        let side_line_len = max(0.0, corner1_x - node_half_width + side_line_shift);
        let bg_line_x = node_half_width - side_line_shift;
        let bg_line_start = Vector2(side * bg_line_x, 0.0);
        if fully_attached {
            let bg_line_len = side * side_line_len;
            fg.side_line.size.set(zero());
            bg.side_line.layout_h(bg_line_start, bg_line_len);
        } else {
            let bg_max_len = NODE_PADDING + side_line_shift;
            let bg_line_len = min(side_line_len, bg_max_len);
            let bg_end_x = bg_line_x + bg_line_len;
            let fg_line_start = Vector2(side * (bg_end_x + LINE_SIDE_OVERLAP), 0.0);
            let fg_line_len = side * (side_line_len - bg_line_len);
            let bg_line_len_overlap = side * min(side_line_len, bg_max_len + LINE_SIDES_OVERLAP);
            bg.side_line.layout_h(bg_line_start, bg_line_len_overlap);
            fg.side_line.layout_h_no_overlap(fg_line_start, fg_line_len);
        }


        // === Main Line (downwards) ===
        //
        // Main line is the long vertical line. In case it is placed below the node and the edge is
        // in drag mode, it is divided into two segments. The upper segment is drawn behind node
        // shadow, while the second is drawn on the top layer. In case of edge in drag mode drawn
        // next to node, only the top layer segment is used.
        //
        // Please note that only applies to edges going down. Refer to docs of main line of edges
        // going up to learn more.
        //
        // Double edge:   Single edge:
        // ╭─────╮        ╭─────╮
        // ╰──┬──╯        ╰─────╯────╮
        //    ╷                      │
        //    ▢                      ▢

        if is_down {
            let main_line_end_y = corner1.y;
            let main_line_len = main_line_end_y - port_line_start.y;
            if !fully_attached && target_is_below_node {
                let back_line_start_y =
                    max(-source_node_half_height - NODE_PADDING, port_line_start.y);
                let back_line_start = Vector2(port_line_start.x, back_line_start_y);
                let back_line_len = main_line_end_y - back_line_start_y;
                let front_line_len = main_line_len - back_line_len;
                bg.main_line.layout_v(back_line_start, back_line_len);
                fg.main_line.layout_v(port_line_start, front_line_len);
            } else if fully_attached {
                let main_line_start_y = port_line_start.y + port_line_len;
                let main_line_start = Vector2(port_line_start.x, main_line_start_y);
                fg.main_line.size.set(zero());
                bg.main_line.layout_v(main_line_start, main_line_len - port_line_len);
            } else {
                bg.main_line.size.set(zero());
                fg.main_line.layout_v(port_line_start, main_line_len);
            }
        }


        if !is_down {
            // === Corner2 & Corner3 Radius ===
            //
            // ╭─────╮ 2╭───╮3
            // ╰─────╯──╯1  ▢

            let corner2_radius = corner1_radius;
            let corner3_radius = upward_corner_radius;

            let corner2_x = corner1_target.x + corner1_radius;
            let corner3_x = port_line_end.x - corner3_radius;
            let corner2_bbox_x = corner2_x - corner2_radius;
            let corner3_bbox_x = corner3_x + corner3_radius;

            let corner_2_3_dist = corner3_bbox_x - corner2_bbox_x;
            let corner_2_3_side = corner_2_3_dist.signum();
            let corner_2_3_dist = corner_2_3_dist.abs();
            let corner_2_3_width = corner2_radius + corner3_radius;
            let corner_2_3_do_scale = corner_2_3_dist < corner_2_3_width;
            let corner_2_3_scale = corner_2_3_dist / corner_2_3_width;
            let corner_2_3_scale = if corner_2_3_do_scale { corner_2_3_scale } else { 1.0 };

            let side_combined = side * corner_2_3_side;
            let corner2_radius = corner2_radius * corner_2_3_scale;
            let corner3_radius = corner3_radius * corner_2_3_scale;
            let is_right_side = (side_combined - 1.0).abs() < std::f32::EPSILON;


            // === Layout State Update ===
            // Corner case: we are above the node and the corners loop back
            match (side < 0.0, corner_2_3_side < 0.0) {
                (false, true) => self.layout_state.set(LayoutState::TopCenterRightLoop),
                (true, true) => self.layout_state.set(LayoutState::TopCenterLeftLoop),
                _ => (),
            };


            // === Corner2 & Corner3 Placement ===
            //
            // ╭─────╮ 2╭───╮3
            // ╰─────╯──╯1  ▢

            let corner3_side = (corner3_radius + PADDING) * 2.0;
            let corner3_size = Vector2(corner3_side, corner3_side);
            let corner3_x = port_line_end.x - corner_2_3_side * corner3_radius;
            let corner3_y = port_line_end.y;
            let corner2_y = corner3_y + corner3_radius - corner2_radius;
            let corner2_y = max(corner2_y, corner1.y);
            let corner3_y = max(corner3_y, corner2_y - corner3_radius + corner2_radius);
            let corner3 = Vector2(corner3_x * side, corner3_y);
            let corner3_angle = if is_right_side { 0.0 } else { -RIGHT_ANGLE };

            if fully_attached {
                fg.corner3.size.set(zero());
                bg.corner3.size.set(corner3_size);
                bg.corner3.start_angle.set(corner3_angle);
                bg.corner3.angle.set(RIGHT_ANGLE);
                bg.corner3.radius.set(corner3_radius);
                bg.corner3.pos.set(corner3);
                bg.corner3.dim.set(Vector2(INFINITE, INFINITE));
                bg.corner3.set_xy(corner3);
            } else {
                bg.corner3.size.set(zero());
                fg.corner3.size.set(corner3_size);
                fg.corner3.start_angle.set(corner3_angle);
                fg.corner3.angle.set(RIGHT_ANGLE);
                fg.corner3.radius.set(corner3_radius);
                fg.corner3.pos.set(corner3);
                fg.corner3.dim.set(zero());
                fg.corner3.set_xy(corner3);
            }

            let corner2_x = corner1_target.x + corner_2_3_side * corner2_radius;
            let corner2 = Vector2(corner2_x * side, corner2_y);
            let corner2_angle = if is_right_side { -RIGHT_ANGLE } else { 0.0 };

            if fully_attached {
                fg.corner2.size.set(zero());
                bg.corner2.size.set(corner1_size);
                bg.corner2.start_angle.set(corner2_angle);
                bg.corner2.angle.set(RIGHT_ANGLE);
                bg.corner2.radius.set(corner2_radius);
                bg.corner2.pos.set(corner2);
                bg.corner2.dim.set(Vector2(INFINITE, INFINITE));
                bg.corner2.set_xy(corner2);
            } else {
                bg.corner2.size.set(zero());
                fg.corner2.size.set(corner1_size);
                fg.corner2.start_angle.set(corner2_angle);
                fg.corner2.angle.set(RIGHT_ANGLE);
                fg.corner2.radius.set(corner2_radius);
                fg.corner2.pos.set(corner2);
                fg.corner2.dim.set(zero());
                fg.corner2.set_xy(corner2);
            }


            // === Main Line (upwards) ===
            //
            // Main line is the first vertical line of the edge placed between the corner 1 and the
            // corner 2. In case the line is long enough, it has an arrow pointing up to show its
            // direction.
            //
            // ╭─────╮ 2╭───╮3
            // ╰─────╯──╯1  ▢

            let main_line_len = corner2_y - corner1.y;
            let main_line_start = Vector2(side * corner1_target.x, corner1.y);

            if fully_attached {
                fg.main_line.size.set(zero());
                bg.main_line.layout_v(main_line_start, main_line_len);
            } else {
                bg.main_line.size.set(zero());
                fg.main_line.layout_v(main_line_start, main_line_len);
            }

            if main_line_len > ARROW_SIZE_Y {
                let arrow_y = (corner1.y - corner1_radius + corner2_y + corner2_radius) / 2.0;
                let arrow_pos = Vector2(main_line_start.x, arrow_y);
                let arrow_size = Vector2(ARROW_SIZE_X, ARROW_SIZE_Y);
                if fully_attached {
                    fg.arrow.size.set(zero());
                    bg.arrow.size.set(arrow_size);
                    bg.arrow.set_xy(arrow_pos);
                } else {
                    bg.arrow.size.set(zero());
                    fg.arrow.size.set(arrow_size);
                    fg.arrow.set_xy(arrow_pos);
                }
            } else {
                bg.arrow.size.set(zero());
                fg.arrow.size.set(zero());
            }


            // === Side Line 2 ===
            //
            // Side line 2 is the horizontal line connecting corner 2 and corner 3.
            //
            // ╭─────╮ 2╭───╮3
            // ╰─────╯──╯1  ▢

            let side_line2_len = side * (corner3_x - corner2_x);
            let side_line2_start = Vector2(side * corner2_x, corner2_y + corner2_radius);
            if fully_attached {
                fg.side_line2.size.set(zero());
                bg.side_line2.layout_h(side_line2_start, side_line2_len);
            } else {
                bg.side_line2.size.set(zero());
                fg.side_line2.layout_h(side_line2_start, side_line2_len);
            }

            port_line_len = corner3_y - port_line_start.y;
        } else {
            fg.arrow.size.set(zero());
            bg.arrow.size.set(zero());
            fg.corner3.size.set(zero());
            bg.corner3.size.set(zero());
            fg.corner2.size.set(zero());
            bg.corner2.size.set(zero());
            fg.side_line2.size.set(zero());
            bg.side_line2.size.set(zero());
        }


        // === Port Line ===
        fg.port_line.layout_v(port_line_start, port_line_len);
    }
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
        if self.input_and_output_y_too_close() {
            return self.closest_end_for_point(point);
        }
        let input_port_is_in_upper_half = self.layout_state.get().is_input_above_output();
        let point_is_in_upper_half = self.is_in_upper_half(point);

        // We always detach the port that is on the opposite side of the cursor.
        if point_is_in_upper_half != input_port_is_in_upper_half {
            PortType::InputPort
        } else {
            PortType::OutputPort
        }
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

    /// Indicates whether the height difference between input and output is too small to  use the
    /// y value to assign the `EndDesignation` for a given point.
    fn input_and_output_y_too_close(&self) -> bool {
        let target_y = self.position().y;
        let source_y = self.target_position.get().y;
        let delta_y = target_y - source_y;
        delta_y > 0.0 && delta_y < MIN_SOURCE_TARGET_DIFFERENCE_FOR_Y_VALUE_DISCRIMINATION
    }

    /// Return the correct cut angle for the given `shape_id` at the `position` to focus the
    /// `target_end`. Will return `None` if the `shape_id` is not a valid sub-shape of this edge.
    fn cut_angle_for_shape(
        &self,
        shape_id: display::object::Id,
        position: Vector2<f32>,
        target_end: PortType,
    ) -> Option<f32> {
        let shape = self.get_shape(shape_id)?;
        let shape_role = self.get_shape_role(shape_id)?;

        let cut_angle_correction = self.get_cut_angle_correction(shape_role);
        let target_angle = self.get_target_angle(target_end);

        let base_rotation = shape.display_object().rotation().z + 2.0 * RIGHT_ANGLE;
        let shape_normal = shape.normal(position).angle();
        Some(shape_normal - base_rotation + cut_angle_correction + target_angle)
    }

    /// Return the cut angle value needed to focus the given end of the shape. This takes into
    /// account the current layout.
    fn get_target_angle(&self, target_end: PortType) -> f32 {
        let output_on_top = self.layout_state.get().is_output_above_input();
        match (output_on_top, target_end) {
            (false, PortType::InputPort) => 2.0 * RIGHT_ANGLE,
            (false, PortType::OutputPort) => 0.0,
            (true, PortType::InputPort) => 0.0,
            (true, PortType::OutputPort) => 2.0 * RIGHT_ANGLE,
        }
    }

    /// These corrections are needed as sometimes shapes are in places that lead to inconsistent
    /// results, e.g., the side line leaving the node from left/right or right/left. The shape
    /// itself does not have enough information about its own placement to determine which end
    /// is pointed towards the `Target` or `Source` part of the whole edge. So we need to account
    /// for these here based on the specific layout state we are in.
    ///
    /// Example:
    /// ```text
    /// 
    ///    Case 1
    ///
    ///           (===)----...
    ///         Node     Side Line
    ///
    ///    Case 2
    ///
    ///          ...----(===)
    ///     Side Line     Node
    /// ```
    ///
    /// In both case 1 and 2 the side line is oriented left to right just placed in a different
    /// location. However, in Case 1 the left side of the line is "output side" and in Case 2 the
    /// right side is "output side". So if we want to set an equivalent angle, we need to apply a
    /// correction based on this layout property.
    fn get_cut_angle_correction(&self, shape_role: ShapeRole) -> f32 {
        let layout_state = self.layout_state.get();

        let flip = 2.0 * RIGHT_ANGLE;

        // These rules are derived from the algorithm in `redraw`. In some layout configurations
        // shapes are inverted top/down or left/right and we need to apply the appropriate
        // corrections here. Sometimes these are just the side-effect of some layouting mechanics
        // without visual justification (e.g., the `PortLine` sometimes ends up with a negative
        // height and is thus flipped upside down.
        match (layout_state, shape_role) {
            (LayoutState::DownLeft, ShapeRole::SideLine) => flip,
            (LayoutState::DownLeft, ShapeRole::Corner) => flip,

            (LayoutState::UpLeft, ShapeRole::PortLine) => flip,
            (LayoutState::UpLeft, ShapeRole::Corner) => flip,

            (LayoutState::UpRight, ShapeRole::PortLine) => flip,
            (LayoutState::UpRight, ShapeRole::Corner3) => flip,
            (LayoutState::UpRight, ShapeRole::SideLine2) => flip,
            (LayoutState::UpRight, ShapeRole::Corner2) => flip,
            (LayoutState::UpRight, ShapeRole::SideLine) => flip,

            (LayoutState::TopCenterRightLoop, ShapeRole::SideLine) => flip,
            (LayoutState::TopCenterRightLoop, ShapeRole::PortLine) => flip,

            (LayoutState::TopCenterLeftLoop, ShapeRole::SideLine2) => flip,
            (LayoutState::TopCenterLeftLoop, ShapeRole::Corner2) => flip,
            (LayoutState::TopCenterLeftLoop, ShapeRole::Corner) => flip,
            (LayoutState::TopCenterLeftLoop, ShapeRole::Corner3) => flip,
            (LayoutState::TopCenterLeftLoop, ShapeRole::PortLine) => flip,

            (_, ShapeRole::Arrow) => RIGHT_ANGLE,

            _ => 0.0,
        }
    }

    /// Return a reference to sub-shape indicated by the given shape id.
    fn get_shape(&self, id: display::object::Id) -> Option<&dyn EdgeShape> {
        let shape_ref = self.back.get_shape(id);
        if shape_ref.is_some() {
            return shape_ref;
        }
        self.front.get_shape(id)
    }

    /// Return the `ShapeRole` for the given sub-shape.
    fn get_shape_role(&self, id: display::object::Id) -> Option<ShapeRole> {
        let shape_type = self.back.get_shape_type(id);
        if shape_type.is_some() {
            return shape_type;
        }
        self.front.get_shape_type(id)
    }

    /// Check whether the provided point is close enough to be snapped to the edge.
    fn try_point_snap(
        &self,
        point: Vector2<f32>,
        focus_shape_id: display::object::Id,
    ) -> Option<SnapTarget> {
        let focus_shape = self.get_shape(focus_shape_id)?;
        let snap_position = focus_shape.snap(point)?;
        Some(SnapTarget::new(snap_position, focus_shape_id))
    }

    /// Disable the splitting of the shape.
    fn focus_none(&self) {
        for shape in self.shapes() {
            shape.focus_none();
        }
    }

    /// FocusSplit the shape at the given `position` and focus the given `EndDesignation`. This
    /// might fail if the given position is too far from the shape.
    fn try_enable_focus_split(
        &self,
        position: Vector2<f32>,
        focus_shape_id: display::object::Id,
        part: PortType,
    ) -> Result<SnapTarget, ()> {
        let snap_data = self.try_point_snap(position, focus_shape_id).ok_or(())?;
        let semantic_split = SemanticSplit::new(self, snap_data.target_shape_id).ok_or(())?;
        let angle = self.cut_angle_for_shape(snap_data.target_shape_id, position, part).ok_or(())?;

        // Completely disable/enable focus for shapes that are not close to the split based on their
        // relative position within the shape. This avoids issues with splitting not working
        // correctly when a split would intersect the edge at multiple points.
        semantic_split.output_side_shapes().iter().for_each(|shape_id| {
            if let Some(shape) = self.get_shape(*shape_id) {
                match part {
                    PortType::OutputPort => shape.focus_all(),
                    PortType::InputPort => shape.focus_none(),
                }
            }
        });
        semantic_split.input_side_shapes().iter().for_each(|shape_id| {
            if let Some(shape) = self.get_shape(*shape_id) {
                match part {
                    PortType::OutputPort => shape.focus_none(),
                    PortType::InputPort => shape.focus_all(),
                }
            }
        });
        // Apply a split to the shapes at the split location, and next to the split shapes. The
        // extension to neighbours is required to show the correct transition from one shape to the
        // next.
        semantic_split.split_shapes().iter().for_each(|shape_id| {
            if let Some(shape) = self.get_shape(*shape_id) {
                let split_data = FocusSplit::new(snap_data.position, angle);
                shape.set_focus_split(split_data)
            }
        });
        Ok(snap_data)
    }
}
