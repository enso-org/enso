//! FIXME[everyone] Modules should be documented.

use crate::prelude::*;
use enso_text::unit::*;

use crate::application::tooltip;
use crate::application::tooltip::Placement;
use crate::component::node;
use crate::component::type_coloring;
use crate::view;
use crate::Type;

use enso_frp as frp;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::shape::primitive::def::class::ShapeOps;
use ensogl::display::shape::AnyShape;
use ensogl::display::shape::BottomHalfPlane;
use ensogl::display::shape::Circle;
use ensogl::display::shape::PixelDistance;
use ensogl::display::shape::Pixels;
use ensogl::display::shape::Rect;
use ensogl::display::shape::StyleWatch;
use ensogl::display::shape::StyleWatchFrp;
use ensogl::display::shape::Var;
use ensogl::gui::component;
use ensogl::gui::text;
use ensogl::Animation;



// =================
// === Constants ===
// =================

const PORT_SIZE: f32 = 4.0;
const PORT_OPACITY_HOVERED: f32 = 1.0;
const PORT_OPACITY_NOT_HOVERED: f32 = 0.25;
const SEGMENT_GAP_WIDTH: f32 = 2.0;
const HOVER_AREA_PADDING: f32 = 20.0;
const INFINITE: f32 = 99999.0;
const FULL_TYPE_ONSET_DELAY_MS: f32 = 2000.0;

const TOOLTIP_LOCATION: Placement = Placement::Bottom;

// We have currently implemented two possible ways to display the output types of ports on hover:
// as a tooltip next to the mouse coursor or as a label that is fixed right next to the port itself.
// Right now, there is no final decision, which one we will keep. Therefore, we have the following
// two constants which can be used to turn those methods on or off.
const SHOW_TYPE_AS_TOOLTIP: bool = false;
const SHOW_TYPE_AS_LABEL: bool = true;



// =====================
// === AllPortsShape ===
// =====================

/// Generic port shape implementation. The shape is of the width of the whole node and is used as a
/// base shape for all port drawing. In case of a multi-port output, the shape is cropped from both
/// sides for each port separately. The shape looks roughly like this:

/// ```text
///  ╭╮                            ╭╮
///  │╰────────────────────────────╯│ ▲ height
///  ╰──────────────────────────────╯ ▼ (node_size / 2) + PORT_SIZE
///  ◄──────────────────────────────►
///   width = node_width + PORT_SIZE
/// ```
///
/// The corners are rounded with the `radius = inner_radius + port_area_size`. The shape also
/// contains an underlying hover area with a padding defined as `HOVER_AREA_PADDING`.
struct AllPortsShape {
    /// The radius of the node, not the outer port radius.
    inner_radius: Var<Pixels>,
    /// The width of the node, not the outer port area width.
    inner_width:  Var<Pixels>,
    shape:        AnyShape,
    hover:        AnyShape,
}

impl AllPortsShape {
    #[profile(Debug)]
    fn new(
        canvas_width: &Var<Pixels>,
        canvas_height: &Var<Pixels>,
        size_multiplier: &Var<f32>,
    ) -> Self {
        // === Generic Info ===

        let inner_width = canvas_width - HOVER_AREA_PADDING.px() * 2.0;
        let inner_height = canvas_height - HOVER_AREA_PADDING.px() * 2.0;
        let inner_radius = node::RADIUS.px();
        let top_mask = BottomHalfPlane();


        // === Main Shape ===

        let shrink = 1.px() - 1.px() * size_multiplier;
        let port_area_size = PORT_SIZE.px() * size_multiplier;
        let port_area_width = &inner_width + (&port_area_size - &shrink) * 2.0;
        let port_area_height = &inner_height + (&port_area_size - &shrink) * 2.0;
        let outer_radius = &inner_radius + &port_area_size;
        let shape = Rect((&port_area_width, &port_area_height));
        let shape = shape.corners_radius(&outer_radius);
        let shape = shape - &top_mask;
        let corner_radius = &port_area_size / 2.0;
        let corner_offset = &port_area_width / 2.0 - &corner_radius;
        let corner = Circle(&corner_radius);
        let left_corner = corner.translate_x(-&corner_offset);
        let right_corner = corner.translate_x(&corner_offset);
        let shape = (shape + left_corner + right_corner).into();


        // === Hover Area ===

        let hover_radius = &inner_radius + &HOVER_AREA_PADDING.px();
        let hover = Rect((canvas_width, canvas_height)).corners_radius(&hover_radius);
        let hover = (hover - &top_mask).into();

        AllPortsShape { inner_radius, inner_width, shape, hover }
    }
}

/// The length of the port shape's inner border. (the part that touches the node)
///
/// # Arguments
///
/// * `corner_radius` - The inner radius of the port shape's corner segments. (the round parts)
/// * `width`         - The inner width of the port shape. (also the width of the node)
fn shape_border_length<T>(corner_radius: T, width: T) -> T
where T: Clone + Mul<f32, Output = T> + Sub<Output = T> + Add<Output = T> {
    let corner_segment_length = corner_segment_length(corner_radius.clone());
    let center_segment_length = center_segment_length(corner_radius, width);
    center_segment_length + corner_segment_length * 2.0
}

/// The length of the border on the inside of a single corner segment. (the round part at the end of
/// the node)
///
/// # Arguments
///
///  * `corner_radius` - The inner radius of the corner segment.
fn corner_segment_length<T>(corner_radius: T) -> T
where T: Mul<f32, Output = T> {
    let corner_circumference = corner_radius * 2.0 * PI;
    corner_circumference * 0.25
}

/// The length of the center segment of a port shape. (the straight part in the center of the shape)
///
/// # Arguments
///
/// * `corner_radius` - The inner radius of the port shape's corner segments. (the round parts)
/// * `width`         - The inner width of the port shape. (also the width of the node)
fn center_segment_length<T>(corner_radius: T, width: T) -> T
where T: Mul<f32, Output = T> + Sub<Output = T> {
    width - corner_radius * 2.0
}



// ======================
// === SinglePortView ===
// ======================

pub use single_port::View as SinglePortView;

/// A single port shape implementation. In contrast to `MultiPortView`, this produces a much faster
/// shader code.
pub mod single_port {
    use super::*;
    use ensogl::display::shape::*;

    ensogl::shape! {
        (style:Style, size_multiplier:f32, opacity:f32, color_rgb:Vector3<f32>) {
            let overall_width  = Var::<Pixels>::from("input_size.x");
            let overall_height = Var::<Pixels>::from("input_size.y");
            let ports          = AllPortsShape::new(&overall_width,&overall_height,&size_multiplier);
            let color          = Var::<color::Rgba>::from("srgba(input_color_rgb,input_opacity)");
            let shape          = ports.shape.fill(color);
            let hover          = ports.hover.fill(INVISIBLE_HOVER_COLOR);
            (shape + hover).into()
        }
    }
}



// =====================
// === MultiPortView ===
// =====================

use ensogl::animation::animation::delayed::DelayedAnimation;
use ensogl::application::Application;
pub use multi_port::View as MultiPortView;
use std::f32::consts::PI;

/// Implements the shape for a segment of the OutputPort with multiple output ports.
pub mod multi_port {
    use super::*;
    use ensogl::display::shape::*;

    /// Compute the angle perpendicular to the shape border.
    fn compute_border_perpendicular_angle(
        shape_border_length: &Var<f32>,
        corner_segment_length: &Var<f32>,
        position: &Var<f32>,
    ) -> Var<f32> {
        // Here we use a trick to use a pseudo-boolean float that is either 0 or 1 to multiply a
        // value that should be returned, iff it's case is true. That way we can add return values
        // of different "branches" of which exactly one will be non-zero.

        // Transform position to be centered in the shape.
        // The `distance`s here describe the distance along the shape border, so not straight line
        // x coordinate, but the length of the path along the shape.
        let center_distance = position - shape_border_length / 2.0;
        let center_distance_absolute = center_distance.abs();
        let center_distance_sign = center_distance.signum();

        let end = shape_border_length / 2.0;
        let center_segment_end = &end - corner_segment_length;
        let default_rotation = Var::<f32>::from(90.0_f32.to_radians());

        // Case 1: The center segment, always zero, so not needed, due to clamping.
        // Case 2: The right circle segment.
        let relative_position =
            (center_distance_absolute - center_segment_end) / corner_segment_length;
        let relative_position = relative_position.clamp(0.0.into(), 1.0.into());
        let corner_base_rotation = (-90.0_f32).to_radians();
        let corner_rotation_delta = relative_position * corner_base_rotation;

        corner_rotation_delta * center_distance_sign + default_rotation
    }

    /// Returns the x position of the crop plane as a fraction of the base shapes center segment.
    /// To get the actual x position of the plane, multiply this value by the length of the center
    /// segment and apply the appropriate x-offset.
    ///
    /// * `shape_border_length`      should be the length of the shapes border path.
    /// * `corner_segment_length`    should be the quarter circumference of the circles on the sides
    ///   of base shape.
    /// * `position_on_path`         should be the position along the shape border (not the pure
    ///   x-coordinate).
    fn calculate_crop_plane_position_relative_to_center_segment(
        shape_border_length: &Var<f32>,
        corner_segment_length: &Var<f32>,
        position_on_path: &Var<f32>,
    ) -> Var<f32> {
        let middle_segment_start_point = corner_segment_length;
        let middle_segment_end_point = shape_border_length - corner_segment_length;
        // Case 1: The left circle, always 0, achieved through clamping.
        // Case 2: The middle segment.
        let middle_segment_plane_position_x = (position_on_path - middle_segment_start_point)
            / (&middle_segment_end_point - middle_segment_start_point);
        // Case 3: The right circle, always 1, achieved through clamping.
        middle_segment_plane_position_x.clamp(0.0.into(), 1.0.into())
    }

    /// Compute the crop plane at the location of the given port index. Also takes into account an
    /// `position_offset` that is given as an offset along the shape boundary.
    ///
    /// The crop plane is a `HalfPlane` that is perpendicular to the border of the shape and can be
    /// used to crop the shape at the specified port index.
    fn compute_crop_plane(
        index: &Var<f32>,
        port_num: &Var<f32>,
        width: &Var<f32>,
        corner_radius: &Var<f32>,
        position_offset: &Var<f32>,
    ) -> AnyShape {
        let corner_segment_length = corner_segment_length(corner_radius.clone());
        let center_segment_length = center_segment_length(corner_radius.clone(), width.clone());
        let shape_border_length = shape_border_length(corner_radius.clone(), width.clone());

        let position_relative = index / port_num;
        let crop_segment_pos = &position_relative * &shape_border_length + position_offset;

        let crop_plane_pos_relative = calculate_crop_plane_position_relative_to_center_segment(
            &shape_border_length,
            &corner_segment_length,
            &crop_segment_pos,
        );
        let crop_plane_pos = crop_plane_pos_relative * &center_segment_length;
        let crop_plane_pos = crop_plane_pos + corner_radius;

        let plane_rotation_angle = compute_border_perpendicular_angle(
            &shape_border_length,
            &corner_segment_length,
            &crop_segment_pos,
        );
        let plane_shape_offset = Var::<Pixels>::from(&crop_plane_pos - width * 0.5);

        let crop_shape = HalfPlane();
        let crop_shape = crop_shape.rotate(plane_rotation_angle);
        let crop_shape = crop_shape.translate_x(plane_shape_offset);

        crop_shape.into()
    }

    ensogl::shape! {
        ( style           : Style
        , size_multiplier : f32
        , index           : f32
        , opacity         : f32
        , port_count      : f32
        , padding_left    : f32
        , padding_right   : f32
        , color_rgb       : Vector3<f32>
        ) {
            let overall_width  = Var::<Pixels>::from("input_size.x");
            let overall_height = Var::<Pixels>::from("input_size.y");
            let ports          = AllPortsShape::new(&overall_width,&overall_height,&size_multiplier);

            let inner_radius = Var::<f32>::from(ports.inner_radius);
            let inner_width  = Var::<f32>::from(ports.inner_width);
            let next_index   = &index + &Var::<f32>::from(1.0);

            let left_shape_crop = compute_crop_plane
                (&index,&port_count,&inner_width,&inner_radius,&0.0.into());
            let right_shape_crop = compute_crop_plane
                (&next_index,&port_count,&inner_width,&inner_radius,&0.0.into());

            let hover_area = ports.hover.difference(&left_shape_crop);
            let hover_area = hover_area.intersection(&right_shape_crop);
            let hover_area = hover_area.fill(INVISIBLE_HOVER_COLOR);

            let padding_left  = Var::<Pixels>::from(padding_left);
            let padding_right = Var::<Pixels>::from(padding_right);

            let left_shape_crop  = left_shape_crop.grow(padding_left);
            let right_shape_crop = right_shape_crop.grow(padding_right);

            let port_area = ports.shape.difference(&left_shape_crop);
            let port_area = port_area.intersection(&right_shape_crop);

            let color     = Var::<color::Rgba>::from("srgba(input_color_rgb,input_opacity)");
            let port_area = port_area.fill(color);

            (port_area + hover_area).into()
        }
    }
}



// ==================
// === Shape View ===
// ==================

/// Abstraction over [`SinglePortView`] and [`MultiPortView`].
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub enum PortShapeView {
    Single(SinglePortView),
    Multi(MultiPortView),
}

macro_rules! fn_helper {
    ($( $name:ident($this:ident,$($arg:ident : $arg_tp:ty),*) {$($body1:tt)*} {$($body2:tt)*} )*)
    => {$(
        #[allow(unused_variables)]
        fn $name(&self, $($arg : $arg_tp),*) {
            match self {
                Self::Single ($this) => $($body1)*,
                Self::Multi  ($this) => $($body2)*,
            }
        }
    )*};
}

macro_rules! fn_both {
    ($( $name:ident $args:tt $body:tt )*) => {
        fn_helper! {$($name $args $body $body)*}
    };
}

macro_rules! fn_multi_only {
    ($( $name:ident $args:tt $body:tt )*) => {
        fn_helper! {$($name $args {{}} $body)*}
    };
}

impl PortShapeView {
    #[profile(Debug)]
    fn new(number_of_ports: usize) -> Self {
        if number_of_ports <= 1 {
            Self::Single(SinglePortView::new())
        } else {
            Self::Multi(MultiPortView::new())
        }
    }

    fn_both! {
        set_size            (this,t:Vector2)     {this.size.set(t)}
        set_size_multiplier (this,t:f32)         {this.size_multiplier.set(t)}
        set_color           (this,t:color::Rgba) {this.color_rgb.set(t.opaque.into())}
        set_opacity         (this,t:f32)         {this.opacity.set(t)}
    }

    fn_multi_only! {
        set_index         (this,t:usize) { this.index.set(t as f32) }
        set_port_count    (this,t:usize) { this.port_count.set(t as f32) }
        set_padding_left  (this,t:f32)   { this.padding_left.set(t) }
        set_padding_right (this,t:f32)   { this.padding_right.set(t) }
    }

    fn events(&self) -> &component::PointerTarget {
        match self {
            Self::Single(t) => &t.events,
            Self::Multi(t) => &t.events,
        }
    }
}

impl display::Object for PortShapeView {
    fn display_object(&self) -> &display::object::Instance {
        match self {
            Self::Single(view) => view.display_object(),
            Self::Multi(view) => view.display_object(),
        }
    }
}



// =================
// === Port Frp  ===
// =================

ensogl::define_endpoints! {
    Input {
        set_size_multiplier       (f32),
        set_definition_type       (Option<Type>),
        set_usage_type            (Option<Type>),
        set_type_label_visibility (bool),
        set_size                  (Vector2),
        set_view_mode             (view::Mode),
    }

    Output {
        tp       (Option<Type>),
        on_hover (bool),
        on_press (),
        tooltip  (tooltip::Style),
        size     (Vector2),
    }
}

#[derive(Clone, Debug, Default)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct Model {
    pub frp:            Option<Frp>,
    pub shape:          Option<PortShapeView>,
    pub type_label:     Option<text::Text>,
    pub display_object: Option<display::object::Instance>,
    pub index:          ByteDiff,
    pub length:         ByteDiff,
    port_count:         usize,
    port_index:         usize,
}

impl Model {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn init_shape(
        &mut self,
        app: &Application,
        styles: &StyleWatch,
        styles_frp: &StyleWatchFrp,
        port_index: usize,
        port_count: usize,
    ) -> (display::object::Instance, Frp) {
        let shape = PortShapeView::new(port_count);

        let is_first = port_index == 0;
        let is_last = port_index == port_count.saturating_sub(1);
        let padding_left = if is_first { -INFINITE } else { SEGMENT_GAP_WIDTH / 2.0 };
        let padding_right = if is_last { INFINITE } else { -SEGMENT_GAP_WIDTH / 2.0 };
        shape.set_index(port_index);
        shape.set_port_count(port_count);
        shape.set_padding_left(padding_left);
        shape.set_padding_right(padding_right);
        self.shape = Some(shape.clone());

        let type_label = app.new_view::<text::Text>();
        let offset_y =
            styles.get_number(ensogl_hardcoded_theme::graph_editor::node::type_label::offset_y);
        type_label.set_y(offset_y);
        self.type_label = Some(type_label.clone());

        let display_object = display::object::Instance::new();
        display_object.add_child(&shape);
        display_object.add_child(&type_label);
        self.display_object = Some(display_object.clone());

        self.port_count = max(port_count, 1);
        self.port_index = port_index;

        self.init_frp(&shape, &type_label, styles, styles_frp);
        (display_object, self.frp.as_ref().unwrap().clone_ref())
    }

    fn init_frp(
        &mut self,
        shape: &PortShapeView,
        type_label: &text::Text,
        styles: &StyleWatch,
        styles_frp: &StyleWatchFrp,
    ) {
        let frp = Frp::new();
        let network = &frp.network;
        let events = shape.events();
        let opacity = Animation::<f32>::new(network);
        let color = color::Animation::new(network);
        let type_label_opacity = Animation::<f32>::new(network);
        let port_count = self.port_count;
        let port_index = self.port_index;
        let full_type_timer = DelayedAnimation::new(network);
        full_type_timer.set_delay(FULL_TYPE_ONSET_DELAY_MS);
        full_type_timer.set_duration(0.0);

        frp::extend! { network

            // === Mouse Event Handling ===

            frp.source.on_hover <+ bool(&events.mouse_out,&events.mouse_over);
            frp.source.on_press <+ events.mouse_down_primary;


            // === Opacity ===

            opacity.target <+ events.mouse_over.constant(PORT_OPACITY_HOVERED);
            opacity.target <+ events.mouse_out.constant(PORT_OPACITY_NOT_HOVERED);
            eval opacity.value ((t) shape.set_opacity(*t));


            // === Size ===

            frp.source.size <+ frp.set_size;
            eval frp.size ((&s)
                shape.set_size(s + Vector2(HOVER_AREA_PADDING,HOVER_AREA_PADDING) * 2.0));
            set_type_label_x <- all_with(&frp.size,&type_label.width,
                f!([port_count,port_index](port_size,type_label_width) {
                    let shape_length   = shape_border_length(node::RADIUS, port_size.x);
                    let shape_left     = - shape_length / 2.0;
                    let port_width     = shape_length / port_count as f32;
                    let port_left      = shape_left + port_width * port_index as f32;
                    let port_center_x  = port_left + port_width / 2.0;
                    let label_center_x = port_center_x;
                    label_center_x - type_label_width / 2.0
                }));
            eval set_type_label_x ((&t) type_label.set_x(t));
            eval frp.set_size_multiplier ((t) shape.set_size_multiplier(*t));


            // === Type ===

            frp.source.tp <+ all_with(&frp.set_usage_type,&frp.set_definition_type,
                |usage_tp,def_tp| usage_tp.clone().or_else(|| def_tp.clone())
            );

            normal_color        <- frp.tp.map(f!([styles](t)
                type_coloring::compute_for_selection(t.as_ref(),&styles)));
            init_color          <- source::<()>();
            let profiling_color  = styles_frp.get_color(ensogl_hardcoded_theme::code::types::any::selection);
            profiling_color     <- all_with(&profiling_color,&init_color,|c,_|color::Lcha::from(c));
            in_profiling_mode   <- frp.set_view_mode.map(|mode| mode.is_profiling());
            color_tgt           <- in_profiling_mode.switch(&normal_color,&profiling_color);
            color.target        <+ color_tgt;
            eval color.value ((t) shape.set_color(t.into()));

            full_type_timer.start <+ frp.on_hover.on_true();
            full_type_timer.reset <+ type_label_opacity.value.filter(|&o| o == 0.0).constant(());
            showing_full_type     <- bool(&full_type_timer.on_reset,&full_type_timer.on_end);
            type_description      <- all_with(&frp.tp,&showing_full_type,|tp,&show_full_tp| {
                tp.map_ref(|tp| {
                    if show_full_tp { tp.to_im_string() } else { tp.abbreviate().to_im_string() }
                })
            });
        }
        init_color.emit(());

        if SHOW_TYPE_AS_LABEL {
            frp::extend! { network

                // === Type Label ===

                type_label_visibility <- frp.on_hover.and(&frp.set_type_label_visibility);
                on_type_label_visible <- type_label_visibility.on_true();
                type_label_opacity.target <+ on_type_label_visible.constant(PORT_OPACITY_HOVERED);
                type_label_opacity.target <+ type_label_visibility.on_false().constant(0.0);

                type_label_color <- all_with(&color.value,&type_label_opacity.value,
                    |color,&opacity| color.opaque.with_alpha(opacity));
                type_label.set_property <+ type_label_color.ref_into_some().map(|t| ((..).into(),*t));
                type_label.set_property_default <+ type_label_color.ref_into_some();
                type_label.set_content <+ type_description.map(|s| s.clone().unwrap_or_default());
            }
        }

        if SHOW_TYPE_AS_TOOLTIP {
            frp::extend! { network

                // === Tooltip ===

                frp.source.tooltip <+ all_with(&type_description,&frp.on_hover,|text,&hovering| {
                    if hovering {
                        if let Some(text) = text.clone() {
                            tooltip::Style::set_label(text.into()).with_placement(TOOLTIP_LOCATION)
                        } else {
                            tooltip::Style::unset_label()
                        }
                    } else {
                            tooltip::Style::unset_label()
                    }
                });
            }
        }

        opacity.target.emit(PORT_OPACITY_NOT_HOVERED);
        color.target.emit(type_coloring::compute_for_code(None, styles));

        self.frp = Some(frp);
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn set_size(&self, size: Vector2) {
        if let Some(frp) = &self.frp {
            frp.set_size(size);
        }
    }
}
