//! Implements the segmented output port area.
use crate::prelude::*;

use ensogl::display::traits::*;

use enso_frp as frp;
use enso_frp;
use ensogl::data::color;
use ensogl::display::Attribute;
use ensogl::display::Buffer;
use ensogl::display::Sprite;
use ensogl::display::scene::Scene;
use ensogl::display::shape::AnyShape;
use ensogl::display::shape::BottomHalfPlane;
use ensogl::display::shape::Circle;
use ensogl::display::shape::Distance;
use ensogl::display::shape::PixelDistance;
use ensogl::display::shape::Pixels;
use ensogl::display::shape::Rect;
use ensogl::display::shape::Var;
use ensogl::display::shape::primitive::def::class::ShapeOps;
use ensogl::display;
use ensogl::gui::component::Animation;
use ensogl::gui::component::Tween;
use ensogl::gui::component;
use ensogl::math::algebra::Clamp;
use ensogl::math::algebra::Signum;
use std::num::NonZeroU32;

use crate::node;



// =================
// === Constants ===
// =================
// TODO: These values should be in some IDE configuration.

const BASE_SIZE             : f32 = 0.5;
const HIGHLIGHT_SIZE        : f32 = 1.0;
const SEGMENT_GAP_WIDTH     : f32 = 2.0;

const SHOW_DELAY_DURATION   : f32 = 150.0;
const HIDE_DELAY_DURATION   : f32 = 150.0;

const SHAPE_HOVER_AREA_SIZE : f32 = 20.0;
const SHAPE_MAX_WIDTH       : f32 = 4.0;



// ==================
// === Base Shape ===
// ==================

/// The base port shape for the output port shape. This shape is later on used to create a
/// simple single output port shape and a more complex multi-output port shape.
///
/// The base shape looks roughly like this:
/// ```text
///      .                            .
///      .                            .
///       .                          .
///          .                    .
///              """""""""""""
///     |  r    |              |   r   |
///     |            width             |
/// ```
/// where r is the radius of the left and right quarter circle shapes.
///
/// The struct that contains the port base shapes, and some information about the shapes that is
/// required for the computation of segments.
struct BaseShapeData {
    port_area  : AnyShape,
    hover_area : AnyShape,
    radius     : Var<Distance<Pixels>>,
    width      : Var<Distance<Pixels>>,
}

impl BaseShapeData {
    fn new
    (width:&Var<Distance<Pixels>>, height:&Var<Distance<Pixels>>, grow:&Var<f32>) -> Self {
        let width  = width  - node::NODE_SHAPE_PADDING.px() * 2.0;
        let height = height - node::NODE_SHAPE_PADDING.px() * 2.0;

        let hover_area_width  = &width  + &SHAPE_HOVER_AREA_SIZE.px() * 2.0;
        let hover_area_height = &height / 2.0 + &SHAPE_HOVER_AREA_SIZE.px();
        let hover_area        = Rect((&hover_area_width,&hover_area_height));
        let hover_area        = hover_area.translate_y(-hover_area_height/2.0);

        let shrink           = 1.px() - 1.px() * grow;
        let radius           = node::NODE_SHAPE_RADIUS.px();
        let port_area_size   = SHAPE_MAX_WIDTH.px() * grow;
        let port_area_width  = &width  + (&port_area_size - &shrink) * 2.0;
        let port_area_height = &height + (&port_area_size - &shrink) * 2.0;
        let bottom_radius    = &radius + &port_area_size;
        let port_area        = Rect((&port_area_width,&port_area_height));
        let port_area        = port_area.corners_radius(&bottom_radius);
        let port_area        = port_area - BottomHalfPlane();
        let corner_radius    = &port_area_size / 2.0;
        let corner_offset    = &port_area_width / 2.0 - &corner_radius;
        let corner           = Circle(&corner_radius);
        let left_corner      = corner.translate_x(-&corner_offset);
        let right_corner     = corner.translate_x(&corner_offset);
        let port_area        = port_area + left_corner + right_corner;
        let port_area        = port_area.into();
        let hover_area       = hover_area.into();
        BaseShapeData{port_area,hover_area,radius,width}
    }
}

/// Trait that allows us to abstract the API of the `multi_port_area::Shape` and the
/// `single_port_area::Shape`. This is needed to avoid code duplication for functionality that can
/// work with either shape.
#[allow(missing_docs)]
trait PortShape {
    fn set_grow(&self, grow_value:f32);
    fn set_opacity(&self, opacity:f32);
}



// ========================
// === Multi Port Shape ===
// ========================

/// Implements the shape for a segment of the OutputPort with multiple output ports.
pub mod multi_port_area {
    use super::*;
    use ensogl::display::shape::*;
    use std::f32::consts::PI;

    /// Compute the angle perpendicular to the shape border.
    fn compute_border_perpendicular_angle
    (full_shape_border_length:&Var<f32>, corner_segment_length:&Var<f32>, position:&Var<f32>)
    -> Var<f32> {
        // TODO implement proper abstraction for non-branching "if/then/else" or "case" in
        // shaderland
        // Here we use a trick to use a pseudo-boolean float that is either 0 or 1 to multiply a
        // value that should be returned, iff it's case is true. That way we can add return values
        // of different "branches" of which exactly one will be non-zero.

        // Transform position to be centered in the shape.
        // The `distance`s here describe the distance along the shape border, so not straight line
        // x coordinate, but the length of the path along the shape.
        let center_distance          = position - full_shape_border_length  / 2.0;
        let center_distance_absolute = center_distance.abs();
        let center_distance_sign     = center_distance.signum();

        let end                = full_shape_border_length / 2.0;
        let center_segment_end = &end - corner_segment_length;
        let default_rotation   = Var::<f32>::from(90.0_f32.to_radians());

        // Case 1: The center segment, always zero, so not needed, due to clamping.
        // Case 2: The right circle segment.
        let relative_position     = (center_distance_absolute - center_segment_end)
                                    / corner_segment_length;
        let relative_position     = relative_position.clamp(0.0.into(),1.0.into());
        let corner_base_rotation  = (-90.0_f32).to_radians();
        let corner_rotation_delta = relative_position * corner_base_rotation;

        corner_rotation_delta * center_distance_sign + default_rotation
    }

    /// Returns the x position of the crop plane as a fraction of the base shapes center segment.
    /// To get the actual x position of the plane, multiply this value by the length of the center
    /// segment and apply the appropriate x-offset.
    ///
    /// * `full_shape_border_length` should be the length of the shapes border path.
    /// * `corner_segment_length`    should be the quarter circumference of the circles on the
    ///                              sides of base shape.
    /// * `position_on_path`         should be the position along the shape border
    ///                              (not the pure x-coordinate).
    fn calculate_crop_plane_position_relative_to_center_segment
    (full_shape_border_length:&Var<f32>, corner_segment_length:&Var<f32>, position_on_path:&Var<f32>)
    -> Var<f32> {
        let middle_segment_start_point = corner_segment_length;
        let middle_segment_end_point   = full_shape_border_length - corner_segment_length;

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
    fn compute_crop_plane
    (index:&Var<f32>, port_num: &Var<f32>, width: &Var<f32>, corner_radius:&Var<f32>,
     position_offset:&Var<f32>) -> AnyShape {
        let corner_circumference     = corner_radius * 2.0 * PI;
        let corner_segment_length    = &corner_circumference * 0.25;
        let center_segment_length    = width - corner_radius * 2.0;
        let full_shape_border_length = &center_segment_length + &corner_segment_length * 2.0;

        let position_relative = index / port_num;
        let crop_segment_pos  = &position_relative * &full_shape_border_length + position_offset;

        let crop_plane_pos_relative = calculate_crop_plane_position_relative_to_center_segment
            (&full_shape_border_length,&corner_segment_length,&crop_segment_pos);
        let crop_plane_pos          = crop_plane_pos_relative * &center_segment_length;
        let crop_plane_pos          = crop_plane_pos + corner_radius;

        let plane_rotation_angle = compute_border_perpendicular_angle
            (&full_shape_border_length,&corner_segment_length,&crop_segment_pos);
        let plane_shape_offset  = Var::<Distance<Pixels>>::from(&crop_plane_pos - width * 0.5);

        let crop_shape = HalfPlane();
        let crop_shape = crop_shape.rotate(plane_rotation_angle);
        let crop_shape = crop_shape.translate_x(plane_shape_offset);

        crop_shape.into()
    }

    ensogl::define_shape_system! {
        (style:Style, grow:f32, index:f32, port_num:f32, opacity:f32, padding_left:f32,
        padding_right:f32) {
            let overall_width  : Var<Distance<Pixels>> = "input_size.x".into();
            let overall_height : Var<Distance<Pixels>> = "input_size.y".into();

            let base_shape_data = BaseShapeData::new(&overall_width,&overall_height,&grow);
            let BaseShapeData{ port_area,hover_area,radius,width } = base_shape_data;

            let radius:Var::<f32> = radius.into();
            let width:Var::<f32>  = width.into();

            let left_shape_crop  = compute_crop_plane
                (&index,&port_num,&width,&radius,&0.0.into());
            let right_shape_crop = compute_crop_plane
                (&(Var::<f32>::from(1.0) + &index),&port_num,&width,&radius,&0.0.into());

            let hover_area = hover_area.difference(&left_shape_crop);
            let hover_area = hover_area.intersection(&right_shape_crop);
            let hover_area = hover_area.fill(color::Rgba::new(0.0,0.0,0.0,0.000_001));

            let padding_left  = Var::<Distance<Pixels>>::from(padding_left);
            let padding_right = Var::<Distance<Pixels>>::from(padding_right);

            let left_shape_crop  = left_shape_crop.grow(padding_left);
            let right_shape_crop = right_shape_crop.grow(padding_right);

            let port_area = port_area.difference(&left_shape_crop);
            let port_area = port_area.intersection(&right_shape_crop);

            // FIXME: Use color from style and apply transparency there.
            let color     = Var::<color::Rgba>::from("srgba(0.25,0.58,0.91,input_opacity)");
            let port_area = port_area.fill(color);

            (port_area + hover_area).into()
        }
    }

    impl PortShape for Shape {
        fn set_grow(&self, grow_value:f32) {
            self.grow.set(grow_value)
        }

        fn set_opacity(&self, opacity:f32) {
            self.opacity.set(opacity)
        }
    }
}



// =========================
// === Single Port Shape ===
// =========================

/// Implements a simplified version of the multi_port_area::Shape shape for the case where there is
/// only a single output port.
pub mod single_port_area {
    use super::*;
    use ensogl::display::shape::*;

    ensogl::define_shape_system! {
        (style:Style, grow:f32, opacity:f32) {
            let overall_width  : Var<Distance<Pixels>> = "input_size.x".into();
            let overall_height : Var<Distance<Pixels>> = "input_size.y".into();

            let base_shape_data = BaseShapeData::new(&overall_width,&overall_height,&grow);
            let BaseShapeData{ port_area,hover_area, .. } = base_shape_data;

            // FIXME: Use color from style and apply transparency there.
            let color     = Var::<color::Rgba>::from("srgba(0.25,0.58,0.91,input_opacity)");
            let port_area = port_area.fill(color);

            let hover_area = hover_area.fill(color::Rgba::new(0.0,0.0,0.0,0.000_001));

            (port_area + hover_area).into()
        }
    }

    impl PortShape for Shape {
        fn set_grow(&self, grow_value:f32) {
            self.grow.set(grow_value)
        }

        fn set_opacity(&self, opacity:f32) {
            self.opacity.set(opacity)
        }
    }
}



// ==================
// === Shape View ===
// ==================

/// Wrapper that handles the distinction between a single ShapeView<single_port_area::Shape>
/// and collection of component::ShapeView<multi_port_area::Shape>.
#[derive(Clone,Debug)]
enum ShapeView {
    Single { view  : component::ShapeView<single_port_area::Shape> },
    Multi  {
        display_object: display::object::Instance,
        views : Vec<component::ShapeView<multi_port_area::Shape>>,
    },
}

impl ShapeView {
    /// Constructor.
    fn new(number_of_ports:NonZeroU32, logger:&Logger, scene:&Scene) -> Self {
        let number_of_ports = number_of_ports.get();
        if number_of_ports == 1 {
            ShapeView::Single { view: component::ShapeView::new(&logger,&scene) }
        } else {
            let display_object  = display::object::Instance::new(logger);
            let mut views       = Vec::default();
            let number_of_ports = number_of_ports as usize;
            views.resize_with(number_of_ports,|| component::ShapeView::new(&logger,&scene));
            views.iter().for_each(|view| view.display_object().set_parent(&display_object));
            ShapeView::Multi {display_object,views}
        }
    }

    /// Set up the frp for all ports.
    fn init_frp(&self, network:&frp::Network, port_frp:PortFrp) {
        match self {
            ShapeView::Single {view}      => init_port_frp(&view,PortId::new(0),port_frp,network),
            ShapeView::Multi  {views, ..} => {
                views.iter().enumerate().for_each(|(index,view)| {
                    init_port_frp(&view,PortId::new(index),port_frp.clone_ref(),network)
                })
            }
        }
    }

    /// Resize all the port output shapes to fit the new layout requirements for thr given
    /// parameters.
    fn update_shape_layout_based_on_size_and_gap(&self, size:Vector2<f32>, gap_width:f32) {
        match self {
            ShapeView::Single { view } => {
                let shape = &view.shape;
                shape.sprite.size.set(size);
            }
            ShapeView::Multi { views, .. } => {
                let port_num  = views.len() as f32;
                for (index,view) in views.iter().enumerate(){
                    let shape = &view.shape;
                    shape.sprite.size.set(size);
                    shape.index.set(index as f32);
                    shape.port_num.set(port_num);
                    shape.padding_left.set(gap_width * 0.5);
                    shape.padding_right.set(-gap_width * 0.5);
                }
                views[0]              .shape.padding_left .set(0.0);
                views[views.len() - 1].shape.padding_right.set(0.0);
            }
        }
    }
}

impl display::Object for ShapeView {
    fn display_object(&self) -> &display::object::Instance {
        match self {
            ShapeView::Single { view }               => view.display_object(),
            ShapeView::Multi  { display_object, .. } => display_object,
        }
    }
}



// ===============
// === PortId  ===
// ===============

/// Id of a specific port inside of `OutPutPortsData`.
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct PortId {
    index: usize,
}

impl PortId {
    fn new(index:usize) -> Self {
        Self{index}
    }
}



// =================
// === Port Frp  ===
// =================

/// Struct that contains all required FRP endpoints to set up the FRP of a port shape view.
#[derive(Clone,CloneRef,Debug)]
struct PortFrp {
    mouse_over : frp::Source<PortId>,
    mouse_out  : frp::Source<PortId>,
    mouse_down : frp::Source<PortId>,

    hide       : frp::Stream<()>,
    /// Activate the port and if it has the given PortId, show it highlighted.
    activate_and_highlight_selected : frp::Stream<PortId>
}

/// Set up the FRP system for a ShapeView of a shape that implements the PortShapeApi.
///
/// This allows us to use the same setup code for bot the `multi_port_area::Shape` and the
/// `single_port_area::Shape`.
fn init_port_frp<Shape: PortShape + CloneRef + 'static>
(view:&component::ShapeView<Shape>, port_id:PortId, frp:PortFrp, network:&frp::Network) {
    let PortFrp { mouse_over,mouse_out,mouse_down,
        hide,activate_and_highlight_selected} = frp;

    let shape        = &view.shape;
    let port_size    = Animation::<f32>::new(&network);
    let port_opacity = Animation::<f32>::new(&network);

    frp::extend! { network

        // === Mouse Event Handling == ///

        eval_ view.events.mouse_over(mouse_over.emit(port_id));
        eval_ view.events.mouse_out(mouse_out.emit(port_id));
        eval_ view.events.mouse_down(mouse_down.emit(port_id));


        // === Animation Handling == ///

        eval port_size.value    ((size) shape.set_grow(*size));
        eval port_opacity.value ((size) shape.set_opacity(*size));


        // === Visibility and Highlight Handling == ///

         eval_ hide ([port_size,port_opacity]{
             port_size.set_target_value(0.0);
             port_opacity.set_target_value(0.0);
         });

        // Through the provided ID we can infer whether this port should be highlighted.
        is_selected      <- activate_and_highlight_selected.map(move |id| *id == port_id);
        show_normal      <- activate_and_highlight_selected.gate_not(&is_selected);
        show_highlighted <- activate_and_highlight_selected.gate(&is_selected);

        eval_ show_highlighted ([port_opacity,port_size]{
            port_opacity.set_target_value(1.0);
            port_size.set_target_value(HIGHLIGHT_SIZE);
        });

        eval_ show_normal ([port_opacity,port_size] {
            port_opacity.set_target_value(0.5);
            port_size.set_target_value(BASE_SIZE);
        });
    }
}



// ===========
// === Frp ===
// ===========

/// Frp API of the `OutPutPorts`.
#[derive(Clone,CloneRef,Debug)]
pub struct Frp {
    /// Update the size of the `OutPutPorts`. Should match the size of the parent node for visual
    /// correctness.
    pub set_size        : frp::Source<Vector2<f32>>,
    /// Emitted whenever one of the ports receives a `MouseDown` event. The `PortId` indicates the
    /// source port.
    pub port_mouse_down : frp::Stream<PortId>,
    /// Emitted whenever one of the ports receives a `MouseOver` event. The `PortId` indicates the
    /// source port.
    pub port_mouse_over : frp::Stream<PortId>,
    /// Emitted whenever one of the ports receives a `MouseOut` event. The `PortId` indicates the
    /// source port.
    pub port_mouse_out  : frp::Stream<PortId>,

    on_port_mouse_down  : frp::Source<PortId>,
    on_port_mouse_over  : frp::Source<PortId>,
    on_port_mouse_out   : frp::Source<PortId>,
}

impl Frp {
    fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            def set_size           = source();
            def on_port_mouse_down = source();
            def on_port_mouse_over = source();
            def on_port_mouse_out  = source();

            let port_mouse_down = on_port_mouse_down.clone_ref().into();
            let port_mouse_over = on_port_mouse_over.clone_ref().into();
            let port_mouse_out  = on_port_mouse_out.clone_ref().into();
        }
        Self{set_size,on_port_mouse_down,on_port_mouse_over,on_port_mouse_out,
             port_mouse_down,port_mouse_over,port_mouse_out}
    }
}



// =======================
// === OutputPortsData ===
// =======================

/// Internal data of the `OutputPorts`.
#[derive(Debug)]
pub struct OutputPortsData {
    display_object : display::object::Instance,
    logger         : Logger,
    size           : Cell<Vector2<f32>>,
    gap_width      : Cell<f32>,
    ports          : RefCell<ShapeView>,
}

impl OutputPortsData {

    fn new(scene:&Scene, number_of_ports:NonZeroU32) -> Self {
        let logger         = Logger::new("OutputPorts");
        let display_object = display::object::Instance::new(&logger);
        let size           = Cell::new(Vector2::zero());
        let gap_width      = Cell::new(SEGMENT_GAP_WIDTH);
        let ports          = ShapeView::new(number_of_ports,&logger,scene);
        let ports          = RefCell::new(ports);

        OutputPortsData {display_object,logger,size,ports,gap_width}.init()
    }

    fn init(self) -> Self {
        self.ports.borrow().display_object().set_parent(&self.display_object);
        self.update_shape_layout_based_on_size_and_gap();
        self
    }

    fn update_shape_layout_based_on_size_and_gap(&self) {
        let size      = self.size.get();
        let gap_width = self.gap_width.get();
        self.ports.borrow().update_shape_layout_based_on_size_and_gap(size,gap_width);
    }

    fn set_size(&self, size:Vector2<f32>) {
        self.size.set(size);
        self.update_shape_layout_based_on_size_and_gap();
    }
}



// ===================
// === OutputPorts ===
// ===================

/// Implements the segmented output port area. Provides shapes that can be attached to a `Node` to
/// add an interactive area with output ports.
///
/// The `OutputPorts` facilitate the falling behaviour:
///  * when one of the output ports is hovered, after a set time, all ports are show and the hovered
///    port is highlighted.
///  * when a different port is hovered, it is highlighted immediately.
///  * when none of the ports is hovered all of the `OutputPorts` disappear. Note: there is a very
///    small delay for disappearing to allow for smooth switching between ports.
///
#[derive(Debug,Clone,CloneRef)]
pub struct OutputPorts {
    /// The FRP api of the `OutPutPorts`.
    pub frp     : Frp,
        network : frp::Network,
        data    : Rc<OutputPortsData>,
}

impl OutputPorts {
    /// Constructor.
    pub fn new(scene:&Scene, number_of_ports:NonZeroU32) -> Self {
        let network = default();
        let frp     = Frp::new(&network);
        let data    = OutputPortsData::new(scene,number_of_ports);
        let data    = Rc::new(data);
        OutputPorts {data,network,frp}.init()
    }

    fn init(self) -> Self {
        let network = &self.network;
        let frp     = &self.frp;
        let data    = &self.data;

        // Used to set and detect the end of the tweens. The actual value is irrelevant, only the
        // duration of the tween matters and that this value is reached after that time.
        const TWEEN_END_VALUE:f32 = 1.0;

        // Timer used to measure whether the hover has been long enough to show the ports.
        let delay_show = Tween::new(&network);
        delay_show.set_duration(SHOW_DELAY_DURATION);

        // Timer used to measure whether the mouse has been gone long enough to hide all ports.
        let delay_hide = Tween::new(&network);
        delay_hide.set_duration(HIDE_DELAY_DURATION);

        let mouse_down = frp.on_port_mouse_down.clone_ref();
        let mouse_over = frp.on_port_mouse_over.clone_ref();
        let mouse_out  = frp.on_port_mouse_out.clone_ref();

        frp::extend! { network

            // === Size Change Handling == ///

            eval frp.set_size ((size) data.set_size(*size));


            // === Hover Event Handling == ///

            delay_show_finished    <- delay_show.value.map(|t| *t>=TWEEN_END_VALUE );
            delay_hide_finished    <- delay_hide.value.map(|t| *t>=TWEEN_END_VALUE );
            on_delay_show_finished <- delay_show_finished.gate(&delay_show_finished).constant(());
            on_delay_hide_finished <- delay_hide_finished.gate(&delay_hide_finished).constant(());

            visible                <- delay_show_finished.map(|v| *v);

            mouse_over_while_inactive  <- mouse_over.gate_not(&visible).constant(());
            mouse_over_while_active    <- mouse_over.gate(&visible).constant(());

            eval_ mouse_over_while_inactive ([delay_show,delay_hide]{
                delay_hide.stop();
                delay_show.reset();
                delay_show.set_target_value(TWEEN_END_VALUE);
            });
            eval_ mouse_out ([delay_hide,delay_show]{
                delay_show.stop();
                delay_hide.reset();
                delay_hide.set_target_value(TWEEN_END_VALUE);
            });

            activate_ports <- any(mouse_over_while_active,on_delay_show_finished);
            eval_ activate_ports (delay_hide.stop_and_rewind());

            activate_and_highlight_selected <- mouse_over.sample(&activate_ports);

            hide_all <- on_delay_hide_finished.map(f_!(delay_show.stop_and_rewind()));

        }

        let port_frp = PortFrp {mouse_down,mouse_over,mouse_out,
            hide: hide_all,
            activate_and_highlight_selected
        };

        data.ports.borrow().init_frp(&network,port_frp);

        // // FIXME this is a hack to ensure the ports are invisible at startup.
        // // Right now we get some of FRP mouse events on startup that leave the
        // // ports visible by default.
        // // Once that is fixed, remove this line.
        delay_hide.from_now_to(TWEEN_END_VALUE);

        self
    }

    // TODO: Implement proper sorting and remove.
    /// Hack function used to register the elements for the sorting purposes. To be removed.
    pub(crate) fn order_hack(scene:&Scene) {
        let logger = Logger::new("output shape order hack");
        component::ShapeView::<multi_port_area::Shape>::new(&logger,scene);
        component::ShapeView::<single_port_area::Shape>::new(&logger,scene);
    }
}

impl display::Object for OutputPorts {
    fn display_object(&self) -> &display::object::Instance {
        &self.data.display_object
    }
}
