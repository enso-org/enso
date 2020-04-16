//! This module defines the shapes and logic required for drawing node ports.
use crate::prelude::*;

use crate::component::node::Node;

use core::f32::consts::PI;
use ensogl::data::color::*;
use ensogl::display::Attribute;
use ensogl::display::Buffer;
use ensogl::display::Scene;
use ensogl::display::object::ObjectOps;
use ensogl::display::scene::ShapeRegistry;
use ensogl::display::shape::*;
use ensogl::display::shape::primitive::def::class::ShapeOps;
use ensogl::display::symbol::geometry::Sprite;
use ensogl::display;
use ensogl::gui::component::ShapeViewDefinition;
use ensogl::gui::component;
use ensogl::math::geometry::circle::segment::Segment;
use ensogl::math::geometry::triangle::Triangle;
use ensogl::math::topology::unit::Angle;
use ensogl::math::topology::unit::AngleOps;
use ensogl::math::topology::unit::Degrees;
use ensogl::math::topology::unit::Distance;
use ensogl::math::topology::unit::Pixels;



// ===========================
// === Port Specification ===
// ===========================

/// Indicates whether a port is inwards facing  or outwards facing.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug)]
pub enum Direction{
    In,
    Out,
}

/// Defines the properties of a port shape and can then
/// be used to build the port shape.
///
/// Ports are constructed around an inner circle, and thus
/// most measurements are in degrees, which are measured around
/// a inner circle that is defined by the `inner_radius`.
#[derive(Clone,Copy,Debug)]
pub struct Specification {
    /// Height of the port.
    pub height      : f32,
    /// Width of the port in degrees.
    pub width       : Angle<Degrees>,
    /// Radius of the inner circle that the port is constructed around.
    pub inner_radius: f32,
    /// Location of the port along the inner circle.
    pub position: Angle<Degrees>,
    /// Fill colour of the port.
    /// TODO unused in the shape at the moment
    pub color       : Srgb<f32>,
    /// Indicates whether this port is facing inwards or outwards.
    pub direction   : Direction,
}

impl Default for Specification {
    fn default() -> Self {
        Specification {
            height       : 20.0,
            width        : Angle::from(15.0),
            inner_radius : 70.0,
            position     : Angle::from(0.0),
            color        : Srgb::new(0.26, 0.69, 0.99),
            direction    : Direction::Out,
        }
    }
}



// ====================
// === Port Shapes ===
// ====================


mod shape {
    use super::*;

    /// Construct an inwards facing port.
    ///
    /// The port consists of an angle embedded between two rings. The corner of the angle can
    /// either face towards the center of thr ring of away from it. If it faces towards the
    /// center, the side facing the outer ring, have a bulging shape on that side. If the angle
    /// faces away from the center, the angle gets limited by the inner ring and shows a convex
    /// "hole" on that side.
    ///
    /// Illustrations (not to scale)
    /// ------------------------------
    /// ```text
    ///  Inwards facing port                           | Outwards facing port
    ///                                               |
    ///                                               |
    ///       *-------------------- inner radius      |          *  ---------------- outer radius
    ///     *    *                |                   |        *   *               |
    ///   *        *              |  height           |      *       *             |
    ///  *          *             |                   |     *    *    * ------------ inner radius
    ///   *        *              |                   |    * *       * *     |
    ///      *  *------------------ outer radius      |    *            *    -this is some extra
    ///                                                                        space that depends
    ///  \----------|                                      \------------|       on the radius
    ///     width                                              width
    /// ```
    /// The shape description sets up both shapes and then applies only one of the variants:
    /// either it limits of the inward facing angle with the outer ring or it cuts of the
    /// angle with the inner ring.
    ///
    fn new_port
    (height:Var<f32>,width:Var<f32>,inner_radius:Var<f32>,is_inwards:Var<f32>) -> AnyShape {
        let zoom_factor                  = Var::<f32>::from("1.0 / input_zoom");
        let height                       = &height * &zoom_factor;
        let outer_radius      : Var<f32> = &inner_radius + &height;
        let segment_width_rad : Var<f32> = &width * &zoom_factor;

        // This describes the segment between the angle and the outer ring.
        let segment_outer_radius  = outer_radius.clone();
        let segment_outer         = Segment::new(
            segment_outer_radius,segment_width_rad.clone()
        );

        // This describes the segment between the angle and the inner ring.
        let segment_inner_radius = inner_radius.clone();
        let segment_inner = Segment::new(segment_inner_radius, segment_width_rad);

        // And derive the shape outline parameters from it.
        let shape_height = height.clone() + segment_outer.sagitta();
        let shape_width  = segment_outer.chord_length();

        // The angle used as a base shape needs to be computed based on our desired width along
        // the inner circle and the desired height. From width and height, which have a 90 degree
        // angle between them, we can compute the angle of the shape.
        let angle_inner = Var::from(90_f32.to_radians());
        let triangle    = Triangle::<Var<f32>>::from_sides_and_angle(height,shape_width,angle_inner);

        let corner_angle = Var::<Angle<Radians>>::from(triangle.angle_b().clone());
        let base_shape   = Plane().cut_angle(corner_angle);

        // `circle_outer_radius_scale` toggles whether the circle will have any effect on the
        // shape. This circle will be used with an `Union`, thus a large enough radius will
        // negate its effect.
        let circle_outer_radius_scale = Var::from(1.0) + (&is_inwards * Var::from(999_999.9));
        let circle_outer_radius       = circle_outer_radius_scale * &outer_radius;
        let circle_outer_radius       = Var::<Distance<Pixels>>::from(circle_outer_radius);
        let circle_outer              = Circle(circle_outer_radius);

        // `circle_inner_radius_scale` toggles whether the circle will have any effect on the
        // shape. This circle will be used with an `Difference`, thus a zero radius will negate
        // its effect.
        let circle_inner_radius_scale = &is_inwards * Var::<f32>::from(1.0);
        let circle_inner_radius       = circle_inner_radius_scale * &inner_radius;
        let circle_inner_radius       = Var::<Distance<Pixels>>::from(circle_inner_radius);
        let circle_inner              = Circle(circle_inner_radius);

        // We don't want to move around the angle so we position the two circles relative to the
        // angle.

        let circle_outer_offset_y  = &shape_height - segment_outer.sagitta() - &outer_radius;
        let circle_outer_offset_y  = Var::<Distance<Pixels>>::from(circle_outer_offset_y);
        let circle_outer           = circle_outer.translate_y(circle_outer_offset_y);

        let circle_inner_offset_y = &shape_height - segment_outer.sagitta();
        let circle_inner_offset_y = circle_inner_offset_y - segment_inner.sagitta() + &inner_radius;
        let circle_inner_offset_y = Var::<Distance<Pixels>>::from(circle_inner_offset_y);
        let circle_inner          = circle_inner.translate_y(circle_inner_offset_y);

        // Now we shape the angle by applying the circles.Note that only one of them will have an
        // effect, based on the radius that we modified set through `is_inwards`.
        let sculpted_shape = base_shape;
        let sculpted_shape = Intersection(sculpted_shape,circle_outer);
        let sculpted_shape = Difference(sculpted_shape,circle_inner);
        let sculpted_shape = sculpted_shape.fill(Srgb::new(0.26, 0.69, 0.99));

        // The angle should be centered on (0,0) to make it easier to rotate and minimise the
        // required canvas.
        let center_offset    = Var::<Distance<Pixels>>::from(&shape_height * Var::from(0.5));
        let sculpted_shape   = sculpted_shape.translate_y(-center_offset);

        // This is a conditional rotation that allows the port to either point inwards
        // or outwards.
        let pi             = Var::from(PI);
        let rotation_angle = is_inwards * pi;
        let rotation_angle = Var::<Angle<Radians>>::from(rotation_angle);
        let sculpted_shape = sculpted_shape.rotate(rotation_angle);

        sculpted_shape.into()
    }

    ensogl::define_shape_system! {
        (height:f32,width:f32,inner_radius:f32,is_inwards:f32) {
            // FIXME: `is_inwards` should only be 0.0 or 1.0.
            new_port(height,width,inner_radius,is_inwards)
        }
    }

    impl Shape{
        /// Set the shape parameters derived from the `Specification`.
        pub fn update_from_spec(&self,spec:&Specification){
            self.update_parameters(spec.height,spec.inner_radius, spec.width, spec.direction)
        }

        /// Set the shape parameters.
        pub fn update_parameters
        (&self, height:f32, inner_radius:f32, width:Angle<Degrees>, direction:Direction){
            self.height.set(height);
            self.inner_radius.set(inner_radius);
            self.width.set(width.value.to_radians());
            match direction{
                Direction::In  => self.is_inwards.set(1.0),
                Direction::Out => self.is_inwards.set(0.0),
            };
        }
    }
}



// ============
// === Port ===
// ============

/// Initialise the shape with sensible defaults.
fn init_shape(shape:&shape::Shape, direction:Direction){
    let spec  = Specification::default();
    shape.update_from_spec(&spec);
    match direction{
        Direction::In  => shape.is_inwards.set(1.0),
        Direction::Out => shape.is_inwards.set(1.0),
    };

    // Add some extra space to the shape can grow when resized.
    let padding_factor = 2.0;
    let bbox = Vector2::new(padding_factor * shape.height.get(), padding_factor * shape.height.get());
    shape.sprite.size().set(bbox);
}

/// Shape view for input port.
#[derive(Debug,Default,Clone,Copy)]
pub struct InputPortView {}
impl ShapeViewDefinition for InputPortView {
    type Shape = shape::Shape;
    fn new(shape:&Self::Shape, _scene:&Scene,_shape_registry:&ShapeRegistry) -> Self {
        init_shape(shape, Direction::In);
        Self {}
    }
}

/// Shape view for output port.
#[derive(Debug,Default,Clone,Copy)]
pub struct OutputPortView {}
impl ShapeViewDefinition for OutputPortView {
    type Shape = shape::Shape;
    fn new(shape:&Self::Shape, _scene:&Scene,_shape_registry:&ShapeRegistry) -> Self {
        init_shape(shape, Direction::Out);
        Self {}
    }
}

/// Helper trait that describes a `ShapeViewDefinition` with a port shape.
pub trait PortShapeViewDefinition = ShapeViewDefinition<Shape=shape::Shape>;

/// Port definition.
///
/// A port must always be instantiated as an `InputPort` or an `OutputPort`. This determines its
/// actual shape.
///
/// Ports are constructed around an inner circle, and thus most measurements are in degrees,
/// which are measured around a inner circle that is defined by the `inner_radius`.
///
/// Example
/// -------
/// ```
/// # use ensogl::math::topology::unit::Angle;
/// # use ensogl::math::topology::unit::Degrees;
/// # use graph_editor::component::node::port::InputPort;
/// # use graph_editor::component::node::port::OutputPort;
///
/// let input_port  = InputPort::default();
/// input_port.set_position(Angle::<Degrees>::new(45.0));
/// let output_port = OutputPort::default();
/// ```
#[derive(CloneRef,Debug,Derivative)]
#[derivative(Clone(bound=""))]
#[allow(missing_docs)]
pub struct Port<T:PortShapeViewDefinition> {
    pub data : Rc<PortData<T>>
}

/// Type that represents an input port.
pub type InputPort = Port<InputPortView>;

/// Type that represents an output port.
pub type OutputPort = Port<OutputPortView>;

/// Internal data of `Port.
#[derive(Debug,Clone)]
#[allow(missing_docs)]
pub struct PortData<T:PortShapeViewDefinition> {
    /// Height of the port.
    height       : Cell<f32>,
    /// Width of the port in degrees.
    width        : Cell<Angle<Degrees>>,
    /// Radius of the inner circle that the port is constructed around.
    inner_radius : Cell<f32>,
    /// Location of the port along the inner circle.
    position     : Cell<Angle<Degrees>>,
    /// Indicates whether this port is facing inwards or outwards.
    direction    : Cell<Direction>,

    pub view : Rc<component::ShapeView<T>>
}

impl<T:PortShapeViewDefinition> Port<T> {

    /// Internal constructor based on a given specification.
    pub fn new() -> Self {
        let spec = Specification::default();

        let logger = Logger::new("node");
        let view   = Rc::new(component::ShapeView::new(&logger));

        let data   = Rc::new(PortData{
            height       : Cell::new(spec.height),
            width        : Cell::new(spec.width),
            inner_radius : Cell::new(spec.inner_radius),
            position     : Cell::new(spec.position),
            direction    : Cell::new(spec.direction),
            view
        });
        Self {data} . init()
    }

    fn init(self) -> Self {
        self.update_sprite_position();
        self
    }

    /// Sets the port's position.
    ///
    /// Ports exist around an inner circle, and thus the position is given as an angle on that
    /// circle. To change the radius of the circle use `set_inner_radius`.
    pub fn set_position(&self, position:Angle<Degrees>) {
        self.data.position.set(position);
        self.update_sprite_position();
    }

    /// Sets the ports inner radius.
    pub fn set_inner_radius(&self, inner_radius:f32) {
        self.data.inner_radius.set(inner_radius);
        if let Some(t) = self.data.view.data.borrow().as_ref() {
            t.shape.inner_radius.set(inner_radius);
        }
        self.update_sprite_position()
    }

    /// Sets the ports height.
    pub fn set_height(&self, height:f32) {
        self.data.height.set(height);
        if let Some(t) = self.data.view.data.borrow().as_ref() {
            t.shape.height.set(height);
        }
    }

    /// Sets the ports width.
    pub fn set_width(&self, width:Angle<Degrees>) {
        self.data.width.set(width);
        if let Some(t) = self.data.view.data.borrow().as_ref() {
            t.shape.width.set(width.value.to_radians());
        }
    }

    /// Update the position of the sprite according to the Port specification.
    /// The position is given along a circle, thus the position and rotation of the sprite
    /// are tied together, so the Port always point in the right direction.
    /// Needs to be called whenever the `position` or `inner_radius` is modified.
    fn update_sprite_position(&self) {
        let inner_radius = self.data.inner_radius.get();
        let position     = self.data.position.get();

        let translation_vector = Vector3::new(0.0,inner_radius,0.0);
        let rotation_vector    = -Vector3::new(0.0,0.0,position.rad().value);
        let rotation           = nalgebra::Rotation3::new(rotation_vector);
        let translation        = rotation * translation_vector;
        self.data.view.display_object.set_position(translation);
        self.data.view.display_object.set_rotation(rotation_vector);
    }
}

impl<T:PortShapeViewDefinition> Default for Port<T> {
    fn default() -> Self {
        Self::new()
    }
}



// ===================
// === Port Buffer ===
// ===================

/// Data structure that creates and stores port shapes.
#[derive(Debug,Default)]
#[allow(missing_docs)]
pub struct PortBuffer<T:PortShapeViewDefinition> {
    ports: RefCell<Vec<Port<T>>>,
}

/// Helper type that represents a `PortBuffer` for `InputPorts`.
type InputPortBuffer = PortBuffer<InputPortView>;

/// Helper type that represents a `PortBuffer` for `OutputPorts`.
type OutputPortBuffer = PortBuffer<OutputPortView>;

impl<T:PortShapeViewDefinition> PortBuffer<T> {
    /// Create a new port in this buffer and sets it as a child node of the given parent.
    pub fn create(&self, parent:&Node) -> Port<T> {
        let port = Port::default();
        parent.add_child(&port.data.view.display_object);
        self.ports.borrow_mut().push(port.clone_ref());
        port
    }
}



// ====================
// === Port Manager ===
// ====================

/// Handles creation and layouting of ports around a node.
///
/// Example
/// -------
///  TODO: Get this to run as test on non-wasm targets.
/// ```no_run
///
/// use graph_editor::component::node::Node;
/// use graph_editor::component::node::port::Registry;
///
/// let parent_node = Node::new();
/// let ports       = Registry::default();
///
/// ports.input.create(&parent_node);
/// ports.output.create(&parent_node);
/// ```
///
/// TODO implement the layouting
#[derive(Debug,Default)]
pub struct Registry {
    /// Buffer of input ports.
    pub input  : InputPortBuffer,
    /// Buffer of output ports.
    pub output : OutputPortBuffer,
}
