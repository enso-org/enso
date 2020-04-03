//! Definition of the Node component.

use crate::prelude::*;

use ensogl::data::color::Srgba;
use ensogl::display;
use ensogl::display::traits::*;
use ensogl::display::{Sprite, Attribute};
use enso_frp;
use enso_frp as frp;
use enso_frp::frp;
use ensogl::display::Buffer;
use ensogl::data::color::*;
use ensogl::display::shape::*;
use ensogl::display::scene::{Scene,ShapeRegistry};
use ensogl::gui::component::animation;
use ensogl::gui::component;



/// Icons definitions.
pub mod icons {
    use super::*;

    /// History icon.
    pub fn history() -> AnyShape {
        let radius_diff    = 0.5.px();
        let corners_radius = 2.0.px();
        let width_diff     = &corners_radius * 3.0;
        let offset         = 2.px();
        let width          = 32.px();
        let height         = 16.px();
        let persp_diff1    = 6.px();

        let width2          = &width  - &width_diff;
        let width3          = &width2 - &width_diff;
        let corners_radius2 = &corners_radius  - &radius_diff;
        let corners_radius3 = &corners_radius2 - &radius_diff;
        let persp_diff2     = &persp_diff1 * 2.0;

        let rect1 = Rect((&width ,&height)).corners_radius(&corners_radius);
        let rect2 = Rect((&width2,&height)).corners_radius(&corners_radius2).translate_y(&persp_diff1);
        let rect3 = Rect((&width3,&height)).corners_radius(&corners_radius3).translate_y(&persp_diff2);

        let rect3 = rect3 - rect2.translate_y(&offset);
        let rect2 = rect2 - rect1.translate_y(&offset);

        let rect1 = rect1.fill(Srgba::new(0.26, 0.69, 0.99, 1.00));
        let rect2 = rect2.fill(Srgba::new(0.26, 0.69, 0.99, 0.6));
        let rect3 = rect3.fill(Srgba::new(0.26, 0.69, 0.99, 0.4));

        let icon = (rect3 + rect2 + rect1).translate_y(-persp_diff2/2.0);
        icon.into()
    }
}

/// Ring angle shape definition.
pub fn ring_angle<R,W,A>(inner_radius:R, width:W, angle:A) -> AnyShape
    where R : Into<Var<Distance<Pixels>>>,
          W : Into<Var<Distance<Pixels>>>,
          A : Into<Var<Angle<Radians>>> {
    let inner_radius = inner_radius.into();
    let width        = width.into();
    let angle        = angle.into();

    let angle2  = &angle / 2.0;
    let radius  = &width / 2.0;
    let inner   = Circle(&inner_radius);
    let outer   = Circle(&inner_radius + &width);
    let section = Plane().cut_angle(&angle);
    let corner1 = Circle(&radius).translate_y(inner_radius + radius);
    let corner2 = corner1.rotate(&angle2);
    let corner1 = corner1.rotate(-&angle2);
    let ring    = &outer - &inner;
    let pie     = &ring * &section;
    let out     = &pie + &corner1 + &corner2;
    let out     = out.fill(Srgba::new(0.9,0.9,0.9,1.0));
    out.into()
}



// ============
// === Node ===
// ============

/// Canvas node shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (selection:f32, creation:f32) {
            let border_size_f = 16.0;
            let node_radius   = 32.0.px() * creation;
            let border_size   = border_size_f.px();

            let node = Circle(&node_radius);
            let node = node.fill(Srgb::new(0.97,0.96,0.95));

            let shadow       = Circle(&node_radius + &border_size);
            let shadow_color = LinearGradient::new()
                .add(0.0,Srgba::new(0.0,0.0,0.0,0.0).into_linear())
                .add(1.0,Srgba::new(0.0,0.0,0.0,0.14).into_linear());
            let shadow_color = SdfSampler::new(shadow_color).max_distance(border_size_f).slope(Slope::Exponent(4.0));
            let shadow       = shadow.fill(shadow_color);

            let selection_ring = Circle(&node_radius - 1.px() + &border_size * selection);
            let selection_ring = selection_ring.fill(Srgba::new(0.22,0.83,0.54,1.0));

            let loader_angle : Var<Angle<Radians>> = "Radians(clamp(input_time/2000.0 - 1.0) * 1.99 * PI)".into();
            let loader        = ring_angle(&node_radius, &border_size, &loader_angle);
            let loader        = loader.rotate(loader_angle / 2.0);
            let loader        = loader.rotate("Radians(input_time/200.0)");
            let icon          = icons::history();
            let out           = loader + selection_ring + shadow + node + icon;
            out.into()
        }
    }
}



// ==============
// === Events ===
// ==============

/// Node events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Events {
    pub select     : frp::Dynamic<()>,
    pub deselect   : frp::Dynamic<()>,
}


// ============
// === Node ===
// ============

/// Node definition.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct Node {
    data : Rc<NodeData>,
}

/// Weak version of `Node`.
#[derive(Clone,CloneRef,Debug)]
pub struct WeakNode {
    data : Weak<NodeData>
}

/// Shape view for Node.
#[derive(Debug,Clone,Copy)]
pub struct NodeView {}
impl component::ShapeViewDefinition for NodeView {
    type Shape = shape::Shape;
    fn new(shape:&Self::Shape, _scene:&Scene, _shape_registry:&ShapeRegistry) -> Self {
        shape.sprite.size().set(Vector2::new(200.0,200.0));
        Self {}
    }
}

/// Internal data of `Node`
#[derive(Debug)]
#[allow(missing_docs)]
pub struct NodeData {
    pub logger : Logger,
    pub label  : frp::Dynamic<String>,
    pub events : Events,
    pub view   : component::ShapeView<NodeView>,
}

impl Node {
    /// Constructor.
    pub fn new() -> Self {
        frp! {
            label    = source::<String> ();
            select   = source::<()>     ();
            deselect = source::<()>     ();
        }

        let logger = Logger::new("node");
        let view   = component::ShapeView::new(&logger);
        let events = Events {select,deselect};
        let data   = Rc::new(NodeData {logger,label,events,view});
        Self {data} . init()
    }

    fn init(self) -> Self {
        // FIXME: This is needed now because frp leaks memory.
        let weak_view_data = Rc::downgrade(&self.view.data);
        let creation = animation(move |value| {
            weak_view_data.upgrade().for_each(|view_data| {
                view_data.borrow().as_ref().for_each(|t| t.shape.creation.set(value))
            })
        });
        creation.set_target_position(1.0);

        // FIXME: This is needed now because frp leaks memory.
        let weak_view_data = Rc::downgrade(&self.view.data);
        let selection = animation(move |value| {
            weak_view_data.upgrade().for_each(|view_data| {
                view_data.borrow().as_ref().for_each(|t| t.shape.selection.set(value))
            })
        });

        let selection_ref = selection.clone_ref();
        self.events.select.map("select", move |_| {
            selection_ref.set_target_position(1.0);
        });

        let selection_ref = selection.clone_ref();
        self.events.deselect.map("deselect", move |_| {
            selection_ref.set_target_position(0.0);
        });

        self
    }
}

impl Default for Node {
    fn default() -> Self {
        Self::new()
    }
}

impl StrongRef for Node {
    type WeakRef = WeakNode;
    fn downgrade(&self) -> WeakNode {
        WeakNode {data:Rc::downgrade(&self.data)}
    }
}

impl WeakRef for WeakNode {
    type StrongRef = Node;
    fn upgrade(&self) -> Option<Node> {
        self.data.upgrade().map(|data| Node{data})
    }
}

impl<'t> From<&'t Node> for &'t display::object::Node {
    fn from(t:&'t Node) -> Self {
        &t.view.display_object
    }
}
