//! Definition of the Node component.

pub mod port;

use crate::prelude::*;

use crate::component::node::port::Registry;
use crate::component::visualization;

use enso_frp;
use enso_frp as frp;
use ensogl::data::color::*;
use ensogl::data::color::Srgba;
use ensogl::display::Attribute;
use ensogl::display::Buffer;
use ensogl::display::Sprite;
use ensogl::display::scene::Scene;
use ensogl::display::scene::ShapeRegistry;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display;
use ensogl::gui::component::animation;
use ensogl::gui::component;
use ensogl::math::topology::unit::AngleOps;


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
        (style:Style, selection:f32, creation:f32) {
            style.get("node.radius").number().unwrap_or(32.0); // FIXME: this is not used yet
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
    pub network           : frp::Network,
    pub select            : frp::Source,
    pub deselect          : frp::Source,
    pub set_visualization : frp::Source<Option<visualization::Visualization>>,
}



// ============
// === Node ===
// ============

/// Node definition.
#[derive(AsRef,Clone,CloneRef,Debug,Deref)]
pub struct Node {
    data : Rc<NodeData>,
}

impl AsRef<Node> for Node {
    fn as_ref(&self) -> &Self {
        self
    }
}

/// Weak version of `Node`.
#[derive(Clone,CloneRef,Debug)]
pub struct WeakNode {
    data : Weak<NodeData>
}

impl WeakElement for WeakNode {
    type Strong = Node;

    fn new(view: &Self::Strong) -> Self {
        view.downgrade()
    }

    fn view(&self) -> Option<Self::Strong> {
        self.upgrade()
    }
}

impl WeakKey for WeakNode {
    type Key = display::object::Id;

    fn with_key<F, R>(view: &Self::Strong, f: F) -> R where F: FnOnce(&Self::Key) -> R {
        f(&view.id())
    }
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
    pub logger                  : Logger,
    pub label                   : frp::Source<String>,
    pub events                  : Events,
    pub view                    : component::ShapeView<NodeView>,
    pub ports                   : Registry,
    pub visualization_container : visualization::Container
}

impl Node {
    /// Constructor.
    pub fn new() -> Self {
        frp::new_network! { node_network
            def label             = source::<String> ();
            def select            = source::<()> ();
            def deselect          = source::<()> ();
            def set_visualization = source::<Option<visualization::Visualization>> ();
        }
        let network       = node_network;
        let logger        = Logger::new("node");
        let view          = component::ShapeView::new(&logger);
        let events        = Events {network,select,deselect,set_visualization};
        let ports         = Registry::default() ;
        let visualization = default();
        let data          = Rc::new(NodeData{logger,label,events,view,ports, visualization_container: visualization });
        Self {data} . init()
    }

    fn init(self) -> Self {
        self.data.visualization_container.set_position(Vector3::new(0.0, -50.0, 0.0));
        self.add_child(&self.data.visualization_container);

        let network = &self.data.events.network;

        // FIXME: This is needed now because frp leaks memory.
        let weak_view_data = Rc::downgrade(&self.view.data);
        let creation = animation(network, move |value| {
            weak_view_data.upgrade().for_each(|view_data| {
                view_data.borrow().as_ref().for_each(|t| t.shape.creation.set(value))
            })
        });
        creation.set_target_position(1.0);

        // FIXME: This is needed now because frp leaks memory.
        let weak_view_data = Rc::downgrade(&self.view.data);
        let selection = animation(network, move |value| {
            weak_view_data.upgrade().for_each(|view_data| {
                view_data.borrow().as_ref().for_each(|t| t.shape.selection.set(value))
            })
        });


        frp::extend! { network
            let selection_ref = selection.clone_ref();
            def _f_select = self.events.select.map(move |_| {
                selection_ref.set_target_position(1.0);
            });

            let selection_ref = selection.clone_ref();
            def _f_deselect = self.events.deselect.map(move |_| {
                selection_ref.set_target_position(0.0);
            });

            let weak_node = self.downgrade();
            def _f_set_vis = self.events.set_visualization.map(move |content| {
                if let Some(node) = weak_node.upgrade() {
                    node.visualization_container.frp.set_visualization.emit(content)
                }
            });
        }

        // TODO this is sample functionality. Needs to be replaced with logic creating ports.
        let input_port = self.data.ports.input.create(&self);
        input_port.set_position(90.0_f32.degrees());
        let output_port = self.data.ports.output.create(&self);
        output_port.set_position(270.0_f32.degrees());

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

impl display::Object for Node {
    fn display_object(&self) -> &display::object::Instance {
        &self.view.display_object
    }
}

impl display::WeakObject for WeakNode {
    fn try_display_object(&self) -> Option<display::object::Instance> {
        self.upgrade().map(|ref t| t.display_object().clone_ref())
    }
}
