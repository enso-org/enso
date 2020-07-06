//! Definition of the Node component.

#![allow(missing_docs)]
// WARNING! UNDER HEAVY DEVELOPMENT. EXPECT DRASTIC CHANGES.

pub mod port;

pub use port::Expression;

use crate::prelude::*;

use enso_frp as frp;
use enso_frp;
use ensogl::data::color;
use ensogl::display::Attribute;
use ensogl::display::Buffer;
use ensogl::display::Sprite;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display;
use ensogl::gui::component::Animation;
use ensogl::gui::component;
use std::num::NonZeroU32;

use super::edge;
use crate::graph_editor::component::visualization;
use crate::graph_editor::component::node::port::output::OutputPorts;



// =================
// === Constants ===
// =================

pub const NODE_SHAPE_PADDING : f32 = 40.0;
pub const NODE_SHAPE_RADIUS  : f32 = 14.0;



// ============
// === Node ===
// ============

/// Canvas node shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style, selection:f32) {
            let bg_color = style.get("graph_editor.node.background.color").color().unwrap_or_else(|| color::Rgba::new(1.0,0.0,0.0,1.0).into());
            let selection_color = style.get("graph_editor.node.selection.color").color().unwrap_or_else(|| color::Rgba::new(1.0,0.0,0.0,1.0).into());
            let _selection_size = style.get("graph_editor.node.selection.size").number().unwrap_or(8.0);

            let border_size_f = 16.0;

            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let width  = width  - NODE_SHAPE_PADDING.px() * 2.0;
            let height = height - NODE_SHAPE_PADDING.px() * 2.0;
            let radius = NODE_SHAPE_RADIUS.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            let shape  = shape.fill(color::Rgba::from(bg_color));


            // === Shadow ===

            let shadow_size   = SHADOW_SIZE.px();
            let shadow_width  = &width  + &shadow_size * 2.0;
            let shadow_height = &height + &shadow_size * 2.0;
            let shadow_radius = &shadow_height / 2.0;
            let shadow        = Rect((shadow_width,shadow_height)).corners_radius(shadow_radius);
            let shadow_color  = color::LinearGradient::new()
                .add(0.0,color::Rgba::new(0.0,0.0,0.0,0.0).into_linear())
                .add(1.0,color::Rgba::new(0.0,0.0,0.0,0.20).into_linear());
            let shadow_color  = color::SdfSampler::new(shadow_color).max_distance(border_size_f).slope(color::Slope::Exponent(2.0));
            let shadow        = shadow.fill(shadow_color);


            // === Selection ===

            let selection_offset = 4.px();
            let selection_size   = 7.px();
            let select_width   = &width  - 2.px() + &selection_offset * 2.0 * &selection;
            let select_height  = &height - 2.px() + &selection_offset * 2.0 * &selection;
            let select_radius  = &select_height / 2.0;
            let select         = Rect((&select_width,&select_height)).corners_radius(&select_radius);

            let select2_width   = &width  - 2.px() + &selection_size * 2.0 * &selection;
            let select2_height  = &height - 2.px() + &selection_size * 2.0 * &selection;
            let select2_radius  = &select2_height / 2.0;
            let select2         = Rect((&select2_width,&select2_height)).corners_radius(&select2_radius);

            let select         = select2 - select;
            let select         = select.fill(color::Rgba::from(selection_color));

            let out = select + shadow + shape;
            out.into()
        }
    }
}

pub mod drag_area {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let width  = width  - NODE_SHAPE_PADDING.px() * 2.0;
            let height = height - NODE_SHAPE_PADDING.px() * 2.0;
            let radius = 14.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            let shape  = shape.fill(color::Rgba::new(0.0,0.0,0.0,0.000_001));

            let out = shape;
            out.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

/// Node events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct InputEvents {
    pub select            : frp::Source,
    pub deselect          : frp::Source,
    pub set_expression    : frp::Source<Expression>,
    pub set_visualization : frp::Source<Option<visualization::Instance>>,
}

impl InputEvents {
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def select            = source();
            def deselect          = source();
            def set_expression    = source();
            def set_visualization = source();
        }
        Self {select,deselect,set_expression,set_visualization}
    }
}


#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Frp {
    pub input        : InputEvents,
}

impl Deref for Frp {
    type Target = InputEvents;
    fn deref(&self) -> &Self::Target {
        &self.input
    }
}



// ============
// === Node ===
// ============


/// Internal data of `Node`
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Node {
    pub model       : Rc<NodeModel>,
    pub frp_network : frp::Network,
}

impl AsRef<Node> for Node {
    fn as_ref(&self) -> &Self {
        self
    }
}


impl Deref for Node {
    type Target = NodeModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

/// Internal data of `Node`
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct NodeModel {
    pub scene          : Scene,
    pub display_object : display::object::Instance,
    pub logger         : Logger,
    pub frp            : Frp,
    pub main_area      : component::ShapeView<shape::Shape>,
    pub drag_area      : component::ShapeView<drag_area::Shape>,
    pub ports          : port::Manager,
    pub visualization  : visualization::Container,
    pub output_ports   : OutputPorts,
}

pub const CORNER_RADIUS : f32 = 14.0;
pub const NODE_HEIGHT   : f32 = 28.0;
pub const TEXT_OFF      : f32 = 10.0;
pub const SHADOW_SIZE   : f32 = 10.0;


impl NodeModel {
    /// Constructor.
    pub fn new(scene:&Scene, network:&frp::Network) -> Self {

        let logger  = Logger::new("node");
        edge::sort_hack_1(scene);

        OutputPorts::order_hack(&scene);
        let main_logger = Logger::sub(&logger,"main_area");
        let drag_logger = Logger::sub(&logger,"drag_area");
        let main_area   = component::ShapeView::<shape      ::Shape>::new(&main_logger  ,scene);
        let drag_area   = component::ShapeView::<drag_area  ::Shape>::new(&drag_logger  ,scene);
        edge::sort_hack_2(scene);

        port::sort_hack(scene); // FIXME hack for sorting

        let display_object  = display::object::Instance::new(&logger);
        display_object.add_child(&drag_area);
        display_object.add_child(&main_area);

        // FIXME: maybe we can expose shape system from shape?
        let shape_system = scene.shapes.shape_system(PhantomData::<shape::Shape>);
        shape_system.shape_system.set_pointer_events(false);

        let ports = port::Manager::new(&logger,scene);
        let scene = scene.clone_ref();
        let input = InputEvents::new(&network);

        let visualization = visualization::Container::new(&logger,&scene);
        visualization.mod_position(|t| {
            t.x = 60.0;
            t.y = -120.0;
        });

        display_object.add_child(&visualization);

        ports.mod_position(|p| {
            p.x = TEXT_OFF;
            p.y = NODE_HEIGHT/2.0;
        });
        display_object.add_child(&ports);

        let frp = Frp{input};


        // TODO: Determine number of output ports based on node semantics.
        let output_ports = OutputPorts::new(&scene, NonZeroU32::new(10).unwrap());
        display_object.add_child(&output_ports);


        Self {scene,display_object,logger,frp,main_area,drag_area,output_ports,ports
             ,visualization} . init()
    }

    fn init(self) -> Self {
        self.set_expression(Expression::debug_from_str("empty"));
        self
    }

    pub fn width(&self) -> f32 {
        self.ports.width() + TEXT_OFF * 2.0
    }

    pub fn height(&self) -> f32 {
        NODE_HEIGHT
    }

    fn set_expression(&self, expr:impl Into<Expression>) {
        let expr = expr.into();
        self.ports.set_expression(expr);

        let width = self.width();
        let height = self.height();

        let size = Vector2::new(width+NODE_SHAPE_PADDING*2.0, height+NODE_SHAPE_PADDING*2.0);
        self.main_area.shape.sprite.size.set(size);
        self.drag_area.shape.sprite.size.set(size);
        self.main_area.mod_position(|t| t.x = width/2.0);
        self.main_area.mod_position(|t| t.y = height/2.0);
        self.drag_area.mod_position(|t| t.x = width/2.0);
        self.drag_area.mod_position(|t| t.y = height/2.0);

        self.output_ports.frp.set_size.emit(size);
        self.output_ports.mod_position(|t| t.x = width/2.0);
        self.output_ports.mod_position(|t| t.y = height/2.0);
    }

    pub fn visualization(&self) -> &visualization::Container {
        &self.visualization
    }
}

impl Node {
    pub fn new(scene:&Scene) -> Self {
        let frp_network      = frp::Network::new();
        let model            = Rc::new(NodeModel::new(scene,&frp_network));
        let inputs           = &model.frp.input;
        let selection        = Animation::<f32>::new(&frp_network);


        frp::extend! { frp_network
            eval  selection.value ((v) model.main_area.shape.selection.set(*v));
            eval_ inputs.select   (selection.set_target_value(1.0));
            eval_ inputs.deselect (selection.set_target_value(0.0));

            eval inputs.set_expression ((expr) model.set_expression(expr));

            eval inputs.set_visualization ((content)
                model.visualization.frp.set_visualization.emit(content)
            );
        }

        Self {frp_network,model}
    }
}


impl display::Object for Node {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



//// =============
//// === Icons ===
//// =============
//
///// Icons definitions.
//pub mod icons {
//    use super::*;
//
//    /// History icon.
//    pub fn history() -> AnyShape {
//        let radius_diff    = 0.5.px();
//        let corners_radius = 2.0.px();
//        let width_diff     = &corners_radius * 3.0;
//        let offset         = 2.px();
//        let width          = 32.px();
//        let height         = 16.px();
//        let persp_diff1    = 6.px();
//
//        let width2          = &width  - &width_diff;
//        let width3          = &width2 - &width_diff;
//        let corners_radius2 = &corners_radius  - &radius_diff;
//        let corners_radius3 = &corners_radius2 - &radius_diff;
//        let persp_diff2     = &persp_diff1 * 2.0;
//
//        let rect1 = Rect((&width ,&height)).corners_radius(&corners_radius);
//        let rect2 = Rect((&width2,&height)).corners_radius(&corners_radius2).translate_y(&persp_diff1);
//        let rect3 = Rect((&width3,&height)).corners_radius(&corners_radius3).translate_y(&persp_diff2);
//
//        let rect3 = rect3 - rect2.translate_y(&offset);
//        let rect2 = rect2 - rect1.translate_y(&offset);
//
//        let rect1 = rect1.fill(color::Rgba::new(0.26, 0.69, 0.99, 1.00));
//        let rect2 = rect2.fill(color::Rgba::new(0.26, 0.69, 0.99, 0.6));
//        let rect3 = rect3.fill(color::Rgba::new(0.26, 0.69, 0.99, 0.4));
//
//        let icon = (rect3 + rect2 + rect1).translate_y(-persp_diff2/2.0);
//        icon.into()
//    }
//}
//
///// Ring angle shape definition.
//pub fn ring_angle<R,W,A>(inner_radius:R, width:W, angle:A) -> AnyShape
//    where R : Into<Var<Pixels>>,
//          W : Into<Var<Pixels>>,
//          A : Into<Var<Angle<Radians>>> {
//    let inner_radius = inner_radius.into();
//    let width        = width.into();
//    let angle        = angle.into();
//
//    let angle2  = &angle / 2.0;
//    let radius  = &width / 2.0;
//    let inner   = Circle(&inner_radius);
//    let outer   = Circle(&inner_radius + &width);
//    let section = Plane().cut_angle(&angle);
//    let corner1 = Circle(&radius).translate_y(inner_radius + radius);
//    let corner2 = corner1.rotate(&angle2);
//    let corner1 = corner1.rotate(-&angle2);
//    let ring    = &outer - &inner;
//    let pie     = &ring * &section;
//    let out     = &pie + &corner1 + &corner2;
//    let out     = out.fill(color::Rgba::new(0.9,0.9,0.9,1.0));
//    out.into()
//}
//
