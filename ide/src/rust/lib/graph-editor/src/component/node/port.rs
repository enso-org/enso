//! Definition of the Port component.

use crate::prelude::*;

//use crate::component::node::port::Registry;

use enso_frp;
use enso_frp as frp;
use ensogl::data::color;
use ensogl::display::Attribute;
use ensogl::display::Buffer;
use ensogl::display::Sprite;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display;
use ensogl::gui::component;

use crate::component::cursor;
use super::super::node;
use span_tree::SpanTree;




// ============
// === Port ===
// ============

/// Canvas node shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style, hover:f32) {
            let width  : Var<Distance<Pixels>> = "input_size.x".into();
            let height : Var<Distance<Pixels>> = "input_size.y".into();
            let radius = 6.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            // let color  : Var<color::Rgba> = "srgba(1.0,1.0,1.0,0.00001 + 0.1*input_hover)".into();
            let color  : Var<color::Rgba> = "srgba(1.0,1.0,1.0,0.00001)".into();
            let shape  = shape.fill(color);
            shape.into()
        }
    }
}


pub fn sort_hack(scene:&Scene) {
    let logger = Logger::new("hack");
    component::ShapeView::<shape::Shape>::new(&logger,scene);
}


#[derive(Debug,Clone,CloneRef)]
pub struct Events {
    pub network        : frp::Network,
    pub cursor_mode    : frp::Stream<cursor::Mode>,
    pub press          : frp::Stream<span_tree::Crumbs>,
    press_source       : frp::Source<span_tree::Crumbs>,
    cursor_mode_source : frp::Merge<cursor::Mode>,
}


// ==================
// === Expression ===
// ==================

#[derive(Clone,Debug,Default)]
pub struct Expression {
    pub code             : String,
    pub input_span_tree  : SpanTree,
    pub output_span_tree : SpanTree,
}

impl From<&Expression> for Expression {
    fn from(t:&Expression) -> Self {
        t.clone()
    }
}



// ===============
// === Manager ===
// ===============

#[derive(Clone,CloneRef,Debug)]
pub struct Manager {
    logger         : Logger,
    display_object : display::object::Instance,
    pub frp        : Events,
    scene          : Scene,
    expression     : Rc<RefCell<Expression>>,
    ports          : Rc<RefCell<Vec<component::ShapeView<shape::Shape>>>>,
    port_networks  : Rc<RefCell<Vec<frp::Network>>>,
}

impl Manager {
    pub fn new(logger:&Logger, scene:&Scene) -> Self {
        frp::new_network! { network
            def cursor_mode_source = gather::<cursor::Mode>();
            def press_source       = source::<span_tree::Crumbs>();
        }

        let logger         = logger.sub("port_manager");
        let display_object = display::object::Instance::new(&logger);
        let scene          = scene.clone_ref();
        let expression     = default();
        let port_networks  = default();
        let ports          = default();
        let cursor_mode    = (&cursor_mode_source).into();
        let press          = (&press_source).into();
        let frp            = Events {network,cursor_mode,press,cursor_mode_source,press_source};
        Self {logger,display_object,frp,ports,scene,expression,port_networks}
    }

    pub fn set_expression<E:Into<Expression>>(&self, expression:E) {
        let     expression    = expression.into();
        let mut to_visit      = vec![expression.input_span_tree.root_ref()];
        let mut ports         = vec![];
        let mut port_networks = vec![];

        loop {
            match to_visit.pop() {
                None => break,
                Some(node) => {
                    let span          = node.span();
                    let contains_root = span.index.value == 0;
                    let skip          = node.kind.is_empty() || contains_root;
                    if !skip {
                        let port   = component::ShapeView::<shape::Shape>::new(&self.logger,&self.scene);
                        let unit   = 7.224_609_4;
                        let width  = unit * span.size.value as f32;
                        let width2  = width + 4.0;
                        let node_height = 28.0;
                        let height = 18.0;
                        port.shape.sprite.size().set(Vector2::new(width2,node_height));
                        let x = width/2.0 + unit * span.index.value as f32;
                        port.mod_position(|t| t.x = x);
                        self.add_child(&port);

//                        let network = &port.events.network;
                        let hover   = &port.shape.hover;
                        let crumbs  = node.crumbs.clone();
                        frp::new_network! { port_network
                            def _foo = port.events.mouse_over . map(f_!(hover.set(1.0);));
                            def _foo = port.events.mouse_out  . map(f_!(hover.set(0.0);));

                            def out  = port.events.mouse_out.constant(cursor::Mode::Normal);
                            def over = port.events.mouse_over.constant(cursor::Mode::highlight(&port,Vector2::new(x,0.0),Vector2::new(width2,height)));
                            self.frp.cursor_mode_source.attach(&over); // FIXME: It leaks memory in the current FRP implementation
                            self.frp.cursor_mode_source.attach(&out);

                            let press_source = &self.frp.press_source;
                            def _press = port.events.mouse_down.map(f_!(press_source.emit(&crumbs)));
                        }
                        ports.push(port);
                        port_networks.push(port_network);
                    }

                    to_visit.extend(node.children_iter());
                }
            }
        }

        *self.expression.borrow_mut()    = expression;
        *self.ports.borrow_mut()         = ports;
        *self.port_networks.borrow_mut() = port_networks;
    }

    pub fn get_port_offset(&self, crumbs:&[span_tree::Crumb]) -> Option<Vector2<f32>> {
        let span_tree = &self.expression.borrow().input_span_tree;
        span_tree.root_ref().get_descendant(crumbs).map(|node|{
            let span  = node.span();
            let unit  = 7.224_609_4;
            let width = unit * span.size.value as f32;
            let x     = width/2.0 + unit * span.index.value as f32;
            Vector2::new(x + node::TEXT_OFF,node::NODE_HEIGHT/2.0) // FIXME
        }).ok()
    }
}

impl display::Object for Manager {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
