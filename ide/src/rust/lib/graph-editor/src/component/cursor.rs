//! Definition of the Cursor (known as well as mouse pointer) component.

#![allow(missing_docs)]
// WARNING! UNDER HEAVY DEVELOPMENT. EXPECT DRASTIC CHANGES.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::control::callback;
use ensogl::data::color;
use ensogl::display::Buffer;
use ensogl::display::layout::alignment;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::{Sprite, Attribute};
use ensogl::display;
use ensogl::gui::component::animation;
use ensogl::gui::component::animation2;
use ensogl::gui::component;
use ensogl::system::web;


#[derive(Debug,Clone)]
pub enum Mode {
    Normal,
    Cursor,
    Highlight {
        host     : display::object::Instance,
        position : Vector2<f32>,
        size     : Vector2<f32>,
    }
}

impl Mode {
    pub fn highlight<H>(host:H, position:Vector2<f32>, size:Vector2<f32>) -> Self
    where H:display::Object {
        let host = host.display_object().clone_ref();
        Self::Highlight {host,position,size}
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal
    }
}



// ==================
// === CursorView ===
// ==================

/// Canvas shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (position:Vector2<f32>, width:f32, height:f32, selection_size:Vector2<f32>, press:f32, radius:f32) {
            let press_diff       = 2.px() * &press;
            let radius           = 1.px() * radius - &press_diff;
            let selection_width  = 1.px() * &selection_size.x() * &press;
            let selection_height = 1.px() * &selection_size.y() * &press;
            let width            = (1.px() * &width  - &press_diff * 2.0) + selection_width.abs();
            let height           = (1.px() * &height - &press_diff * 2.0) + selection_height.abs();
            let cursor = Rect((width,height))
                .corners_radius(radius)
                .translate((-&selection_width/2.0, -&selection_height/2.0))
                .translate(("input_position.x","input_position.y"))
                .fill(color::Rgba::new(1.0,1.0,1.0,0.2));
            cursor.into()
        }
    }
}

///// Shape view for Cursor.
//#[derive(Clone,CloneRef,Debug)]
//#[allow(missing_docs)]
//pub struct CursorView {}
//
//impl component::ShapeViewDefinition for CursorView {
//    type Shape = shape::Shape;
//}



// ==============
// === InputEvents ===
// ==============

/// Cursor events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct InputEvents {
    pub network  : frp::Network,
    pub set_mode : frp::Source<Mode>,
    pub press    : frp::Source,
    pub release  : frp::Source,
}

impl Default for InputEvents {
    fn default() -> Self {
        frp::new_network! { cursor_events
            def set_mode = source();
            def press    = source();
            def release  = source();
        }
        let network = cursor_events;
        Self {network,set_mode,press,release}
    }
}

/// Cursor events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Events {
    pub input    : InputEvents,
    pub position : frp::Stream<frp::Position>,
}

impl Deref for Events {
    type Target = InputEvents;
    fn deref(&self) -> &Self::Target {
        &self.input
    }
}



// ==============
// === Cursor ===
// ==============

/// Cursor (mouse pointer) definition.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct Cursor {
    data : Rc<CursorData>
}

/// Weak version of `Cursor`.
#[derive(Clone,CloneRef,Debug)]
pub struct WeakCursor {
    data : Weak<CursorData>
}

/// Internal data for `Cursor`.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct CursorData {
    pub logger : Logger,
    pub frp    : Events,
    pub view   : component::ShapeView<shape::Shape>,
//    pub scene_view    : scene::View,
    pub resize_handle : callback::Handle,
}

impl Cursor {
    /// Constructor.
    pub fn new(scene:&Scene) -> Self {
        let logger = Logger::new("cursor");
        let view   = component::ShapeView::<shape::Shape>::new(&logger,scene);
        let input = InputEvents::default();




        let scene_shape = scene.shape();
        let shape       = &view.shape;
        shape.sprite.size().set(Vector2::new(scene_shape.width(),scene_shape.height()));

        let resize_handle = scene.on_resize(enclose!((shape) move |scene_shape:&web::dom::ShapeData| {
            shape.sprite.size().set(Vector2::new(scene_shape.width(),scene_shape.height()));
        }));

        let shape_system = scene.shapes.shape_system(PhantomData::<shape::Shape>);
        shape_system.shape_system.set_alignment(alignment::HorizontalAlignment::Left, alignment::VerticalAlignment::Bottom);
        shape_system.shape_system.set_pointer_events(false);

        scene.views.main.remove(&shape_system.shape_system.symbol);
        scene.views.cursor.add(&shape_system.shape_system.symbol);
//        let scene_view = scene.views.new();
//        scene.views.main.remove(&shape_system.shape_system.symbol);
//        scene_view.add(&shape_system.shape_system.symbol);




        let network = &input.network;

        let view_data = view.shape.clone_ref();
        let press = animation(network,move |value| {
            view_data.press.set(value)
        });

        let view_data = view.shape.clone_ref();
        let radius = animation(network,move |value| {
            view_data.radius.set(value)
        });

        let view_data = view.shape.clone_ref();
        let width = animation(network,move |value| {
            view_data.width.set(value)
        });

        let view_data = view.shape.clone_ref();
        let height = animation(network,move |value| {
            view_data.height.set(value)
        });


        let (anim_use_fixed_pos_setter,anim_use_fixed_pos) = animation2(network);
        let (anim_pos_x_setter,anim_pos_x) = animation2(network);
        let (anim_pos_y_setter,anim_pos_y) = animation2(network);


        let mouse = &scene.mouse.frp;




        frp::extend! { network

            def anim_position      = anim_pos_x.zip_with(&anim_pos_y,|x,y| frp::Position::new(*x,*y));

            def _t_press = input.press.map(enclose!((press) move |_| {
                press.set_target_position(1.0);
            }));

            def _t_release = input.release.map(enclose!((press) move |_| {
                press.set_target_position(0.0);
            }));

            def fixed_position = input.set_mode.map(enclose!((anim_pos_x_setter,anim_pos_y_setter) move |m| {
                match m {
                    Mode::Highlight {host,..} => {
                        let p = host.global_position();
                        anim_pos_x_setter.set_target_position(p.x);
                        anim_pos_y_setter.set_target_position(p.y);
                        anim_use_fixed_pos_setter.set_target_position(1.0);
                        Some(p)
                    }
                    _ => {
                        anim_use_fixed_pos_setter.set_target_position(0.0);
                        None
                    }
                }
            }));

            def uses_mouse_position = fixed_position.map(|p| p.is_none());
            def mouse_position = mouse.position.gate(&uses_mouse_position);

            def position = mouse.position.zip_with3(&anim_position,&anim_use_fixed_pos, |p,ap,au| {
                let x = ap.x * au + p.x * (1.0 - au);
                let y = ap.y * au + p.y * (1.0 - au);
                frp::Position::new(x,y)
            });

            def _position = position.map(f!((p) {
                view.shape.position.set(Vector2::new(p.x,p.y));
            }));

            def _position = mouse_position.map(f!([anim_pos_x_setter,anim_pos_y_setter](p) {
                anim_pos_x_setter.set_target_position(p.x);
                anim_pos_y_setter.set_target_position(p.y);
            }));

            def _t_mode = input.set_mode.map(enclose!((radius,width,height) move |m| {
                match m {
                    Mode::Normal => {
                        radius.set_target_position(8.0);
                        width.set_target_position(16.0);
                        height.set_target_position(16.0);
                    }
                    Mode::Highlight {size,..} => {
                        radius.set_target_position(4.0);
                        width.set_target_position(size.x);
                        height.set_target_position(size.y);
                    }
                    _ => panic!()
                };
            }));
        }

        radius.set_target_position(8.0);
        width.set_target_position(16.0);
        height.set_target_position(16.0);

        input.set_mode.emit(Mode::Normal);


        let frp    = Events {input,position};
        let data   = CursorData {logger,frp,view,resize_handle};
        let data   = Rc::new(data);

        Cursor {data}

    }

//    /// Position setter.
//    pub fn set_position(&self, pos:Vector2<f32>) {
//        self.view.shape.position.set(pos);
//    }

    /// Selection size setter.
    pub fn set_selection_size(&self, pos:Vector2<f32>) {
        self.view.shape.selection_size.set(pos);
    }
}

impl StrongRef for Cursor {
    type WeakRef = WeakCursor;
    fn downgrade(&self) -> WeakCursor {
        WeakCursor {data:Rc::downgrade(&self.data)}
    }
}

impl WeakRef for WeakCursor {
    type StrongRef = Cursor;
    fn upgrade(&self) -> Option<Cursor> {
        self.data.upgrade().map(|data| Cursor{data})
    }
}

impl display::Object for Cursor {
    fn display_object(&self) -> &display::object::Instance {
        &self.view.display_object
    }
}
