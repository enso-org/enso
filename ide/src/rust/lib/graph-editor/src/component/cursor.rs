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
use ensogl::gui::component::animator;
use ensogl::gui::component;
use ensogl::system::web;



#[derive(Debug,Clone)]
pub struct StyleParam<T> {
    pub value   : T,
    pub animate : bool,
}

impl<T:Default> Default for StyleParam<T> {
    fn default() -> Self {
        let value   = default();
        let animate = true;
        Self {value,animate}
    }
}

impl<T> StyleParam<T> {
    pub fn new(value:T) -> Self {
        let animate = true;
        Self {value,animate}
    }

    pub fn new_no_animation(value:T) -> Self {
        let animate = false;
        Self {value,animate}
    }
}


#[derive(Debug,Clone,Default)]
pub struct Style {
    host   : Option<display::object::Instance>,
    size   : Option<StyleParam<Vector2<f32>>>,
    offset : Option<StyleParam<Vector2<f32>>>,
    color  : Option<StyleParam<color::Lcha>>,
    radius : Option<f32>,
    press  : Option<f32>,
}

impl Style {
    pub fn highlight<H>
    (host:H, size:Vector2<f32>, color:Option<color::Lcha>) -> Self
    where H:display::Object {
        let host  = Some(host.display_object().clone_ref());
        let size  = Some(StyleParam::new(size));
        let color = color.map(StyleParam::new);
        Self {host,size,color,..default()}
    }

    pub fn color(color:color::Lcha) -> Self {
        let color = Some(StyleParam::new(color));
        Self {color,..default()}
    }

    pub fn color_no_animation(color:color::Lcha) -> Self {
        let color = Some(StyleParam::new_no_animation(color));
        Self {color,..default()}
    }

    pub fn selection(size:Vector2<f32>) -> Self {
        let offset = Some(StyleParam::new_no_animation(-size / 2.0));
        let size   = Some(StyleParam::new_no_animation(size.abs() + Vector2::new(16.0,16.0)));
        Self {size,offset,..default()}
    }

    pub fn pressed() -> Self {
        let press = Some(1.0);
        Self {press,..default()}
    }

    pub fn press(mut self) -> Self {
        self.press = Some(1.0);
        self
    }
}

impl PartialSemigroup<&Style> for Style {
    #[allow(clippy::clone_on_copy)]
    fn concat_mut(&mut self, other:&Self) {
        if self.host   . is_none() { self.host   = other.host   . clone() }
        if self.size   . is_none() { self.size   = other.size   . clone() }
        if self.offset . is_none() { self.offset = other.offset . clone() }
        if self.color  . is_none() { self.color  = other.color  . clone() }
        if self.radius . is_none() { self.radius = other.radius . clone() }
        if self.press  . is_none() { self.press  = other.press  . clone() }
    }
}

impl PartialSemigroup<Style> for Style {
    fn concat_mut(&mut self, other:Self) {
        self.concat_mut(&other)
    }
}



// ==================
// === CursorView ===
// ==================

/// Canvas shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        ( position       : Vector2<f32>
        , dim            : V2
        , offset         : V2
        , selection_size : Vector2<f32>
        , press          : f32
        , radius         : f32
        , color          : Vector4<f32>
        ) {
            let press_diff       = 2.px() * &press;
            let radius           = 1.px() * radius - &press_diff;
            let offset : Var<V2<Distance<Pixels>>>           = offset.px();
            let selection_width  = 1.px() * &selection_size.x(); // * &press;
            let selection_height = 1.px() * &selection_size.y(); // * &press;
            let width            = (1.px() * &dim.x() - &press_diff * 2.0) + selection_width.abs();
            let height           = (1.px() * &dim.y() - &press_diff * 2.0) + selection_height.abs();
            let cursor = Rect((width,height))
                .corners_radius(radius)
                .translate(offset)
                .translate(("input_position.x","input_position.y"))
                .fill("srgba(input_color)");
            cursor.into()
        }
    }
}



// ===================
// === InputEvents ===
// ===================

/// Cursor events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct InputEvents {
    pub network   : frp::Network,
    pub set_style : frp::Source<Style>,
}

impl Default for InputEvents {
    fn default() -> Self {
        frp::new_network! { cursor_events
            def set_style = source();
        }
        let network = cursor_events;
        Self {network,set_style}
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

        let network = &input.network;

        let view_data = view.shape.clone_ref();
        let press = animation(network,move |value| {
            view_data.press.set(value)
        });

        let view_data = view.shape.clone_ref();
        let radius = animation(network,move |value| {
            view_data.radius.set(value)
        });

        let (size,current_size) = animator::<V2>(network);
        let (offset,current_offset) = animator::<V2>(network);

        let (anim_use_fixed_pos_setter,anim_use_fixed_pos) = animation2(network);
        let (anim_pos_x_setter,anim_pos_x) = animation2(network);
        let (anim_pos_y_setter,anim_pos_y) = animation2(network);

        let (anim_color_lab_l_setter,anim_color_lab_l) = animation2(network);
        let (anim_color_lab_a_setter,anim_color_lab_a) = animation2(network);
        let (anim_color_lab_b_setter,anim_color_lab_b) = animation2(network);
        let (anim_color_alpha_setter,anim_color_alpha) = animation2(network);


        anim_color_lab_l_setter.set_target_value(1.0);
        anim_color_alpha_setter.set_target_value(0.2);

        radius.set_target_value(8.0);
        size.set_target_value(V2::new(16.0,16.0));

        let mouse = &scene.mouse.frp;



        frp::extend! { network

            eval current_size ((v) view.shape.dim.set(*v));
            eval current_offset ((v) view.shape.offset.set(*v));

            def anim_position = anim_pos_x.all_with(&anim_pos_y,|x,y| frp::Position::new(*x,*y));

            anim_color <- all_with4(&anim_color_lab_l,&anim_color_lab_a,&anim_color_lab_b,&anim_color_alpha,
                |l,a,b,alpha| color::Rgba::from(color::Laba::new(*l,*a,*b,*alpha))
            );

            def _ev = input.set_style.map(enclose!((size,anim_pos_x_setter,anim_pos_y_setter) move |style| {
                match &style.press {
                    None    => press.set_target_value(0.0),
                    Some(t) => press.set_target_value(*t),
                }

                match &style.host {
                    None       => anim_use_fixed_pos_setter.set_target_value(0.0),
                    Some(host) => {
                        let position = host.global_position();
                        anim_pos_x_setter.set_target_value(position.x);
                        anim_pos_y_setter.set_target_value(position.y);
                        anim_use_fixed_pos_setter.set_target_value(1.0);
                    }
                }

                match &style.color {
                    None => {
                        anim_color_lab_l_setter.set_target_value(1.0);
                        anim_color_lab_a_setter.set_target_value(0.0);
                        anim_color_lab_b_setter.set_target_value(0.0);
                        anim_color_alpha_setter.set_target_value(0.2);
                    }
                    Some(new_color) => {
                        let lab = color::Laba::from(new_color.value);
                        anim_color_lab_l_setter.set_target_value(lab.lightness);
                        anim_color_lab_a_setter.set_target_value(lab.a);
                        anim_color_lab_b_setter.set_target_value(lab.b);
                        anim_color_alpha_setter.set_target_value(lab.alpha);
                        if !new_color.animate {
                            anim_color_lab_l_setter.skip();
                            anim_color_lab_a_setter.skip();
                            anim_color_lab_b_setter.skip();
                            anim_color_alpha_setter.skip();
                        }
                    }
                }

                match &style.size {
                    None => {
                        size.set_target_value(V2::new(16.0,16.0));
                    }
                    Some(new_size) => {
                        size.set_target_value(V2::new(new_size.value.x,new_size.value.y));
                        if !new_size.animate { size.skip() }
                    }
                }

                match &style.offset {
                    None => {
                        offset.set_target_value(V2::new(0.0,0.0));
                    }
                    Some(new_offset) => {
                        offset.set_target_value(V2::new(new_offset.value.x,new_offset.value.y));
                        if !new_offset.animate { offset.skip() }
                    }
                }

                match &style.radius {
                    None    => radius.set_target_value(8.0),
                    Some(r) => radius.set_target_value(*r),
                }
            }));

            def fixed_position = input.set_style.map(|style| style.host.as_ref().map(|t| t.global_position()));

            def uses_mouse_position = fixed_position.map(|p| p.is_none());
            def mouse_position = mouse.position.gate(&uses_mouse_position);

            def position = mouse.position.all_with3(&anim_position,&anim_use_fixed_pos, |p,ap,au| {
                let x = ap.x * au + p.x * (1.0 - au);
                let y = ap.y * au + p.y * (1.0 - au);
                frp::Position::new(x,y)
            });

            eval anim_color    ((t) view.shape.color.set(Vector4::new(t.red,t.green,t.blue,t.alpha)));
            eval position ((p) view.shape.position.set(Vector2::new(p.x,p.y)));

            def _position = mouse_position.map(f!([anim_pos_x_setter,anim_pos_y_setter](p) {
                anim_pos_x_setter.set_target_value(p.x);
                anim_pos_y_setter.set_target_value(p.y);
            }));
        }





        input.set_style.emit(Style::default());


        let frp    = Events {input,position};
        let data   = CursorData {logger,frp,view,resize_handle};
        let data   = Rc::new(data);

        Cursor {data}
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
