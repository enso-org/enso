//! Definition of the Cursor (known as well as mouse pointer) component.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::data::color;
use ensogl::display::Buffer;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::{Sprite, Attribute};
use ensogl::display;
use ensogl::gui::component::Animation;
use ensogl::gui::component::Tween;
use ensogl::gui::component;



// ==================
// === StyleValue ===
// ==================

/// Defines a value of the cursor style and also information if the value should be animated.
/// Sometimes disabling animation is required. A good example is the implementation of a selection
/// box. When drawing selection box with the mouse, the user wants to see it in real-time, without
/// it growing over time.
#[derive(Debug,Clone)]
#[allow(missing_docs)]
pub struct StyleValue<T> {
    pub value   : T,
    pub animate : bool,
}

impl<T:Default> Default for StyleValue<T> {
    fn default() -> Self {
        let value   = default();
        let animate = true;
        Self {value,animate}
    }
}

impl<T> StyleValue<T> {
    /// Constructor.
    pub fn new(value:T) -> Self {
        let animate = true;
        Self {value,animate}
    }

    /// Constructor with disabled animation.
    pub fn new_no_animation(value:T) -> Self {
        let animate = false;
        Self {value,animate}
    }
}



// =============
// === Style ===
// =============

macro_rules! define_style {( $($field:ident : $field_type:ty),* $(,)? ) => {
    /// Set of cursor style parameters. You can construct this object in FRP network, merge it using
    /// its `Semigroup` instance, and finally pass to the cursor to apply the style. Please note
    /// that cursor does not implement any complex style management (like pushing or popping a style
    /// from a style stack) on purpose, as it is stateful, while it is straightforward to implement
    /// it in FRP.
    #[derive(Debug,Clone,Default)]
    pub struct Style {
        $($field : $field_type),*
    }

    impl PartialSemigroup<&Style> for Style {
        #[allow(clippy::clone_on_copy)]
        fn concat_mut(&mut self, other:&Self) {
            $(if self.$field . is_none() { self.$field = other.$field . clone() })*
        }
    }

    impl PartialSemigroup<Style> for Style {
        fn concat_mut(&mut self, other:Self) {
            self.concat_mut(&other)
        }
    }
};}

define_style! {
    host   : Option<display::object::Instance>,
    size   : Option<StyleValue<Vector2<f32>>>,
    offset : Option<StyleValue<Vector2<f32>>>,
    color  : Option<StyleValue<color::Lcha>>,
    radius : Option<StyleValue<f32>>,
    press  : Option<StyleValue<f32>>,
}


// === Smart Constructors ===

#[allow(missing_docs)]
impl Style {
    pub fn new_highlight<H>
    (host:H, size:Vector2<f32>, color:Option<color::Lcha>) -> Self
    where H:display::Object {
        let host  = Some(host.display_object().clone_ref());
        let size  = Some(StyleValue::new(size));
        let color = color.map(StyleValue::new);
        Self {host,size,color,..default()}
    }

    pub fn new_color(color:color::Lcha) -> Self {
        let color = Some(StyleValue::new(color));
        Self {color,..default()}
    }

    pub fn new_color_no_animation(color:color::Lcha) -> Self {
        let color = Some(StyleValue::new_no_animation(color));
        Self {color,..default()}
    }

    pub fn new_box_selection(size:Vector2<f32>) -> Self {
        let offset = Some(StyleValue::new_no_animation(-size / 2.0));
        let size   = Some(StyleValue::new_no_animation(size.abs() + Vector2::new(16.0,16.0)));
        Self {size,offset,..default()}
    }

    pub fn new_press() -> Self {
        let press = Some(StyleValue::new(1.0));
        Self {press,..default()}
    }
}


// === Setters ===

#[allow(missing_docs)]
impl Style {
    pub fn press(mut self) -> Self {
        self.press = Some(StyleValue::new(1.0));
        self
    }
}


// === Getters ===

#[allow(missing_docs)]
impl Style {
    pub fn host_position(&self) -> Option<Vector3<f32>> {
        self.host.as_ref().map(|t| t.position())
    }
}



// ==================
// === CursorView ===
// ==================

/// Canvas shape definition.
pub mod shape {
    use super::*;
    ensogl::define_shape_system! {
        ( selection_size : Vector2<f32>
        , press          : f32
        , radius         : f32
        , color          : Vector4<f32>
        ) {
            let width  : Var<Distance<Pixels>> = "input_size.x".into();
            let height : Var<Distance<Pixels>> = "input_size.y".into();
            let press_diff       = 2.px() * &press;
            let radius           = 1.px() * radius - &press_diff;
            let selection_width  = 1.px() * &selection_size.x();
            let selection_height = 1.px() * &selection_size.y();
            let width            = (&width  - &press_diff * 2.0) + selection_width.abs();
            let height           = (&height - &press_diff * 2.0) + selection_height.abs();
            let cursor = Rect((width,height))
                .corners_radius(radius)
                .fill("srgba(input_color)");
            cursor.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

/// Cursor events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Frp {
    pub network  : frp::Network,
    pub input    : FrpInputs,
    pub position : frp::Stream<V3>,
}

impl Deref for Frp {
    type Target = FrpInputs;
    fn deref(&self) -> &Self::Target {
        &self.input
    }
}

/// Cursor events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct FrpInputs {
    pub set_style : frp::Source<Style>,
}

impl FrpInputs {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def set_style = source();
        }
        Self {set_style}
    }
}



// ===================
// === CursorModel ===
// ===================

/// Internal data for `Cursor`.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct CursorModel {
    pub logger : Logger,
    pub scene  : Scene,
    pub frp    : FrpInputs,
    pub view   : component::ShapeView<shape::Shape>,
    pub style  : Rc<RefCell<Style>>,
}

impl CursorModel {
    /// Constructor.
    pub fn new(scene:&Scene, network:&frp::Network) -> Self {
        let scene  = scene.clone_ref();
        let logger = Logger::new("cursor");
        let frp    = FrpInputs::new(network);
        let view   = component::ShapeView::<shape::Shape>::new(&logger,&scene);
        let style  = Rc::new(RefCell::new(Style::default()));

        let shape_system = scene.shapes.shape_system(PhantomData::<shape::Shape>);
        shape_system.shape_system.set_pointer_events(false);
        scene.views.main.remove(&shape_system.shape_system.symbol);
        scene.views.cursor.add(&shape_system.shape_system.symbol);

        Self {logger,scene,frp,view,style}
    }
}



// ==============
// === Cursor ===
// ==============

/// Cursor (mouse pointer) definition.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
#[allow(missing_docs)]
pub struct Cursor {
    #[shrinkwrap(main_field)]
    model   : Rc<CursorModel>,
    pub frp : Frp,
}

impl Cursor {
    /// Constructor.
    pub fn new(scene:&Scene) -> Self {
        let network = frp::Network::new();
        let model   = CursorModel::new(scene,&network);
        let input   = &model.frp;
        let mouse   = &scene.mouse.frp;

        // === Animations ===
        //
        // The following animators are used for smooth cursor transitions. There are two components
        // with a non-obvious behavior, namely the `host_follow_weight` and `host_attached_weight`.
        // The mouse position can be in three stages:
        //
        //   - Real-time cursor mode.
        //     Cursor follows the system mouse position.
        //
        //   - Host-follow mode.
        //     Cursor follows the host using dynamic simulator. The `host_follow_weight` variable is
        //     a weight between real-time mode and this one.
        //
        //   - Host-attached mode.
        //     Cursor follows the host without any delay. The `host_attached_weight` variable is a
        //     weight between host-follow mode and this one. The host-attached mode is especially
        //     useful when panning the stage in such way, that cursor starts to be attached to a
        //     host during the movement. After it is fully attached, cursor moves with the same
        //     speed as the scene when panning.
        //
        let press                = Animation :: <f32> :: new(&network);
        let radius               = Animation :: <f32> :: new(&network);
        let size                 = Animation :: <V2>  :: new(&network);
        let offset               = Animation :: <V2>  :: new(&network);
        let color_lab            = Animation :: <V3>  :: new(&network);
        let color_alpha          = Animation :: <f32> :: new(&network);
        let host_position        = Animation :: <V3>  :: new(&network);
        let host_follow_weight   = Animation :: <f32> :: new(&network);
        let host_attached_weight = Tween     :: new(&network);

        let default_size   = V2(16.0,16.0);
        let default_radius = 8.0;
        let default_lab    = V3(1.0,0.0,0.0);
        let default_alpha  = 0.2;

        host_attached_weight.set_duration(300.0);
        color_lab.set_target_value(default_lab);
        color_alpha.set_target_value(default_alpha);
        radius.set_target_value(default_radius);
        size.set_target_value(default_size);


        frp::extend! { network
            eval press.value  ((v) model.view.shape.press.set(*v));
            eval radius.value ((v) model.view.shape.radius.set(*v));
            eval size.value   ((v) model.view.shape.sprite.size().set(Vector2::new(v.x,v.y)));

            anim_color <- all_with(&color_lab.value,&color_alpha.value,
                |lab,alpha| color::Rgba::from(color::Laba::new(lab.x,lab.y,lab.z,*alpha))
            );

            eval input.set_style([host_attached_weight,size,offset,model] (new_style) {
                host_attached_weight.rewind();
                if new_style.host.is_some() { host_attached_weight.start() }

                match &new_style.press {
                    None => press.set_target_value(0.0),
                    Some(new_press) => {
                        press.set_target_value(new_press.value);
                        if !new_press.animate {
                            press.skip();
                        }
                    }
                }

                match &new_style.color {
                    None => {
                        color_lab.set_target_value(default_lab);
                        color_alpha.set_target_value(default_alpha);
                    }
                    Some(new_color) => {
                        let lab = color::Laba::from(new_color.value);
                        color_lab.set_target_value(V3(lab.lightness,lab.a,lab.b));
                        color_alpha.set_target_value(lab.alpha);
                        if !new_color.animate {
                            color_lab.skip();
                            color_alpha.skip();
                        }
                    }
                }

                match &new_style.size {
                    None => size.set_target_value(default_size),
                    Some(new_size) => {
                        size.set_target_value(V2::new(new_size.value.x,new_size.value.y));
                        if !new_size.animate { size.skip() }
                    }
                }

                match &new_style.offset {
                    None => offset.set_target_value(default()),
                    Some(new_offset) => {
                        offset.set_target_value(V2::new(new_offset.value.x,new_offset.value.y));
                        if !new_offset.animate { offset.skip() }
                    }
                }

                match &new_style.radius {
                    None => radius.set_target_value(default_radius),
                    Some(new_radius) => {
                        radius.set_target_value(new_radius.value);
                        if !new_radius.animate { radius.skip() }
                    }
                }

                *model.style.borrow_mut() = new_style.clone();
            });

            host_changed    <- any_(input.set_style,scene.frp.camera_changed);
            hosted_position <- host_changed.map(f_!(model.style.borrow().host_position()));
            is_not_hosted   <- hosted_position.map(|p| p.is_none());
            mouse_pos_rt    <- mouse.position.gate(&is_not_hosted);

            eval_ host_changed([model,host_position,host_follow_weight] {
                match &model.style.borrow().host {
                    None       => host_follow_weight.set_target_value(0.0),
                    Some(host) => {
                        host_follow_weight.set_target_value(1.0);
                        let m1          = model.scene.views.cursor.camera.inversed_view_matrix();
                        let m2          = model.scene.camera().view_matrix();
                        let position    = host.global_position();
                        let position    = Vector4::new(position.x,position.y,position.z,1.0);
                        let position    = m2 * (m1 * position);
                        host_position.set_target_value(V3(position.x,position.y,position.z));
                    }
                }
            });

            host_attached <- host_changed.all_with3
                (&host_attached_weight.value,&host_position.value, f!((_,weight,pos_anim) {
                    host_position.target_value() * weight + pos_anim * (1.0 - weight)
                })
            );

            position <- mouse.position.all_with4
                (&host_attached,&host_follow_weight.value,&offset.value,
                |pos_rt,pos_attached,weight,offset| {
                    let pos_rt : V3 = (V2(pos_rt.x,pos_rt.y) + *offset).into();
                    pos_attached * weight + pos_rt * (1.0 - weight)
                }
            );

            eval mouse_pos_rt ((t) host_position.set_target_value(V3(t.x,t.y,0.0)));
            eval anim_color   ((t) model.view.shape.color.set(t.into()));
            eval position     ((t) model.view.set_position(t.into()));
        }

        input.set_style.emit(Style::default());
        let input = input.clone_ref();
        let model = Rc::new(model);
        let frp   = Frp {network,input,position};
        Cursor {frp,model}
    }
}

impl display::Object for Cursor {
    fn display_object(&self) -> &display::object::Instance {
        &self.view.display_object
    }
}
