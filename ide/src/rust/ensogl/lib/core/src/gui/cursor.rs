//! Definition of the Cursor (known as well as mouse pointer) component.

use crate::prelude::*;

use crate::frp;
use crate::data::color;
use crate::display::scene::Scene;
use crate::display::shape::*;
use crate::display;
use crate::gui::component::Animation;
use crate::gui::component::Tween;
use crate::gui::component;



// =================
// === Constants ===
// =================

/// Default radius of the mouse cursor symbol.
pub const DEFAULT_RADIUS : f32 = 8.0;
const PADDING            : f32 = 2.0;
const SIDES_PADDING      : f32 = PADDING * 2.0;
const DEFAULT_COLOR      : color::Lcha = color::Lcha::new(0.7,0.0,0.0,0.5);
const TEXT_CURSOR_COLOR  : color::Lcha = color::Lcha::new(0.8,0.0,0.0,0.7);
const FADE_OUT_TIME      : f32 = 3000.0;

#[allow(non_snake_case)]
fn DEFAULT_SIZE() -> Vector2<f32> { Vector2(16.0,16.0) }



// ==================
// === StyleValue ===
// ==================

/// Defines a value of the cursor style.
#[derive(Debug,Clone)]
#[allow(missing_docs)]
pub struct StyleValue<T> {
    /// Defines the value of the style. In case it is set to `None`, the default value will be used.
    /// Please note that setting it to `None` has a different effect than not providing the value
    /// in the `Style` at all. If the value is provided it can override the existing values when
    /// used in a semigroup operation.
    pub value : Option<T>,

    /// Defines if the state transition should be used. Sometimes disabling animation is required.
    /// A good example is the implementation of a selection box. When drawing selection box with the
    /// mouse, the user wants to see it in real-time, without it growing over time.
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
        let value   = Some(value);
        let animate = true;
        Self {value,animate}
    }

    /// Constructor for a default value setter. Please note that this is not made a `Default` impl
    /// on purpose. This method creates a non-empty value setter which sets the target to its
    /// default value. Read `Style` docs to learn more.
    pub fn new_default() -> Self {
        let value   = None;
        let animate = true;
        Self {value,animate}
    }

    /// Constructor with disabled animation.
    pub fn new_no_animation(value:T) -> Self {
        let value   = Some(value);
        let animate = false;
        Self {value,animate}
    }
}



// =============
// === Style ===
// =============

macro_rules! define_style {( $( $(#$meta:tt)* $field:ident : $field_type:ty),* $(,)? ) => {
    /// Set of cursor style parameters. You can construct this object in FRP network, merge it using
    /// its `Semigroup` instance, and finally pass to the cursor to apply the style. Please note
    /// that cursor does not implement any complex style management (like pushing or popping a style
    /// from a style stack) on purpose, as it is stateful, while it is straightforward to implement
    /// it in FRP.
    #[derive(Debug,Clone,Default)]
    pub struct Style {
        $($(#$meta)? $field : Option<StyleValue<$field_type>>),*
    }

    impl Style {
        /// Create a new style with all fields set to default value. Please note that it is
        /// different than empty style, as this one overrides fields with default values when
        /// used in a semigroup operation.
        pub fn new_with_all_fields_default() -> Self {
            $(let $field = Some(StyleValue::new_default());)*
            Self {$($field),*}
        }
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
    /// Host defines an object which the cursor position is bound to. It is used to implement
    /// label selection. After setting the host to the label, cursor will not follow mouse anymore,
    /// it will inherit its position from the label instead.
    host   : display::object::Instance,
    size   : Vector2<f32>,
    offset : Vector2<f32>,
    color  : color::Lcha,
    radius : f32,
    press  : f32,
}


// === Smart Constructors ===

#[allow(missing_docs)]
impl Style {
    pub fn new_highlight<H,Color:Into<color::Lcha>>
    (host:H, size:Vector2<f32>, color:Option<Color>) -> Self
    where H:display::Object {
        let host  = Some(StyleValue::new(host.display_object().clone_ref()));
        let size  = Some(StyleValue::new(size));
        let color = color.map(|color|{
            let color = color.into();
            StyleValue::new(color)
        });
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

    pub fn new_press() -> Self {
        let press = Some(StyleValue::new(1.0));
        Self {press,..default()}
    }

    pub fn new_text_cursor() -> Self {
        let size  = Vector2::new(3.0,DEFAULT_SIZE().y);
        let size  = Some(StyleValue::new(size));
        let color = Some(StyleValue::new(TEXT_CURSOR_COLOR));
        Self {size,color,..default()}
    }
}


// === Setters ===

#[allow(missing_docs)]
impl Style {
    pub fn press(mut self) -> Self {
        self.press = Some(StyleValue::new(1.0));
        self
    }

    pub fn box_selection(mut self, size:Vector2<f32>) -> Self {
        let def_size = DEFAULT_SIZE();
        self.offset  = Some(StyleValue::new_no_animation(-size / 2.0));
        self.size    = Some(StyleValue::new_no_animation(size.abs() + def_size));
        self
    }
}


// === Getters ===

#[allow(missing_docs)]
impl Style {
    pub fn host_position(&self) -> Option<Vector3<f32>> {
        self.host.as_ref().and_then(|t| t.value.as_ref().map(|t| t.position()))
    }
}



// ==================
// === CursorView ===
// ==================

/// Canvas shape definition.
pub mod shape {
    use super::*;
    crate::define_shape_system! {
        ( press  : f32
        , radius : f32
        , color  : Vector4<f32>
        ) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let press_side_shrink = 2.px();
            let press_diff        = press_side_shrink * &press;
            let radius            = 1.px() * radius - &press_diff;
            let sides_padding     = 1.px() * SIDES_PADDING;
            let width             = &width  - &press_diff * 2.0 - &sides_padding;
            let height            = &height - &press_diff * 2.0 - &sides_padding;
            let cursor            = Rect((width,height)).corners_radius(radius);
            let cursor            = cursor.fill("srgba(input_color)");
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
    pub position : frp::Stream<Vector3>,
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
        let style  = default();

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
        // The following animators are used for smooth cursor transitions. There are two of them
        // with a non-obvious behavior, namely the `host_follow_weight` and `host_attached_weight`.
        // The mouse position is driven by three factors:
        //
        //   - Real-time cursor mode.
        //     Cursor follows the system mouse position.
        //
        //   - Host-follow mode.
        //     Cursor follows the host using dynamic inertia simulator. The `host_follow_weight`
        //     variable is a weight between real-time mode and this one.
        //
        //   - Host-attached mode.
        //     Cursor follows the host without any delay. The `host_attached_weight` variable is a
        //     weight between host-follow mode and this one. The host-attached mode is especially
        //     useful when panning the stage in such way, that cursor starts to be attached to a
        //     host during the movement. After it is fully attached, cursor moves with the same
        //     speed as the scene when panning.
        //
        let press                = Animation :: <f32>     :: new(&network);
        let radius               = Animation :: <f32>     :: new(&network);
        let size                 = Animation :: <Vector2> :: new(&network);
        let offset               = Animation :: <Vector2> :: new(&network);
        let color_lab            = Animation :: <Vector3> :: new(&network);
        let color_alpha          = Animation :: <f32>     :: new(&network);
        let inactive_fade        = Animation :: <f32>     :: new(&network);
        let host_position        = Animation :: <Vector3> :: new(&network);
        let host_follow_weight   = Animation :: <f32>     :: new(&network);
        let host_attached_weight = Tween     :: new(&network);

        host_attached_weight.set_duration(300.0);
        color_lab.set_target_value(DEFAULT_COLOR.opaque.into());
        color_alpha.set_target_value(DEFAULT_COLOR.alpha);
        radius.set_target_value(DEFAULT_RADIUS);
        size.set_target_value(DEFAULT_SIZE());

        let fade_out_spring = inactive_fade.spring() * 0.2;
        let fade_in_spring  = inactive_fade.spring();

        frp::extend! { network
            eval press.value  ((v) model.view.shape.press.set(*v));
            eval radius.value ((v) model.view.shape.radius.set(*v));
            eval size.value   ([model] (v) {
                let dim = Vector2(v.x+SIDES_PADDING,v.y+SIDES_PADDING);
                model.view.shape.sprite.size.set(dim);
            });

            alpha <- all_with(&color_alpha.value,&inactive_fade.value,|s,t| s*t);

            anim_color <- all_with(&color_lab.value,&alpha,
                |lab,alpha| color::Rgba::from(color::Laba::new(lab.x,lab.y,lab.z,*alpha))
            );

            eval input.set_style([host_attached_weight,size,offset,model] (new_style) {
                host_attached_weight.stop_and_rewind();
                if new_style.host.is_some() { host_attached_weight.start() }

                let def = 0.0;
                match &new_style.press {
                    None => press.set_target_value(def),
                    Some(t) => {
                        let value = t.value.unwrap_or(def);
                        press.set_target_value(value);
                        if !t.animate {
                            press.skip();
                        }
                    }
                }

                match &new_style.color {
                    None => {
                        color_lab.set_target_value(DEFAULT_COLOR.opaque.into());
                        color_alpha.set_target_value(DEFAULT_COLOR.alpha);
                    }
                    Some(t) => {
                        let value = t.value.unwrap_or(DEFAULT_COLOR);
                        let lab = color::Laba::from(value);
                        color_lab.set_target_value(Vector3::new(lab.lightness,lab.a,lab.b));
                        color_alpha.set_target_value(lab.alpha);
                        if !t.animate {
                            color_lab.skip();
                            color_alpha.skip();
                        }
                    }
                }

                match &new_style.size {
                    None => size.set_target_value(DEFAULT_SIZE()),
                    Some(t) => {
                        let value = t.value.unwrap_or_else(DEFAULT_SIZE);
                        size.set_target_value(Vector2(value.x,value.y));
                        if !t.animate { size.skip() }
                    }
                }

                match &new_style.offset {
                    None => offset.set_target_value(default()),
                    Some(t) => {
                        let value = t.value.unwrap_or_default();
                        offset.set_target_value(Vector2(value.x,value.y));
                        if !t.animate { offset.skip() }
                    }
                }

                match &new_style.radius {
                    None => radius.set_target_value(DEFAULT_RADIUS),
                    Some(t) => {
                        let value = t.value.unwrap_or(DEFAULT_RADIUS);
                        radius.set_target_value(value);
                        if !t.animate { radius.skip() }
                    }
                }

                *model.style.borrow_mut() = new_style.clone();
            });

            host_changed    <- any_(input.set_style,scene.frp.camera_changed);
            hosted_position <- host_changed.map(f_!(model.style.borrow().host_position()));
            is_not_hosted   <- hosted_position.map(|p| p.is_none());
            mouse_pos_rt    <- mouse.position.gate(&is_not_hosted);

            eval_ host_changed([model,host_position,host_follow_weight] {
                match model.style.borrow().host.as_ref().and_then(|t|t.value.as_ref()) {
                    None       => host_follow_weight.set_target_value(0.0),
                    Some(host) => {
                        host_follow_weight.set_target_value(1.0);
                        let m1       = model.scene.views.cursor.camera.inversed_view_matrix();
                        let m2       = model.scene.camera().view_matrix();
                        let position = host.global_position();
                        let position = Vector4::new(position.x,position.y,position.z,1.0);
                        let position = m2 * (m1 * position);
                        host_position.set_target_value(Vector3(position.x,position.y,position.z));
                    }
                }
            });

            host_attached <- host_changed.all_with3
                (&host_attached_weight.value,&host_position.value, f!((_,weight,pos_anim) {
                    host_position.target_value() * *weight + pos_anim * (1.0 - weight)
                })
            );

            position <- mouse.position.all_with4
                (&host_attached,&host_follow_weight.value,&offset.value,
                |pos_rt,pos_attached,weight,offset| {
                    let pos_rt = Vector2(pos_rt.x,pos_rt.y) + *offset;
                    let pos_rt = Vector3(pos_rt.x,pos_rt.y,0.0);
                    *pos_attached * *weight + pos_rt * (1.0 - weight)
                }
            );


            // === Fade-out when not moved ===

            move_time            <- scene.frp.frame_time.sample(&mouse.position);
            time_since_last_move <- scene.frp.frame_time.map2(&move_time,|t,s|t-s);
            check_fade_time      <- time_since_last_move.gate(&is_not_hosted).gate(&mouse.ever_moved);
            eval check_fade_time ([inactive_fade](time) {
                if *time > FADE_OUT_TIME {
                    inactive_fade.set_spring(fade_out_spring);
                    inactive_fade.set_target_value(0.0)
                } else {
                    inactive_fade.set_spring(fade_in_spring);
                    inactive_fade.set_target_value(1.0)
                }
            });


            // === Evals ===

            eval mouse_pos_rt ((t) host_position.set_target_value(Vector3(t.x,t.y,0.0)));
            eval anim_color   ((t) model.view.shape.color.set(t.into()));
            eval position     ((t) model.view.set_position(*t));
        }

        // Hide on init.
        inactive_fade.set_target_value(0.0);
        inactive_fade.skip();

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
