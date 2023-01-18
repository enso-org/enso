//! Definition of the Cursor (known as well as mouse pointer) component.

use crate::display::shape::*;
use crate::gui::style::*;
use crate::prelude::*;

use crate::application::command::FrpNetworkProvider;
use crate::data::color;
use crate::define_style;
use crate::display;
use crate::display::scene::Scene;
use crate::frp;
use crate::Animation;
use crate::DEPRECATED_Animation;
use crate::Easing;



// =================
// === Constants ===
// =================

/// Default radius of the mouse cursor symbol.
pub const DEFAULT_RADIUS: f32 = 8.0;
const PADDING: f32 = 2.0;
/// Sum of the padding at both sides of the cursor shape.
pub const SIDES_PADDING: f32 = PADDING * 2.0;
const DEFAULT_COLOR: color::Lcha = color::Lcha::new(0.7, 0.0, 0.0, 0.5);
const TEXT_CURSOR_COLOR: color::Lcha = color::Lcha::new(0.8, 0.0, 0.0, 0.7);
const FADE_OUT_TIME: f32 = 3000.0;

#[allow(non_snake_case)]
fn DEFAULT_SIZE() -> Vector2<f32> {
    Vector2(16.0, 16.0)
}



// =============
// === Style ===
// =============

define_style! {
    /// Host defines an object which the cursor position is bound to. It is used to implement
    /// label selection. After setting the host to the label, cursor will not follow mouse anymore,
    /// it will inherit its position from the label instead.
    host                 : display::object::Instance,
    size                 : Vector2<f32>,
    offset               : Vector2<f32>,
    color                : color::Lcha,
    radius               : f32,
    press                : f32,
    port_selection_layer : bool
}


// === Smart Constructors ===

#[allow(missing_docs)]
impl Style {
    pub fn new_highlight<H, Color: Into<color::Lcha>>(
        host: H,
        size: Vector2<f32>,
        color: Option<Color>,
    ) -> Self
    where
        H: display::Object,
    {
        let host = Some(StyleValue::new(host.display_object().clone_ref()));
        let size = Some(StyleValue::new(size));
        let color = color.map(|color| {
            let color = color.into();
            StyleValue::new(color)
        });
        let port_selection_layer = Some(StyleValue::new_no_animation(true));
        Self { host, size, color, port_selection_layer, ..default() }
    }

    pub fn new_color(color: color::Lcha) -> Self {
        let color = Some(StyleValue::new(color));
        Self { color, ..default() }
    }

    pub fn new_color_no_animation(color: color::Lcha) -> Self {
        let color = Some(StyleValue::new_no_animation(color));
        Self { color, ..default() }
    }

    pub fn new_press() -> Self {
        let press = Some(StyleValue::new(1.0));
        Self { press, ..default() }
    }

    pub fn cursor() -> Self {
        let size = Vector2::new(3.0, DEFAULT_SIZE().y);
        let size = Some(StyleValue::new(size));
        let color = Some(StyleValue::new(TEXT_CURSOR_COLOR));
        Self { size, color, ..default() }
    }
}


// === Setters ===

#[allow(missing_docs)]
impl Style {
    pub fn press(mut self) -> Self {
        self.press = Some(StyleValue::new(1.0));
        self
    }

    pub fn box_selection(mut self, size: Vector2<f32>) -> Self {
        let def_size = DEFAULT_SIZE();
        self.offset = Some(StyleValue::new_no_animation(-size / 2.0));
        self.size = Some(StyleValue::new_no_animation(size.abs() + def_size));
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
    crate::shape! {
        pointer_events = false;
        ( style  : Style
        , press  : f32
        , radius : f32
        , color  : Vector4
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

crate::define_endpoints_2! {
    Input {
        set_style (Style),
    }

    Output {
        position              (Vector3),
        screen_position       (Vector3),
        scene_position        (Vector3),
         /// Change between the current and the previous scene position.
        scene_position_delta  (Vector3),
    }
}



// ===================
// === CursorModel ===
// ===================

/// Internal data for `Cursor`.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct CursorModel {
    pub logger:         Logger,
    pub scene:          Scene,
    pub display_object: display::object::Instance,
    pub view:           shape::View,
    pub port_selection: shape::View,
    pub style:          Rc<RefCell<Style>>,
}

impl CursorModel {
    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        let scene = scene.clone_ref();
        let logger = Logger::new("cursor");
        let display_object = display::object::Instance::new();
        let view = shape::View::new();
        let port_selection = shape::View::new();
        let style = default();

        display_object.add_child(&view);
        display_object.add_child(&port_selection);
        let tgt_layer = &scene.layers.cursor;
        let port_selection_layer = &scene.layers.port_selection;
        tgt_layer.add(&view);
        port_selection_layer.add(&port_selection);

        Self { logger, scene, display_object, view, port_selection, style }
    }

    fn for_each_view(&self, f: impl Fn(&shape::View)) {
        for view in &[&self.view, &self.port_selection] {
            f(view)
        }
    }
}



// ==============
// === Cursor ===
// ==============

/// Cursor (mouse pointer) definition.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Cursor {
    pub frp: Frp,
    model:   Rc<CursorModel>,
}

impl Cursor {
    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let model = CursorModel::new(scene);
        let mouse = &scene.mouse.frp;

        // === Animations ===
        //
        // The following animators are used for smooth cursor transitions. There are two of them
        // with a non-obvious behavior, namely the `host_follow_weight` and `host_attached_weight`.
        // The mouse position is driven by three factors:
        //
        //   - Real-time cursor mode. Cursor follows the system mouse position.
        //
        //   - Host-follow mode. Cursor follows the host using dynamic inertia simulator. The
        //     `host_follow_weight` variable is a weight between real-time mode and this one.
        //
        //   - Host-attached mode. Cursor follows the host without any delay. The
        //     `host_attached_weight` variable is a weight between host-follow mode and this one.
        //     The host-attached mode is especially useful when panning the stage in such way, that
        //     cursor starts to be attached to a host during the movement. After it is fully
        //     attached, cursor moves with the same speed as the scene when panning.
        //
        let press = Animation::<f32>::new(network);
        let radius = DEPRECATED_Animation::<f32>::new(network);
        let size = DEPRECATED_Animation::<Vector2>::new(network);
        let offset = DEPRECATED_Animation::<Vector2>::new(network);
        let color_lab = DEPRECATED_Animation::<Vector3>::new(network);
        let color_alpha = DEPRECATED_Animation::<f32>::new(network);
        let inactive_fade = DEPRECATED_Animation::<f32>::new(network);
        let host_position = DEPRECATED_Animation::<Vector3>::new(network);
        let host_follow_weight = DEPRECATED_Animation::<f32>::new(network);
        let host_attached_weight = Easing::new(network);
        let port_selection_layer_weight = Animation::<f32>::new(network);

        host_attached_weight.set_duration(300.0);
        color_lab.set_target_value(DEFAULT_COLOR.opaque.into());
        color_alpha.set_target_value(DEFAULT_COLOR.alpha);
        radius.set_target_value(DEFAULT_RADIUS);
        size.set_target_value(DEFAULT_SIZE());

        let fade_out_spring = inactive_fade.spring() * 0.2;
        let fade_in_spring = inactive_fade.spring();

        frp::extend! { network
            eval press.value  ((v) model.for_each_view(|vw| vw.press.set(*v)));
            eval radius.value ((v) model.for_each_view(|vw| vw.radius.set(*v)));
            eval size.value   ([model] (v) {
                let dim = Vector2(v.x+SIDES_PADDING,v.y+SIDES_PADDING);
                model.for_each_view(|vw| {vw.set_size(dim);});
            });

            alpha <- all_with(&color_alpha.value,&inactive_fade.value,|s,t| s*t);

            anim_color <- all_with(&color_lab.value,&alpha,
                |lab,alpha| color::Rgba::from(color::Laba::new(lab.x,lab.y,lab.z,*alpha))
            );
            front_color <- all_with(&anim_color,&port_selection_layer_weight.value, |color,w| {
                color::Rgba::new(color.red,color.green,color.blue,color.alpha*(1.0 - w))
            });
            port_selection_color <- all_with(&anim_color,&port_selection_layer_weight.value, |color,w| {
                color::Rgba::new(color.red,color.green,color.blue,color.alpha*w)
            });

            eval frp.set_style([host_attached_weight,size,offset,model] (new_style) {
                host_attached_weight.stop_and_rewind(0.0);
                if new_style.host.is_some() { host_attached_weight.target(1.0) }

                let def = 0.0;
                match &new_style.press {
                    None => press.target.emit(def),
                    Some(t) => {
                        let value = t.value.unwrap_or(def);
                        press.target.emit(value);
                        if !t.animate {
                            press.skip.emit(());
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

            port_selection_layer_weight.target <+ frp.set_style.map(|new_style| {
                let val_opt = new_style.port_selection_layer.as_ref().and_then(|t| t.value);
                let val     = val_opt.unwrap_or(false);
                if val {1.0} else {0.0}
            });
            port_selection_layer_weight.skip <+ frp.set_style.filter(|new_style|
                new_style.port_selection_layer.as_ref().map_or(false, |t| !t.animate)
            ).constant(());

            host_changed    <- any_(frp.set_style,scene.frp.camera_changed);
            hosted_position <- host_changed.map(f_!(model.style.borrow().host_position()));
            is_not_hosted   <- hosted_position.map(|p| p.is_none());
            mouse_pos_rt    <- mouse.position.gate(&is_not_hosted);

            eval_ host_changed([model,host_position,host_follow_weight] {
                match model.style.borrow().host.as_ref().and_then(|t|t.value.as_ref()) {
                    None       => host_follow_weight.set_target_value(0.0),
                    Some(host) => {
                        host_follow_weight.set_target_value(1.0);
                        let m1       = model.scene.layers.cursor.camera().inversed_view_matrix();
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

            // Simple Tales equation applied to compute screen position.
            //
            // ◄───►  target_x
            //    ◄►  position.x
            // ─────┐ z = 0
            //  ╲   │
            //   ╲──┤ z = position.z
            //    ╲ │
            //     ╲│ z = camera.z
            screen_position <- position.map(f!([model](position) {
                let cam_pos = model.scene.layers.cursor.camera().position();
                let coeff   = cam_pos.z / (cam_pos.z - position.z);
                let x       = position.x * coeff;
                let y       = position.y * coeff;
                Vector3(x,y,0.0)
            }));

            scene_position       <- screen_position.map(f!((p) scene.screen_to_scene_coordinates(*p)));
            scene_position_prev  <- scene_position.previous();
            scene_position_delta <- scene_position.map2(&scene_position_prev, |p1,p2| p2 - p1);

            // === Fade-out when not active ===

            action_event           <- any_(&mouse.position,&frp.set_style);
            action_time            <- scene.frp.frame_time.sample(&action_event);
            time_since_last_action <- scene.frp.frame_time.map2(&action_time,|t,s|t-s);
            check_fade_time        <- time_since_last_action.gate(&mouse.ever_moved);
            _eval <- check_fade_time.map2(&frp.set_style, f!([inactive_fade](time,style) {
                if *time > FADE_OUT_TIME && style.is_default() {
                    inactive_fade.set_spring(fade_out_spring);
                    inactive_fade.set_target_value(0.0)
                } else {
                    inactive_fade.set_spring(fade_in_spring);
                    inactive_fade.set_target_value(1.0)
                }
            }));


            // === Evals ===

            eval mouse_pos_rt         ((t) host_position.set_target_value(Vector3(t.x,t.y,0.0)));
            eval position             ((t) model.display_object.set_position(*t));
            eval front_color          ((t) model.view.color.set(t.into()));
            eval port_selection_color ((t) model.port_selection.color.set(t.into()));


            // === Outputs ===

            frp.private.output.position             <+ position;
            frp.private.output.screen_position      <+ screen_position;
            frp.private.output.scene_position       <+ scene_position;
            frp.private.output.scene_position_delta <+ scene_position_delta;
        }

        // Hide on init.
        inactive_fade.set_target_value(0.0);
        inactive_fade.skip();

        frp.set_style.emit(Style::default());
        let model = Rc::new(model);
        Cursor { frp, model }
    }
}

impl display::Object for Cursor {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
