//! Definition of the Cursor (known as well as mouse pointer) component.

use crate::display::shape::*;
use crate::gui::style::*;
use crate::prelude::*;

use crate::application::command::FrpNetworkProvider;
use crate::control::io::mouse;
use crate::data::color;
use crate::define_style;
use crate::display;
use crate::display::scene::Scene;
use crate::frp;
use crate::Animation;
use crate::DEPRECATED_Animation;
use crate::Easing;

use std::f32::consts::PI;



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
    host: display::object::Instance,
    pointer_events: bool,
    size: Vector2<f32>,
    offset: Vector2<f32>,
    color: color::Lcha,
    radius: f32,
    press: f32,
    port_selection_layer : bool,
    trash: f32,
    plus: f32,
}


// === Smart Constructors ===

#[allow(missing_docs)]
impl Style {
    pub fn new_highlight(host: display::object::Instance, size: Vector2<f32>, radius: f32) -> Self {
        let host = Some(StyleValue::new(host));
        let size = Some(StyleValue::new(size));
        let radius = Some(StyleValue::new(radius));
        let press = Some(StyleValue::new(0.0));
        let port_selection_layer = Some(StyleValue::new_no_animation(true));
        Self { host, size, radius, port_selection_layer, press, ..default() }
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

    /// Set the cursor to a trash icon, a red circle with "x" inside of it.
    pub fn trash() -> Self {
        let trash = Some(StyleValue::new(1.0));
        Self { trash, ..default() }
    }

    pub fn plus() -> Self {
        let plus = Some(StyleValue::new(1.0));
        Self { plus, ..default() }
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
        self.pointer_events = Some(StyleValue::new_no_animation(true));
        self
    }
}



// ==================
// === CursorView ===
// ==================

/// Canvas shape definition.
pub mod shape {
    use super::*;
    crate::shape! {
        pointer_events_instanced = true;
        alignment = center; (
            style: Style,
            press: f32,
            radius: f32,
            color: Vector4,
            trash: f32,
            plus: f32,
        ) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let press_side_shrink = 2.px();
            let press_diff        = press_side_shrink * &press;
            let radius            = radius.px() - &press_diff;
            let sides_padding     = SIDES_PADDING.px();
            let width             = &width  - &press_diff * 2.0 - &sides_padding;
            let height            = &height - &press_diff * 2.0 - &sides_padding;
            let cursor            = Rect((&width,&height)).corners_radius(radius);

            let color: Var<color::Rgba> = color.into();
            let trash_color: Var<color::Rgba> = color::Rgba::new(0.91, 0.32, 0.32, 1.0).into();
            let color = color.mix(&trash_color, &trash);

            let plus_color: Var<color::Rgba> = color::Rgba::new(0.39, 0.71, 0.15, 1.0).into();
            let color = color.mix(&plus_color, &plus);


            let cursor            = cursor.fill(color);

            let trash_bar1 = Rect((2.px(), (&height - 4.px()) * &trash - 1.px()));
            let trash_bar2 = trash_bar1.rotate((PI/2.0).radians());
            let trash_bar_x = (trash_bar1 + trash_bar2).rotate((PI/4.0).radians());
            let trash_bar_x = trash_bar_x.fill(color::Rgba::new(1.0,1.0,1.0,0.8));

            let plus_bar1 = Rect((2.px(), (&height - 4.px()) * &plus - 1.px()));
            let plus_bar2 = plus_bar1.rotate((PI/2.0).radians());
            let plus_sign = plus_bar1 + plus_bar2;
            let plus_sign = plus_sign.fill(color::Rgba::new(1.0,1.0,1.0,0.8));

            let cursor = cursor + trash_bar_x + plus_sign;
            cursor.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

crate::define_endpoints_2! {
    Input {
        set_style_override (Option<Style>),
        set_style (Style),
        switch_drag_target((DragTarget, bool)),
    }

    Output {
        position              (Vector3),
        screen_position       (Vector3),
        scene_position        (Vector3),
         /// Change between the current and the previous scene position.
        scene_position_delta  (Vector3),
        start_drag(),
        stop_drag(),
        is_dragging(bool),
    }
}



// ===================
// === CursorModel ===
// ===================

/// Internal data for `Cursor`.
#[derive(Clone, CloneRef, Debug, display::Object)]
#[allow(missing_docs)]
pub struct CursorModel {
    pub scene:          Scene,
    pub display_object: display::object::Instance,
    pub dragged_elem:   display::object::Instance,
    pub view:           shape::View,
    pub port_selection: shape::View,
    pub style:          Rc<RefCell<Style>>,
    pub dragged_item:   Rc<RefCell<Option<(Box<dyn Any>, display::object::Instance)>>>,
    frp:                WeakFrp,
}

impl CursorModel {
    /// Constructor.
    pub fn new(scene: &Scene, frp: WeakFrp) -> Self {
        let scene = scene.clone_ref();
        let display_object = display::object::Instance::new_no_debug();
        let dragged_elem = display::object::Instance::new_named("dragged_elem");
        let view = shape::View::new();
        let port_selection = shape::View::new();
        let style = default();

        display_object.add_child(&view);
        display_object.add_child(&port_selection);
        scene.add_child(&dragged_elem);
        let tgt_layer = &scene.layers.cursor;
        let port_selection_layer = &scene.layers.port_selection;
        tgt_layer.add(&view);
        port_selection_layer.add(&port_selection);

        let dragged_item = default();
        Self { scene, display_object, dragged_elem, view, port_selection, style, dragged_item, frp }
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
#[derive(Clone, CloneRef, Debug, Deref, display::Object)]
#[allow(missing_docs)]
pub struct Cursor {
    pub frp:   Frp,
    #[deref]
    #[display_object]
    pub model: Rc<CursorModel>,
}

impl Cursor {
    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let model = CursorModel::new(scene, frp.downgrade());
        let mouse = &scene.mouse.frp_deprecated;

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
        let trash = Animation::<f32>::new(network);
        let plus = Animation::<f32>::new(network);

        host_attached_weight.set_duration(300.0);
        color_lab.set_target_value(DEFAULT_COLOR.opaque.into());
        color_alpha.set_target_value(DEFAULT_COLOR.alpha);
        radius.set_target_value(DEFAULT_RADIUS);
        size.set_target_value(DEFAULT_SIZE());

        let fade_out_spring = inactive_fade.spring() * 0.2;
        let fade_in_spring = inactive_fade.spring();

        let on_up = scene.on_event::<mouse::Up>();

        frp::extend! { network

            // === Drag Target ===

            drag_target <- any_mut::<Option<DragTarget>>();
            drag_target <+ frp.switch_drag_target.map2(&drag_target,
                |(target, enabled), prev| {
                    if let Some(prev) = prev.as_ref() {
                        let new_target = *enabled && target != prev;
                        let revoke_target = !enabled && target == prev;
                        if new_target {
                            prev.revoke.emit(());
                            target.grant.emit(());
                            Some(target.clone())
                        } else if revoke_target {
                            prev.revoke.emit(());
                            None
                        } else {
                            Some(target.clone())
                        }
                    } else {
                        target.grant.emit(());
                        Some(target.clone())
                    }
                }
            );

            has_drag_target <- drag_target.map(|t| t.is_some()).on_change();
            should_trash <- has_drag_target.map(f!([model] (has_drag_target) {
                model.dragged_item.borrow().is_some() && !has_drag_target
            }));
            frp.set_style_override <+ should_trash.then_constant(Style::trash());
            perform_trash <- on_up.gate(&should_trash);
            frp.set_style_override <+ perform_trash.constant(None);
            eval_ perform_trash (model.trash_dragged_item());



            // === Press / Release ===

            eval press.value  ((v) model.for_each_view(|vw| vw.press.set(*v)));
            eval radius.value ((v) model.for_each_view(|vw| vw.radius.set(*v)));
            eval size.value   ([model] (v) {
                let dim = Vector2(v.x+SIDES_PADDING,v.y+SIDES_PADDING);
                model.for_each_view(|vw| {vw.set_size(dim);});
            });
            eval trash.value ((v) model.for_each_view(|vw| vw.trash.set(*v)));
            eval plus.value ((v) model.for_each_view(|vw| vw.plus.set(*v)));

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

            style <- frp.set_style_override.unwrap_or(&frp.set_style);
            eval style([host_attached_weight,size,offset,model] (new_style) {
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

                match &new_style.trash {
                    None => trash.target.emit(0.0),
                    Some(t) => trash.target.emit(t.value.unwrap_or(0.0)),
                }

                match &new_style.plus {
                    None => plus.target.emit(0.0),
                    Some(t) => plus.target.emit(t.value.unwrap_or(0.0)),
                }

                let pointer_events = match &new_style.pointer_events {
                    None => false,
                    Some(t) => t.value.unwrap_or(false),
                };
                let disable_pointer_events = (!pointer_events) as i32 as f32;
                model.for_each_view(|vw| vw.disable_pointer_events.set(disable_pointer_events));

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
            is_not_hosted   <- host_changed.map(f!((_) model.style.borrow().host.is_none()));
            mouse_pos_rt    <- mouse.position.gate(&is_not_hosted);
            host_moved      <- host_changed.flat_map(f!([model] (_) {
                match model.style.borrow().host.as_ref().and_then(|t|t.value.as_ref()) {
                    None       => frp::Source::<()>::new().into(),
                    Some(host) => host.on_transformed.clone(),
                }
            }));

            host_needs_update <- any(host_moved, host_changed);
            eval_ host_needs_update([model,host_position,host_follow_weight] {
                match model.style.borrow().host.as_ref().and_then(|t|t.value.as_ref()) {
                    None       => host_follow_weight.set_target_value(0.0),
                    Some(host) => {
                        host_follow_weight.set_target_value(1.0);
                        let m1       = model.scene.layers.cursor.camera().inversed_view_matrix();
                        let m2       = model.scene.camera().view_matrix();
                        let position = host.global_position();
                        let size = host.computed_size();
                        let position = Vector4::new(
                            position.x + size.x * 0.5,
                            position.y + size.y * 0.5,
                            position.z,
                            1.0
                        );
                        let position = m2 * (m1 * position);
                        host_position.set_target_value(position.xyz());
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
                let z_diff = cam_pos.z - position.z;
                if z_diff == 0.0 {
                    Vector3::zero()
                } else {
                    let coeff = cam_pos.z / z_diff;
                    (position.xy() * coeff).push(0.0)
                }
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
            eval_ screen_position     (model.update_drag_position());

            // === Outputs ===

            frp.private.output.position             <+ position;
            frp.private.output.screen_position      <+ screen_position;
            frp.private.output.scene_position       <+ scene_position;
            frp.private.output.scene_position_delta <+ scene_position_delta;
            frp.private.output.is_dragging <+ bool(&frp.stop_drag, &frp.start_drag);
        }

        // Hide on init.
        inactive_fade.set_target_value(0.0);
        inactive_fade.skip();

        frp.set_style.emit(Style::default());
        let model = Rc::new(model);
        Cursor { frp, model }
    }
}

impl CursorModel {
    /// Initialize item dragging. The provided item should implement [`display::Object`]. It will be
    /// stored in the cursor and moved around with it. You can retrieve the item back using the
    /// [`Self::stop_drag`] method or another similar one.
    pub fn start_drag<T: display::Object + 'static>(&self, target: T) {
        if self.dragged_item.borrow().is_some() {
            warn!("Can't start dragging an item because another item is already being dragged.");
        } else {
            let object = target.display_object().clone();

            if let Some(object_layer) = object.display_layer() {
                object_layer.add(&self.dragged_elem);
            }
            let scene = scene();
            let screen_pos = self.frp.screen_position.value().xy();
            let offset = scene.screen_to_object_space(&object, screen_pos);
            object.set_xy(-offset);
            self.dragged_elem.add_child(&object);

            *self.dragged_item.borrow_mut() = Some((Box::new(target), object));

            self.frp.private.output.start_drag.emit(());
        }
    }

    fn update_drag_position(&self) {
        if self.dragged_item.borrow().is_some() {
            let scene = scene();
            let layer = self.dragged_elem.display_layer();
            let camera = layer.map_or(scene.camera(), |l| l.camera());

            let screen_pos = self.frp.screen_position.value().xy() / camera.zoom();
            let pos_in_layer = camera.inversed_view_matrix() * screen_pos.push(0.0).push(1.0);
            self.dragged_elem.set_xy(pos_in_layer.xy());
        }
    }

    /// Remove the dragged item and return it as [`Any`]. If you want to retrieve the item if it is
    /// of a particular type, use the [`Self::stop_drag_if_is`] method instead.
    pub fn stop_drag(&self) -> Option<Box<dyn Any>> {
        let item_and_object = mem::take(&mut *self.dragged_item.borrow_mut());
        if let Some((item, _)) = item_and_object {
            self.stop_drag_internal();
            Some(item)
        } else {
            None
        }
    }

    /// Check whether the dragged item is of a particular type. If it is, remove and return it.
    pub fn stop_drag_if_is<T: 'static>(&self) -> Option<T> {
        let item_and_object = mem::take(&mut *self.dragged_item.borrow_mut());
        if let Some((item, obj)) = item_and_object {
            match item.downcast::<T>() {
                Ok(item) => {
                    self.stop_drag_internal();
                    Some(*item)
                }
                Err(item) => {
                    *self.dragged_item.borrow_mut() = Some((item, obj));
                    None
                }
            }
        } else {
            None
        }
    }

    fn stop_drag_internal(&self) {
        let elems = self.dragged_elem.remove_all_children();
        let cursor_position = self.frp.scene_position.value().xy();
        for elem in &elems {
            elem.update_xy(|t| t + cursor_position);
        }
        self.frp.private.output.stop_drag.emit(());
    }

    /// The display object of the dragged item, if any.
    pub fn dragged_display_object(&self) -> Option<display::object::Instance> {
        self.dragged_item.borrow().as_ref().map(|t| t.1.clone())
    }

    /// Check whether the dragged item is of a particular type.
    pub fn dragged_item_is<T: 'static>(&self) -> bool {
        self.dragged_item.borrow().as_ref().map(|item| item.0.is::<T>()).unwrap_or(false)
    }

    /// Check whether the dragged item is of a particular type. If it is, call the provided function
    /// on it's reference.
    pub fn with_dragged_item_if_is<T, Out>(&self, f: impl FnOnce(&T) -> Out) -> Option<Out>
    where T: 'static {
        self.dragged_item.borrow().as_ref().and_then(|item| item.0.downcast_ref::<T>().map(f))
    }

    /// Trash the dragged item.
    pub fn trash_dragged_item(&self) {
        let obj = self.dragged_display_object();
        if let Some(obj) = obj {
            self.stop_drag();
            self.dragged_elem.add_child(&Trash::new(obj));
        }
    }
}



// ==================
// === DragTarget ===
// ==================

/// Abstraction for display elements that can handle dragged item drop.
///
/// If a display element wants to handle dragged item, for example after the mouse hovers it, it
/// should have an instance of this struct and use the [`Cursor::switch_drag_target`] FRP endpoint
/// to notify the cursor that it wants to handle the drop. Only one drag target can be registered
/// globally at a time. If your drag target was granted the permission to handle the drop, the
/// [`Self::granted`] event will be set to `true`. In case another drag target was granted the
/// permission, your drag target's [`Self::granted`] event will turn false.
#[derive(Debug, Clone, CloneRef, Deref, Default)]
pub struct DragTarget {
    model: Rc<DragTargetModel>,
}

impl DragTarget {
    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }
}

impl PartialEq for DragTarget {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.model, &other.model)
    }
}

/// Internal representation of [`DragTarget`].
#[allow(missing_docs)]
#[derive(Debug)]
pub struct DragTargetModel {
    network:     frp::Network,
    grant:       frp::Source,
    revoke:      frp::Source,
    pub granted: frp::Sampler<bool>,
}

impl DragTargetModel {
    /// Constructor.
    pub fn new() -> Self {
        let network = frp::Network::new("DragTarget");
        frp::extend! { network
            grant <- source();
            revoke <- source();
            granted <- bool(&revoke, &grant).sampler();
        }
        DragTargetModel { network, grant, revoke, granted }
    }
}

impl Default for DragTargetModel {
    fn default() -> Self {
        Self::new()
    }
}



// =============
// === Trash ===
// =============

mod trash {
    use super::*;
    crate::define_endpoints_2! {}

    /// A wrapper over any display object. After construction, the display object will be gradually
    /// scaled to zero and then will be removed.
    #[derive(Debug, CloneRef, Derivative)]
    #[derivative(Clone(bound = ""))]
    pub struct Trash<T> {
        model: Rc<TrashModel<T>>,
    }

    /// Internal representation of [`Trash`].
    #[derive(Debug)]
    pub struct TrashModel<T> {
        _frp: Frp,
        elem: T,
    }

    impl<T: display::Object + 'static> Trash<T> {
        /// Constructor.
        pub fn new(elem: T) -> Self {
            let self_ref = Rc::new(RefCell::new(None));
            let _frp = Frp::new();
            let display_object = elem.display_object();
            let network = &_frp.network;
            let scale_animation = Animation::<f32>::new_with_init(network, 1.0);
            // scale_animation.simulator.update_spring(|s| s * DEBUG_ANIMATION_SPRING_FACTOR);
            frp::extend! { network
                eval scale_animation.value ((t) display_object.set_scale_xy(Vector2(*t,*t)));
                // FIXME: does it handle detaching display object?
                eval_ scale_animation.on_end (self_ref.borrow_mut().take(););
            }
            scale_animation.target.emit(0.0);

            let model = TrashModel { _frp, elem };
            let model = Rc::new(model);
            *self_ref.borrow_mut() = Some(model.clone());
            Self { model }
        }
    }

    impl<T: display::Object> display::Object for Trash<T> {
        fn display_object(&self) -> &display::object::Instance {
            self.model.elem.display_object()
        }

        fn focus_receiver(&self) -> &display::object::Instance {
            self.model.elem.focus_receiver()
        }
    }
}
pub use trash::Trash;
