//! Definition of the Cursor (known as well as mouse pointer) component.

use crate::display::shape::*;
use crate::gui::style::*;
use crate::prelude::*;

use crate::animation::linear_interpolation;
use crate::application::command::FrpNetworkProvider;
use crate::control::io::mouse;
use crate::data::color;
use crate::define_style;
use crate::display;
use crate::display::scene::Scene;
use crate::display::shape::compound::rectangle;
use crate::frp;
use crate::Animation;
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
    use_host_layer : bool,
    pointer_events: bool,
    size: Vector2<f32>,
    offset: Vector2<f32>,
    color: color::Lcha,
    radius: f32,
    press: f32,
    trash: f32,
    plus: f32,
    double_arrow: f32,
    double_arrow_rotation: f32,
}


// === Smart Constructors ===

#[allow(missing_docs)]
impl Style {
    pub fn new_highlight(host: display::object::Instance, size: Vector2<f32>, radius: f32) -> Self {
        let host = Some(StyleValue::new(host));
        let size = Some(StyleValue::new(size));
        let radius = Some(StyleValue::new(radius));
        let press = Some(StyleValue::new(0.0));
        let use_host_layer = Some(StyleValue::new_no_animation(true));
        Self { host, size, radius, use_host_layer, press, ..default() }
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

    /// Show the cursor as a double arrow. The rotation is in radians. The arrow is horizontal by
    /// default.
    pub fn double_arrow(rotation: f32) -> Self {
        let double_arrow = Some(StyleValue::new(1.0));
        let double_arrow_rotation = Some(StyleValue::new(rotation));
        Self { double_arrow, double_arrow_rotation, ..default() }
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

// === Getters ===

impl Style {
    /// Get the computed cursor size given this style.
    pub fn get_size(&self) -> Vector2<f32> {
        self.size.as_ref().and_then(|s| s.value).unwrap_or(DEFAULT_SIZE())
    }
}



// ==================
// === CursorView ===
// ==================

/// Double arrow shape for use in the cursor.
///
/// The `double_arrow` parameter is a weight between 0.0 and 1.0 that indicates the visibility of
/// the double arrow. The `rotation` parameter is in radians. The arrow is horizontal by default.
fn double_arrow_shape(double_arrow: &Var<f32>, rotation: &Var<f32>) -> AnyShape {
    let line = Rect(((double_arrow * 13.0).px(), (double_arrow * 1.5).px()));
    let triangle = Triangle(5.0, 4.0).rotate((PI / 2.0).radians());
    let left_head = triangle.translate_x((double_arrow * 6.0).px());
    let right_head = triangle.rotate(PI.radians()).translate_x((double_arrow * (-6.0)).px());
    let double_arrow_shape = line + left_head + right_head;
    let rotation: Var<Radians> = rotation.clone().into();
    let shape = double_arrow_shape.rotate(rotation);
    let transparent: Var<color::Rgba> = color::Rgba::transparent().into();
    let color: Var<color::Rgba> = color::Rgba::new(0.0, 0.0, 0.0, 0.8).into();
    let color = transparent.mix(&color, double_arrow);
    let shape = shape.fill(color);
    shape.into()
}

/// Canvas shape definition.
pub mod shape {
    use super::*;
    crate::shape! {
        below = [rectangle];
        pointer_events_instanced = true;
        alignment = center; (
            style: Style,
            press: f32,
            radius: f32,
            color: Vector4,
            trash: f32,
            plus: f32,
            double_arrow: f32,
            double_arrow_rotation: f32,
        ) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let press_side_shrink = 2.px();
            let press_diff        = press_side_shrink * &press;
            let radius            = radius.px() - &press_diff;
            let sides_padding     = SIDES_PADDING.px();
            let scale             = Var::from(1.0) - &double_arrow;
            let width             = (&width  - &press_diff * 2.0 - &sides_padding) * &scale;
            let height            = (&height - &press_diff * 2.0 - &sides_padding) * scale;
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

            let double_arrow = double_arrow_shape(&double_arrow, &double_arrow_rotation);

            let cursor = cursor + double_arrow + trash_bar_x + plus_sign;
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
        box_size              (Vector2),
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
    pub scene:           Scene,
    pub display_object:  display::object::Instance,
    pub dragged_elem:    display::object::Instance,
    pub view:            shape::View,
    pub host_layer_view: shape::View,
    pub style:           Rc<RefCell<Style>>,
    pub dragged_item:    Rc<RefCell<Option<(Box<dyn Any>, display::object::Instance)>>>,
    frp:                 WeakFrp,
}

impl CursorModel {
    /// Constructor.
    pub fn new(scene: &Scene, frp: WeakFrp) -> Self {
        let scene = scene.clone_ref();
        let display_object = display::object::Instance::new_no_debug();
        let dragged_elem = display::object::Instance::new_named("dragged_elem");
        let view = shape::View::new();
        let host_layer_view = shape::View::new();
        let style = default();

        display_object.add_child(&view);
        scene.add_child(&dragged_elem);
        scene.layers.cursor.add(&view);
        // host_layer_view is intentionally not assigned to any layer yet. Instead, it is assigned
        // on demand once a cursor is in host-attached mode.

        let dragged_item = default();
        Self {
            scene,
            display_object,
            dragged_elem,
            view,
            host_layer_view,
            style,
            dragged_item,
            frp,
        }
    }

    fn for_each_view(&self, f: impl Fn(&shape::View)) {
        for view in &[&self.view, &self.host_layer_view] {
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
        let radius = Animation::<f32>::new(network);
        let size = Animation::<Vector2>::new(network);
        let offset = Animation::<Vector2>::new(network);
        let color = color::Animation::new(network);
        let inactive_fade = Animation::<f32>::new(network);
        let host_position = Animation::<Vector3>::new(network);
        let host_follow_weight = Animation::<f32>::new(network);
        let host_attached_weight = Easing::new(network);
        let custom_layer_weight = Animation::<f32>::new(network);
        let trash = Animation::<f32>::new(network);
        let plus = Animation::<f32>::new(network);
        let double_arrow = Animation::<f32>::new(network);
        let double_arrow_rotation = Animation::<f32>::new(network);


        let fade_out_spring = inactive_fade.simulator.spring() * 0.2;
        let fade_in_spring = inactive_fade.simulator.spring();

        let on_up = scene.on_event::<mouse::Up>();

        let cursor_camera = scene.layers.cursor.camera();

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
            eval double_arrow.value ((v) model.for_each_view(|vw| vw.double_arrow.set(*v)));
            eval double_arrow_rotation.value ((v) model.for_each_view(|vw| vw.double_arrow_rotation.set(*v)));

            let weight = &custom_layer_weight.value;
            anim_color <- color.value.all_with(&inactive_fade.value, |c, f| c.multiply_alpha(*f));
            front_color <- anim_color.all_with(weight, |c, w| c.multiply_alpha(1.0 - w));
            custom_layer_color <- anim_color.all_with(weight, |c,w| c.multiply_alpha(*w));

            style <- frp.set_style_override.unwrap_or(&frp.set_style);
            eval style([host_attached_weight,size,offset,color,radius,model] (new_style) {
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
                        color.target.emit(DEFAULT_COLOR);
                    }
                    Some(t) => {
                        let value = t.value.unwrap_or(DEFAULT_COLOR);
                        color.target.emit(value);
                        if !t.animate {
                            color.skip.emit(());
                        }
                    }
                }

                match &new_style.size {
                    None => size.target.emit(DEFAULT_SIZE()),
                    Some(t) => {
                        let value = t.value.unwrap_or_else(DEFAULT_SIZE);
                        size.target.emit(Vector2(value.x,value.y));
                        if !t.animate { size.skip.emit(()) }
                    }
                }

                match &new_style.offset {
                    None => offset.target.emit(Vector2::zero()),
                    Some(t) => {
                        let value = t.value.unwrap_or_default();
                        offset.target.emit(Vector2(value.x,value.y));
                        if !t.animate { offset.skip.emit(()) }
                    }
                }

                match &new_style.radius {
                    None => radius.target.emit(DEFAULT_RADIUS),
                    Some(t) => {
                        let value = t.value.unwrap_or(DEFAULT_RADIUS);
                        radius.target.emit(value);
                        if !t.animate { radius.skip.emit(()) }
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

                match &new_style.double_arrow {
                    None => double_arrow.target.emit(0.0),
                    Some(t) => double_arrow.target.emit(t.value.unwrap_or(0.0)),
                }

                match &new_style.double_arrow_rotation {
                    None => double_arrow_rotation.target.emit(0.0),
                    Some(t) => double_arrow_rotation.target.emit(t.value.unwrap_or(0.0)),
                }

                let pointer_events = match &new_style.pointer_events {
                    None => false,
                    Some(t) => t.value.unwrap_or(false),
                };
                let disable_pointer_events = (!pointer_events) as i32 as f32;
                model.for_each_view(|vw| vw.disable_pointer_events.set(disable_pointer_events));

                *model.style.borrow_mut() = new_style.clone();
            });

            host_instance <- frp.set_style.map(|style| style.host.clone()?.value).on_change();
            host_layer_to_use <- frp.set_style.map(|new_style| {
                let true = new_style.use_host_layer.clone()?.value? else { return None };
                let host = new_style.host.as_ref()?.value.as_ref()?;
                let host_layer = host.display_layer()?;
                Some(host_layer.downgrade())
            });
            host_camera <- host_layer_to_use.and_then(|layer| Some(layer.upgrade()?.camera()));
            set_host_layer <- host_layer_to_use.unwrap().on_change();
            eval set_host_layer([model] (layer) {
                if let Some(layer) = layer.upgrade() {
                    layer.add(&model.host_layer_view)
                }
            });

            custom_layer_weight.target <+ host_layer_to_use.is_some().switch_constant(0.0, 1.0);
            custom_layer_weight.skip <+ frp.set_style.filter(|new_style|
                new_style.use_host_layer.as_ref().map_or(false, |t| !t.animate)
            ).constant(());
            show_host_layer_view <- custom_layer_weight.value.map(|w| *w >= 0.01).on_change();
            eval show_host_layer_view([model, scene] (show) {
                if *show {
                    scene.add_child(&model.host_layer_view);
                } else {
                    scene.remove_child(&model.host_layer_view);
                }
            });

            is_not_hosted   <- host_instance.is_none();
            mouse_pos_rt    <- mouse.position.gate(&is_not_hosted);
            host_moved      <- host_instance.flat_map(|host| match host {
                None => frp::Source::<()>::new().into(),
                Some(host) => host.on_transformed.clone(),
            });
            last_known_host_camera <- host_camera.unwrap();
            transformed_host_position <- all_with4(
                &host_moved, &scene.frp.camera_changed,
                &host_instance, &last_known_host_camera,
                f!([cursor_camera] (_, _, host, host_camera) {
                    let host = host.as_ref()?;
                    let m1 = cursor_camera.inversed_view_matrix();
                    let m2 = host_camera.view_matrix();
                    let half_size = host.computed_size() * 0.5;
                    let position = host.global_position() + half_size.push(0.0);
                    let transformed = m2 * (m1 * position.push(1.0));
                    Some(transformed.xyz())
                })
            ).unwrap();

            host_follow_weight.target <+ host_camera.is_some().switch_constant(0.0, 1.0);
            host_position.target <+ transformed_host_position;
            host_position.target <+ mouse_pos_rt.map(|t| Vector3(t.x,t.y,0.0));

            host_attached <- all_with3(
                &host_attached_weight.value, &host_position.value, &transformed_host_position,
                |weight, animated, target| linear_interpolation(*animated, *target, *weight)
            );

            position <- mouse.position.all_with4(
                &host_attached, &host_follow_weight.value, &offset.value,
                |pos_rt,pos_attached,weight,offset| {
                    let pos_rt = pos_rt.xy() + *offset;
                    linear_interpolation(pos_rt.push(0.0), *pos_attached, *weight)
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
            screen_position <- position.map(f!([cursor_camera] (position) {
                let cam_pos = cursor_camera.position();
                let z_diff = cam_pos.z - position.z;
                if z_diff == 0.0 {
                    Vector3::zero()
                } else {
                    let coeff = cam_pos.z / z_diff;
                    (position.xy() * coeff).push(0.0)
                }
            }));

            scene_position <- screen_position.all_with(&scene.frp.camera_changed,
                f!((p, _) scene.screen_to_scene_coordinates(*p))
            ).on_change();
            scene_position_prev  <- scene_position.previous();
            scene_position_delta <- scene_position.map2(&scene_position_prev, |p1,p2| p2 - p1);

            // === Fade-out when not active ===

            action_event           <- any_(&mouse.position,&frp.set_style);
            action_time            <- scene.frp.frame_time.sample(&action_event);
            time_since_last_action <- scene.frp.frame_time.map2(&action_time,|t,s|t-s);
            check_fade_time        <- time_since_last_action.gate(&mouse.ever_moved);
            _eval <- check_fade_time.map2(&frp.set_style, f!([inactive_fade](time,style) {
                if *time > FADE_OUT_TIME && style.is_default() {
                    inactive_fade.set_spring.emit(fade_out_spring);
                    inactive_fade.target.emit(0.0)
                } else {
                    inactive_fade.set_spring.emit(fade_in_spring);
                    inactive_fade.target.emit(1.0)
                }
            }));

            position_in_host_camera <- map2(&position, &last_known_host_camera,
                f!([cursor_camera] (position, host_camera) {
                    let m1 = cursor_camera.view_matrix();
                    let m2 = host_camera.inversed_view_matrix();
                    let position = m2 * (m1 * position.push(1.0));
                    Some(position.xyz())
                })
            ).unwrap();



            // === Evals ===

            eval position ((t) model.display_object.set_position(*t));
            eval position_in_host_camera ((t) model.host_layer_view.set_position(*t));
            eval front_color ((t) model.view.color.set(color::Rgba::from(t).into()));
            eval custom_layer_color ((t) model.host_layer_view.color.set(color::Rgba::from(t).into()));
            eval_ screen_position (model.update_drag_position());

            // === Outputs ===

            frp.private.output.position             <+ position;
            frp.private.output.screen_position      <+ screen_position;
            frp.private.output.scene_position       <+ scene_position;
            frp.private.output.scene_position_delta <+ scene_position_delta;
            frp.private.output.is_dragging <+ bool(&frp.stop_drag, &frp.start_drag);
            frp.private.output.box_size <+ size.value;
        }

        // Hide on init.
        host_attached_weight.set_duration(300.0);
        color.target.emit(DEFAULT_COLOR);
        radius.target.emit(DEFAULT_RADIUS);
        size.target.emit(DEFAULT_SIZE());
        inactive_fade.target.emit(0.0);

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
