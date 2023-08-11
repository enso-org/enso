//! Defines a scrollbar component. See definition of [`Scrollbar`] for details.

#![recursion_limit = "512"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::animation::delayed::DelayedAnimation;
use ensogl_core::animation::overshoot::OvershootAnimation;
use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::control::io::mouse;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::Rectangle;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_hardcoded_theme as theme;
use ensogl_selector::Bounds;



// =============
// === Style ===
// =============

#[derive(Debug, Clone, Copy, Default, FromTheme)]
struct Style {
    #[theme_path = "theme::component::slider::overshoot_limit"]
    overshoot_limit:  f32,
    #[theme_path = "theme::component::slider::track::color"]
    default_color:    color::Lcha,
    #[theme_path = "theme::component::slider::track::hover_color"]
    hover_color:      color::Lcha,
    #[theme_path = "theme::component::slider::background::color"]
    bg_default_color: color::Lcha,
    #[theme_path = "theme::component::slider::background::hover_color"]
    bg_hover_color:   color::Lcha,
}


// =================
// === Constants ===
// =================

// TODO: Some of those values could be defined by the theme instead. But currently, this does not
//       seem to be worth it because the FRP initialization introduces a lot of complexity, as
//       described at https://github.com/enso-org/ide/issues/1654.

/// Amount the scrollbar moves on a single click, relative to the viewport size.
const CLICK_JUMP_PERCENTAGE: f32 = 0.80;
/// Width of the scrollbar in px.
pub const WIDTH: f32 = 13.0;
/// The amount of padding on each side inside the scrollbar.
pub const PADDING: f32 = 3.0;
/// The thumb will be displayed with at least this size to make it more visible and dragging easier.
const MIN_THUMB_SIZE: f32 = 12.0;
/// After an animation, the thumb will be visible for this time, before it hides again.
const HIDE_DELAY: f32 = 1000.0;
/// Time delay before holding down a mouse button triggers first scroll, in milliseconds.
const CLICK_AND_HOLD_DELAY_MS: u32 = 500;
/// Time interval between scrolls while holding down a mouse button, in milliseconds.
const CLICK_AND_HOLD_INTERVAL_MS: u32 = 200;
/// Minimum scroll movement in pixels per frame required to show the scrollbar.
const ERROR_MARGIN_FOR_ACTIVITY_DETECTION: f32 = 0.1;



// ===========
// === Frp ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        /// Sets the length of the scrollbar as display object in px.
        set_length            (f32),
        /// Sets the number of scroll units on the scroll bar. Should usually be the size of the
        /// scrolled area in px.
        set_max               (f32),
        /// Sets the thumb size in scroll units.
        set_thumb_size        (f32),
        /// Determines if scrolling is allowed to overshoot the scrollbar bounds. Overshoot is
        /// enabled by default.
        set_overshoot_enabled (bool),
        /// Scroll smoothly by the given amount in scroll units. Allows scroll to overshoot and
        /// bounce back if `set_overshoot_enabled` is `true`.
        scroll_by             (f32),
        /// Scroll smoothly to the given position in scroll units. Always bounded by the scroll
        /// area, independently of `set_overshoot_enabled` flag.
        scroll_to             (f32),
        /// Jumps to the given position in scroll units without animation and without revealing the
        /// scrollbar.
        jump_to               (f32),
    }
    Output {
        /// Scroll position in scroll units.
        thumb_position (f32),
        /// The target of the scroll position animation in scroll units.
        thumb_position_target (f32),
    }
}

impl Frp {
    /// Initialize the FRP network.
    fn init(&self, app: &Application, model: &Rc<Model>, style: &StyleWatchFrp) {
        let frp = &self;
        let network = &frp.network;
        let scene = &app.display.default_scene;
        let thumb_position = OvershootAnimation::new(network);
        let activity_cool_off = DelayedAnimation::new(network);
        activity_cool_off.frp.set_delay(HIDE_DELAY);
        activity_cool_off.frp.set_duration(0.0);

        let style = Style::from_theme(network, style);

        let click_and_hold_timer = frp::io::timer::DelayedInterval::new(network);
        let click_and_hold_config = frp::io::timer::DelayedIntervalConfig::new(
            CLICK_AND_HOLD_DELAY_MS,
            CLICK_AND_HOLD_INTERVAL_MS,
        );

        frp::extend! { network

            // == Mouse events ==

            let track_over = model.track.on_event::<mouse::Over>();
            let track_out = model.track.on_event::<mouse::Out>();
            let track_down = model.track.on_event::<mouse::Down>();
            let background_down = model.background.on_event::<mouse::Down>();
            let scene_up = scene.on_event::<mouse::Up>();
            let mouse_move = scene.on_event::<mouse::Move>();

            track_hover <- bool(&track_out, &track_over).on_change();
            is_dragging_track <- bool(&scene_up, &track_down).on_change();
            is_dragging_background <- bool(&scene_up, &background_down).on_change();


            // == Thumb position ==

            // Overshoot control
            bar_not_filled <- all_with(&frp.set_thumb_size, &frp.set_max, |&size, &max| size < max);
            overshoot_enabled <- frp.set_overshoot_enabled && bar_not_filled;

            // Scrolling and Jumping
            thumb_position.set_overshoot_limit <+ style.map(|s| s.overshoot_limit);
            thumb_position.soft_change_by <+ frp.scroll_by.gate(&overshoot_enabled);
            thumb_position.hard_change_by <+ frp.scroll_by.gate_not(&overshoot_enabled);
            thumb_position.hard_change_to <+ any(&frp.scroll_to,&frp.jump_to);
            thumb_position.set_max_bound <+ all_with(&frp.set_thumb_size, &frp.set_max,
                |thumb_size, max| (max - thumb_size).max(0.0));
            thumb_position.skip <+_ frp.jump_to;

            frp.source.thumb_position_target <+ thumb_position.target;
            frp.source.thumb_position <+ thumb_position.value;

            // === Mouse position in local coordinates ===

            mouse_position <- mouse_move.map(f!([scene, model] (event)
                scene.screen_to_object_space(&model.display_object, event.client_centered())
            ));

            // We will initialize the mouse position with `Vector2(f32::NAN,f32::NAN)`, because the
            // default `Vector2(0.0,0.0)` would reveal the scrollbar before we get the actual mouse
            // coordinates.
            init_mouse_position <- source::<Vector2>();
            mouse_position      <- any(&mouse_position,&init_mouse_position);


            // === Hiding ===

            // We start a delayed animation whenever the bar is scrolled to a new place (it is
            // active). This will instantly reveal the scrollbar and hide it after the delay has
            // passed.
            thumb_position_previous <- thumb_position.value.previous();
            thumb_position_changed  <- thumb_position.value.map2(&thumb_position_previous,
                |t1, t2| (t1 - t2).abs() > ERROR_MARGIN_FOR_ACTIVITY_DETECTION);
            thumb_position_changed <- thumb_position_changed.on_true().constant(());
            activity_cool_off.frp.reset <+ thumb_position_changed;
            activity_cool_off.frp.start <+ thumb_position_changed;

            recently_active <- bool(&activity_cool_off.frp.on_end,&activity_cool_off.frp.on_reset);

            // The signed distance between the cursor and the edge of the scrollbar. If the cursor
            // is further left or right than the ends of the scrollbar then we count the distance as
            // infinite. We use this distance to reveal the scrollbar when approached by the cursor.
            // Returning infinity has the effect that we do not reveal it when the cursor approaches
            // from the sides. This could be handled differently, but the solution was chosen for
            // the simplicity of the implementation and the feeling of the interaction.
            vert_mouse_distance <- all_with(&mouse_position,&frp.set_length,|&pos,&length| {
                let scrollbar_x_range = 0.0..=length;
                if scrollbar_x_range.contains(&pos.x) {
                    (pos.y - WIDTH / 2.0).abs() - WIDTH / 2.0
                } else {
                    f32::INFINITY
                }
            });


            // === Color ===

            engaged <- track_hover || is_dragging_track;
            alpha <- all_with5(&recently_active,&is_dragging_track,
                &vert_mouse_distance,&frp.set_thumb_size,&frp.set_max,Self::compute_target_alpha);
            target_colors <- all_with3(&style, &engaged, &alpha, |s, &engaged, &alpha| {
                let thumb = if engaged { s.hover_color } else { s.default_color };
                let bg = if engaged { s.bg_hover_color } else { s.bg_default_color };
                (thumb.multiply_alpha(alpha), bg.multiply_alpha(alpha))
            });
            let thumb_color = color::Animation::new(network);
            let background_color = color::Animation::new(network);
            thumb_color.target <+ target_colors._0();
            background_color.target <+ target_colors._1();
            eval thumb_color.value((c) model.set_track_color(c.into()));
            eval background_color.value((c) model.set_background_color(c.into()));
        }
        frp::extend! { network

            // === Position on Screen ===

            // Space that the thumb can actually move in
            inner_length        <- frp.set_length.map(|length| *length - 2.0 * PADDING);
            // Thumb position as a number between 0 and 1
            normalized_position <- all_with3(&frp.thumb_position,&frp.set_thumb_size,&frp.set_max,
                |&pos,&size,&max| pos / (max - size));
            normalized_size     <- all_with(&frp.set_thumb_size,&frp.set_max,|&size,&max|
                size / max);
            // Minimum thumb size in normalized units
            min_visual_size     <- inner_length.map(|&length| MIN_THUMB_SIZE / length.max(MIN_THUMB_SIZE));
            // The size at which we render the thumb on screen, in normalized units. Can differ from
            // the actual thumb size if the thumb is smaller than the min.
            visual_size         <- all_with(&normalized_size,&min_visual_size,|&size,&min|
                size.min(1.0).max(min));
            // The position at which we render the thumb on screen, in normalized units.
            visual_start        <- all_with(&normalized_position,&visual_size,|&pos,&size|
                pos * (1.0 - size));
            visual_bounds       <- all_with(&visual_start,&visual_size,|&start,&size|
                Bounds::new(start,start+size));
            visual_center       <- visual_bounds.map(|bounds| bounds.center());
            thumb_center_px     <- all_with(&visual_center,&inner_length,
                |normalized,length| normalized * length);

            // Because of overshoot, we want to further clamp true visual bounds, so the thumb does
            // not go outside the bar. We want to only limit the bounds we use for drawing the thumb
            // itself, without influencing other logic that depends on true thumb size or position.
            clamped_visual_bounds <- visual_bounds.map(|bounds|
                Bounds::new(bounds.start.max(0.0), bounds.end.min(1.0)));
            size <- frp.set_length.map(|&length| Vector2::new(length,WIDTH));
            update_slider <- all(&clamped_visual_bounds,&size);
            eval update_slider(((value,size)) model.update_layout(*value,*size));


            // === Clicking ===

            background_click <- mouse_position.sample(&background_down);
            frp.scroll_by <+ background_click.map3(&thumb_center_px,&frp.set_thumb_size,
                |click_position,thumb_center,thumb_size| {
                    let direction = if click_position.x > *thumb_center { 1.0 } else { -1.0 };
                    direction * thumb_size * CLICK_JUMP_PERCENTAGE
                });


            // === Click and hold repeated scrolling ===

            background_drag_start <- is_dragging_background.on_true();
            click_and_hold_timer.restart <+ background_drag_start.constant(click_and_hold_config);
            click_and_hold_timer.stop <+ is_dragging_background.on_false();

            mouse_pos_at_timer_trigger <- mouse_position.sample(&click_and_hold_timer.on_trigger);
            offset_from_thumb_px <- mouse_pos_at_timer_trigger.map2(&thumb_center_px,
                |mouse_pos, thumb_center| thumb_center - mouse_pos.x);
            offset_from_thumb <- offset_from_thumb_px.map3(&inner_length, &frp.set_max,
                |offset_px, length_px, max| offset_px / length_px * max);

            frp.scroll_by <+ offset_from_thumb.map2(&frp.set_thumb_size,
                |mouse_offset, thumb_size| {
                    let half_thumb_size = thumb_size / 2.0;
                    let mouse_outside_thumb = mouse_offset.abs() > half_thumb_size;
                    let direction = if mouse_outside_thumb { -mouse_offset.signum() } else { 0.0 };
                    direction * thumb_size * CLICK_JUMP_PERCENTAGE
                });


            // === Dragging ===

            drag_started <- is_dragging_track.on_change().on_true().constant(());
            x            <- all4(&mouse_position,&inner_length,&frp.set_max,&frp.thumb_position);
            x            <- x.sample(&drag_started);
            drag_offset  <- x.map(|(mouse_px,length_px,max,thumb_pos)| {
                    let thumb_position_px = thumb_pos / max * length_px;
                    mouse_px.x - thumb_position_px
                });
            x           <- all4(&mouse_position,&drag_offset,&inner_length,&frp.set_max);
            x           <- x.gate(&is_dragging_track);
            frp.jump_to <+ x.map(|(mouse_px,offset_px,length_px,max)| {
                    let target_px = mouse_px.x - offset_px;
                    target_px / length_px * max
                });
        }


        // === Init Network ===

        frp.set_overshoot_enabled(true);
        frp.set_length(200.0);
        frp.set_thumb_size(0.2);
        frp.set_max(1.0);
        init_mouse_position.emit(Vector2(f32::NAN, f32::NAN));
    }

    fn compute_target_alpha(
        &recently_active: &bool,
        &dragging: &bool,
        &cursor_distance: &f32,
        &thumb_size: &f32,
        &max: &f32,
    ) -> f32 {
        let thumb_fills_bar = thumb_size >= max;
        if thumb_fills_bar {
            0.0
        } else if recently_active || dragging {
            1.0
        } else {
            #[allow(clippy::collapsible_else_if)]
            if cursor_distance <= 0.0 {
                1.0
            } else {
                // The opacity approaches 0.7 when the cursor is right next to the bar and fades
                // linearly to 0.0 at 20 px distance.
                (0.7 - cursor_distance / 20.0).max(0.0)
            }
        }
    }
}



// ===========================
// === Scrollbar Component ===
// ===========================

/// Scrollbar component that can be used to implement scrollable components.
///
/// We say "thumb" to mean the object inside the bar that indicates the scroll position and can be
/// dragged to change that position. Clicking on the scrollbar on either side of the thumb will move
/// the thumb a step in that direction. The scrollbar is hidden by default and will show when it is
/// animated, dragged or approached by the cursor.
///
/// The scrollbar has a horizontal orientation with the beginning on the left and the end on the
/// right. But it can be rotated arbitrarily. The origin is in the center.
///
/// All operations related to the scroll position take as argument a number of pixels describing a
/// position or distance on the scrolled area. We call them scroll units.
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct Scrollbar {
    /// Public FRP api of the Component.
    pub frp: Rc<Frp>,
    #[display_object]
    model:   Rc<Model>,
}

impl Scrollbar {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new());
        let frp = Frp::default();
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        frp.init(app, &model, &style);
        let frp = Rc::new(frp);
        Self { frp, model }
    }
}

impl Deref for Scrollbar {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl FrpNetworkProvider for Scrollbar {
    fn network(&self) -> &frp::Network {
        self.frp.network()
    }
}

impl application::View for Scrollbar {
    fn label() -> &'static str {
        "Scrollbar"
    }

    fn new(app: &Application) -> Self {
        Scrollbar::new(app)
    }
}

#[derive(Debug, display::Object)]
struct Model {
    display_object: display::object::Instance,
    background:     Rectangle,
    track:          Rectangle,
}

impl Model {
    fn new() -> Self {
        let display_object = display::object::Instance::new_named("Scrollbar");
        let track = Rectangle();
        let background = Rectangle();
        background.allow_grow().set_alignment_center();
        track.set_inset(PADDING).set_corner_radius_max();
        display_object.add_child(&background);
        display_object.add_child(&track);
        Self { display_object, background, track }
    }

    fn set_track_color(&self, color: color::Rgba) {
        self.track.set_color(color);
    }

    fn set_background_color(&self, color: color::Rgba) {
        self.background.set_color(color);
    }

    fn update_layout(&self, value: Bounds, size: Vector2) {
        let start_px = value.start * size.x;
        let end_px = value.end * size.x;
        let length_px = end_px - start_px;
        self.display_object.set_size(size);
        self.track.set_size(Vector2(length_px, size.y));
        self.track.set_xy((start_px, 0.0));
    }
}
