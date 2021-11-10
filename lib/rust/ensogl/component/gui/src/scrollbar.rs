//! Defines a scrollbar component. See definition of [`Scrollbar`] for details.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::shape::*;
use ensogl_core::Animation;
use ensogl_hardcoded_theme as theme;

use crate::component;
use crate::selector;
use crate::selector::model::Model;
use crate::selector::Bounds;
use ensogl_core::animation::delayed::DelayedAnimation;



// =================
// === Constants ===
// =================

// TODO: Some of those values could be defined by the theme instead. But currently, this does not
//       seem to be worth it because the FRP initialization introduces a lot of complexity, as
//       described at https://github.com/enso-org/ide/issues/1654.

/// Amount the scrollbar moves on a single click, relative to the viewport size.
const CLICK_JUMP_PERCENTAGE: f32 = 0.80;
/// Width of the scrollbar in px.
pub const WIDTH: f32 = 11.0;
/// The amount of padding on each side inside the scrollbar.
const PADDING: f32 = 2.0;
/// The thumb will be displayed with at least this size to make it more visible and dragging easier.
const MIN_THUMB_SIZE: f32 = 12.0;
/// After an animation, the thumb will be visible for this time, before it hides again.
const HIDE_DELAY: f32 = 1000.0;

const ERROR_MARGIN_FOR_ACTIVITY_DETECTION: f32 = 0.1;



// ===========
// === Frp ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        /// Sets the length of the scrollbar as display object in px.
        set_length      (f32),
        /// Sets the number of scroll units on the scroll bar. Should usually be the size of the
        /// scrolled area in px.
        set_max         (f32),
        /// Sets the thumb size in scroll units.
        set_thumb_size  (f32),

        /// Scroll smoothly by the given amount in scroll units.
        scroll_by       (f32),
        /// Scroll smoothly to the given position in scroll units.
        scroll_to       (f32),
        /// Jumps to the given position in scroll units without animation and without revealing the
        /// scrollbar.
        jump_to         (f32),
    }
    Output {
        /// Scroll position in scroll units.
        thumb_position (f32),
    }
}

impl Frp {
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

impl component::Frp<Model> for Frp {
    fn init(&self, app: &Application, model: &Model, style: &StyleWatchFrp) {
        let frp = &self;
        let network = &frp.network;
        let scene = app.display.scene();
        let mouse = &scene.mouse.frp;
        let thumb_position = Animation::new(network);
        let thumb_color = color::Animation::new(network);
        let activity_cool_off = DelayedAnimation::new(network);
        activity_cool_off.frp.set_delay(HIDE_DELAY);
        activity_cool_off.frp.set_duration(0.0);

        frp::extend! { network
            resize <- frp.set_length.map(|&length| Vector2::new(length,WIDTH));
        }

        let base_frp = selector::Frp::new(model, style, network, resize.clone(), mouse);

        model.use_track_handles(false);
        model.set_track_corner_round(true);
        model.show_background(false);
        model.show_left_overflow(false);
        model.show_right_overflow(false);
        model.set_padding(PADDING);

        let default_color = style.get_color(theme::component::slider::track::color);
        let hover_color = style.get_color(theme::component::slider::track::hover_color);

        frp::extend! { network

            // Scrolling and Jumping

            frp.scroll_to <+ frp.scroll_by.map2(&thumb_position.target,|delta,pos| *pos+*delta);

            // We will use this to reveal the scrollbar on scrolling. It has to be defined before
            // the following nodes that update `thumb_position.target`.
            active <- frp.scroll_to.map2(&thumb_position.target,|&new:&f32,&old:&f32| {
                (new - old).abs() > ERROR_MARGIN_FOR_ACTIVITY_DETECTION
            }).on_true();

            unbounded_target_position <- any(&frp.scroll_to,&frp.jump_to);
            thumb_position.target     <+ all_with3(&unbounded_target_position,&frp.set_thumb_size,
                &frp.set_max,|target,&size,&max| target.min(max-size).max(0.0));
            thumb_position.skip       <+ frp.jump_to.constant(());
            frp.source.thumb_position <+ thumb_position.value;


            // === Mouse position in local coordinates ===

            mouse_position <- mouse.position.map(f!([scene,model](pos)
                scene.screen_to_object_space(&model,*pos)));

            // We will initialize the mouse position with `Vector2(f32::NAN,f32::NAN)`, because the
            // default `Vector2(0.0,0.0)` would reveal the scrollbar before we get the actual mouse
            // coordinates.
            init_mouse_position <- source::<Vector2>();
            mouse_position      <- any(&mouse_position,&init_mouse_position);


            // === Color ===

            init_color    <- any_mut::<()>();
            default_color <- all(&default_color,&init_color)._0().map(|c| color::Lch::from(*c));
            hover_color   <- all(&hover_color,&init_color)._0().map(|c| color::Lch::from(*c));

            engaged                  <- base_frp.track_hover || base_frp.is_dragging_track;
            thumb_color.target_color <+ engaged.switch(&default_color,&hover_color);
            eval thumb_color.value((c) model.set_track_color(color::Rgba::from(*c)));


            // === Hiding ===

            // We start a delayed animation whenever the bar is scrolled to a new place (it is
            // active). This will instantly reveal the scrollbar and hide it after the delay has
            // passed.
            activity_cool_off.frp.reset <+ active;
            activity_cool_off.frp.start <+ active;

            recently_active <- bool(&activity_cool_off.frp.on_end,&activity_cool_off.frp.on_reset);

            // The signed distance between the cursor and the edge of the scrollbar. If the cursor
            // is further left or right than the ends of the scrollbar then we count the distance as
            // infinite. We use this distance to reveal the scrollbar when approached by the cursor.
            // Returning infinity has the effect that we do not reveal it when the cursor approaches
            // from the sides. This could be handled differently, but the solution was chosen for
            // the simplicity of the implementation and the feeling of the interaction.
            vert_mouse_distance <- all_with(&mouse_position,&frp.set_length,|&pos,&length| {
                let scrollbar_x_range = (-length/2.0)..=(length/2.0);
                if scrollbar_x_range.contains(&pos.x) {
                    pos.y.abs() - WIDTH / 2.0
                } else {
                    f32::INFINITY
                }
            });

            thumb_color.target_alpha <+ all_with5(&recently_active,&base_frp.is_dragging_track,
                &vert_mouse_distance,&frp.set_thumb_size,&frp.set_max,Self::compute_target_alpha);


            // === Position on Screen ===

            // Space that the thumb can actually move in
            inner_length        <- frp.set_length.map(|length| *length - 2.0 * PADDING);
            // Thumb position as a number between 0 and 1
            normalized_position <- all_with3(&frp.thumb_position,&frp.set_thumb_size,&frp.set_max,
                |&pos,&size,&max| pos / (max - size));
            normalized_size     <- all_with(&frp.set_thumb_size,&frp.set_max,|&size,&max|
                size / max);
            // Minimum thumb size in normalized units
            min_visual_size     <- inner_length.map(|&length| MIN_THUMB_SIZE / length);
            // The size at which we render the thumb on screen, in normalized units. Can differ from
            // the actual thumb size if the thumb is smaller than the min.
            visual_size         <- all_with(&normalized_size,&min_visual_size,|&size,&min|
                size.max(min).min(1.0));
            // The position at which we render the thumb on screen, in normalized units.
            visual_start        <- all_with(&normalized_position,&visual_size,|&pos,&size|
                pos * (1.0 - size));
            visual_bounds       <- all_with(&visual_start,&visual_size,|&start,&size|
                Bounds::new(start,start+size));
            visual_center       <- visual_bounds.map(|bounds| bounds.center());
            thumb_center_px     <- all_with(&visual_center,&inner_length, |normalized,length|
                (normalized - 0.5) * length);

            update_slider <- all(&visual_bounds,&resize);
            eval update_slider(((value,size)) model.set_background_range(*value,*size));


            // === Clicking ===

            frp.scroll_by <+ base_frp.background_click.map3(&thumb_center_px,&frp.set_thumb_size,
                |click_position,thumb_center,thumb_size| {
                    let direction = if click_position.x > *thumb_center { 1.0 } else { -1.0 };
                    direction * thumb_size * CLICK_JUMP_PERCENTAGE
                });


            // === Dragging ===

            drag_started <- base_frp.is_dragging_track.on_change().on_true().constant(());
            x            <- all4(&mouse_position,&inner_length,&frp.set_max,&frp.thumb_position);
            x            <- x.sample(&drag_started);
            drag_offset  <- x.map(|(mouse_px,length_px,max,thumb_pos)| {
                    let thumb_position_px = thumb_pos / max * length_px;
                    mouse_px.x - thumb_position_px
                });
            x           <- all4(&mouse_position,&drag_offset,&inner_length,&frp.set_max);
            x           <- x.gate(&base_frp.is_dragging_track);
            frp.jump_to <+ x.map(|(mouse_px,offset_px,length_px,max)| {
                    let target_px = mouse_px.x - offset_px;
                    target_px / length_px * max
                });
        }


        // === Init Network ===

        frp.set_length(200.0);
        frp.set_thumb_size(0.2);
        frp.set_max(1.0);
        init_mouse_position.emit(Vector2(f32::NAN, f32::NAN));
        init_color.emit(());
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
pub type Scrollbar = crate::component::Component<Model, Frp>;

impl application::View for Scrollbar {
    fn label() -> &'static str {
        "Scrollbar"
    }
    fn new(app: &Application) -> Self {
        Scrollbar::new(app)
    }
    fn app(&self) -> &Application {
        &self.app
    }
}
