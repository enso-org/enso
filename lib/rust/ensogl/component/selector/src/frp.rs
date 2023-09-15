//! Frp logic common to both Number and Range selector.

use crate::prelude::*;

use crate::model::Model;
use crate::shape::relative_shape_down_position;
use crate::shape::shape_is_dragged;

use enso_frp as frp;
use enso_frp::io::Mouse_DEPRECATED;
use enso_frp::Network;
use ensogl_core::application::Application;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_hardcoded_theme as theme;
use ensogl_shadow as shadow;



// ===========================
// === Frp Utility Methods ===
// ===========================

/// Compute the slider width from the given shape size. For use in FRP, thus taking a reference.
#[allow(clippy::trivially_copy_pass_by_ref)]
fn slider_area_width(size: &Vector2) -> f32 {
    // Radius of the rounded corners of the background shape.
    let rounded_width = size.y / 2.0;
    size.x - rounded_width
}



// ===============
// === Frp ===
// ===============

/// Frp endpoints provided for general information about mouse interactions and shape properties
/// of the `common::Model`.
#[derive(Clone, CloneRef, Debug)]
pub struct Frp {
    /// Current maximum extent of the track in scene coordinate space.
    pub track_max_width:            frp::Stream<f32>,
    /// Indicates whether there is an ongoing dragging action from the left overflow shape.
    pub is_dragging_left_overflow:  frp::Stream<bool>,
    /// Indicates whether there is an ongoing dragging action from the right overflow shape.
    pub is_dragging_right_overflow: frp::Stream<bool>,
    /// Indicates whether there is an ongoing dragging action from the track shape.
    pub is_dragging_track:          frp::Stream<bool>,
    /// Indicates whether there is an ongoing dragging action from the background shape.
    pub is_dragging_background:     frp::Stream<bool>,
    /// Indicates whether there is an ongoing dragging action from the invisible shape that covers
    /// the left end of the track.
    pub is_dragging_left_handle:    frp::Stream<bool>,
    /// Indicates whether there is an ongoing dragging action from the invisible shape that covers
    /// the right end of the track.
    pub is_dragging_right_handle:   frp::Stream<bool>,
    /// Indicates whether there is an ongoing dragging action on any of the component shapes.
    pub is_dragging_any:            frp::Stream<bool>,
    /// Position of a click on the background. Position is given relative to the overall shape
    /// origin and normalised to the shape size.
    pub background_click:           frp::Stream<Vector2>,
    /// Position of a click on the track shape. Position is given relative to the overall shape
    /// origin and normalised to the shape size.
    pub track_click:                frp::Stream<Vector2>,
    /// Indicates whether the track is hovered..
    pub track_hover:                frp::Stream<bool>,
}

impl Frp {
    /// Create and initialize the FRP.
    pub fn new(
        app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
        network: &Network,
        size: frp::Stream<Vector2>,
        mouse: &Mouse_DEPRECATED,
    ) -> Frp {
        let net = &network;
        let scene = &app.display.default_scene;
        let shadow = shadow::frp_from_style(style, theme::shadow);
        let text_size = style.get_number(theme::text::size);

        let is_dragging_left_overflow =
            shape_is_dragged(net, &model.left_overflow.events_deprecated, mouse);
        let is_dragging_right_overflow =
            shape_is_dragged(net, &model.right_overflow.events_deprecated, mouse);
        let is_dragging_track = shape_is_dragged(net, &model.track.events_deprecated, mouse);
        let is_dragging_background =
            shape_is_dragged(net, &model.background.events_deprecated, mouse);
        let is_dragging_left_handle =
            shape_is_dragged(net, &model.track_handle_left.events_deprecated, mouse);
        let is_dragging_right_handle =
            shape_is_dragged(net, &model.track_handle_right.events_deprecated, mouse);
        let background_click = relative_shape_down_position(net, scene, &model.background);
        let track_click = relative_shape_down_position(net, scene, &model.track);

        // Initialisation of components. Required for correct layout on startup.
        model.label_right.set_y(text_size.value() / 2.0);
        model.label_left.set_y(text_size.value() / 2.0);
        model.label.set_y(text_size.value() / 2.0);
        model.caption_left.set_y(text_size.value() / 2.0);
        model.caption_center.set_y(text_size.value() / 2.0);

        let bg_color = style.get_color(theme::component::slider::background);
        model.set_background_color(bg_color.value());

        frp::extend! { network

            // Style updates.
            init_shadow_padding <- source::<()>();
            shadow_padding      <- all_with(&shadow.size,&init_shadow_padding,|&v,_| Vector2(v,v));
            eval text_size ((size) {
                model.label.set_y(size / 2.0);
                model.label_right.set_y(size / 2.0);
                model.label_left.set_y(size / 2.0);
            });
            eval bg_color ((color)  model.set_background_color(*color) );

             // Caption updates.
             update_caption_position <- all(&size,&text_size);
             eval update_caption_position((args) model.update_caption_position(args));
             eval model.caption_center.frp.width((width)
                model.caption_center.set_x(-width/2.0)
            );

            // Size updates
            track_max_width <- size.map(slider_area_width);
            size_update <- all(size,shadow_padding);
            eval size_update(((size, shadow_padding)) {
                model.set_size(*size,*shadow_padding)
            });

            // Mouse IO
            is_dragging_overflow <- any(&is_dragging_left_overflow,&is_dragging_right_overflow);
            is_dragging_handle   <- any(&is_dragging_left_handle,&is_dragging_right_handle);
            is_dragging_any      <- any4(
                &is_dragging_track,
                &is_dragging_background,
                &is_dragging_overflow,
                &is_dragging_handle,
            );

            track_hover <- bool(
                &model.track.events_deprecated.mouse_out,
                &model.track.events_deprecated.mouse_over
            );
        }

        init_shadow_padding.emit(());

        Frp {
            track_max_width,
            is_dragging_left_overflow,
            is_dragging_right_overflow,
            is_dragging_track,
            is_dragging_background,
            is_dragging_left_handle,
            is_dragging_right_handle,
            is_dragging_any,
            background_click,
            track_click,
            track_hover,
        }
    }
}
