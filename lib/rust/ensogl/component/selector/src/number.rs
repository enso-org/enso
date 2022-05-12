///! Frp of the number selector.
use crate::prelude::*;
use ensogl_core::display::shape::*;

use crate::bounds::absolute_value;
use crate::bounds::clamp_with_overflow;
use crate::bounds::normalise_value;
use crate::bounds::position_to_normalised_value;
use crate::model::Model;
use crate::shape::relative_shape_down_position;
use crate::Bounds;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_hardcoded_theme as theme;



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_value(f32),
        resize(Vector2),
        set_bounds(Bounds),
        use_overflow_bounds(Option<Bounds>),
        allow_click_selection(bool),
        set_caption(Option<String>),
        set_left_corner_round(bool),
        set_right_corner_round(bool),
        set_track_color(color::Rgba),
    }
    Output {
        value(f32),
        bounds(Bounds),
    }
}

impl Frp {
    pub fn init(&self, app: &Application, model: &Model, style: &StyleWatchFrp) {
        let frp = &self;
        let network = &frp.network;
        let scene = &app.display.default_scene;
        let mouse = &scene.mouse.frp;

        model.show_background(true);

        let base_frp = super::Frp::new(model, style, network, frp.resize.clone().into(), mouse);

        let background_click = relative_shape_down_position(network, scene, &model.background);
        let track_click = relative_shape_down_position(network, scene, &model.track);
        let style_track_color = style.get_color(theme::component::slider::track::color);

        frp::extend! { network

            // Rebind
            frp.source.bounds <+ frp.set_bounds;
            frp.source.value  <+ frp.set_value;

            // Simple Inputs
            eval frp.set_caption((caption) model.set_caption_left(caption.clone()));
            eval frp.set_left_corner_round ((value) model.left_corner_round(*value));
            eval frp.set_right_corner_round((value) model.right_corner_round(*value));
            eval frp.set_track_color((value) model.set_track_color(*value));

            // Internal
            norm_value                 <- all2(&frp.value,&frp.bounds).map(normalise_value);
            normalised_overflow_bounds <- all(&frp.use_overflow_bounds,&frp.bounds).map(
            |(overflow_bounds,bounds)|
                overflow_bounds.map(|Bounds{start,end}|
                    Bounds::new(
                        normalise_value(&(start,*bounds)),normalise_value(&(end,*bounds)))
                )
            );
            has_underflow <- norm_value.map(|value| *value < 0.0);
            has_overflow  <- norm_value.map(|value| *value > 1.0);

            // Value Updates
            eval norm_value((value) model.set_background_value(*value));
            eval has_underflow ((underflow) model.show_left_overflow(*underflow));
            eval has_overflow ((overflow) model.show_right_overflow(*overflow));
            eval frp.value((value) model.set_center_label_content(*value));

            // Mouse IO
            click <- any(&track_click,&background_click).gate(&frp.allow_click_selection);
            click_value_update <- click.map2(
                &base_frp.track_max_width,
                position_to_normalised_value
            );

            is_dragging <- any
                ( base_frp.is_dragging_track
                , base_frp.is_dragging_background
                , base_frp.is_dragging_left_overflow
                , base_frp.is_dragging_right_overflow
                );

            drag_movement <- mouse.translation.gate(&is_dragging);
            delta_value   <- drag_movement.map2(
                &base_frp.track_max_width,
                |delta,width| (delta.x + delta.y) / width
            );

            drag_value_update <- delta_value.map2(&norm_value,|delta,value|*delta+*value);
            value_update      <- any(&click_value_update,&drag_value_update);
            clamped_update    <- value_update.map2(
                &normalised_overflow_bounds,
                clamp_with_overflow
            );
            frp.source.value  <+ all(&frp.set_bounds,&clamped_update).map(absolute_value);
        }

        // Init defaults.
        frp.set_bounds(Bounds::new(0.0, 1.0));
        frp.allow_click_selection(false);
        frp.use_overflow_bounds(None);
        frp.set_value(0.5);
        frp.set_left_corner_round(true);
        frp.set_right_corner_round(true);
        frp.set_track_color(style_track_color.value());
    }
}
