///! Frp of the range selector.
use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::shape::*;
use ensogl_theme as theme;

use crate::component;

use super::bounds::absolute_value;
use super::bounds::normalise_value;
use super::bounds::should_clamp_with_overflow;
use super::Bounds;
use super::Model;



// ===========
// === Frp ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_value(Bounds),
        resize(Vector2),
        set_bounds(Bounds),
        use_overflow_bounds(Option<Bounds>),
        set_caption(Option<String>),
        set_left_corner_round(bool),
        set_right_corner_round(bool),
        set_track_color(color::Rgba),
    }
    Output {
        value(Bounds),
        bounds(Bounds),
    }
}

impl component::Frp<Model> for Frp {
    fn init(&self, app: &Application, model: &Model, style: &StyleWatchFrp) {
        let frp = &self;
        let network = &frp.network;
        let scene = app.display.scene();
        let mouse = &scene.mouse.frp;

        let base_frp = super::Frp::new(model, style, network, frp.resize.clone().into(), mouse);

        model.use_track_handles(true);
        model.show_background(true);

        let style_track_color = style.get_color(theme::component::slider::track::color);

        frp::extend! { network

            // Rebind
            frp.source.bounds <+ frp.set_bounds;
            frp.source.value  <+ frp.set_value;

            // Simple Inputs
            eval frp.set_caption((caption) model.set_caption_center(caption.clone()));
            eval frp.set_left_corner_round ((value) model.left_corner_round(*value));
            eval frp.set_right_corner_round((value) model.right_corner_round(*value));
            eval frp.set_track_color((value) model.set_track_color(*value));

            // Internal
            norm_value <- all2(&frp.value,&frp.bounds).map(|(Bounds{start,end},bounds)|{
                 Bounds::new(normalise_value(&(*start,*bounds)),normalise_value(&(*end,*bounds)))
            });
            normalised_overflow_bounds <- all(&frp.use_overflow_bounds,&frp.bounds).map(
                |(overflow_bounds,bounds)|
                overflow_bounds.map(|Bounds{start,end}|
                    Bounds::new(normalise_value(&(start,*bounds)),normalise_value(&(end,*bounds)))
                )
            );
            has_underflow <- norm_value.map(|value| (value.start.min(value.end)) < 0.0);
            has_overflow  <- norm_value.map(|value| (value.start.max(value.end)) > 1.0);

            // Slider Updates
            update_slider <- all(&norm_value,&frp.resize);
            eval update_slider(((value,size)) model.set_background_range(*value,*size));
            eval has_underflow ((underflow) model.show_left_overflow(*underflow));
            eval has_overflow ((overflow) model.show_right_overflow(*overflow));
            eval frp.value((value) {
                model.set_left_label_content(value.start);
                model.set_right_label_content(value.end);
            });

            // Mouse IO
            let change_left_and_right_value = base_frp.is_dragging_track.clone_ref();
            change_left_value <- base_frp.is_dragging_left_handle
                || base_frp.is_dragging_left_overflow;
            change_right_value <- base_frp.is_dragging_right_handle
                || base_frp.is_dragging_right_overflow;

            drag_movement <- mouse.translation.gate(&base_frp.is_dragging_any);
            drag_delta    <- drag_movement.map2(&base_frp.track_max_width, |delta,width|
                (delta.x + delta.y) / width
            );

            drag_center_delta <- drag_delta.gate(&change_left_and_right_value);
            center_update     <- drag_center_delta.map2(&norm_value,|delta,Bounds{start,end}|
                Bounds::new(start+delta,end+delta)
            );

            drag_left_delta <- drag_delta.gate(&change_left_value);
            left_update     <- drag_left_delta.map2(&norm_value,|delta,Bounds{start,end}|
                Bounds::new(start+delta,*end)
            );

            drag_right_delta <- drag_delta.gate(&change_right_value);
            right_update     <- drag_right_delta.map2(&norm_value,|delta,Bounds{start,end}|
                Bounds::new(*start,end+delta)
            );

            any_update   <- any3(&center_update,&left_update,&right_update);
            is_in_bounds <- any_update.map2(&normalised_overflow_bounds,should_clamp_with_overflow);
            new_value_absolute <- all(&frp.set_bounds,&any_update).map(|(bounds,Bounds{start,end})|
                Bounds::new(
                    absolute_value(&(*bounds,*start)),absolute_value(&(*bounds,*end))).sorted()
            );
            frp.source.value <+ new_value_absolute.gate(&is_in_bounds);
        }

        // Init defaults
        frp.set_bounds(Bounds::new(0.0, 1.0));
        frp.use_overflow_bounds(None);
        frp.set_value(Bounds::new(0.25, 0.75));
        frp.set_left_corner_round(true);
        frp.set_right_corner_round(true);
        frp.set_track_color(style_track_color.value());
    }
}
