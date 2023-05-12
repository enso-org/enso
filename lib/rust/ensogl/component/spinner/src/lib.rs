//! An animated spinner component that can be used to indicate that a process
//! is running.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::display::IntoGlsl;



// ===============
// === Spinner ===
// ===============

const ANIMATION_SPEED: f32 = 0.001;
const SHAPE_RADIUS: f32 = 1.0;
const SHAPE_OFFSET: f32 = 2.0;
const ANIMATION_OFFSET_MS: f32 = 100.0;

/// Convert a time value to an alpha value for the spinner animation. The
/// animation is a sine wave that oscillates between 0 and 1.
fn time_to_alpha<F1: Into<Var<f32>>, F2: Into<Var<f32>>, F3: Into<Var<f32>>>(
    time: F1,
    offset: F2,
    scale: F3,
) -> Var<f32> {
    let time = time.into();
    let offset = offset.into();
    let scale = scale.into();
    Var::from(0.5) + ((time + offset) * scale).sin() / 2.0
}

ensogl_core::shape! {
    alignment = center;
    (style: Style, scale: f32, rgba: Vector4<f32>) {
        let time = &Var::<f32>::from("input_time");
        let radius = (&scale * SHAPE_RADIUS).px();
        let offset = (&scale * (SHAPE_RADIUS * 2.0 + SHAPE_OFFSET)).px();
        let dot1 = Circle(&radius).translate_x(-&offset);
        let dot2 = Circle(&radius);
        let dot3 = Circle(&radius).translate_x(offset);
        let dot3_anim_start = 0.0;
        let dot2_anim_start = dot3_anim_start + ANIMATION_OFFSET_MS;
        let dot1_anim_start = dot2_anim_start + ANIMATION_OFFSET_MS;
        let dot1_alpha = rgba.w() * time_to_alpha(time, dot1_anim_start, ANIMATION_SPEED);
        let dot2_alpha = rgba.w() * time_to_alpha(time, dot2_anim_start, ANIMATION_SPEED);
        let dot3_alpha = rgba.w() * time_to_alpha(time, dot3_anim_start, ANIMATION_SPEED);
        let rgb = rgba.xyz();
        let color1 = format!("srgba({}.x,{}.y,{}.z,{})", rgb, rgb, rgb, dot1_alpha.glsl());
        let color2 = format!("srgba({}.x,{}.y,{}.z,{})", rgb, rgb, rgb, dot2_alpha.glsl());
        let color3 = format!("srgba({}.x,{}.y,{}.z,{})", rgb, rgb, rgb, dot3_alpha.glsl());
        let dot1 = dot1.fill(color1);
        let dot2 = dot2.fill(color2);
        let dot3 = dot3.fill(color3);
        let shape = dot1 + dot2 + dot3;
        shape.into()
    }
}
