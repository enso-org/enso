//! Define `Animatable` for colors. Note that we choose the Lab space as state space for
//! animations to get nicer transitions in terms of lightness/chroma/hue and avoid the
//! discontinuities of the polar coordinates of Lcha (i.e., a transition from hue 1 to 359 would go
//! through all hues instead of taking the shorter trip "backwards").

use super::*;

use crate::gui::component::*;

use nalgebra::Vector4;



// =======================
// === Animatable Lcha ===
// =======================

impl HasAnimationSpaceRepr for Lcha { type AnimationSpaceRepr = Vector4<f32>; }

impl From<Lcha> for AnimationLinearSpace<Vector4<f32>> {
    fn from(value:Lcha) -> AnimationLinearSpace<Vector4<f32>> {
        let value = Laba::from(value).into();
        AnimationLinearSpace { value }
    }
}
impl Into<Lcha> for AnimationLinearSpace<Vector4<f32>> {
    fn into(self) -> Lcha {
        Laba::from(self.value).into()
    }
}



// =======================
// === Animatable Rgba ===
// =======================

impl HasAnimationSpaceRepr for Rgba { type AnimationSpaceRepr = Vector4<f32>; }

impl From<Rgba> for AnimationLinearSpace<Vector4<f32>> {
    fn from(value:Rgba) -> AnimationLinearSpace<Vector4<f32>> {
        let value = Laba::from(value).into();
        AnimationLinearSpace { value }
    }
}

impl Into<Rgba> for AnimationLinearSpace<Vector4<f32>> {
    fn into(self) -> Rgba {
        Laba::from(self.value).into()
    }
}



// =======================
// === Animatable Laba ===
// =======================

impl HasAnimationSpaceRepr for Laba { type AnimationSpaceRepr = Vector4<f32>; }

impl From<Laba> for AnimationLinearSpace<Vector4<f32>> {
    fn from(value:Laba) -> AnimationLinearSpace<Vector4<f32>> {
        let value = value.into();
        AnimationLinearSpace { value }
    }
}

impl Into<Laba> for AnimationLinearSpace<Vector4<f32>> {
    fn into(self) -> Laba {
        self.value.into()
    }
}
