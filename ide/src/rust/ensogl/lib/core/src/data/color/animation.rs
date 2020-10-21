//! Define `Animatable` for colors. Note that we choose the Lab space as state space for
//! animations to get nicer transitions in terms of lightness/chroma/hue and avoid the
//! discontinuities of the polar coordinates of Lcha (i.e., a transition from hue 1 to 359 would go
//! through all hues instead of taking the shorter trip "backwards").

use crate::prelude::*;
use super::*;

use enso_frp as frp;

use crate::application::Application;
use crate::display::shape::*;
use crate::gui::component::*;
use crate::gui::component::Animation;




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



// ======================
// === Animatable Lch ===
// ======================

impl HasAnimationSpaceRepr for Lch { type AnimationSpaceRepr = Vector3<f32>; }

impl From<Lch> for AnimationLinearSpace<Vector3<f32>> {
    fn from(value:Lch) -> AnimationLinearSpace<Vector3<f32>> {
        let value = Lab::from(value).into();
        AnimationLinearSpace { value }
    }
}

impl Into<Lch> for AnimationLinearSpace<Vector3<f32>> {
    fn into(self) -> Lch {
        Lab::from(self.value).into()
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



// ======================
// === Animatable Rgb ===
// ======================

impl HasAnimationSpaceRepr for Rgb { type AnimationSpaceRepr = Vector3<f32>; }

impl From<Rgb> for AnimationLinearSpace<Vector3<f32>> {
    fn from(value:Rgb) -> AnimationLinearSpace<Vector3<f32>> {
        let value = Lab::from(value).into();
        AnimationLinearSpace { value }
    }
}

impl Into<Rgb> for AnimationLinearSpace<Vector3<f32>> {
    fn into(self) -> Rgb {
        Lab::from(self.value).into()
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



// ======================
// === Animatable Lab ===
// ======================

impl HasAnimationSpaceRepr for Lab { type AnimationSpaceRepr = Vector3<f32>; }

impl From<Lab> for AnimationLinearSpace<Vector3<f32>> {
    fn from(value:Lab) -> AnimationLinearSpace<Vector3<f32>> {
        let value = value.into();
        AnimationLinearSpace { value }
    }
}

impl Into<Lab> for AnimationLinearSpace<Vector3<f32>> {
    fn into(self) -> Lab {
        self.value.into()
    }
}



// ===========
// === Frp ===
// ===========

crate::define_endpoints! {
    Input { }
    Output {
        value  (Lcha),
    }
}



// =======================
// === Color Animation ===
// =======================

/// The `ColorAnimation` provides color better animations for colors than the raw `Animation<_>`,
///as it allows controlling the alpha channel separately which is important for nice fade outs.
#[derive(Clone,CloneRef,Debug)]
pub struct ColorAnimation {
    initialized : Rc<Cell<bool>>,
    frp         : Frp,
    /// Animation of the Lch components of the color.
    pub lch     : Animation<Lch>,
    /// Animation of the alpha component of the color.
    pub alpha   : Animation<f32>,
    /// Stream of the full Lcha color.
    pub value   : frp::Stream<Lcha>,
}

#[allow(missing_docs)]
impl ColorAnimation {

    /// Constructor.
    pub fn new(_app:&Application) -> Self {
        let initialized = default();
        let frp         = Frp::new_network();
        let value       = frp.output.value.clone_ref().into();
        let lch         = Animation::<Lch>::new(&frp.network);
        let alpha       = Animation::<f32>::new(&frp.network);

        Self{initialized,lch,alpha,frp,value}.init()
    }

    fn init(self) -> Self {
        let network = &self.frp.network;
        let frp     = &self.frp;
        let lch     = &self.lch;
        let alpha   = &self.alpha;

        frp::extend! { network

            frp.source.value <+ all(lch.value,alpha.value).map(|(lch,alpha)| {
                lch.with_alpha(*alpha)
            });

        }
        self
    }

    pub fn set_target<T:Into<Lcha>>(&self, color:T) {
        let color = color.into();
        if !self.initialized.replace(true) {
            self.lch.set_value(color.opaque);
            self.alpha.set_value(color.alpha);
        }
        self.lch.set_target_value(color.opaque);
        self.alpha.set_target_value(color.alpha);
    }

    pub fn set_value<T:Into<Lcha>>(&self, color:T) {
        let color = color.into();
        self.lch.set_value(color.opaque);
        self.alpha.set_value(color.alpha);
        self.initialized.replace(true);
    }

    pub fn set_target_alpha(&self, alpha:f32) {
        if !self.initialized.replace(true) {
            self.alpha.set_value(alpha);
        }
        self.alpha.set_target_value(alpha);
    }

    pub fn set_target_lch<T:Into<Lch>>(&self, lch:T) {
        let lch = lch.into();
        if !self.initialized.replace(true) {
            self.lch.set_value(lch);
        }
        self.lch.set_target_value(lch);
    }
}
