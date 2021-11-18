//! Define `Animation` for colors. Note that we choose the Lab space as state space for
//! animations to get nicer transitions in terms of lightness/chroma/hue and avoid the
//! discontinuities of the polar coordinates of Lcha (i.e., a transition from hue 1 to 359 would go
//! through all hues instead of taking the shorter trip "backwards").

use super::*;
use crate::prelude::*;

use enso_frp as frp;

use crate::display::shape::*;



// =================
// === Animation ===
// =================

crate::define_endpoints! {
    Input {
        target       (Lcha),
        target_alpha (f32),
        target_color (Lch),
    }
    Output {
        value (Lcha),
    }
}

/// The `Animation` provides color better animations for colors than the raw
/// `component::DEPRECATED_Animation<_>`, as it allows controlling the alpha channel separately
/// which is important for nice fade outs.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Animation {
    frp:        FrpEndpoints,
    color_anim: crate::Animation<Lch>,
    alpha_anim: crate::Animation<f32>,
}

impl Deref for Animation {
    type Target = FrpEndpoints;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Animation {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        let frp = Frp::extend(network);
        let color_anim = crate::Animation::new_non_init(network);
        let alpha_anim = crate::Animation::new_non_init(network);
        Self { frp, color_anim, alpha_anim }.init(network)
    }

    fn init(self, network: &frp::Network) -> Self {
        frp::extend! { network
            color_of_target        <- self.frp.target.map(|t|t.opaque);
            alpha_of_target        <- self.frp.target.map(|t|t.alpha);
            target_color           <- any(&self.frp.target_color,&color_of_target);
            target_alpha           <- any(&self.frp.target_alpha,&alpha_of_target);
            self.color_anim.target <+ target_color;
            self.alpha_anim.target <+ target_alpha;
            self.frp.source.value  <+ all(&self.color_anim.value,&self.alpha_anim.value).map(
                |(color,alpha)| color.with_alpha(*alpha)
            );
        }
        self
    }
}
