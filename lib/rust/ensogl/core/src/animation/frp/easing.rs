//! FRP bindings to the animation engine.

use crate::prelude::*;

use crate::animation::easing;

use enso_frp as frp;



// ==============
// === Easing ===
// ==============

crate::define_endpoints! {
    Input {
        set_duration    (f32),
        target          (f32),
        stop_and_rewind (f32),
        skip (),
    }
    Output {
        value  (f32),
        on_end (easing::EndStatus),
    }
}

/// Easing FRP animator. To learn more about easing functions, follow the link:
/// https://easings.net/en.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Easing {
    pub frp: FrpEndpoints,
}

impl Deref for Easing {
    type Target = FrpEndpoints;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Easing {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        let frp = Frp::extend(network);
        let easing = easing::quad_in_out();
        let on_step = Box::new(f!((t) frp.source.value.emit(t)));
        let on_end = Box::new(f!((t) frp.source.on_end.emit(t)));
        let animator = easing::DynAnimator::new_not_started(0.0, 1.0, easing, on_step, on_end);
        network.store(&animator);
        Self { frp }.init(network, &animator)
    }

    fn init(
        self,
        network: &frp::Network,
        animator: &easing::DynAnimator<f32, easing::QuadInOut>,
    ) -> Self {
        let frp = &self.frp;
        frp::extend! { network
            eval frp.set_duration    ((t) animator.set_duration((*t).ms()));
            eval frp.target          ((t) animator.from_now_to(*t));
            eval frp.stop_and_rewind ((t) animator.stop_and_rewind_to(*t));
            eval_ frp.skip (animator.skip());
        }
        self
    }
}
