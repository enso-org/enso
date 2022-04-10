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
        }
        self
    }
}



// ========================
// === DEPRECATED_Tween ===
// ========================

/// Smart tween handler. Contains tween animator and frp endpoint. Whenever a new value is computed,
/// it is emitted via the endpoint.
///
/// # DEPRECATION
/// This component is deprecated. Use `Easing` instead, which exposes much more FRP-oriented API
/// than this component.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
#[allow(missing_docs)]
#[allow(non_camel_case_types)]
pub struct DEPRECATED_Tween {
    #[shrinkwrap(main_field)]
    pub animator: easing::DynAnimator<f32, easing::QuadInOut>,
    pub value:    frp::Stream<f32>,
}

impl DEPRECATED_Tween {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            def target = source::<f32>();
        }
        let f = easing::quad_in_out();
        let on_step = Box::new(f!((t) target.emit(t)));
        let on_end = Box::new(|_| {});
        let animator = easing::DynAnimator::new_not_started(0.0, 1.0, f, on_step, on_end);
        let value = target.into();
        Self { animator, value }
    }
}
