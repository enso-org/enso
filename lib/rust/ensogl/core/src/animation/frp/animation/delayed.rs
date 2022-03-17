//! Animation that has a delayed onset.

use crate::prelude::*;

use crate::Easing;

use enso_frp as frp;



// ===========
// === Frp ===
// ===========

crate::define_endpoints! {
    Input {
        /// Start the onset of the animation. After the specified delay, the animation will run
        /// with the specified duration.
        start(),
        /// Reset animation immediately to 0.0.
        reset(),
        /// Set the onset delay of the animation.
        set_delay(f32),
        /// Set the duration of the animation.
        set_duration(f32),
    }
    Output {
        /// Represents the numeric state of the animation in the range 0...1.
        value(f32),
        /// Triggered when the state reaches 1.0.
        on_end(),
        /// Triggered when the state reaches 0.0.
        on_reset(),
    }
}



// =========================
// === DelayedAnimation ===
// =========================

/// Animation that has a delayed onset.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct DelayedAnimation {
    #[allow(missing_docs)]
    pub frp: FrpEndpoints,
}

impl DelayedAnimation {
    #[allow(missing_docs)]
    pub fn new(network: &frp::Network) -> Self {
        let frp = Frp::extend(network);
        let delay = Easing::new(network);
        let transition = Easing::new(network);

        frp::extend! { network
            // Set delay duration.
            delay.set_duration      <+ frp.set_delay;
            transition.set_duration <+ frp.set_duration;

            // Start the delay.
            delay.target <+ frp.start.constant(1.0);

            // End delay if 1.0 reached.
            delay_end <- delay.value.map(|t| (t - 1.0).abs() < std::f32::EPSILON).on_true();

            // Start animation.
            transition.target <+ delay_end.constant(1.0);

            // Reset the animation and delay.
            delay.stop_and_rewind      <+ frp.reset.constant(0.0);
            transition.stop_and_rewind <+ frp.reset.constant(0.0);

            // Output bindings.
            frp.source.value    <+ transition.value;
            frp.source.on_reset <+ transition.value.map(|t|
                (t - 0.0).abs() < std::f32::EPSILON).on_true();
            frp.source.on_end   <+ transition.value.map(|t|
                (t - 1.0).abs() < std::f32::EPSILON).on_true();
        }

        Self { frp }
    }
}
