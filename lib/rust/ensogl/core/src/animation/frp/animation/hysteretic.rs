//! Animation that has a delayed onset and offset.
//!
//! The basic idea behind this animation is, that it changes between two states: of (0.0) and on
//! (1.0), but only after a delay (`st`/`et` start delay, end delay). If the animation gets
//! canceled before the delay has passed, nothing happens, and the delay resets.
//!
//! This can be used to hide state changes that are only happening for a short duration. For example
//! consider a UI element that is shown when hovering ofer an icon. When only moving the mouse
//! cursor past the icon we want to avoid the pop-up appearing and disappearing right away. The
//! `HystereticAnimation` allows this by setting a `start_delay_duration` long enough to avoid
//! triggering during the time it takes to move past the icon. Thus, when the cursor moves past the
//! icon nothing is visible to the user, but if the mosque cursor stays on the icon longer than
//! `start_delay_duration`, the animation starts, and the pop-up becomes visible. In reverse, when
//! moving the cursor between multiple icons, the pop-up should not permanently start and disappear
//! and re-appear. Thus setting a `end_delay_duration` will avoid the pop-up from disappearing, if
//! the time the cursor is between icons is less than the `end_delay_duration`. Instead, the hiding
//! will only start iof the cursos has left any icon triggering the pop-up for longer than the
//! `end_delay_duration`.

use crate::prelude::*;

use crate::Animation;
use crate::Easing;

use enso_frp as frp;



// ===========
// === Frp ===
// ===========

crate::define_endpoints! {
    Input {
        /// Trigger start of animation towards start state (0.0). Will be delayed by onset time.
        to_start(),
        /// Trigger start of animation towards end state (1.0). Will be delayed by onset time.
        to_end(),
    }
    Output {
        /// Represents the numeric state of the animation in the range 0..1.
        value(f32),
        /// Triggered when the state reaches 1.0.
        on_end(),
        /// Triggered when the state reaches 0.0.
        on_start(),
    }
}



// ===========================
// === HystereticAnimation ===
// ===========================

/// Animation that has a delayed onset and offset.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct HystereticAnimation {
    #[allow(missing_docs)]
    pub frp: FrpEndpoints,
}

impl HystereticAnimation {
    #[allow(missing_docs)]
    pub fn new(network: &frp::Network, start_delay_duration: f32, end_delay_duration: f32) -> Self {
        let frp = Frp::extend(network);
        let start_delay = Easing::new(network);
        let end_delay = Easing::new(network);
        start_delay.set_duration(start_delay_duration);
        end_delay.set_duration(end_delay_duration);

        let transition = Animation::<f32>::new(network);

        frp::extend! { network

            during_transition <- any_mut();

            on_end                <- frp.to_end.constant(());
            on_end_while_active   <- on_end.gate(&during_transition);
            on_end_while_inactive <- on_end.gate_not(&during_transition);

            on_start                <- frp.to_start.constant(());
            on_start_while_active   <- on_start.gate(&during_transition);
            on_start_while_inactive <- on_start.gate_not(&during_transition);

            start_delay.target <+ on_start_while_inactive.constant(1.0);
            end_delay.target   <+ on_end_while_inactive.constant(1.0);

            start_delay.stop_and_rewind <+ on_end.constant(0.0);
            end_delay.stop_and_rewind   <+ on_start.constant(0.0);

            offset_start <- end_delay.on_end.map(|t| t.is_normal()).on_true();
            onset_start  <- start_delay.on_end.map(|t| t.is_normal()).on_true();

            onset_end  <- transition.value.map(|t| (t - 1.0).abs() < std::f32::EPSILON).on_true();
            offset_end <- transition.value.map(|t| t.abs() < std::f32::EPSILON).on_true();

            transition.target <+ onset_start.constant(1.0);
            transition.target <+ offset_start.constant(0.0);
            transition.target <+ on_end_while_active.constant(0.0);
            transition.target <+ on_start_while_active.constant(1.0);

            during_transition <+ bool(&onset_end,&onset_start);
            during_transition <+ bool(&offset_end,&offset_start);

            frp.source.value    <+ transition.value;
            frp.source.on_end   <+ onset_end;
            frp.source.on_start <+ offset_end;
        }

        HystereticAnimation { frp }
    }
}
