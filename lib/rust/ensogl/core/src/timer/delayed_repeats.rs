//! Delayed repeating timer based on browser timer APIs.

use super::Interval;
use super::Timeout;
use crate::prelude::*;

use std::time::Duration;

/// A delayed repeating emitter based on `setTimeout` and `setIntevral` browser APIs.
///
/// The timer activation is controlled with `active` input.
///
/// When the timer becomes active, it will emit `on_trigger` event
/// after initial `delay` duration, then further events will be emitted
/// every `interval` duration.
#[derive(Clone, CloneRef, Debug)]
pub struct DelayedRepeats {
    delay_timer:    Timeout,
    interval_timer: Interval,
    /// Determines if timer is running. The timer is started when `active` becomes true.
    pub active:     frp::Any<bool>,
    /// Emitted after initial delay and then every interval, as long as the timer is active.
    pub on_trigger: frp::Stream<()>,
}

impl DelayedRepeats {
    /// Constructor. Timer is initially not started.
    pub fn new(network: &frp::Network, delay: Duration, interval: Duration) -> Self {
        let delay_timer = Timeout::new(network);
        let interval_timer = Interval::new(network);

        frp::extend! { network
            active <- any_mut::<bool>();

            delay_timer.start    <+ active.on_true().constant(delay);
            interval_timer.start <+ delay_timer.on_expired.constant(interval);

            deactivate          <- active.on_false();
            delay_timer.cancel  <+ deactivate;
            interval_timer.stop <+ deactivate;

            on_trigger <- any(delay_timer.on_expired, interval_timer.on_interval);
        }
        Self { delay_timer, interval_timer, active, on_trigger }
    }
}
