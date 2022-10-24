//! Delayed repeating timer based on browser timer APIs.

use crate::prelude::*;

use crate as frp;
use frp::io::timer::Interval;
use frp::io::timer::Timeout;

/// Periodic timer with additional initial delay.
///
/// The timer will generate `on_trigger` events over time, as long as it is active.
///
/// Every time `set_active` receives `true`:
///     - the first `on_trigger` event will be emitted after specified `delay_ms` time,
///     - next events will be emitted every `interval_ms` time.
/// Once `set_active` receives `false`, no more `on_trigger` events will be emitted until the timer
/// is activated again.
///
/// in `set_active`:  T----------F----T------T---------F------F-----
/// out `on_trigger`: ----x-x-x-x---------x-x-----x-x-x-------------
/// 
/// The timer is based on `setTimeout` and `setInterval` browser APIs. That means there is no
/// guarantee about when exactly the events will be emitted. In practice, the initial delay might be
/// slightly longer and there might be some jitter in periodic triggers. This effect will be more
/// pronounced when the browser event loop is very busy.
#[derive(Clone, CloneRef, Debug)]
pub struct DelayedInterval {
    delay_timer:    Timeout,
    interval_timer: Interval,
    /// Determines if timer is running. The timer is started when `set_active` is set to `true`,
    /// and stopped when it is set to `false`. Restarting a timer by sending `false` followed
    /// by `true` will start the countdown from the beginning, including configured delay time.
    pub set_active: frp::Any<bool>,
    /// Emitted after initial delay and then every interval, as long as the timer is active.
    pub on_trigger: frp::Stream<()>,
}

impl DelayedInterval {
    /// Constructor. Timer is initially not active.
    /// The time duration of each phase is specified in integer milliseconds.
    pub fn new(network: &frp::Network, delay_ms: i32, interval_ms: i32) -> Self {
        let delay_timer = Timeout::new(network);
        let interval_timer = Interval::new(network);

        frp::extend! { network
            set_active <- any_mut::<bool>();

            delay_timer.start    <+ set_active.on_true().constant(delay_ms);
            interval_timer.start <+ delay_timer.on_expired.constant(interval_ms);

            deactivate          <- set_active.on_false();
            delay_timer.cancel  <+ deactivate;
            interval_timer.stop <+ deactivate;

            on_trigger <- any(delay_timer.on_expired, interval_timer.on_interval);
        }
        Self { delay_timer, interval_timer, set_active, on_trigger }
    }
}
