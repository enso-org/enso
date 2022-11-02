//! Delayed repeating timer based on browser timer APIs.

use crate::prelude::*;

use crate as frp;

use frp::io::timer::Interval;
use frp::io::timer::Timeout;



// =======================
// === DelayedInterval ===
// =======================

/// Periodic timer with additional initial delay.
///
/// The timer will generate `on_trigger` events over time, as long as it is active.
///
/// The timer is activated when `restart` receives an event with [`DelayedIntervalConfig`] structure
/// containing `delay_ms` and `interval_ms` values. Every activation begins a sequence of actions:
///     - If the timer was already active, it is stopped and started again.
///     - First `on_trigger` event will be emitted after specified `delay_ms` time.
///     - Subsequent `on_trigger` events will be emitted every `interval_ms` time.
/// Once `stop` receives an event, no more `on_trigger` events will be emitted until the timer
/// is activated again.
///
/// in `restart`:     -5,2--------------3,1-----4,3-----------------
/// in `stop`:        -------------x-----------------------x---x----
/// out `on_trigger`: ------x-x-x-x--------xxxxx----x--x--x---------
///
/// The timer is based on `setTimeout` and `setInterval` browser APIs. That means there is no
/// guarantee about when exactly the events will be emitted. In practice, the initial delay might be
/// slightly longer and there might be some jitter in periodic triggers. This effect will be more
/// pronounced when the browser event loop is very busy.
#[derive(Clone, CloneRef, Debug)]
pub struct DelayedInterval {
    delay_timer:    Timeout,
    interval_timer: Interval,
    /// Timer activation and configuration input. Any time an event is sent to this input, the
    /// timer will be started with provided delay and interval values. If the timer was already
    /// started, it will be restarted with new values.
    pub restart:    frp::Any<DelayedIntervalConfig>,
    /// Timer deactivation input. Any time an event is sent to this input, the timer will be
    /// stopped and mo more `on_trigger` will be emitted until it is started again.
    pub stop:       frp::Any,
    /// Emitted after initial delay and then every interval, as long as the timer is active.
    pub on_trigger: frp::Stream<()>,
}

impl DelayedInterval {
    /// Constructor. Timer is initially not active.
    pub fn new(network: &frp::Network) -> Self {
        let delay_timer = Timeout::new(network);
        let interval_timer = Interval::new(network);

        frp::extend! { network
            restart <- any_mut::<DelayedIntervalConfig>();
            stop <- any_mut();

            delay_timer.restart    <+ restart.map(|c| c.delay_ms);
            interval_timer.restart <+ restart.map(|c| c.interval_ms).sample(&delay_timer.on_expired);

            delay_timer.cancel  <+ stop;
            interval_timer.stop <+ any_(stop, restart);

            on_trigger <- any(delay_timer.on_expired, interval_timer.on_interval);
        }
        Self { delay_timer, interval_timer, restart, stop, on_trigger }
    }
}

/// Configuration structure for `DelayedInterval`.
/// Specifies the time duration of each phase in integer milliseconds.
#[derive(Debug, Clone, Copy, Default)]
pub struct DelayedIntervalConfig {
    /// Initial delay between timer activation and first `on_trigger` event being emitted.
    pub delay_ms:    i32,
    /// Time between subsequent `on_trigger` events.
    pub interval_ms: i32,
}

impl DelayedIntervalConfig {
    /// Constructor.
    pub fn new(delay_ms: i32, interval_ms: i32) -> Self {
        Self { delay_ms, interval_ms }
    }
}
