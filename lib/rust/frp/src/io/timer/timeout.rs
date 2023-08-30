//! FRP bindings for browser `setTimeout` timer.

use crate::prelude::*;

use crate as frp;

use super::RawTimer;



// ================
// === Timeout ===
// ================

/// One-shot timer.
///
/// The timer can be started or cancelled at any time using `restart` and `cancel` inputs. After it
/// is started, an `on_expired` event will be emitted at most once after the specified time provided
/// during start. If the timer is cancelled before it expires, no event will be emitted during that
/// run. Starting the timer again while it is already running is treated as a restart - previous run
/// is canceled.
///
/// in `restart`:     5ms--------20ms--3ms-------3ms-------
/// in `cancel`:      ----------------------------x----x---
/// out `on_expired`: -----x--------------x----------------
///
/// The timer is based on `setTimeout` browser API. That means there is no guarantee about the
/// exact time the event will be emitted. It might be delayed if the browser event loop is busy. If
/// you need to fire events periodically, prefer [`Interval`] timer in order to avoid unpredictable
/// event trigger rate.
#[derive(Clone, CloneRef, Debug)]
pub struct Timeout {
    /// Starts the timer immediately with provided timeout value, specified in integer
    /// milliseconds. If the timer was already started, it is cancelled and restarted.
    pub restart:    frp::Any<u32>,
    /// Stops the timer if it is running. No `on_expired` events will be emitted until it is
    /// started again.
    pub cancel:     frp::Any,
    /// Emitted when timer expires. At most one event is emitted for each start of the timer.
    pub on_expired: frp::Stream<()>,
    /// Indicates whether the timer is currently running.
    pub is_running: frp::Stream<bool>,
    timer:          Rc<RawTimer>,
}

impl Timeout {
    /// Constructor. Timer is initially not started.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            restart <- any_mut::<u32>();
            cancel  <- any_mut::<()>();

            on_expired <- any_mut();
            stopped_running <- any(&cancel, &on_expired);
            is_running <- bool(&stopped_running, &restart);

            let timer = Rc::new(RawTimer::new(f!(on_expired.emit(()))));
            eval    restart((timeout) timer.set_timeout(*timeout));
            eval_   cancel(timer.stop());
        }
        let on_expired = on_expired.into();
        Self { on_expired, restart, cancel, timer, is_running }
    }
}
