//! FRP bindings for browser `setInterval` timer.

use crate::prelude::*;

use crate as frp;

use super::RawTimer;



// ================
// === Interval ===
// ================

/// Periodic timer.
///
/// The timer can be started or stopped at any time using `restart` and `stop` inputs. After it is
/// started, an `on_interval` event will be emitted periodically with period provided during start.
/// The events will be emitted until the timer is stopped.
///
/// in `restart`:      5ms-------------2ms---------------3ms------
/// in `stop`:         -----------------------------x-----x-------
/// out `on_interval`: -----x-----x-------x--x--x--x--------------
///
/// The timer is based on `setInterval` browser API. That means there is no guarantee about the
/// exact time the events will be emitted. The true period between events will approach provided
/// value over time.
#[derive(Clone, CloneRef, Debug)]
pub struct Interval {
    /// Starts the timer with provided period value, specified in integer milliseconds. If the
    /// timer was already started, it is restarted with new period value.
    pub restart:     frp::Any<u32>,
    /// Stops the timer. No `on_interval` events will be emitted until it is started again.
    pub stop:        frp::Any,
    /// Triggered periodically after the timer is started.
    pub on_interval: frp::Stream<()>,
    raw_interval:    Rc<RawTimer>,
}

impl Interval {
    /// Constructor. Timer is initially not started.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
        }


        frp::extend! { network
            restart <- any_mut::<u32>();
            stop    <- any_mut::<()>();
            on_interval <- any_mut();

            let timer = Rc::new(RawTimer::new(f!(on_interval.emit(()))));
            eval    restart((duration) timer.set_interval(*duration));
            eval_   stop(timer.stop());
        }
        let on_interval = on_interval.into();
        Self { on_interval, restart, stop, raw_interval: timer }
    }
}
