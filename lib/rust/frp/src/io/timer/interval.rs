//! FRP bindings for browser `setInterval` timer.

use crate::prelude::*;
pub use enso_web::traits::*;

use crate as frp;

use enso_web::window;
use enso_web::Closure;


/// Closure type alias for use in `setInterval` call.
type TimerClosure = Closure<dyn FnMut()>;

/// Periodic timer.
///
/// The timer can be started or stopped at any time using `start` and `stop` inputs. After it is
/// started, an `on_interval` event will be emitted periodically with period provided during
/// start. The events will be emitted until the timer is stopped.
///
/// in `start`:        5ms-------------2ms---------------3ms------
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
    pub start:       frp::Any<i32>,
    /// Stops the timer. No `on_interval` events will be emitted until it is started again.
    pub stop:        frp::Any,
    /// Triggered periodically after the timer is started.
    pub on_interval: frp::Stream<()>,
    raw_interval:    Rc<RawInterval>,
}

impl Interval {
    /// Constructor. Timer is initially not started.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            on_interval <- any_mut();
        }

        let closure: TimerClosure = Closure::new(f!(() on_interval.emit(())));
        let raw_interval = Rc::new(RawInterval::new(closure));

        frp::extend! { network
            start <- any_mut::<i32>();
            stop  <- any_mut::<()>();
            eval  start((duration) raw_interval.start(*duration));
            eval_ stop(raw_interval.stop());
        }
        let on_interval = on_interval.into();
        Self { on_interval, start, stop, raw_interval }
    }
}

#[derive(Debug)]
struct RawInterval {
    closure:      TimerClosure,
    timer_handle: RefCell<Option<i32>>,
}

impl RawInterval {
    fn new(closure: TimerClosure) -> Self {
        Self { closure, timer_handle: default() }
    }

    fn start(&self, time: i32) {
        let js_func = self.closure.as_js_function();
        let result = window.set_interval_with_callback_and_timeout_and_arguments_0(js_func, time);
        let handle = result.expect("setInterval should not fail when provided with valid callback");

        if let Some(old_handle) = self.timer_handle.borrow_mut().replace(handle) {
            window.clear_interval_with_handle(old_handle);
        }
    }

    fn stop(&self) {
        if let Some(handle) = self.timer_handle.borrow_mut().take() {
            window.clear_interval_with_handle(handle);
        }
    }
}

impl Drop for RawInterval {
    fn drop(&mut self) {
        self.stop();
    }
}
