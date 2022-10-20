//! FRP bindings for browser `setInterval` timer.

use crate::prelude::*;

use enso_web::traits::ClosureOps;
use enso_web::window;
use enso_web::Closure;

use std::time::Duration;

/// Closure type alias for use in `setInterval` call.
type TimerClosure = Closure<dyn FnMut()>;

/// A repating timer based on `setIntevral` browser API.
///
/// The timer can be started or stopped at any time. The timer value
/// is provided during the start.
///
/// When the timer expires, `expired` is emitted.
#[derive(Clone, CloneRef, Debug)]
pub struct Interval {
    /// Starts the timer immediately with provided interval value.
    /// If the timer was already started, it is stopled and restarted.
    pub start:       frp::Any<Duration>,
    /// Stops the timer if it is running. No `on_interval` events
    /// will be emitted until it is started again.
    pub stop:        frp::Any,
    /// Emitted when timer interval is reached. Events will be emitted
    /// repeatedly until the timer is stopped.
    pub on_interval: frp::Stream<()>,
    raw_interval:    Rc<RawInterval>,
}

impl Interval {
    /// Constructor. Timer is initially not started.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            on_interval <- any_mut();
        }

        let closure: TimerClosure = Closure::new(f!([on_interval]() on_interval.emit(())));
        let raw_interval = Rc::new(RawInterval::new(closure));

        frp::extend! { network
            start <- any_mut::<Duration>();
            stop  <- any_mut::<()>();
            eval  start((duration) raw_interval.start(duration.as_millis() as i32));
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
        let handle = window
            .set_interval_with_callback_and_timeout_and_arguments_0(
                self.closure.as_js_function(),
                time,
            )
            .expect("setInterval should not fail on known good callback");

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
