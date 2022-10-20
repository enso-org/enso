//! FRP bindings for browser `setTimeout` timer.

use crate::prelude::*;

use enso_web::traits::ClosureOps;
use enso_web::window;
use enso_web::Closure;

use std::time::Duration;

/// Closure type alias for use in `setTimeout` call.
type TimerClosure = Closure<dyn FnMut()>;

/// A one-shot timer based on `setIntevral` browser API.
///
/// The timer can be started or stopped at any time. The timer value
/// is provided during the start.
///
/// When the timer expires, `expired` is emitted.
#[derive(Clone, CloneRef, Debug)]
pub struct Timeout {
    /// Starts the timer immediately with provided timeout value.
    /// If the timer was already started, it is cancelled and restarted.
    pub start:      frp::Any<Duration>,
    /// Stops the timer if it is running. No `on_expired` events
    /// will be emitted until it is started again.
    pub cancel:     frp::Any,
    /// Emitted when timer expires. At most one event is emitted for each
    /// start of the timer.
    pub on_expired: frp::Stream<()>,
    raw_timeout:    Rc<RawTimeout>,
}

impl Timeout {
    /// Constructor. Timer is initially not started.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            on_expired <- any_mut();
        }

        let closure: TimerClosure = Closure::new(f!([on_expired]() on_expired.emit(())));
        let raw_timeout = Rc::new(RawTimeout::new(closure));

        frp::extend! { network
            start  <- any_mut::<Duration>();
            cancel <- any_mut::<()>();
            eval   start((duration) raw_timeout.start(duration.as_millis() as i32));
            eval_  cancel(raw_timeout.cancel());
        }
        let on_expired = on_expired.into();
        Self { on_expired, start, cancel, raw_timeout }
    }
}

#[derive(Debug)]
struct RawTimeout {
    closure:      TimerClosure,
    timer_handle: RefCell<Option<i32>>,
}

impl RawTimeout {
    fn new(closure: TimerClosure) -> Self {
        Self { closure, timer_handle: default() }
    }

    fn start(&self, time: i32) {
        let handle = window
            .set_timeout_with_callback_and_timeout_and_arguments_0(
                self.closure.as_js_function(),
                time,
            )
            .expect("setTimeout should not fail on known good callback");

        if let Some(old_handle) = self.timer_handle.borrow_mut().replace(handle) {
            window.clear_timeout_with_handle(old_handle);
        }
    }

    fn cancel(&self) {
        if let Some(handle) = self.timer_handle.borrow_mut().take() {
            window.clear_timeout_with_handle(handle);
        }
    }
}
