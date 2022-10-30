//! FRP bindings for browser `setTimeout` timer.

use crate::prelude::*;
pub use enso_web::traits::*;

use crate as frp;

use enso_web::window;
use enso_web::Closure;


// ================
// === Timeout ===
// ================

/// Closure type alias for use in `setTimeout` call.
type TimerClosure = Closure<dyn FnMut()>;

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
    pub restart:    frp::Any<i32>,
    /// Stops the timer if it is running. No `on_expired` events will be emitted until it is
    /// started again.
    pub cancel:     frp::Any,
    /// Emitted when timer expires. At most one event is emitted for each start of the timer.
    pub on_expired: frp::Stream<()>,
    raw_timeout:    Rc<RawTimeout>,
}

impl Timeout {
    /// Constructor. Timer is initially not started.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            on_expired <- any_mut();
        }

        let closure: TimerClosure = Closure::new(f!(on_expired.emit(())));
        let raw_timeout = Rc::new(RawTimeout::new(closure));

        frp::extend! { network
            restart <- any_mut::<i32>();
            cancel  <- any_mut::<()>();
            eval    restart((timeout) raw_timeout.restart(*timeout));
            eval_   cancel(raw_timeout.cancel());
        }
        let on_expired = on_expired.into();
        Self { on_expired, restart, cancel, raw_timeout }
    }
}


// ==================
// === RawTimeout ===
// ==================

#[derive(Debug)]
struct RawTimeout {
    closure:      TimerClosure,
    timer_handle: RefCell<Option<i32>>,
}

impl RawTimeout {
    fn new(closure: TimerClosure) -> Self {
        Self { closure, timer_handle: default() }
    }

    fn restart(&self, time: i32) {
        let js_func = self.closure.as_js_function();
        let result = window.set_timeout_with_callback_and_timeout_and_arguments_0(js_func, time);
        let handle = result.expect("setTimeout should never fail when callback is a function.");
        self.set_timer_handle(Some(handle));
    }

    fn cancel(&self) {
        self.set_timer_handle(None);
    }

    fn set_timer_handle(&self, handle: Option<i32>) {
        if let Some(old_handle) = self.timer_handle.replace(handle) {
            window.clear_timeout_with_handle(old_handle);
        }
    }
}

impl Drop for RawTimeout {
    fn drop(&mut self) {
        self.cancel();
    }
}
