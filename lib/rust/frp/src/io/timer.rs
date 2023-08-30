//! Browser timer handlers wrapped in FRP API.

use crate::prelude::*;



mod delayed_interval;
mod interval;
mod timeout;


// ==============
// === Export ===
// ==============

pub use delayed_interval::*;
pub use interval::*;
pub use timeout::*;



// ==================
// === RawTimeout ===
// ==================

/// A common low-level abstraction for a reconfigurable JS timer with a non-changing `Fn` handler.
#[derive(Derivative)]
#[derivative(Debug)]
pub(self) struct RawTimer {
    timer_handle: RefCell<Option<enso_web::CleanupHandle>>,
    #[derivative(Debug = "ignore")]
    handler:      Rc<dyn Fn() + 'static>,
}

impl RawTimer {
    /// Create a new timer with a given handler function. The function will be called whenever the
    /// timer fires (either by timeout expiration or interval).
    fn new(handler: impl Fn() + 'static) -> Self {
        Self { handler: Rc::new(handler), timer_handle: default() }
    }

    /// Start the timer with a given timeout. The timer will be stopped if it was already running.
    fn set_timeout(&self, time: u32) {
        let handler = self.handler.clone();
        self.timer_handle.replace(Some(enso_web::set_timeout(move || handler(), time)));
    }

    /// Start the timer with a given interval. The timer will be stopped if it was already running.
    fn set_interval(&self, time: u32) {
        let handler = self.handler.clone();
        self.timer_handle.replace(Some(enso_web::set_interval(move || handler(), time)));
    }

    /// Stop the timer if it was running.
    fn stop(&self) {
        self.timer_handle.take();
    }
}
