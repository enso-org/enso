//! This module implements `ContinuousAnimator`, an object used to run a callback with a continuous
//! time in milliseconds as its input. It can be used to implement a playback mechanism.

use crate::control::EventLoop;
use crate::control::callback::CallbackHandle;
use super::AnimationCallback;

use std::rc::Rc;
use std::cell::RefCell;


// ========================================
// === ContinuousTimeAnimatorProperties ===
// ========================================

struct ContinuousTimeAnimatorProperties {
    callback                   : Box<dyn AnimationCallback>,
    relative_start_ms          : f64,
    absolute_start_ms          : Option<f64>,
    event_loop_callback_handle : Option<CallbackHandle>
}



// ==================================
// === ContinuousTimeAnimatorData ===
// ==================================

struct ContinuousAnimatorData {
    properties : RefCell<ContinuousTimeAnimatorProperties>
}

impl ContinuousAnimatorData {
    fn new<F:AnimationCallback>(f:F) -> Rc<Self> {
        let callback                   = Box::new(f);
        let relative_start_ms          = 0.0;
        let absolute_start_ms          = None;
        let event_loop_callback_handle = None;
        let properties                 = ContinuousTimeAnimatorProperties {
            callback,
            event_loop_callback_handle,
            relative_start_ms,
            absolute_start_ms
        };
        let properties = RefCell::new(properties);
        Rc::new(Self {properties})
    }

    fn on_animation_frame(&self, absolute_time_ms:f64) {
        let absolute_start_ms = self.absolute_start_ms();
        let absolute_start_ms = absolute_start_ms.unwrap_or_else(|| {
            self.set_absolute_start_ms(Some(absolute_time_ms));
            absolute_time_ms
        });
        let relative_start_ms = self.relative_start_ms();
        let relative_time_ms  = absolute_time_ms - absolute_start_ms + relative_start_ms;
        (&mut self.properties.borrow_mut().callback)(relative_time_ms)
    }
}


// === Setters ===

impl ContinuousAnimatorData {
    fn set_time(&self, time:f64) {
        let mut properties = self.properties.borrow_mut();
        properties.relative_start_ms = time;
        properties.absolute_start_ms = None;
    }

    fn set_event_loop_callback_handle(&self, event_loop_callback_handle:Option<CallbackHandle>) {
        self.properties.borrow_mut().event_loop_callback_handle = event_loop_callback_handle;
    }

    fn set_absolute_start_ms(&self, absolute_start_ms:Option<f64>) {
        self.properties.borrow_mut().absolute_start_ms = absolute_start_ms
    }
}


// === Getters ===

impl ContinuousAnimatorData {
    fn relative_start_ms(&self) -> f64 {
        self.properties.borrow().relative_start_ms
    }

    fn absolute_start_ms(&self) -> Option<f64> {
        self.properties.borrow().absolute_start_ms
    }
}

// ==========================
// === ContinuousAnimator ===
// ==========================

/// `ContinuousAnimator` registers itself in `EventLoop`, repeatedly calling an
/// `AnimationCallback` with the playback time in millisecond as its input.
pub struct ContinuousAnimator {
    data : Rc<ContinuousAnimatorData>
}

impl ContinuousAnimator {
    /// Creates `ContinuousAnimator` with an `AnimationCallback`.
    pub fn new<F:AnimationCallback>(event_loop:&mut EventLoop, f:F) -> Self {
        let data            = ContinuousAnimatorData::new(f);
        let weak_data       = Rc::downgrade(&data);
        let callback_handle = event_loop.add_callback(move |absolute_time_ms| {
            weak_data.upgrade().map(|data| data.on_animation_frame(absolute_time_ms));
        });
        data.set_event_loop_callback_handle(Some(callback_handle));
        Self {data}
    }
}


// === Setters ===

impl ContinuousAnimator {
    /// Sets the current playback time.
    pub fn set_time(&mut self, time_ms:f64) {
        self.data.set_time(time_ms);
    }
}
