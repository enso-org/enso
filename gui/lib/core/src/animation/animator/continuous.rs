use crate::system::web::animation_frame_loop::AnimationFrameLoop;
use crate::system::web::animation_frame_loop::AnimationFrameCallbackGuard;
use super::AnimationCallback;

use std::rc::Rc;
use std::cell::RefCell;


// ========================================
// === ContinuousTimeAnimatorProperties ===
// ========================================

struct ContinuousTimeAnimatorProperties {
    callback          : Box<dyn AnimationCallback>,
    relative_start_ms : f32,
    absolute_start_ms : Option<f32>,
    callback_guard    : Option<AnimationFrameCallbackGuard>
}



// ==================================
// === ContinuousTimeAnimatorData ===
// ==================================

struct ContinuousAnimatorData {
    properties : RefCell<ContinuousTimeAnimatorProperties>
}

impl ContinuousAnimatorData {
    fn new<F:AnimationCallback>(f:F) -> Rc<Self> {
        let callback          = Box::new(f);
        let relative_start_ms = 0.0;
        let absolute_start_ms = None;
        let callback_guard    = None;
        let properties        = ContinuousTimeAnimatorProperties {
            callback,
            callback_guard,
            relative_start_ms,
            absolute_start_ms
        };
        let properties = RefCell::new(properties);
        Rc::new(Self {properties})
    }

    fn on_animation_frame(&self, relative_time_ms:f32) {
        (&mut self.properties.borrow_mut().callback)(relative_time_ms)
    }
}


// === Setters ===

impl ContinuousAnimatorData {
    fn set_time(&self, time:f32) {
        let mut properties = self.properties.borrow_mut();
        properties.relative_start_ms = time;
        properties.absolute_start_ms = None;
    }

    fn set_callback_guard(&self, callback_guard:Option<AnimationFrameCallbackGuard>) {
        self.properties.borrow_mut().callback_guard = callback_guard;
    }

    fn set_absolute_start_ms(&self, absolute_start_ms:Option<f32>) {
        self.properties.borrow_mut().absolute_start_ms = absolute_start_ms
    }
}


// === Getters ===

impl ContinuousAnimatorData {
    fn relative_start_ms(&self) -> f32 {
        self.properties.borrow().relative_start_ms
    }

    fn absolute_start_ms(&self) -> Option<f32> {
        self.properties.borrow().absolute_start_ms
    }
}

// ==========================
// === ContinuousAnimator ===
// ==========================

/// This structure runs an animation with continuous time as its input.
pub struct ContinuousAnimator {
    data : Rc<ContinuousAnimatorData>
}

impl ContinuousAnimator {
    pub fn new<F:AnimationCallback>(event_loop:&mut AnimationFrameLoop, f:F) -> Self {
        let data            = ContinuousAnimatorData::new(f);
        let data_clone      = data.clone();
        let callback_guard  = event_loop.add_callback(move |current_time| {
            let absolute_start_ms = data_clone.absolute_start_ms();
            let absolute_start_ms = absolute_start_ms.unwrap_or_else(|| {
                data_clone.set_absolute_start_ms(Some(current_time));
                current_time
            });
            let relative_start_ms = data_clone.relative_start_ms();
            let relative_time_ms  = current_time - absolute_start_ms + relative_start_ms;
            data_clone.on_animation_frame(relative_time_ms);
        });
        data.set_callback_guard(Some(callback_guard));
        Self {data}
    }
}


// === Setters ===

impl ContinuousAnimator {
    /// Sets the current playback time.
    pub fn set_time(&mut self, time:f32) {
        self.data.set_time(time);
    }
}
