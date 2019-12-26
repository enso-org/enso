use crate::system::web::get_performance;
use crate::system::web::animation_frame_loop::AnimationFrameLoop;
use super::FnAnimation;

use std::rc::Rc;
use std::cell::RefCell;


// ==================================
// === ContinuousTimeAnimatorData ===
// ==================================

struct ContinuousTimeAnimatorData {
    closure      : Box<dyn FnMut(f32)>,
    start_time   : f32,
    current_time : f32
}

impl ContinuousTimeAnimatorData {
    pub fn new<F:FnAnimation>(f:F) -> Self {
        let closure      = Box::new(f);
        let start_time   = get_performance().expect("Couldn't get performance timer").now() as f32;
        let current_time = start_time;
        Self { closure,start_time,current_time }
    }

    pub fn set_time(&mut self, time:f32) {
        self.start_time  = self.current_time + time;
    }
}



// ==============================
// === ContinuousTimeAnimator ===
// ==============================

/// This structure runs an animation with continuous time as its input.
pub struct ContinuousTimeAnimator {
    _animation_loop : AnimationFrameLoop,
    data            : Rc<RefCell<ContinuousTimeAnimatorData>>
}

impl ContinuousTimeAnimator {
    pub fn new<F:FnAnimation>(f:F) -> Self {
        let data            = Rc::new(RefCell::new(ContinuousTimeAnimatorData::new(f)));
        let data_clone      = data.clone();
        let _animation_loop = AnimationFrameLoop::new(move |current_time| {
            let mut data : &mut ContinuousTimeAnimatorData = &mut data_clone.borrow_mut();
            (data.closure)(current_time - data.start_time);
            data.current_time = current_time;
        });
        Self { _animation_loop, data }
    }
}


// === Setters ===

impl ContinuousTimeAnimator {
    pub fn set_time(&mut self, time:f32) {
        self.data.borrow_mut().set_time(time);
    }
}