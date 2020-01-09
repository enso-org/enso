use basegl_prelude::*;
use super::request_animation_frame;

use std::rc::Rc;
use std::cell::RefCell;
use wasm_bindgen::prelude::Closure;

use data::opt_vec::{OptVec, Ix};


// =============================
// === AnimationLoopCallback ===
// =============================

pub trait AnimationFrameCallback = FnMut(f32) + 'static;



// ===================================
// === AnimationFrameCallbackGuard ===
// ===================================

pub struct AnimationFrameCallbackGuard {
    data : Rc<AnimationFrameData>,
    ix   : Ix
}

impl Drop for AnimationFrameCallbackGuard {
    fn drop(&mut self) {
        self.data.remove_callback(self.ix);
    }
}


// ================================
// === AnimationFrameProperties ===
// ================================

struct AnimationFrameProperties {
    is_running : bool,
    callbacks  : OptVec<Box<dyn AnimationFrameCallback>>
}

impl Drop for AnimationFrameProperties {
    fn drop(&mut self) {
        self.is_running = false
    }
}

// ==========================
// === AnimationFrameData ===
// ==========================

struct AnimationFrameData {
    properties : RefCell<AnimationFrameProperties>
}

impl AnimationFrameData {
    fn new() -> Rc<Self> {
        let is_running = true;
        let callbacks  = Default::default();
        let properties = RefCell::new(AnimationFrameProperties {is_running,callbacks});
        Rc::new(Self { properties })
    }

    fn on_animation_frame(&self, time_ms:f32) {
        for callback in &mut self.properties.borrow_mut().callbacks {
            (callback)(time_ms)
        }
    }
}


// === Setters ===

impl AnimationFrameData {
    fn add_callback
    <T:AnimationFrameCallback>(self:&Rc<Self>, callback:T) -> AnimationFrameCallbackGuard {
        let ix   = self.properties.borrow_mut().callbacks.insert(Box::new(callback));
        let data = self.clone();
        AnimationFrameCallbackGuard { ix, data }
    }

    fn remove_callback(&self, ix:Ix) {
        self.properties.borrow_mut().callbacks.remove(ix);
    }
}


// === Getters ===

impl AnimationFrameData {
    fn is_running(&self) -> bool {
        self.properties.borrow().is_running
    }
}

// ==========================
// === AnimationFrameLoop ===
// ==========================

// FIXME: This is similar to `control/event_loop.rs` and they need to be merged.
/// This struct is runs the registered callbacks once per frame draw.
#[derive(Clone)]
pub struct AnimationFrameLoop {
    data : Rc<AnimationFrameData>
}

impl Default for AnimationFrameLoop {
    fn default() -> Self {
        let nop_func       = Box::new(|_| ());
        let nop_closure    = Closure::once(nop_func);
        let callback       = Rc::new(RefCell::new(nop_closure));
        let data           = AnimationFrameData::new();
        let callback_clone = callback.clone();
        let data_clone     = data.clone();

        *callback.borrow_mut() = Closure::wrap(Box::new(move |time_ms| {
            if data_clone.is_running() {
                data_clone.on_animation_frame(time_ms);
                let callback = &callback_clone.borrow();
                request_animation_frame(&callback).expect("Couldn't request animation frame");
            }
        }));
        request_animation_frame(&callback.borrow()).expect("Couldn't request animation frame");
        AnimationFrameLoop{data}
    }
}

impl AnimationFrameLoop {
    pub fn new() -> Self { default() }

    pub fn add_callback
    <T:AnimationFrameCallback>(&mut self, callback:T) -> AnimationFrameCallbackGuard {
        self.data.add_callback(callback)
    }
}