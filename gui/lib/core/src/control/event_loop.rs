//! This module contains implementation of `EventLoop`, a loop manager which runs a
//! `EventLoopCallback` once per frame.

use crate::prelude::*;

use crate::control::callback::CallbackMut;
use crate::control::callback::CallbackMutFn;
use crate::control::callback::CallbackHandle;
use crate::control::callback::CallbackRegistry1;
use crate::system::web;
use wasm_bindgen::prelude::Closure;


// =========================
// === EventLoopCallback ===
// =========================

/// A callback to register in EventLoop, taking time_ms:f64 as its input.
pub trait EventLoopCallback = FnMut(f64) + 'static;



// =================
// === EventLoop ===
// =================


// === Definition ===

/// Event loop system.
///
/// It allows registering callbacks and firing them on demand. After a callback
/// is registered, a `CallbackHandle` is returned. The callback is automatically
/// removed as soon as the handle is dropped. You can also use the `forget`
/// method on the handle to make the callback registered forever, but beware
/// that it can easily lead to memory leaks.
#[derive(Debug,Default,Clone)]
pub struct EventLoop {
    rc: Rc<RefCell<EventLoopData>>,
}

impl EventLoop {
    /// Create and start a new event loop.
    pub fn new() -> Self {
        Self::default().init()
    }

    /// Init the event loop.
    fn init(self) -> Self {
        let data = Rc::downgrade(&self.rc);
        let main = move |time_ms| { data.upgrade().map(|t| t.borrow_mut().run(time_ms)); };
        with(self.rc.borrow_mut(), |mut data| {
            data.main = Some(Closure::new(main));
            web::request_animation_frame(&data.main.as_ref().unwrap()).unwrap();
        });
        self
    }

    /// Add new callback. Returns `CallbackHandle` which when dropped, removes
    /// the callback as well.
    pub fn add_callback<F:EventLoopCallback>(&self, callback:F) -> CallbackHandle {
        self.rc.borrow_mut().callbacks.add(Box::new(callback))
    }

    /// Sets a callback which is called when the loop started.
    pub fn set_on_loop_started<F:CallbackMutFn>(&self, f:F) {
        self.rc.borrow_mut().set_on_loop_started(f)
    }

    /// Sets a callback which is called when the loop finished.
    pub fn set_on_loop_finished<F:CallbackMutFn>(&self, f:F) {
        self.rc.borrow_mut().set_on_loop_finished(f);
    }
}



// =====================
// === EventLoopData ===
// =====================

/// The internal state of the `EventLoop`.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct EventLoopData {
    main             : Option<Closure<dyn EventLoopCallback>>,
    callbacks        : CallbackRegistry1<f64>,
    #[derivative(Debug="ignore")]
    on_loop_started  : CallbackMut,
    #[derivative(Debug="ignore")]
    on_loop_finished : CallbackMut,
    main_id          : i32,
}

impl Default for EventLoopData {
    fn default() -> Self {
        let main             = default();
        let callbacks        = default();
        let main_id          = default();
        let on_loop_started  = Box::new(||{});
        let on_loop_finished = Box::new(||{});
        Self {main,callbacks,on_loop_started,on_loop_finished,main_id}
    }
}

impl EventLoopData {
    /// Create new instance.
    pub fn run(&mut self, time_ms:f64) {
        (self.on_loop_started)();
        let callbacks   = &mut self.callbacks;
        let callback_id = self.main.as_ref().map_or(default(), |main| {
            callbacks.run_all(time_ms);
            web::request_animation_frame(main).unwrap()
        });
        self.main_id = callback_id;
        (self.on_loop_finished)();
    }

    /// Sets a callback which is called when the loop started.
    pub fn set_on_loop_started<F:CallbackMutFn>(&mut self, f:F) {
        self.on_loop_started = Box::new(f);
    }

    /// Sets a callback which is called when the loop finished.
    pub fn set_on_loop_finished<F:CallbackMutFn>(&mut self, f:F) {
        self.on_loop_finished = Box::new(f);
    }
}

impl Drop for EventLoopData {
    fn drop(&mut self) {
        web::cancel_animation_frame(self.main_id).ok();
    }
}
