use crate::prelude::*;

use crate::control::callback::Callback;
use crate::control::callback::CallbackHandle;
use crate::control::callback::CallbackRegistry;
use crate::system::web;
use wasm_bindgen::prelude::Closure;


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
#[derive(Shrinkwrap)]
#[derive(Derivative)]
#[derivative(Debug, Default)]
pub struct EventLoop {
    pub rc: Rc<RefCell<EventLoopData>>,
}

impl EventLoop {
    /// Create and start a new event loop.
    pub fn new() -> Self {
        Self::default().init()
    }

    /// Init the event loop.
    fn init(self) -> Self {
        let data = Rc::downgrade(&self.rc);
        let main = move || { data.upgrade().map(|t| t.borrow_mut().run()); };
        with(self.borrow_mut(), |mut data| {
            data.main = Some(Closure::new(main));
            data.run();
        });
        self
    }

    /// Add new callback. Returns `CallbackHandle` which when dropped, removes
    /// the callback as well.
    pub fn add_callback<F: Callback>(&self, callback: F) -> CallbackHandle {
        self.borrow_mut().callbacks.add(callback)
    }
}

impl From<Rc<RefCell<EventLoopData>>> for EventLoop {
    fn from(rc: Rc<RefCell<EventLoopData>>) -> Self {
        Self {rc}
    }
}

// =====================
// === EventLoopData ===
// =====================

/// The internal state of the `EventLoop`.
#[derive(Derivative)]
#[derivative(Debug, Default)]
pub struct EventLoopData {
    pub main      : Option<Closure<dyn FnMut()>>,
    pub main_id   : i32,
    pub callbacks : CallbackRegistry,
}

impl EventLoopData {
    /// Create new instance.
    pub fn run(&mut self) {
        let callbacks   = &mut self.callbacks;
        let callback_id = self.main.as_ref().map_or(default(), |main| {
            callbacks.run_all();
            web::request_animation_frame(main).unwrap()
        });
        self.main_id = callback_id;
    }
}

impl Drop for EventLoopData {
    fn drop(&mut self) {
        web::cancel_animation_frame(self.main_id).ok();
    }
}
