//! Definitions of callback handling utilities.

use crate::prelude::*;



// ================
// === Callback ===
// ================

/// Callback accepted by the `CallbackRegistry`.
pub trait CallbackMut = FnMut() + 'static;



// ======================
// === CallbackHandle ===
// ======================

/// Handle to a callback. When the handle is dropped, the callback is removed.
#[derive(Derivative)]
#[derivative(Debug, Default)]
pub struct CallbackHandle {
    rc: Rc<()>
}

impl CallbackHandle {

    /// Create a new handle.
    pub fn new() -> Self {
        default()
    }

    /// Create guard for this handle.
    pub fn guard(&self) -> Guard {
        let weak = Rc::downgrade(&self.rc);
        Guard {weak}
    }

    /// Forget the handle. Warning! You would not be able to stop the callback after performing this
    /// operation.
    pub fn forget(self) {
        std::mem::forget(self)
    }
}

/// CallbackHandle's guard. Used to check if the handle is still valid.
pub struct Guard {
    weak: Weak<()>
}

impl Guard {
    /// Checks if the handle is still valid.
    pub fn exists(&self) -> bool {
        self.weak.upgrade().is_some()
    }
}



// ========================
// === CallbackRegistry ===
// ========================

/// Registry gathering callbacks. Each registered callback is assigned with a handle. Callback and
/// handle lifetimes are strictly connected. As soon a handle is dropped, the callback is removed
/// as well.
#[derive(Derivative)]
#[derivative(Debug, Default)]
pub struct CallbackRegistry {
    #[derivative(Debug="ignore")]
    callback_list: Vec<(Guard, Box<dyn FnMut()>)>
}

impl CallbackRegistry {

    /// Adds new callback and returns a new handle for it.
    pub fn add<F:CallbackMut>(&mut self, callback:F) -> CallbackHandle {
        let callback = Box::new(callback);
        let handle   = CallbackHandle::new();
        let guard    = handle.guard();
        self.callback_list.push((guard, callback));
        handle
    }

    /// Fires all registered callbacks.
    pub fn run_all(&mut self) {
        self.clear_unused_callbacks();
        self.callback_list.iter_mut().for_each(|(_,callback)| callback());
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&mut self) {
        self.callback_list.retain(|(guard,_)| guard.exists());
    }
}
