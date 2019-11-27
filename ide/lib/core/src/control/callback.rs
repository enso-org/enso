use crate::prelude::*;

// ========================
// === CallbackRegistry ===
// ========================

// === Types ===

pub trait Callback = FnMut() + 'static;

// === Handle ===

/// Handle to a callback. When the handle is dropped, the callback is removed.
#[derive(Derivative)]
#[derivative(Debug, Default)]
pub struct CallbackHandle (Rc<()>);

impl CallbackHandle {
    /// Create a new handle.
    pub fn new() -> Self {
        default()
    }
    /// Create guard for this handle.
    pub fn guard(&self) -> Guard {
        Guard(Rc::downgrade(&self.0))
    }
    /// Forget the handle. Warning! You would not be able to stop the callback
    /// after performing this operation.
    pub fn forget(self) {
        std::mem::forget(self)
    }
}

/// CallbackHandle guard. Used to check if the handle is still valid.
pub struct Guard (Weak<()>);
impl Guard {
    /// Checks if the handle is still valid.
    pub fn exists(&self) -> bool {
        self.0.upgrade().is_some()
    }
}

// === Definition ===

/// Registry gathering callbacks. Each registered callback is assigned with
/// a handle. Callback and handle lifetimes are strictly connected. As soon a
/// handle is dropped, the callback is removed as well.
#[derive(Derivative)]
#[derivative(Debug, Default)]
pub struct CallbackRegistry {
    #[derivative(Debug="ignore")]
    pub list: Vec<(Guard, Box<dyn FnMut()>)>
}

impl CallbackRegistry {
    /// Adds new callback and returns a new handle for it.
    pub fn add<F:Callback>(&mut self, callback:F) -> CallbackHandle {
        let callback = Box::new(callback) as Box<dyn FnMut()>;
        let handle   = CallbackHandle::new();
        let guard    = handle.guard();
        self.list.push((guard, callback));
        handle
    }
    /// Fires all registered callbacks.
    pub fn run_all(&mut self) {
        self.list.retain(|(guard,_)| guard.exists());
        self.list.iter_mut().for_each(|(_,callback)| callback());
    }
}