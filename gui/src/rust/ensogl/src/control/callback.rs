//! Definitions of callback handling utilities.

use crate::prelude::*;

use std::any::TypeId;



// ================
// === Callback ===
// ================

/// Immutable callback type.
pub trait CallbackFn = Fn() + 'static;

/// Immutable callback object.
pub type Callback = Box<dyn CallbackFn>;

/// Callback object smart constructor.
#[allow(non_snake_case)]
pub fn Callback<F:CallbackFn>(f:F) -> Callback {
    Box::new(f)
}

/// Mutable callback type.
pub trait CallbackMutFn = FnMut() + 'static;

/// Mutable callback object.
pub type CallbackMut = Box<dyn CallbackMutFn>;

/// Mutable callback type with one parameter.
pub trait CallbackMut1Fn<T> = FnMut(&T) + 'static;

/// Mutable callback object with one parameter.
pub type CallbackMut1<T> = Box<dyn CallbackMut1Fn<T>>;

/// Mutable callback type with one parameter.
pub trait CopyCallbackMut1Fn<T> = FnMut(T) + 'static;

/// Mutable callback object with one parameter.
pub type CopyCallbackMut1<T> = Box<dyn CopyCallbackMut1Fn<T>>;



// ==============
// === Handle ===
// ==============

/// Handle to a callback. When the handle is dropped, the callback is removed.
#[derive(Clone,CloneRef,Debug,Default)]
pub struct Handle {
    rc: Rc<()>
}

impl Handle {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Create guard for this handle.
    pub fn guard(&self) -> Guard {
        Guard {weak: Rc::downgrade(&self.rc)}
    }

    /// Forget the handle. Warning! You would not be able to stop the callback after performing this
    /// operation.
    pub fn forget(self) {
        std::mem::forget(self)
    }
}


// =============
// === Guard ===
// =============

/// Handle's guard. Used to check if the handle is still valid.
#[derive(Debug)]
pub struct Guard {
    weak: Weak<()>
}

impl Guard {
    /// Checks if the handle is still valid.
    pub fn exists(&self) -> bool {
        self.weak.upgrade().is_some()
    }
}



// ================
// === Registry ===
// ================

/// Registry gathering callbacks. Each registered callback is assigned with a handle. Callback and
/// handle lifetimes are strictly connected. As soon a handle is dropped, the callback is removed
/// as well.
#[derive(Derivative)]
#[derivative(Debug,Default(bound=""))]
pub struct Registry1<T> {
    #[derivative(Debug="ignore")]
    callback_list: Vec<(Guard,CallbackMut1<T>)>
}

impl<T> Registry1<T> {
    /// Adds new callback and returns a new handle for it.
    pub fn add<F:CallbackMut1Fn<T>>(&mut self, callback:F) -> Handle {
        let callback = Box::new(callback);
        let handle   = Handle::new();
        let guard    = handle.guard();
        self.callback_list.push((guard,callback));
        handle
    }

    /// Fires all registered callbacks.
    pub fn run_all(&mut self, t:&T) {
        self.clear_unused_callbacks();
        self.callback_list.iter_mut().for_each(move |(_,callback)| callback(t));
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&mut self) {
        self.callback_list.retain(|(guard,_)| guard.exists());
    }
}



// ====================
// === CopyRegistry ===
// ====================

/// Specialized version of `Registry` for arguments implementing `Copy`. Passing copy-able elements
/// as values is more performant than by reference.
#[derive(Derivative)]
#[derivative(Debug,Default(bound=""))]
pub struct CopyRegistry1<T> {
    #[derivative(Debug="ignore")]
    callback_list: Vec<(Guard,CopyCallbackMut1<T>)>
}

impl<T:Copy> CopyRegistry1<T> {
    /// Adds new callback and returns a new handle for it.
    pub fn add<F:CopyCallbackMut1Fn<T>>(&mut self, callback:F) -> Handle {
        let callback = Box::new(callback);
        let handle   = Handle::new();
        let guard    = handle.guard();
        self.callback_list.push((guard,callback));
        handle
    }

    /// Fires all registered callbacks.
    pub fn run_all(&mut self, t:T) {
        self.clear_unused_callbacks();
        self.callback_list.iter_mut().for_each(move |(_,callback)| callback(t));
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&mut self) {
        self.callback_list.retain(|(guard,_)| guard.exists());
    }
}



// ==========================
// === DynEventDispatcher ===
// ==========================

/// A dynamic event wrapper. Dynamic events can be pattern matched by their types. See docs of
/// `DynEventDispatcher` to learn more.
#[derive(Debug,Clone)]
pub struct DynEvent {
    any: Rc<dyn Any>
}

impl DynEvent {
    /// Constructor.
    pub fn new<T:'static>(t:T) -> Self {
        let any = Rc::new(t);
        DynEvent {any}
    }
}

/// A dynamic event dispatcher. Allows dispatching an event of any type and registering listeners
/// for a particular type.
#[derive(Derivative,Default)]
#[derivative(Debug)]
pub struct DynEventDispatcher {
    #[derivative(Debug="ignore")]
    listener_map: HashMap<TypeId,Vec<(Guard,CallbackMut1<DynEvent>)>>
}

impl DynEventDispatcher {
    /// Registers a new listener for a given type.
    pub fn add_listener<F:CallbackMut1Fn<T>,T:'static>(&mut self, mut f:F) -> Handle {
        let callback = Box::new(move |event:&DynEvent| {
            event.any.downcast_ref::<T>().iter().for_each(|t| { f(t) })
        });
        let type_id   = (&PhantomData::<T>).type_id();
        let handle    = Handle::new();
        let guard     = handle.guard();
        let listeners = self.listener_map.entry(type_id).or_insert_with(default);
        listeners.push((guard,callback));
        handle
    }

    /// Dispatch an event to all listeners registered for that particular event type.
    pub fn dispatch(&mut self, event:&DynEvent) {
        let type_id = event.any.type_id();
        self.listener_map.get_mut(&type_id).iter_mut().for_each(|listeners| {
            listeners.retain(|(guard,_)| guard.exists());
            listeners.iter_mut().for_each(move |(_,callback)| callback(event));
        });
    }
}
