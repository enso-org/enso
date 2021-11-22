#![feature(trait_alias)]

//! Definitions of callback handling utilities.

use enso_prelude::*;

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
pub fn Callback<F: CallbackFn>(f: F) -> Callback {
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
#[derive(Clone, CloneRef, Debug)]
pub struct Handle {
    rc: Rc<Cell<bool>>,
}

impl Handle {
    /// Constructor.
    pub fn new() -> Self {
        let rc = Rc::new(Cell::new(true));
        Self { rc }
    }

    /// Create guard for this handle.
    pub fn guard(&self) -> Guard {
        Guard { weak: Rc::downgrade(&self.rc) }
    }

    /// Invalidates all handles. Even if there exist some active handles, the callback will not be
    /// run anymore after performing this operation.
    pub fn invalidate_all_handles(&self) {
        self.rc.set(false)
    }

    /// Forget the handle. Warning! You would not be able to stop the callback after performing this
    /// operation.
    pub fn forget(self) {
        std::mem::forget(self)
    }
}

impl Default for Handle {
    fn default() -> Self {
        Self::new()
    }
}



// =============
// === Guard ===
// =============

/// Handle's guard. Used to check if the handle is still valid.
#[derive(Clone, Debug)]
pub struct Guard {
    weak: Weak<Cell<bool>>,
}

impl Guard {
    /// Checks if the handle is still valid.
    pub fn exists(&self) -> bool {
        match self.weak.upgrade() {
            None => false,
            Some(t) => t.get(),
        }
    }
}



// ================
// === Registry ===
// ================

/// Registry gathering callbacks. Each registered callback is assigned with a handle. Callback and
/// handle lifetimes are strictly connected. As soon a handle is dropped, the callback is removed
/// as well.
#[derive(Default, Derivative)]
#[derivative(Debug)]
pub struct Registry {
    #[derivative(Debug = "ignore")]
    callback_list: Vec<(Guard, CallbackMut)>,
}

impl Registry {
    /// Adds new callback and returns a new handle for it.
    pub fn add<F: CallbackMutFn>(&mut self, callback: F) -> Handle {
        let callback = Box::new(callback);
        let handle = Handle::new();
        let guard = handle.guard();
        self.callback_list.push((guard, callback));
        handle
    }

    ///Checks whether there are any callbacks registered.
    pub fn is_empty(&self) -> bool {
        self.callback_list.is_empty()
    }

    /// Fires all registered callbacks and removes the ones which got dropped.
    pub fn run_all(&mut self) {
        self.clear_unused_callbacks();
        self.callback_list.iter_mut().for_each(move |(_, callback)| callback());
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&mut self) {
        self.callback_list.retain(|(guard, _)| guard.exists());
    }
}



// =========================
// === SharedRegistryMut ===
// =========================

/// Registry gathering callbacks implemented with internal mutability pattern. Each registered
/// callback is assigned with a handle. Callback and handle lifetimes are strictly connected. As
/// soon a handle is dropped, the callback is removed as well.
#[derive(Clone, CloneRef, Default, Derivative)]
#[derivative(Debug)]
#[allow(clippy::type_complexity)]
pub struct SharedRegistryMut {
    #[derivative(Debug = "ignore")]
    callback_list: Rc<RefCell<Vec<(Guard, Rc<RefCell<dyn CallbackMutFn>>)>>>,
}

impl SharedRegistryMut {
    /// Adds new callback and returns a new handle for it.
    pub fn add<F: CallbackMutFn>(&self, callback: F) -> Handle {
        let callback = Rc::new(RefCell::new(callback));
        let handle = Handle::new();
        let guard = handle.guard();
        self.callback_list.borrow_mut().push((guard, callback));
        handle
    }

    ///Checks whether there are any callbacks registered.
    pub fn is_empty(&self) -> bool {
        self.callback_list.borrow().is_empty()
    }

    /// Fires all registered callbacks and removes the ones which got dropped. The implementation is
    /// safe - you are allowed to change the registry while a callback is running.
    pub fn run_all(&self) {
        self.clear_unused_callbacks();
        let callbacks = self.callback_list.borrow().clone();
        callbacks.iter().for_each(|(_, callback)| (&mut *callback.borrow_mut())());
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&self) {
        self.callback_list.borrow_mut().retain(|(guard, _)| guard.exists());
    }
}


// ==========================
// === SharedRegistryMut1 ===
// ==========================

/// Registry gathering callbacks implemented with internal mutability pattern. Each registered
/// callback is assigned with a handle. Callback and handle lifetimes are strictly connected. As
/// soon a handle is dropped, the callback is removed as well.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(clippy::type_complexity)]
pub struct SharedRegistryMut1<T> {
    #[derivative(Debug = "ignore")]
    callback_list: Rc<RefCell<Vec<(Guard, Rc<RefCell<dyn CallbackMut1Fn<T>>>)>>>,
}

impl<T> SharedRegistryMut1<T> {
    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds new callback and returns a new handle for it.
    pub fn add<F: CallbackMut1Fn<T>>(&self, callback: F) -> Handle {
        let callback = Rc::new(RefCell::new(callback));
        let handle = Handle::new();
        let guard = handle.guard();
        self.callback_list.borrow_mut().push((guard, callback));
        handle
    }

    ///Checks whether there are any callbacks registered.
    pub fn is_empty(&self) -> bool {
        self.callback_list.borrow().is_empty()
    }

    /// Fires all registered callbacks and removes the ones which got dropped. The implementation is
    /// safe - you are allowed to change the registry while a callback is running.
    pub fn run_all(&self, t: &T) {
        self.clear_unused_callbacks();
        let callbacks = self.callback_list.borrow().clone();
        callbacks.iter().for_each(|(_, callback)| (&mut *callback.borrow_mut())(t));
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&self) {
        self.callback_list.borrow_mut().retain(|(guard, _)| guard.exists());
    }
}



// =================
// === Registry1 ===
// =================

/// Registry gathering callbacks. Each registered callback is assigned with a handle. Callback and
/// handle lifetimes are strictly connected. As soon a handle is dropped, the callback is removed
/// as well.
#[derive(Derivative)]
#[derivative(Debug, Default(bound = ""))]
pub struct Registry1<T> {
    #[derivative(Debug = "ignore")]
    callback_list: Vec<(Guard, CallbackMut1<T>)>,
}

impl<T> Registry1<T> {
    /// Adds new callback and returns a new handle for it.
    pub fn add<F: CallbackMut1Fn<T>>(&mut self, callback: F) -> Handle {
        let callback = Box::new(callback);
        let handle = Handle::new();
        let guard = handle.guard();
        self.callback_list.push((guard, callback));
        handle
    }

    ///Checks whether there are any callbacks registered.
    pub fn is_empty(&self) -> bool {
        self.callback_list.is_empty()
    }

    /// Fires all registered callbacks and removes the ones which got dropped.
    pub fn run_all(&mut self, t: &T) {
        self.clear_unused_callbacks();
        self.callback_list.iter_mut().for_each(move |(_, callback)| callback(t));
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&mut self) {
        self.callback_list.retain(|(guard, _)| guard.exists());
    }
}



// ====================
// === CopyRegistry ===
// ====================

/// Specialized version of `Registry` for arguments implementing `Copy`. Passing copy-able elements
/// as values is more performant than by reference.
#[derive(Derivative)]
#[derivative(Debug(bound = ""), Default(bound = ""))]
pub struct CopyRegistry1<T> {
    #[derivative(Debug = "ignore")]
    callback_list: Vec<(Guard, CopyCallbackMut1<T>)>,
}

impl<T: Copy> CopyRegistry1<T> {
    /// Adds new callback and returns a new handle for it.
    pub fn add<F: CopyCallbackMut1Fn<T>>(&mut self, callback: F) -> Handle {
        let callback = Box::new(callback);
        let handle = Handle::new();
        let guard = handle.guard();
        self.callback_list.push((guard, callback));
        handle
    }

    ///Checks whether there are any callbacks registered.
    pub fn is_empty(&self) -> bool {
        self.callback_list.is_empty()
    }

    /// Fires all registered callbacks and removes the ones which got dropped.
    pub fn run_all(&mut self, t: T) {
        self.clear_unused_callbacks();
        self.callback_list.iter_mut().for_each(move |(_, callback)| callback(t));
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&mut self) {
        self.callback_list.retain(|(guard, _)| guard.exists());
    }
}



// ==========================
// === DynEventDispatcher ===
// ==========================

/// A dynamic event wrapper. Dynamic events can be pattern matched by their types. See docs of
/// `DynEventDispatcher` to learn more.
#[derive(Debug, Clone)]
pub struct DynEvent {
    any: Rc<dyn Any>,
}

impl DynEvent {
    /// Constructor.
    pub fn new<T: 'static>(t: T) -> Self {
        let any = Rc::new(t);
        DynEvent { any }
    }
}

/// A dynamic event dispatcher. Allows dispatching an event of any type and registering listeners
/// for a particular type.
#[derive(Derivative, Default)]
#[derivative(Debug)]
pub struct DynEventDispatcher {
    #[derivative(Debug = "ignore")]
    listener_map: HashMap<TypeId, Vec<(Guard, CallbackMut1<DynEvent>)>>,
}

impl DynEventDispatcher {
    /// Registers a new listener for a given type.
    pub fn add_listener<F: CallbackMut1Fn<T>, T: 'static>(&mut self, mut f: F) -> Handle {
        let callback = Box::new(move |event: &DynEvent| {
            event.any.downcast_ref::<T>().iter().for_each(|t| f(t))
        });
        let type_id = (&PhantomData::<T>).type_id();
        let handle = Handle::new();
        let guard = handle.guard();
        let listeners = self.listener_map.entry(type_id).or_insert_with(default);
        listeners.push((guard, callback));
        handle
    }

    /// Dispatch an event to all listeners registered for that particular event type.
    pub fn dispatch(&mut self, event: &DynEvent) {
        let type_id = event.any.type_id();
        self.listener_map.get_mut(&type_id).iter_mut().for_each(|listeners| {
            listeners.retain(|(guard, _)| guard.exists());
            listeners.iter_mut().for_each(move |(_, callback)| callback(event));
        });
    }
}
