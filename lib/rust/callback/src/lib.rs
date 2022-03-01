// === Linter configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
// === Features ===
#![feature(trait_alias)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(unsize)]

//! Definitions of callback handling utilities.

use enso_prelude::*;

use std::any::TypeId;
use std::marker::Unsize;


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
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Handle {
    is_invalidated: Rc<Cell<bool>>,
}

impl Handle {
    /// Create guard for this handle.
    pub fn guard(&self) -> Guard {
        Guard { weak: Rc::downgrade(&self.is_invalidated) }
    }

    /// Invalidates all handles. Even if there exist some active handles, the callback will not be
    /// run anymore after performing this operation.
    pub fn invalidate_all_handles(&self) {
        self.is_invalidated.set(true)
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
#[derive(Clone, Debug)]
pub struct Guard {
    weak: Weak<Cell<bool>>,
}

impl Guard {
    /// Checks if the handle is still valid.
    pub fn exists(&self) -> bool {
        self.weak.upgrade().map_or(false, |t| !t.get())
    }
}



// ==========================
// === SharedRegistryMut1 ===
// ==========================
// pub trait CallbackMut1Fn<T> = FnMut(&T) + 'static;
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
    callback_list: Rc<RefCell<Vec<(Guard, Rc<RefCell<dyn for<'a> FnMut(&'a T) + 'static>>)>>>,
}

impl<T> SharedRegistryMut1<T> {
    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds new callback and returns a new handle for it.
    pub fn add<F: CallbackMut1Fn<T>>(&self, callback: F) -> Handle {
        let callback = Rc::new(RefCell::new(callback));
        let handle = Handle::default();
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



// #[derive(CloneRef, Derivative)]
// #[derivative(Clone(bound = ""))]
// #[derivative(Debug(bound = ""))]
// #[derivative(Default(bound = ""))]
// #[allow(clippy::type_complexity)]
// pub struct XSharedRegistryMut<Args = (), Output = ()> {
//     #[derivative(Debug = "ignore")]
//     callback_list: Rc<RefCell<Vec<(Guard, Rc<RefCell<dyn FnMut<Args, Output = Output>>>)>>>,
// }
//
// impl<Args: Copy, Output> XSharedRegistryMut<Args, Output> {
//     pub fn new() -> Self {
//         Self::default()
//     }
//
//     pub fn add<F>(&self, callback: F) -> Handle
//     where F: FnMut<Args, Output = Output> + 'static {
//         let callback = Rc::new(RefCell::new(callback));
//         let handle = Handle::default();
//         let guard = handle.guard();
//         self.callback_list.borrow_mut().push((guard, callback));
//         handle
//     }
//
//     ///Checks whether there are any callbacks registered.
//     pub fn is_empty(&self) -> bool {
//         self.callback_list.borrow().is_empty()
//     }
//
//     /// Checks all registered callbacks and removes the ones which got dropped.
//     fn clear_unused_callbacks(&self) {
//         self.callback_list.borrow_mut().retain(|(guard, _)| guard.exists());
//     }
//
//     /// Fires all registered callbacks and removes the ones which got dropped. The implementation
//     /// is safe - you are allowed to change the registry while a callback is running.
//     pub fn run_all_with_args(&self, args: Args) {
//         self.clear_unused_callbacks();
//         let callbacks = self.callback_list.borrow().clone();
//         callbacks.iter().for_each(move |(_, callback)| {
//             (&mut *callback.borrow_mut()).call_mut(args);
//         });
//     }
// }
//
// impl<Output> XSharedRegistryMut<(), Output> {
//     pub fn run_all(&self) {
//         self.run_all_with_args(())
//     }
// }
//
// impl<T1: Copy, Output> XSharedRegistryMut<(T1,), Output> {
//     pub fn run_all(&self, t1: T1) {
//         self.run_all_with_args((t1,))
//     }
// }
//
// impl<T1: Copy, T2: Copy, Output> XSharedRegistryMut<(T1, T2), Output> {
//     pub fn run_all(&self, t1: T1, t2: T2) {
//         self.run_all_with_args((t1, t2))
//     }
// }
//
// impl<T1: Copy, T2: Copy, T3: Copy, Output> XSharedRegistryMut<(T1, T2, T3), Output> {
//     pub fn run_all(&self, t1: T1, t2: T2, t3: T3) {
//         self.run_all_with_args((t1, t2, t3))
//     }
// }



// ===================
// === RegistryMut ===
// ===================

pub mod RegistryMut {
    use super::*;
    pub type NoArgs = RegistryMutGen<dyn FnMut()>;
    pub type Copy_1<T1> = RegistryMutGen<dyn FnMut(T1)>;
    pub type Copy_2<T1, T2> = RegistryMutGen<dyn FnMut(T1, T2)>;
    pub type Copy_3<T1, T2, T3> = RegistryMutGen<dyn FnMut(T1, T2, T3)>;
    pub type Ref_1<T1> = RegistryMutGen<dyn FnMut(&T1)>;
    pub type Ref_2<T1, T2> = RegistryMutGen<dyn FnMut(&T1, &T2)>;
    pub type Ref_3<T1, T2, T3> = RegistryMutGen<dyn FnMut(&T1, &T2, &T3)>;
}


#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(clippy::type_complexity)]
pub struct RegistryMutGen<F: ?Sized> {
    #[derivative(Debug = "ignore")]
    callback_list: Rc<RefCell<Vec<(Guard, Rc<RefCell<F>>)>>>,
}

impl<F: ?Sized> RegistryMutGen<F> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add<C>(&self, callback: C) -> Handle
    where C: Unsize<F> + 'static {
        let callback = Rc::new(RefCell::new(callback));
        let handle = Handle::default();
        let guard = handle.guard();
        self.callback_list.borrow_mut().push((guard, callback));
        handle
    }

    ///Checks whether there are any callbacks registered.
    pub fn is_empty(&self) -> bool {
        self.callback_list.borrow().is_empty()
    }

    /// Checks all registered callbacks and removes the ones which got dropped.
    fn clear_unused_callbacks(&self) {
        self.callback_list.borrow_mut().retain(|(guard, _)| guard.exists());
    }
}

impl<F: ?Sized> RegistryMutGen<F> {
    /// Fires all registered callbacks and removes the ones which got dropped. The implementation
    /// is safe - you are allowed to change the registry while a callback is running.
    pub fn run_all_with_args<Args: Copy>(&self, args: Args)
    where F: FnMut<Args> {
        self.clear_unused_callbacks();
        let callbacks = self.callback_list.borrow().clone();
        callbacks.iter().for_each(move |(_, callback)| {
            (&mut *callback.borrow_mut()).call_mut(args);
        });
    }
}

impl<Out> RegistryMutGen<dyn FnMut() -> Out> {
    /// Run all callbacks.
    pub fn run_all(&self) {
        self.run_all_with_args(())
    }
}

impl<Out, T1: Copy> RegistryMutGen<dyn FnMut(T1) -> Out> {
    /// Run all callbacks.
    pub fn run_all(&self, t1: T1) {
        self.run_all_with_args((t1,))
    }
}

impl<Out, T1: Copy, T2: Copy> RegistryMutGen<dyn FnMut(T1, T2) -> Out> {
    /// Run all callbacks.
    pub fn run_all(&self, t1: T1, t2: T2) {
        self.run_all_with_args((t1, t2))
    }
}



// #[derive(CloneRef, Derivative)]
// #[derivative(Clone(bound = ""))]
// #[derivative(Debug(bound = ""))]
// #[derivative(Default(bound = ""))]
// #[allow(clippy::type_complexity)]
// pub struct XSharedRegistry<Args, Output = ()> {
//     #[derivative(Debug = "ignore")]
//     callback_list: Rc<RefCell<Vec<(Guard, Rc<dyn Fn<Args, Output = Output>>)>>>,
// }
//
// impl<Args: Copy, Output> XSharedRegistry<Args, Output> {
//     pub fn new() -> Self {
//         Self::default()
//     }
//
//     pub fn add<F>(&self, callback: F) -> Handle
//     where F: Fn<Args, Output = Output> + 'static {
//         let callback = Rc::new(callback);
//         let handle = Handle::default();
//         let guard = handle.guard();
//         self.callback_list.borrow_mut().push((guard, callback));
//         handle
//     }
//
//     ///Checks whether there are any callbacks registered.
//     pub fn is_empty(&self) -> bool {
//         self.callback_list.borrow().is_empty()
//     }
//
//     /// Checks all registered callbacks and removes the ones which got dropped.
//     fn clear_unused_callbacks(&self) {
//         self.callback_list.borrow_mut().retain(|(guard, _)| guard.exists());
//     }
//
//     /// Fires all registered callbacks and removes the ones which got dropped. The implementation
//     /// is safe - you are allowed to change the registry while a callback is running.
//     pub fn run_all(&self, args: Args) {
//         self.clear_unused_callbacks();
//         let callbacks = self.callback_list.borrow().clone();
//         callbacks.iter().for_each(move |(_, callback)| {
//             callback.call(args);
//         });
//     }
// }



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
        let handle = Handle::default();
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
