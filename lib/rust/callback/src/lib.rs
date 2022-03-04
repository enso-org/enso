//! Definitions of a callback registry – utility allowing attaching and running attached functions.

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

use enso_prelude::*;

use std::any::TypeId;
use std::marker::Unsize;



// ==============================
// === Popular Callback Types ===
// ==============================

pub use callback_types::*;

/// Popular callback types. These are aliases for static [`Fn`] and [`FnMut`] with different amount
/// of arguments. The names directly correspond to the [`::registry`] namespace. For example,
/// the [`::registry::CopyMut3`] is a callback registry for [`CopyMut3`] callbacks.
#[allow(missing_docs)]
mod callback_types {
    pub trait NoArgs = 'static + Fn();
    pub trait MutNoArgs = 'static + FnMut();

    pub trait Copy1<T1> = 'static + Fn(T1);
    pub trait Copy2<T1, T2> = 'static + Fn(T1, T2);
    pub trait Copy3<T1, T2, T3> = 'static + Fn(T1, T2, T3);
    pub trait Copy4<T1, T2, T3, T4> = 'static + Fn(T1, T2, T3, T4);
    pub trait Copy5<T1, T2, T3, T4, T5> = 'static + Fn(T1, T2, T3, T4, T5);

    pub trait Ref1<T1> = 'static + Fn(&T1);
    pub trait Ref2<T1, T2> = 'static + Fn(&T1, &T2);
    pub trait Ref3<T1, T2, T3> = 'static + Fn(&T1, &T2, &T3);
    pub trait Ref4<T1, T2, T3, T4> = 'static + Fn(&T1, &T2, &T3, &T4);
    pub trait Ref5<T1, T2, T3, T4, T5> = 'static + Fn(&T1, &T2, &T3, &T4, &T5);

    pub trait CopyMut1<T1> = 'static + FnMut(T1);
    pub trait CopyMut2<T1, T2> = 'static + FnMut(T1, T2);
    pub trait CopyMut3<T1, T2, T3> = 'static + FnMut(T1, T2, T3);
    pub trait CopyMut4<T1, T2, T3, T4> = 'static + FnMut(T1, T2, T3, T4);
    pub trait CopyMut5<T1, T2, T3, T4, T5> = 'static + FnMut(T1, T2, T3, T4, T5);

    pub trait RefMut1<T1> = 'static + FnMut(&T1);
    pub trait RefMut2<T1, T2> = 'static + FnMut(&T1, &T2);
    pub trait RefMut3<T1, T2, T3> = 'static + FnMut(&T1, &T2, &T3);
    pub trait RefMut4<T1, T2, T3, T4> = 'static + FnMut(&T1, &T2, &T3, &T4);
    pub trait RefMut5<T1, T2, T3, T4, T5> = 'static + FnMut(&T1, &T2, &T3, &T4, &T5);
}



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



// ==================
// === RegistryFn ===
// ==================

/// An abstraction for a [`Fn`] functions kept in the [`Registry`]. It is used to unify the handling
/// of [`Fn`] and [`FnMut`] functions. They are kept either in this structure or in the
/// [`RegistryFnMut`] one, both exposing the same API.
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct RegistryFn<F: ?Sized> {
    #[derivative(Debug = "ignore")]
    function: Rc<F>,
}

/// An abstraction for a [`FnMut`] functions kept in the [`Registry`]. See the documentation of
/// [`RegistryFn`] to learn more.
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct RegistryFnMut<F: ?Sized> {
    #[derivative(Debug = "ignore")]
    function: Rc<RefCell<F>>,
}

/// Constructor abstraction for [`RegistryFn`] and [`RegistryFnMut`].
#[allow(missing_docs)]
pub trait RegistryFnNew {
    type InternalF: ?Sized;
    fn new<C: Unsize<Self::InternalF> + 'static>(f: C) -> Self;
}

/// Call abstraction for [`RegistryFn`] and [`RegistryFnMut`].
#[allow(missing_docs)]
pub trait RegistryFnCall<Args> {
    fn call(&self, args: Args);
}

impl<F: ?Sized> RegistryFnNew for RegistryFn<F> {
    type InternalF = F;
    fn new<C: Unsize<F> + 'static>(f: C) -> Self {
        let function = Rc::new(f);
        Self { function }
    }
}

impl<F: ?Sized> RegistryFnNew for RegistryFnMut<F> {
    type InternalF = F;
    fn new<C: Unsize<F> + 'static>(f: C) -> Self {
        let function = Rc::new(RefCell::new(f));
        Self { function }
    }
}

impl<Args, F: ?Sized + Fn<Args>> RegistryFnCall<Args> for RegistryFn<F> {
    fn call(&self, args: Args) {
        (&*self.function).call(args);
    }
}

impl<Args, F: ?Sized + FnMut<Args>> RegistryFnCall<Args> for RegistryFnMut<F> {
    fn call(&self, args: Args) {
        (&mut *self.function.borrow_mut()).call_mut(args);
    }
}



// ================
// === Registry ===
// ================

/// The main callback registry structure. The [`F`] parameter is either instantiated to
/// [`RegistryFn<dyn Fn<Args>>`] or to [`RegistryFnMut<dyn FnMut<Args>>`]. See the generated aliases
/// for common types below, in the [`registry`] module.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(missing_docs)]
pub struct Registry<F> {
    #[derivative(Debug = "ignore")]
    callback_list: Rc<RefCell<Vec<(Guard, F)>>>,
}

impl<F> Registry<F> {
    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new callback. Returns a new [`Handle`], which dropped, will unregister the callback.
    pub fn add<C>(&self, callback: C) -> Handle
    where
        F: RegistryFnNew,
        C: Unsize<<F as RegistryFnNew>::InternalF> + 'static, {
        let callback = F::new(callback);
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

    /// Fires all registered callbacks and removes the ones which got dropped. The implementation
    /// is safe - you are allowed to change the registry while a callback is running.
    pub fn run_all_with_args<Args: Copy>(&self, args: Args)
    where F: Clone + RegistryFnCall<Args> {
        self.clear_unused_callbacks();
        // The clone is performed in order for the callbacks to be able to register new ones.
        let callbacks = self.callback_list.borrow().clone();
        callbacks.iter().for_each(move |(_, callback)| {
            callback.call(args);
        });
    }
}

/// Aliases for common [`Registry`] instantiations. The names directly correspond to the
/// [`::callback_types`] namespace. For example, the [`::registry::CopyMut3`] is a callback registry
/// for [`CopyMut3`] callbacks.
///
/// The used naming convention is `("Copy" | "Ref") ("Mut" | "") ("NoArgs" | <arg number>)`:
/// - The "Copy" prefix means that arguments are passed as copies. You should use this registry type
///   when working with callbacks consuming primitive types, such as [`usize`].
/// - The "Ref" prefix means that the arguments are passed as references. You should use this
///   registry type when working with callbacks consuming complex types, not implementing [`Copy`].
/// - The registry types which contain the "Mut" part accept `FnMut<Args>` functions. The rest
///   accepts the `Fn<Args>` ones.
/// - The arg number is a number from 1 to 5 indicating the number of arguments callbacks accept.
///
/// For example:
/// - The [`CopyMut2`] registry accepts callbacks in a form of [`FnMut(T1,T2)`], where both [`T1`]
///   and [`T2`] will be passed as copies.
/// - The [`Ref1`] registry accepts callbacks in a form of [`Fn(&T1)`].
/// - The [`NoArgs`] registry is a registry for [`Fn()`] functions.
/// - The [`MutNoArgs`] registry is a registry for [`FnMut()`] functions.
///
/// It is possible to define a registry which uses callbacks whose arguments are a mix of references
/// and copy-able values. However, such types need to be defined manually and are not provided by
/// the alias set below.
#[allow(missing_docs)]
pub mod registry {
    use super::*;

    pub type NoArgs = Registry<RegistryFn<dyn Fn()>>;
    pub type MutNoArgs = Registry<RegistryFnMut<dyn FnMut()>>;

    pub type Copy1<T1> = Registry<RegistryFn<dyn Fn(T1)>>;
    pub type Copy2<T1, T2> = Registry<RegistryFn<dyn Fn(T1, T2)>>;
    pub type Copy3<T1, T2, T3> = Registry<RegistryFn<dyn Fn(T1, T2, T3)>>;
    pub type Copy4<T1, T2, T3, T4> = Registry<RegistryFn<dyn Fn(T1, T2, T3, T4)>>;
    pub type Copy5<T1, T2, T3, T4, T5> = Registry<RegistryFn<dyn Fn(T1, T2, T3, T4, T5)>>;

    pub type Ref1<T1> = Registry<RegistryFn<dyn Fn(&T1)>>;
    pub type Ref2<T1, T2> = Registry<RegistryFn<dyn Fn(&T1, &T2)>>;
    pub type Ref3<T1, T2, T3> = Registry<RegistryFn<dyn Fn(&T1, &T2, &T3)>>;
    pub type Ref4<T1, T2, T3, T4> = Registry<RegistryFn<dyn Fn(&T1, &T2, &T3, &T4)>>;
    pub type Ref5<T1, T2, T3, T4, T5> = Registry<RegistryFn<dyn Fn(&T1, &T2, &T3, &T4, &T5)>>;

    pub type CopyMut1<T1> = Registry<RegistryFnMut<dyn FnMut(T1)>>;
    pub type CopyMut2<T1, T2> = Registry<RegistryFnMut<dyn FnMut(T1, T2)>>;
    pub type CopyMut3<T1, T2, T3> = Registry<RegistryFnMut<dyn FnMut(T1, T2, T3)>>;
    pub type CopyMut4<T1, T2, T3, T4> = Registry<RegistryFnMut<dyn FnMut(T1, T2, T3, T4)>>;
    pub type CopyMut5<T1, T2, T3, T4, T5> = Registry<RegistryFnMut<dyn FnMut(T1, T2, T3, T4, T5)>>;

    pub type RefMut1<T1> = Registry<RegistryFnMut<dyn FnMut(&T1)>>;
    pub type RefMut2<T1, T2> = Registry<RegistryFnMut<dyn FnMut(&T1, &T2)>>;
    pub type RefMut3<T1, T2, T3> = Registry<RegistryFnMut<dyn FnMut(&T1, &T2, &T3)>>;
    pub type RefMut4<T1, T2, T3, T4> = Registry<RegistryFnMut<dyn FnMut(&T1, &T2, &T3, &T4)>>;
    pub type RefMut5<T1, T2, T3, T4, T5> =
        Registry<RegistryFnMut<dyn FnMut(&T1, &T2, &T3, &T4, &T5)>>;
}



// ======================
// === RegistryRunner ===
// ======================

/// Generator of traits allowing the usage of a [`run_all`] function. It is an alias for the
/// [`Registry::run_all_with_args`] where arguments are passed in a convenient way, instead than in
/// a tuple.
macro_rules! gen_runner_traits {
    ($name:ident, $ref_name:ident, ($($arg:ident),*)) => {
        #[allow(non_snake_case)]
        pub trait $name {
            $( type $arg; )*
            fn run_all(&self, $($arg : Self::$arg),*);
        }

        #[allow(non_snake_case)]
        pub trait $ref_name {
            $( type $arg; )*
            fn run_all(&self, $($arg : &Self::$arg),*);
        }
    };
}

macro_rules! gen_runner {
    ($name:ident, $ref_name:ident, $data:ident, $data_ref:ident, <$($arg:ident),*>) => {
        #[allow(non_snake_case)]
        impl<$($arg: Copy),*> $name for registry::$data<$($arg),*> {
            $(type $arg = $arg;)*
            fn run_all(&self, $($arg : Self::$arg),*) {
                self.run_all_with_args(($($arg),*,))
            }
        }

        #[allow(non_snake_case)]
        impl<$($arg),*> $ref_name for registry::$data_ref<$($arg),*>  {
            $(type $arg = $arg;)*
            fn run_all(&self, $($arg : &Self::$arg),*) {
                self.run_all_with_args(($($arg),*,))
            }
        }
    };
}



// ==============
// === Traits ===
// ==============

/// All trait types that should be imported to the scope when using callback registry.
#[allow(missing_docs)]
#[allow(non_camel_case_types)]
pub mod traits {
    use super::*;

    pub trait RegistryRunner0 {
        fn run_all(&self);
    }

    impl RegistryRunner0 for registry::MutNoArgs {
        fn run_all(&self) {
            self.run_all_with_args(())
        }
    }

    impl RegistryRunner0 for registry::NoArgs {
        fn run_all(&self) {
            self.run_all_with_args(())
        }
    }

    gen_runner_traits!(RegistryRunner1, RegistryRunnerRef1, (T1));
    gen_runner_traits!(RegistryRunner2, RegistryRunnerRef2, (T1, T2));
    gen_runner_traits!(RegistryRunner3, RegistryRunnerRef3, (T1, T2, T3));
    gen_runner_traits!(RegistryRunner4, RegistryRunnerRef4, (T1, T2, T3, T4));
    gen_runner_traits!(RegistryRunner5, RegistryRunnerRef5, (T1, T2, T3, T4, T5));

    gen_runner!(RegistryRunner1, RegistryRunnerRef1, CopyMut1, RefMut1, <T1>);
    gen_runner!(RegistryRunner2, RegistryRunnerRef2, CopyMut2, RefMut2, <T1,T2>);
    gen_runner!(RegistryRunner3, RegistryRunnerRef3, CopyMut3, RefMut3, <T1,T2,T3>);
    gen_runner!(RegistryRunner4, RegistryRunnerRef4, CopyMut4, RefMut4, <T1,T2,T3,T4>);
    gen_runner!(RegistryRunner5, RegistryRunnerRef5, CopyMut5, RefMut5, <T1,T2,T3,T4,T5>);
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
    #[allow(clippy::type_complexity)]
    listener_map: HashMap<TypeId, Vec<(Guard, Box<dyn RefMut1<DynEvent>>)>>,
}

impl DynEventDispatcher {
    /// Registers a new listener for a given type.
    pub fn add_listener<F: RefMut1<T>, T: 'static>(&mut self, mut f: F) -> Handle {
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
