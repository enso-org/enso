//! Definitions of a callback registry – utility allowing attaching and running attached functions.

// === Features ===
#![feature(trait_alias)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(unsize)]
#![feature(test)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use enso_prelude::*;

use std::marker::Unsize;


// ==============
// === Export ===
// ==============

pub use callback_types::*;



// ==============
// === Handle ===
// ==============

/// Handle to a callback. When the handle is dropped, the callback is removed.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Handle {
    rc: Rc<()>,
}

impl Handle {
    /// Create guard for this handle.
    pub fn guard(&self) -> Guard {
        Guard { weak: Rc::downgrade(&self.rc) }
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
    weak: Weak<()>,
}

impl Guard {
    /// Checks if the handle expired.
    pub fn is_expired(&self) -> bool {
        self.weak.is_expired()
    }
}



// ================
// === Registry ===
// ================

/// The main callback registry structure. The [`F`] parameter is always instantiated to `dyn
/// FnMut<...>`. See the generated aliases for common types below, in the [`registry`] module.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(missing_docs)]
pub struct Registry<F: ?Sized> {
    model: Rc<RegistryModel<F>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(missing_docs)]
struct RegistryModel<F: ?Sized> {
    is_running:               Cell<bool>,
    #[derivative(Debug = "ignore")]
    callback_list:            RefCell<Vec<(Guard, Box<F>)>>,
    /// Temporary buffer to store new callbacks that are registered while the registry is running.
    /// During a run, the [`callback_list`] is borrowed and thus new callbacks cannot be added
    /// because it cannot be mutated. The buffer is processed and emptied after the registry
    /// has finished processing the existing callbacks.
    #[derivative(Debug = "ignore")]
    callback_list_during_run: RefCell<Vec<(Guard, Box<F>)>>,
}

impl<F: ?Sized> Registry<F> {
    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new callback. Returns a new [`Handle`], which dropped, will unregister the callback.
    pub fn add<C>(&self, callback: C) -> Handle
    where C: Unsize<F> + 'static {
        self.model.add(callback)
    }

    ///Checks whether there are any callbacks registered.
    pub fn is_empty(&self) -> bool {
        self.model.is_empty()
    }

    /// Fires all registered callbacks and removes the ones which got dropped. The implementation
    /// is safe - you are allowed to change the registry while a callback is running.
    fn run_impl<Args: Copy>(&self, args: Args)
    where F: FnMut<Args> {
        self.model.run_impl(args)
    }
}

impl<F: ?Sized> RegistryModel<F> {
    fn add<C>(&self, callback: C) -> Handle
    where C: Unsize<F> + 'static {
        let callback = Box::new(callback);
        let handle = Handle::default();
        let guard = handle.guard();
        if self.is_running.get() {
            self.callback_list_during_run.borrow_mut().push((guard, callback));
        } else {
            self.callback_list.borrow_mut().push((guard, callback));
        }
        handle
    }

    fn is_empty(&self) -> bool {
        self.callback_list.borrow().is_empty() && self.callback_list_during_run.borrow().is_empty()
    }

    fn run_impl<Args: Copy>(&self, args: Args)
    where F: FnMut<Args> {
        if self.is_running.get() {
            error!("Trying to run callback manager while it's already running, ignoring.");
            return;
        }
        self.is_running.set(true);
        self.callback_list.borrow_mut().retain_mut(|(guard, callback)| {
            let is_valid = !guard.is_expired();
            if is_valid {
                callback.call_mut(args);
            }
            is_valid
        });
        let mut callback_list_during_run = self.callback_list_during_run.borrow_mut();
        if !callback_list_during_run.is_empty() {
            self.callback_list.borrow_mut().extend(mem::take(&mut *callback_list_during_run));
        }
        self.is_running.set(false);
    }
}



// ========================
// === Registry Aliases ===
// ========================

/// Aliases for common [`Registry`] instances. The names directly correspond to the
/// [`::callback_types`] namespace. For example, the [`::registry::Copy3`] is a callback registry
/// for [`Copy3`] callbacks.
///
/// The used naming convention meaning is provided below:
/// - The "Copy" prefix means that arguments are passed as copies. You should use this registry type
///   when working with callbacks consuming primitive types, such as [`usize`].
/// - The "Ref" prefix means that the arguments are passed as references. You should use this
///   registry type when working with callbacks consuming complex types, not implementing [`Copy`].
/// - The arg number is a number from 1 to 5 indicating the number of arguments callbacks accept.
///
/// For example:
/// - The [`Copy2`] registry accepts callbacks in a form of [`FnMut(T1,T2)`], where both [`T1`] and
///   [`T2`] will be passed as copies.
/// - The [`Ref1`] registry accepts callbacks in a form of [`FnMut(&T1)`].
/// - The [`NoArgs`] registry is a registry for [`FnMut()`] functions.
///
/// It is possible to define a registry which uses an unwrapped closure or a callback whose
/// arguments are a mix of references and copy-able values. However, such types need to be defined
/// manually and are not provided by the alias set below.
#[allow(missing_docs)]
pub mod registry {
    use super::*;

    pub type NoArgs = Registry<dyn FnMut()>;

    pub type Copy1<T1> = Registry<dyn FnMut(T1)>;
    pub type Copy2<T1, T2> = Registry<dyn FnMut(T1, T2)>;
    pub type Copy3<T1, T2, T3> = Registry<dyn FnMut(T1, T2, T3)>;
    pub type Copy4<T1, T2, T3, T4> = Registry<dyn FnMut(T1, T2, T3, T4)>;
    pub type Copy5<T1, T2, T3, T4, T5> = Registry<dyn FnMut(T1, T2, T3, T4, T5)>;

    pub type Ref1<T1> = Registry<dyn FnMut(&T1)>;
    pub type Ref2<T1, T2> = Registry<dyn FnMut(&T1, &T2)>;
    pub type Ref3<T1, T2, T3> = Registry<dyn FnMut(&T1, &T2, &T3)>;
    pub type Ref4<T1, T2, T3, T4> = Registry<dyn FnMut(&T1, &T2, &T3, &T4)>;
    pub type Ref5<T1, T2, T3, T4, T5> = Registry<dyn FnMut(&T1, &T2, &T3, &T4, &T5)>;
}

/// Popular callback types. These are aliases for static [`FnMut`] with different amount of
/// arguments. The names directly correspond to the [`::registry`] namespace. For example,
/// the [`::registry::CopyMut3`] is a callback registry for [`CopyMut3`] callbacks.
#[allow(missing_docs)]
mod callback_types {
    pub trait NoArgs = 'static + FnMut();

    pub trait Copy1<T1> = 'static + FnMut(T1);
    pub trait Copy2<T1, T2> = 'static + FnMut(T1, T2);
    pub trait Copy3<T1, T2, T3> = 'static + FnMut(T1, T2, T3);
    pub trait Copy4<T1, T2, T3, T4> = 'static + FnMut(T1, T2, T3, T4);
    pub trait Copy5<T1, T2, T3, T4, T5> = 'static + FnMut(T1, T2, T3, T4, T5);

    pub trait Ref1<T1> = 'static + FnMut(&T1);
    pub trait Ref2<T1, T2> = 'static + FnMut(&T1, &T2);
    pub trait Ref3<T1, T2, T3> = 'static + FnMut(&T1, &T2, &T3);
    pub trait Ref4<T1, T2, T3, T4> = 'static + FnMut(&T1, &T2, &T3, &T4);
    pub trait Ref5<T1, T2, T3, T4, T5> = 'static + FnMut(&T1, &T2, &T3, &T4, &T5);
}



// ======================
// === RegistryRunner ===
// ======================

/// Generator of traits allowing the usage of a [`run_all`] function. It is an alias for the
/// [`Registry::run_impl`] where values are not passed as a tuple but as separate arguments.
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
                self.run_impl(($($arg),*,))
            }
        }

        #[allow(non_snake_case)]
        impl<$($arg),*> $ref_name for registry::$data_ref<$($arg),*>  {
            $(type $arg = $arg;)*
            fn run_all(&self, $($arg : &Self::$arg),*) {
                self.run_impl(($($arg),*,))
            }
        }
    };
}

macro_rules! gen_fn_trait_impls {
    (<$($arg:ident),*>) => {
        impl<$($arg),*> FnOnce<($($arg),*,)> for Registry<dyn FnMut($($arg),*)>
        where $($arg:Copy),* {
            type Output = ();
            extern "rust-call" fn call_once(self, args: ($($arg),*,)) -> Self::Output {
                self.run_impl(args)
            }
        }

        impl<$($arg),*> FnMut<($($arg),*,)> for Registry<dyn FnMut($($arg),*)>
        where $($arg:Copy),* {
            extern "rust-call" fn call_mut(&mut self, args: ($($arg),*,)) -> Self::Output {
                self.run_impl(args)
            }
        }

        impl<$($arg),*> Fn<($($arg),*,)> for Registry<dyn FnMut($($arg),*)>
        where $($arg:Copy),* {
            extern "rust-call" fn call(&self, args: ($($arg),*,)) -> Self::Output {
                self.run_impl(args)
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

    impl RegistryRunner0 for registry::NoArgs {
        fn run_all(&self) {
            self.run_impl(())
        }
    }

    impl FnOnce<()> for registry::NoArgs {
        type Output = ();
        extern "rust-call" fn call_once(self, args: ()) -> Self::Output {
            self.run_impl(args)
        }
    }

    impl FnMut<()> for registry::NoArgs {
        extern "rust-call" fn call_mut(&mut self, args: ()) -> Self::Output {
            self.run_impl(args)
        }
    }

    impl Fn<()> for registry::NoArgs {
        extern "rust-call" fn call(&self, args: ()) -> Self::Output {
            self.run_impl(args)
        }
    }

    gen_fn_trait_impls!(<T1>);
    gen_fn_trait_impls!(<T1, T2>);
    gen_fn_trait_impls!(<T1, T2, T3>);
    gen_fn_trait_impls!(<T1, T2, T3, T4>);
    gen_fn_trait_impls!(<T1, T2, T3, T4, T5>);

    gen_runner_traits!(RegistryRunner1, RegistryRunnerRef1, (T1));
    gen_runner_traits!(RegistryRunner2, RegistryRunnerRef2, (T1, T2));
    gen_runner_traits!(RegistryRunner3, RegistryRunnerRef3, (T1, T2, T3));
    gen_runner_traits!(RegistryRunner4, RegistryRunnerRef4, (T1, T2, T3, T4));
    gen_runner_traits!(RegistryRunner5, RegistryRunnerRef5, (T1, T2, T3, T4, T5));

    gen_runner!(RegistryRunner1, RegistryRunnerRef1, Copy1, Ref1, <T1>);
    gen_runner!(RegistryRunner2, RegistryRunnerRef2, Copy2, Ref2, <T1,T2>);
    gen_runner!(RegistryRunner3, RegistryRunnerRef3, Copy3, Ref3, <T1,T2,T3>);
    gen_runner!(RegistryRunner4, RegistryRunnerRef4, Copy4, Ref4, <T1,T2,T3,T4>);
    gen_runner!(RegistryRunner5, RegistryRunnerRef5, Copy5, Ref5, <T1,T2,T3,T4,T5>);
}



// ==================
// === Benchmarks ===
// ==================

/// # Conclusion
/// For small amount of elems (< 10), all implementations seem to provide so similar times that
/// there is no difference between them. For large amount of elems (~ 1M), the HashMap
/// implementation is significantly slower. The HashMap implementation can be provided with a custom
/// hasher function, which might improve it for a bigger collection. However, for small collections,
/// vec should always be fastest.

#[cfg(test)]
mod tests {
    use super::*;

    extern crate test;
    use test::Bencher;

    const ITERS: usize = 1_000_000;
    const ELEMS: usize = 5;

    #[bench]
    fn bench_plain_vec(b: &mut Bencher) {
        let mut vec: Vec<Box<dyn Fn()>> = default();
        let val: Rc<Cell<usize>> = default();
        for _ in 0..ELEMS {
            let val = val.clone();
            vec.push(Box::new(move || val.set(val.get() + 1)));
        }
        let vec = Rc::new(RefCell::new(vec));
        b.iter(|| {
            for _ in 0..ITERS {
                val.set(0);
                for f in &*vec.borrow() {
                    (f)();
                }
                assert_eq!(val.get(), ELEMS);
            }
        });
    }

    #[bench]
    fn bench_vec_retain(b: &mut Bencher) {
        let mut rcs: Vec<Rc<()>> = default();
        let mut vec: Vec<(Box<dyn Fn()>, Weak<()>)> = default();
        let val: Rc<Cell<usize>> = default();
        for _ in 0..ELEMS {
            let rc = Rc::new(());
            let weak = Rc::downgrade(&rc);
            let val = val.clone();
            rcs.push(rc);
            vec.push((Box::new(move || val.set(val.get() + 1)), weak));
        }
        let vec = Rc::new(RefCell::new(vec));
        b.iter(|| {
            for _ in 0..ITERS {
                val.set(0);
                vec.borrow_mut().retain(|(f, weak)| {
                    (f)();
                    !weak.is_expired()
                });
                assert_eq!(val.get(), ELEMS);
            }
        });
    }

    #[bench]
    fn bench_small_vec_retain(b: &mut Bencher) {
        let mut rcs: Vec<Rc<()>> = default();
        let mut vec: SmallVec<[(Box<dyn Fn()>, Weak<()>); 5]> = default();
        let val: Rc<Cell<usize>> = default();
        for _ in 0..ELEMS {
            let rc = Rc::new(());
            let weak = Rc::downgrade(&rc);
            let val = val.clone();
            rcs.push(rc);
            vec.push((Box::new(move || val.set(val.get() + 1)), weak));
        }
        let vec = Rc::new(RefCell::new(vec));
        b.iter(|| {
            for _ in 0..ITERS {
                val.set(0);
                vec.borrow_mut().retain(|(f, weak)| {
                    (f)();
                    !weak.is_expired()
                });
                assert_eq!(val.get(), ELEMS);
            }
        });
    }

    #[bench]
    fn bench_hash_map(b: &mut Bencher) {
        let mut map: HashMap<usize, Box<dyn Fn()>> = default();
        let val: Rc<Cell<usize>> = default();
        for i in 0..ELEMS {
            let val = val.clone();
            map.insert(i, Box::new(move || val.set(val.get() + 1)));
        }
        let map = Rc::new(RefCell::new(map));
        b.iter(|| {
            for _ in 0..ITERS {
                val.set(0);
                for f in map.borrow().values() {
                    (f)();
                }
                assert_eq!(val.get(), ELEMS);
            }
        });
    }
}
