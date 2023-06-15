use crate::*;



// ==============
// === Export ===
// ==============

#[cfg(target_arch = "wasm32")]
pub mod internal {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_namespace=console)]
        fn error(msg: String);

        type Error;

        #[wasm_bindgen(constructor)]
        fn new() -> Error;

        #[wasm_bindgen(structural, method, getter)]
        fn stack(error: &Error) -> String;
    }

    /// Print the current backtrace.
    pub fn backtrace() -> String {
        Error::new().stack()
    }
}

#[cfg(not(target_arch = "wasm32"))]
mod internal {
    extern crate backtrace as bt;
    use bt::Backtrace;

    /// Print the current backtrace.
    pub fn backtrace() -> String {
        let bt = Backtrace::new();
        format!("{bt:?}")
    }
}

pub use internal::backtrace;



// ===================
// === TraceCopies ===
// ===================

/// A utility for tracing all copies of CloneRef-able entity.
///
/// This structure should be added as a field to structure implementing Clone or CloneRef. It will
/// mark each copy with unique id (the original copy has id of 0). Once enabled, it will print
/// backtrace of each clone, clone_ref or drop operation with assigned name (the same for all
/// copies) and copy id.
#[derive(Debug)]
pub struct TraceCopies {
    clone_id: u64,
    handle:   Rc<RefCell<Option<ImString>>>,
}

thread_local! {
    static NEXT_CLONE_ID : Cell<u64> = Cell::new(1);
}

fn next_clone_id() -> u64 {
    NEXT_CLONE_ID.with(|id| {
        let next = id.get();
        id.set(next + 1);
        next
    })
}

impl TraceCopies {
    pub fn new() -> Self {
        Self { clone_id: next_clone_id(), handle: default() }
    }

    /// Create enabled structure with appointed entity name (shared between all copies).
    pub fn enabled(name: impl Into<ImString>) -> Self {
        let this: Self = default();
        this.enable(name);
        this
    }

    /// Assign a name to the entity (shared between all copies) and start printing logs.
    pub fn enable(&self, name: impl Into<ImString>) {
        let name = name.into();
        debug!("[{name}] TraceCopies enabled");
        *self.handle.borrow_mut() = Some(name);
    }
}

impl Default for TraceCopies {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for TraceCopies {
    fn clone(&self) -> Self {
        let borrow = self.handle.borrow();
        let clone_id = next_clone_id();
        let handle = self.handle.clone();
        if let Some(name) = &*borrow {
            let bt = backtrace();
            debug!("[{name}] Cloning {} -> {clone_id} {bt}", self.clone_id);
        }
        Self { clone_id, handle }
    }
}

impl CloneRef for TraceCopies {
    fn clone_ref(&self) -> Self {
        let borrow = self.handle.borrow();
        let clone_id = next_clone_id();
        let handle = self.handle.clone_ref();
        if let Some(name) = &*borrow {
            let bt = backtrace();
            debug!("[{name}] Cloning {} -> {clone_id} {bt}", self.clone_id);
        }
        Self { clone_id, handle }
    }
}

impl Drop for TraceCopies {
    fn drop(&mut self) {
        let borrow = self.handle.borrow();
        if let Some(name) = &*borrow {
            let bt = backtrace();
            let instances = Rc::strong_count(&self.handle) - 1;
            debug!("[{name}] Dropping {}; instances left: {instances} {bt}", self.clone_id);
        }
    }
}



// ====================
// === LeakDetector ===
// ====================

/// A module containing an utility for detecting leaks.
///
/// If you suspect a particular struct is leaking, i.e. its instances are still present when we
/// expect them to be removed, you may add a [`Trace`] field to it and call [`enable`]. Then, at
/// the point where we expect all instances to be dropped, we may check the [`TRACKED_OBJECTS`]
/// global variable, what instances are still alive and their creation backtraces.
pub mod leak_detector {
    use crate::*;

    thread_local! {
        /// The structure mapping the existing tracking copies with [`Trace`] structure to their
        /// creation backtraces.
        ///
        /// You may check/print it at various points where you expect no traced objects should
        /// persist.
        pub static TRACKED_OBJECTS: RefCell<HashMap<u64, String>> = default();
    }

    /// A utility for tracing all copies of CloneRef-able entity and keeping list of existing ones.
    ///
    /// This is a wrapper for [`TraceCopies`] which also register each enabled copy in
    /// [`TRACKED_OBJECTS`] global variable. The variable may be then checked for leaks in moments
    /// when we expect it to be empty.
    #[derive(Debug, Default)]
    pub struct Trace {
        instance: TraceCopies,
    }

    impl Trace {
        /// Create enabled structure with appointed entity name (shared between all copies).
        ///
        /// See [`TraceCopies::enabled`] and [`Trace::enable`].
        pub fn enabled(name: impl Into<ImString>) -> Self {
            let instance = TraceCopies::enabled(name);
            Self::register_tracked_object(&instance);
            Self { instance }
        }

        /// Assign a name to the entity (shared between all copies), start printing logs and
        /// register its creation backtrace in [`TRACKED_OBJECTS`].
        ///
        /// See [`TraceCopies::enable`].
        pub fn enable(&self, name: impl Into<ImString>) {
            self.instance.enable(name);
            Self::register_tracked_object(&self.instance);
        }

        fn register_tracked_object(instance: &TraceCopies) {
            let id = instance.clone_id;
            let bt = backtrace();
            TRACKED_OBJECTS.with(|objs| objs.borrow_mut().insert(id, bt));
        }
    }

    impl Clone for Trace {
        fn clone(&self) -> Self {
            let instance = self.instance.clone();
            let enabled = instance.handle.borrow().is_some();
            if enabled {
                Self::register_tracked_object(&instance);
            }
            Self { instance }
        }
    }

    impl_clone_ref_as_clone!(Trace);

    impl Drop for Trace {
        fn drop(&mut self) {
            let id = self.instance.clone_id;
            TRACKED_OBJECTS.with(|objs| objs.borrow_mut().remove(&id));
        }
    }
}
