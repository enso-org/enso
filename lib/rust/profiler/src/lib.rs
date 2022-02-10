//! Instrumentation for timing execution of code.
//!
//! Supports the
//! [Profiling](https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md)
//! design.
//!
//! # Profiler hierarchy
//!
//! Every profiler has a parent, except the special profiler value [`APP_LIFETIME`]. Each of its
//! children is considered a *root profiler*.
//!
//! ## Parents of async tasks
//!
//! `async` tasks do not have an inherent parent-child relationship. To fit them into a hierarchical
//! data model, we must rely on convention: when a profiler is created, **its parent must be
//! chosen** such that the lifetime of the child is entirely bounded within the lifetime of the
//! parent. The appropriate parent for a profiler will not necessarily be its direct ancestor in
//! scope, nor necessarily the most recent profiler that has been started and not yet finished--
//! typically, it will be both of those things, but users must be aware of exceptional cases.
//! Exceptions will usually occur when a task is *spawned*, e.g. with `executor::global::spawn`.
//!
//! # Profiling levels
//!
//! Profiling has performance overhead; to support fine-grained measurement when it is needed, but
//! avoid its costs when it is not, measurements are classified into *profiling levels*.
//!
//! This API only allows creating a profiler of the same or finer-grained level than its parent.
//!
//! #### Objective
//! Measurements that correspond directly to aspects of the user experience. An *objective* can
//! contain other *objective*s, e.g. *GUI initialization* (which might be defined as: time from
//! opening the app until the app is ready to receive user input) contains *time until the loading
//! spinner finishes*.
//! #### Task
//! Coarse-grained tasks, such as app window initialization, GUI downloading, or WASM compilation. A
//! *task* can contain other *task*s e.g. GUI initialization contains GUI downloading.
//! #### Detail
//! All processes which can be removed in compile-time for the official stable release for users. We
//! might provide some users with special releases with enabled *detailed* profiling, however, it
//! should be possible to debug and understand most of user-provided logs with disabled *details*
//! view.
//! #### Debug
//! All processes which should be removed in compile-time by default even during app development. It
//! applies to every heavy-usage of the profiling framework, such as per-frame rendering profiling.
//!
//! ## Conditional compilation
//!
//! The level of profiling detail is set at compile time with an environment variable, e.g.
//! `ENSO_MAX_PROFILING_LEVEL=task`. When using the `run` script, this can be accomplished by
//! passing the argument `--profiling-level=task`.
//!
//! If the environment variable is not set, the level will default to the minimum supported,
//! *objective*.
//!
//! # Structured measurement
//!
//! This API can be used to make arbitrary measurements; in order to ensure measurements are easy to
//! interpret, the intervals selected for measurement should correspond as much as possible to the
//! units of organization of the code.
//!
//! To support such structured measurement, the **primary interface is a
//! [`#[profile]`](macro@profile)  attribute macro**, which instruments a whole function.
//!
//! # Low-level: RAII interface
//!
//! When it is not feasible to measure at the function level (for example if moving the section of
//! interest into its own function would divide the code into unreasonably small functions), or a
//! measurement needs to be made with special properties (e.g. with its start time inherited from
//! its parent), a *RAII interface* supports **instrumenting a block of code**.
//!
//! The core of the interface is a set of [macros](index.html#macros) that create a new profiler,
//! and return a *RAII guard* object of a type like [`Started<Task>`]. The guard object will
//! automatically log the end of a measurement when it is dropped.
//!
//! In rare cases, it will be necessary to measure an interval that doesn't correspond to a block at
//! any level of the code. This can be achieved using the RAII interface by allowing the guard
//! object to escape the scope in which it is created to control its `drop()` time.
//!
//! ## Basic usage
//!
//! ```
//! # use enso_profiler as profiler;
//! fn using_low_level_api(input: u32, profiler: impl profiler::Parent<profiler::Task>) {
//!     if input == 4 {
//!         let _profiler = profiler::start_task!(profiler, "subtask_4");
//!         // ...
//!     } else {
//!         let _profiler = profiler::start_task!(profiler, "subtask_other");
//!         // ...
//!     }
//! }
//! ```
//!
//! ## Measuring a block
//!
//! When a measurement is ended by implicitly dropping its profiler at the end of a block, the
//! profiler should be created as **the first line of the block**; it measures one full block, and
//! the line containing [`start_task!`] (or the like) acts as a title for the block.
//!
//! In this case, the binding used to control the scope of the measurement should have a **name
//! beginning with an underscore**, even if it is referenced (e.g. to create a child profiler). This
//! indicates that the binding is used to identify a scope, even if it is *also* used for its a
//! value.
//!
//! ## Accepting a parent argument
//!
//! A function using the low-level API may need to accept a profiler argument to use as the parent
//! for a new profiler. The function should be able to accept any type of profiler that is of a
//! suitable level to be a parent of the profiler it is creating. This is supported by accepting an
//! **argument that is generic over the [`Parent`] trait**.
//!
//! ## Advanced Example: creating a root profiler
//!
//! ```
//! # use enso_profiler as profiler;
//! fn root_objective_that_starts_at_time_origin() {
//!     let _profiler = profiler::objective_with_same_start!(
//!         profiler::APP_LIFETIME,
//!         "root_objective_that_starts_at_time_origin"
//!     );
//!     // ...
//! }
//! ```
//!
//! ### Root profilers
//!
//! The profiler constructor macros require a parent. To create a *root profiler*, specify the
//! special value [`APP_LIFETIME`] as the parent.
//!
//! ### Inheriting a start time
//!
//! Sometimes, multiple measurements need to start at the same time. To support this, an alternate
//! set of constructors create profilers that inherit their start time from the specified parent,
//! e.g. [`objective_with_same_start!`] in the example above.

#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

extern crate test;

use std::cell;
use std::num;
use std::str;



// =============
// === Label ===
// =============

/// The label of a profiler; this includes the name given at its creation, along with file and
/// line-number information.
pub type Label = &'static str;



// =================
// === Timestamp ===
// =================

/// Time elapsed since the [time origin](https://www.w3.org/TR/hr-time-2/#sec-time-origin).
///
/// Stored in units of 100us, because that is maximum resolution of performance.now():
/// - [in the specification](https://www.w3.org/TR/hr-time-3), 100us is the limit
/// - in practice, as observed in debug consoles: Chromium 97 (100us) and Firefox 95 (1ms)
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Timestamp(num::NonZeroU64);

/// Offset used to encode a timestamp, which may be 0, in a [`NonZeroU64`].
/// To maximize the supported range, this is the smallest positive integer.
const TS_OFFSET: u64 = 1;

impl Timestamp {
    /// Return the current time, relative to the time origin.
    pub fn now() -> Self {
        Self::from_ms(js::performance::now())
    }

    /// Return the timestamp corresponding to an offset from the time origin, in ms.
    #[allow(unsafe_code)]
    pub fn from_ms(ms: f64) -> Self {
        let ticks = (ms * 10.0).round() as u64;
        // Safety: ticks + 1 will not be 0 unless a Timestamp wraps.
        // It takes (2 ** 64) * 100us = 58_455_453 years for a Timestamp to wrap.
        unsafe { Self(num::NonZeroU64::new_unchecked(ticks + TS_OFFSET)) }
    }

    /// Convert to an offset from the time origin, in ms.
    pub fn into_ms(self) -> f64 {
        (self.0.get() - TS_OFFSET) as f64 / 10.0
    }
}

// === FFI ===

#[cfg(target_arch = "wasm32")]
/// Web APIs.
pub mod js {
    /// [The `Performance` API](https://developer.mozilla.org/en-US/docs/Web/API/Performance)
    pub mod performance {
        use wasm_bindgen::prelude::*;

        #[wasm_bindgen]
        extern "C" {
            /// The
            /// [performance.now](https://developer.mozilla.org/en-US/docs/Web/API/Performance/now)
            /// method returns a double-precision float, measured in milliseconds.
            ///
            /// The returned value represents the time elapsed since the time origin, which is when
            /// the page began to load.
            #[wasm_bindgen(js_namespace = performance)]
            pub fn now() -> f64;
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
/// Web APIs.
pub mod js {
    /// [The `Performance` API](https://developer.mozilla.org/en-US/docs/Web/API/Performance)
    pub mod performance {
        /// The
        /// [performance.now](https://developer.mozilla.org/en-US/docs/Web/API/Performance/now)
        /// method returns a double-precision float, measured in milliseconds.
        ///
        /// The returned value represents the time elapsed since the time origin, which is when
        /// the page began to load.
        // This mock implementation returns a dummy value.
        pub fn now() -> f64 {
            0.0
        }
    }
}



// ==================
// === ProfilerId ===
// ==================

/// Uniquely identifies a runtime measurement.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ProfilerId(u32);

impl ProfilerId {
    fn new() -> Self {
        thread_local! {
            pub static NEXT_ID: cell::Cell<u32> = cell::Cell::new(1);
        }
        ProfilerId(NEXT_ID.with(|next_id| {
            let id = next_id.get();
            next_id.set(id + 1);
            id
        }))
    }
}

/// Pseudo-profiler serving as the root of the measurement hierarchy.
pub const APP_LIFETIME: Objective = Objective(ProfilerId(0));



// =======================
// === StartedProfiler ===
// =======================

/// The interface supported by profiler-data objects.
pub trait StartedProfiler {
    /// Log a measurement, identified by `profiler`, with end-time set to now.
    fn finish(&self, profiler: ProfilerId);
}

// === Implementation for disabled profilers ===

impl StartedProfiler for () {
    fn finish(&self, _: ProfilerId) {}
}



// ====================
// === ProfilerData ===
// ====================

/// Data used by a started [`Measurement`] for an enabled profile level.
#[derive(Debug, Copy, Clone)]
pub struct ProfilerData {
    /// Identifier of the parent [`Measurement`].
    pub parent: ProfilerId,
    /// Start time for this [`Measurement`], or None to indicate it is the same as `parent`.
    pub start:  Option<Timestamp>,
    /// Identifies where in the code this [`Measurement`] originates.
    pub label:  Label,
}

// === Trait Implementations ===

impl StartedProfiler for ProfilerData {
    fn finish(&self, profiler: ProfilerId) {
        let parent = self.parent;
        let start = self.start;
        let label = self.label;
        let end = Timestamp::now();
        let measurement = Measurement { parent, profiler, start, end, label };
        crate::MEASUREMENTS.with(move |log| log.push(measurement));
    }
}



// ===================
// === Measurement ===
// ===================

/// Record of a profiled section, the parent it was reached by, and its entry and exit times.
#[derive(Debug, Copy, Clone)]
pub struct Measurement {
    /// A unique identifier.
    pub profiler: ProfilerId,
    /// Identifies parent [`Measurement`] by its `profiler` field.
    pub parent:   ProfilerId,
    /// Start time, or None to indicate it is the same as `parent`.
    pub start:    Option<Timestamp>,
    /// The end time.
    pub end:      Timestamp,
    /// Identifies where in the code this [`Measurement`] originates.
    pub label:    Label,
}

/// Internal
pub mod internal {
    use crate::*;

    use std::cell;
    use std::mem;

    // =======================
    // === LocalVecBuilder ===
    // =======================

    /// Data structure supporting limited interior mutability, to build up a collection.
    #[derive(Debug)]
    pub struct LocalVecBuilder<T>(cell::UnsafeCell<Vec<T>>);
    #[allow(unsafe_code)]
    impl<T> LocalVecBuilder<T> {
        #[allow(clippy::new_without_default)]
        /// Create a new, empty vec builder.
        pub fn new() -> Self {
            Self(cell::UnsafeCell::new(vec![]))
        }

        /// Push an element.
        pub fn push(&self, element: T) {
            // Note [LocalVecBuilder Safety]
            unsafe {
                (&mut *self.0.get()).push(element);
            }
        }

        /// Return (and consume) all elements pushed so far.
        pub fn build(&self) -> Vec<T> {
            // Note [LocalVecBuilder Safety]
            unsafe { mem::take(&mut *self.0.get()) }
        }
    }
    // Note [LocalVecBuilder Safety]
    // =============================
    // When obtaining a reference from the UnsafeCell, all accessors follow these rules:
    // - There must be a scope that the reference doesn't escape.
    // - There must be no other references obtained in the same scope.
    // Consistently following these rules ensures the no-alias rule of mutable references is
    // satisfied.



    // ====================
    // === MEASUREMENTS ===
    // ====================

    thread_local! {
        /// Global log of [`Measurement`]s.
        pub static MEASUREMENTS: LocalVecBuilder<Measurement> = LocalVecBuilder::new();
    }
}

#[doc(inline)]
pub use internal::MEASUREMENTS;

/// Gather all measurements taken since the last time take_log() was called.
pub fn take_log() -> Vec<Measurement> {
    MEASUREMENTS.with(|log| log.build())
}


// ================
// === Profiler ===
// ================

/// The interface supported by profilers of all profiling levels.
pub trait Profiler: Copy {
    /// Metadata this profiler stores from when it is started until it is finished.
    type Started: StartedProfiler;
    /// Log a measurement, using `self` as identifier, the present time as end time, and metadata as
    /// provided in `data`.
    fn finish(self, data: &Self::Started);
}



// ==============
// === Parent ===
// ==============

/// Any object representing a profiler that is a valid parent for a profiler of type T.
pub trait Parent<T: Profiler> {
    /// Start a new profiler, with `self` as its parent.
    fn new_child(&self, label: Label) -> Started<T>;
    /// Create a new profiler, with `self` as its parent, and the same start time as `self`.
    fn new_child_same_start(&self, label: Label) -> Started<T>;
}



// ===============
// === Started ===
// ===============

/// A profiler that has a start time set, and will complete its measurement when dropped.
#[derive(Debug)]
pub struct Started<T: Profiler> {
    /// The ID to log this measurement as.
    pub profiler: T,
    /// Metadata to associate with this measurement when it is logged.
    pub data:     T::Started,
}


// === Trait Implementations ===

impl<T: Profiler> Drop for Started<T> {
    fn drop(&mut self) {
        let profiler = self.profiler;
        profiler.finish(&self.data);
    }
}

impl<T, U> Parent<T> for Started<U>
where
    U: Parent<T> + Profiler,
    T: Profiler,
{
    fn new_child(&self, label: Label) -> Started<T> {
        self.profiler.new_child(label)
    }

    fn new_child_same_start(&self, label: Label) -> Started<T> {
        self.profiler.new_child_same_start(label)
    }
}



// ===================================
// === profiler_macros Invocations ===
// ===================================

/// Instruments a function.
///
/// For each call to the function, a measurement of the time interval corresponding to the
/// function's body is logged under the name of the function, with file:line information
/// attached.
///
/// # Usage
///
/// The last argument must be an instance of a profiler type. The type given determines the
/// profiling level at which measurement is enabled; the macro will modify the function's
/// signature so that it can be called with any profiler that could be a *parent* of the
/// profiler.
///
/// ```
/// # use enso_profiler as profiler;
/// # use enso_profiler::profile;
/// #[profile]
/// fn small_computation(input: i16, profiler: profiler::Detail) -> u32 {
///     todo!()
/// }
///
/// #[enso_profiler_macros::profile]
/// fn bigger_computation(profiler: profiler::Task) -> u32 {
///     // Our task-level profiler is not the same type as the detail-level profiler available
///     // inside `small_computation`; it will be converted implicitly.
///     small_computation(7, profiler)
/// }
/// ```
///
/// This will expand to the equivalent of:
///
/// ```
/// # use enso_profiler as profiler;
/// # use enso_profiler::profile;
/// fn small_computation(input: i16, profiler: impl profiler::Parent<profiler::Detail>) -> u32 {
///     let _profiler = profiler.new_child("small_computation (file.rs:43)");
///     let profiler = _profiler.profiler;
///     return (|input: i16, profiler: profiler::Detail| todo!())(input, profiler);
/// }
///
/// fn bigger_computation(profiler: impl profiler::Parent<profiler::Task>) -> u32 {
///     let _profiler = profiler.new_child("bigger_computation (file.rs:48)");
///     let profiler = _profiler.profiler;
///     return (|profiler: profiler::Task| {
///         // Our task-level profiler is not the same type as the detail-level profiler available
///         // inside `small_computation`; it will be converted implicitly.
///         small_computation(7, profiler)
///     })(profiler);
/// }
/// ```
///
/// # Limitations
///
/// Some syntactic constructs are not (currently) supported.
///
/// ## Unsafe functions
///
/// Unsafe functions cannot be wrapped (yet). Use the low-level API.
///
/// ## Destructuring binding in function signatures
///
/// This won't compile:
///
/// ```compile_fail
/// # use enso_profiler as profiler;
/// # use profiler::profile;
/// #[profile]
/// fn unsupported_binding((x, y): (u32, u32), profiler: profiler::Task) -> u32 {
///     x + y
/// }
/// ```
///
/// Instead, rewrite the function to take the destructuring out of the signature:
///
/// ```
/// # use enso_profiler as profiler;
/// # use profiler::profile;
/// #[profile]
/// fn supported_binding(xy: (u32, u32), profiler: profiler::Task) -> u32 {
///     let (x, y) = xy;
///     x + y
/// }
/// ```
///
/// ## Method definitions in nested items
///
/// Use of the `self` keyword to refer to anything except the receiver of the wrapped item is
/// not supported; this means you can't define methods *inside* a wrapped function, like this:
///
/// ```compile_fail
/// # use enso_profiler as profiler;
/// # use profiler::profile;
/// #[profile]
/// fn bad_nested_method_def(profiler: profiler::Task) {
///     // This is technically legal syntax, but #[profile] doesn't support it.
///     struct Foo;
///     impl Foo {
///         fn consume(self) {}
///         fn call_consume(self) {
///             self.consume()
///         }
///     }
///     // ...
/// }
/// ```
///
/// Instead, define the items outside the lexical scope of the profiled function:
///
/// ```
/// # use enso_profiler as profiler;
/// # use profiler::profile;
/// struct Foo;
/// impl Foo {
///     fn consume(self) {}
///     fn call_consume(self) {
///         self.consume()
///     }
/// }
///
/// #[profile]
/// fn no_nested_method_def(profiler: profiler::Task) {
///     // ...
/// }
/// ```
#[doc(inline)]
pub use enso_profiler_macros::profile;

enso_profiler_macros::define_hierarchy![Objective, Task, Detail, Debug];



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use crate as profiler;
    use profiler::profile;

    #[test]
    fn root() {
        {
            // In any other crate we would refer to the macro as `profiler::start_objective!`, but
            // "macro-expanded `macro_export` macros from the current crate cannot be referred to
            // by absolute paths" (<https://github.com/rust-lang/rust/issues/52234>).
            let _profiler = start_objective!(profiler::APP_LIFETIME, "test");
        }
        let measurements = profiler::take_log();
        assert_eq!(measurements.len(), 1);
        assert_eq!(measurements[0].parent, profiler::APP_LIFETIME.0);
        assert!(measurements[0].label.starts_with("test "));
        assert!(measurements[0].end >= measurements[0].start.unwrap());
    }

    #[test]
    fn with_same_start() {
        {
            let _profiler0 = start_objective!(profiler::APP_LIFETIME, "test0");
            let _profiler1 = objective_with_same_start!(_profiler0, "test1");
        }
        let measurements = profiler::take_log();
        assert_eq!(measurements.len(), 2);
        // _profiler1 is with_same_start, indicated by None in the log
        // Note: _profiler1 is _measurements[0] because position in the log is determined by end
        // order, not start order.
        assert_eq!(measurements[0].start, None);
        // _profiler0 has a start time
        assert!(measurements[1].start.is_some());
    }

    #[test]
    fn profile() {
        #[profile]
        fn profiled(_profiler: profiler::Objective) {}
        profiled(profiler::APP_LIFETIME);
        let measurements = profiler::take_log();
        assert_eq!(measurements.len(), 1);
    }

    #[test]
    fn profile_async() {
        #[profile]
        async fn profiled(_profiler: profiler::Objective) -> u32 {
            let block = async { 4 };
            block.await
        }
        let future = profiled(profiler::APP_LIFETIME);
        futures::executor::block_on(future);
        let measurements = profiler::take_log();
        assert_eq!(measurements.len(), 1);
    }
}

// Performance analysis [KW]
//
// Performance impact: Except at low numbers of measurements, run time is dominated by growing the
// vector. I'm measuring about 1.6ns per logged measurement [Ryzen 5950X], when accumulating 10k
// measurements.
// I think the cost of the unavoidable performance.now() will be on the order of 1μs, in which case
// the overhead of #[profile] is within 0.1 % of an optimal implementation.
//
// Performance variability impact: There's no easy way to measure this, so I'm speaking
// theoretically here. The only operation expected to have a significantly variable cost is the
// Vec::push to grow the MEASUREMENTS log; it sometimes needs to reallocate. However even at its
// most expensive, it should be on the order of a 1μs (for reasonable numbers of measurements); so
// the variance introduced by this framework shouldn't disturb even very small measurements (I
// expect <1% added variability for a 1ms measurement).
#[cfg(test)]
mod bench {
    use crate as profiler;

    /// Perform a specified number of measurements, for benchmarking.
    fn log_measurements(count: usize) {
        for _ in 0..count {
            let _profiler = start_objective!(profiler::APP_LIFETIME, "");
        }
        test::black_box(profiler::take_log());
    }

    #[bench]
    fn log_measurements_1000(b: &mut test::Bencher) {
        b.iter(|| log_measurements(1000));
    }

    #[bench]
    fn log_measurements_10_000(b: &mut test::Bencher) {
        b.iter(|| log_measurements(10_000));
    }

    /// For comparison with time taken by [`log_measurements`].
    fn push_vec(count: usize, measurements: &mut Vec<profiler::Measurement>) {
        let some_timestamp = profiler::Timestamp(std::num::NonZeroU64::new(1).unwrap());
        for _ in 0..count {
            measurements.push(profiler::Measurement {
                parent:   profiler::ProfilerId(0),
                profiler: profiler::ProfilerId(0),
                start:    None,
                end:      some_timestamp,
                label:    "",
            });
        }
        test::black_box(&measurements);
        measurements.clear();
    }

    #[bench]
    fn push_vec_1000(b: &mut test::Bencher) {
        let mut measurements = vec![];
        b.iter(|| push_vec(1000, &mut measurements));
    }

    #[bench]
    fn push_vec_10_000(b: &mut test::Bencher) {
        let mut measurements = vec![];
        b.iter(|| push_vec(10_000, &mut measurements));
    }
}

#[cfg(test)]
mod compile_tests {
    use crate as profiler;
    use profiler::profile;

    /// Decorating a pub fn.
    #[profile]
    pub fn profiled_pub(_profiler: profiler::Objective) {}

    #[profile]
    async fn profiled_async(_profiler: profiler::Objective) {}

    #[profile]
    #[allow(unsafe_code)]
    unsafe fn profiled_unsafe(_profiler: profiler::Objective) {}

    #[test]
    fn mut_binding() {
        #[profile]
        fn profiled(mut _x: u32, _profiler: profiler::Objective) {
            _x = 4;
        }
        profiled(0, profiler::APP_LIFETIME);
        let measurements = profiler::take_log();
        assert_eq!(measurements.len(), 1);
    }

    // Unsupported:
    // #[profile]
    // fn profiled_destructuring((_x, _y): (u32, u32), _profiler: profiler::Objective) {}

    #[allow(dead_code)]
    struct Foo;

    impl Foo {
        #[profile]
        fn profiled_method(&mut self, _arg: u32, _profiler: profiler::Objective) {
            profiled_pub(_profiler)
        }
    }
}
