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
//! [`#[profile]`](macro@profile) attribute macro**, which instruments a whole function.
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
//! # use profiler::profile;
//! async fn using_low_level_api(input: u32, profiler: impl profiler::Parent<profiler::Task>) {
//!     if input == 4 {
//!         let _profiler = profiler::start_task!(profiler, "subtask_4");
//!         // ...
//!     } else {
//!         let _profiler = profiler::start_task!(profiler, "subtask_other");
//!         profiler::await_!(callee(input), _profiler);
//!         // ...
//!     }
//! }
//!
//! #[profile(Detail)]
//! async fn callee(input: u32) {}
//! ```
//!
//! ### Measuring a block
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
//! ### Accepting a parent argument
//!
//! A function using the low-level API may need to accept a profiler argument to use as the parent
//! for a new profiler. The function should be able to accept any type of profiler that is of a
//! suitable level to be a parent of the profiler it is creating. This is supported by accepting an
//! **argument that is generic over the [`Parent`] trait**.
//!
//! ### Profiling `.await`
//!
//! Within a profiled scope, `.await` should not be used directly. The wrapper [`await_!`] is
//! provided to await a future while making the profiling framework aware of the start and end times
//! of the await-interval.
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



// ===============
// === EventId ===
// ===============

/// Identifies an event in the profiling log.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct EventId(u32);

// === Special Profilers / IDs ===

/// Special value indicating that no explicit prior event is associated.
///
/// When used to identify a parent, this indicates that the parent can be inferred to be the
/// current profiler.
pub const IMPLICIT_ID: EventId = EventId(u32::MAX);
const APP_LIFETIME_ID: EventId = EventId(u32::MAX - 1);

/// Pseudo-profiler serving as the root of the measurement hierarchy.
pub const APP_LIFETIME: Objective = Objective(APP_LIFETIME_ID);



// =======================
// === StartedProfiler ===
// =======================

/// The interface supported by profiler-data objects.
pub trait StartedProfiler {
    /// Log the end of a measurement, with end-time set to now.
    fn finish(self);
}


// === Implementation for enabled profilers ===

impl StartedProfiler for EventId {
    fn finish(self) {
        let timestamp = Timestamp::now();
        EventLog.end(self, timestamp);
    }
}


// === Implementation for disabled profilers ===

impl StartedProfiler for () {
    fn finish(self) {}
}



// ================
// === EventLog ===
// ================

/// The log of profiling events. Data is actually stored globally.
#[derive(Copy, Clone, Debug)]
pub struct EventLog;

impl EventLog {
    /// Log the beginning of a measurement.
    pub fn start(self, parent: EventId, label: Label, start: Option<Timestamp>) -> EventId {
        let m = Start { parent, label, start };
        let id = EVENTS.with(move |log| log.len()) as u32;
        EVENTS.with(move |log| log.push(Event::Start(m)));
        EventId(id)
    }

    /// Log the end of a measurement.
    pub fn end(self, id: EventId, timestamp: Timestamp) {
        EVENTS.with(move |log| log.push(Event::End { id, timestamp }));
    }

    /// Log the beginning of an interval in which the measurement is not active.
    pub fn pause(self, id: EventId, timestamp: Timestamp) {
        EVENTS.with(move |log| log.push(Event::Pause { id, timestamp }));
    }

    /// Log the end of an interval in which the measurement is not active.
    pub fn resume(self, id: EventId, timestamp: Timestamp) {
        EVENTS.with(move |log| log.push(Event::Resume { id, timestamp }));
    }
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

        /// The number of elements that are currently available.
        #[allow(clippy::len_without_is_empty)]
        pub fn len(&self) -> usize {
            // Note [LocalVecBuilder Safety]
            unsafe { &*self.0.get() }.len()
        }
    }
    // Note [LocalVecBuilder Safety]
    // =============================
    // When obtaining a reference from the UnsafeCell, all accessors follow these rules:
    // - There must be a scope that the reference doesn't escape.
    // - There must be no other references obtained in the same scope.
    // Consistently following these rules ensures the no-alias rule of mutable references is
    // satisfied.



    // ==============
    // === EVENTS ===
    // ==============

    thread_local! {
        /// Global log of [`Events`]s.
        pub static EVENTS: LocalVecBuilder<Event> = LocalVecBuilder::new();
    }
}

#[doc(inline)]
pub use internal::EVENTS;

/// Gather all events logged since the last time take_log() was called.
///
/// Except in testing, this should only be done once. (Supporting incremental output would
/// require generating EventIds with a counter that isn't reset on log.build()).
pub fn take_log() -> Vec<Event> {
    EVENTS.with(|log| log.build())
}



// =============
// === Event ===
// =============

/// An entry in the profiling log.
#[derive(Debug, Copy, Clone)]
pub enum Event {
    /// The beginning of a measurement.
    Start(Start),
    /// The end of a measurement.
    End {
        /// Identifies the measurement by the ID of its Start event.
        id:        EventId,
        /// When the event occurred.
        timestamp: Timestamp,
    },
    /// The beginning of an interruption to a measurement, e.g. an await point.
    Pause {
        /// Identifies the measurement by the ID of its Start event.
        id:        EventId,
        /// When the event occurred.
        timestamp: Timestamp,
    },
    /// The end of an interruption to an a measurement, e.g. an await point.
    Resume {
        /// Identifies the measurement by the ID of its Start event.
        id:        EventId,
        /// When the event occurred.
        timestamp: Timestamp,
    },
}

// =============
// === Start ===
// =============

/// A measurement-start entry in the profiling log.
#[derive(Debug, Copy, Clone)]
pub struct Start {
    /// Specifies parent measurement by its [`Start`].
    pub parent: EventId,
    /// Start time, or None to indicate it is the same as `parent`.
    pub start:  Option<Timestamp>,
    /// Identifies where in the code this measurement originates.
    pub label:  Label,
}



// ================
// === Profiler ===
// ================

/// The interface supported by profilers of all profiling levels.
pub trait Profiler {
    /// Log the beginning of a measurement.
    ///
    /// Return an object that can be used to end the measurement.
    fn start(parent: EventId, label: Label, time: Option<Timestamp>) -> Self;
    /// Log the end of a measurement.
    fn finish(self);
    /// Log the beginning of an interval in which the profiler is not active.
    fn pause(&self);
    /// Log the end of an interval in which the profiler is not active.
    fn resume(&self);
}



// ==============
// === Parent ===
// ==============

/// Any object representing a profiler that is a valid parent for a profiler of type T.
pub trait Parent<T: Profiler + Copy> {
    /// Start a new profiler, with `self` as its parent.
    fn new_child(&self, label: Label) -> Started<T>;
    /// Create a new profiler, with `self` as its parent, and the same start time as `self`.
    fn new_child_same_start(&self, label: Label) -> Started<T>;
}



// ===============
// === Started ===
// ===============

/// A profiler that can be used as a parent for async profilers, has a start time set, and will
/// complete its measurement when dropped.
#[derive(Debug)]
pub struct Started<T: Profiler + Copy>(pub T);


// === Trait Implementations ===

impl<T: Profiler + Copy> Profiler for Started<T> {
    fn start(parent: EventId, label: Label, time: Option<Timestamp>) -> Self {
        Self(T::start(parent, label, time))
    }
    fn finish(self) {
        self.0.finish()
    }
    fn pause(&self) {
        self.0.pause()
    }
    fn resume(&self) {
        self.0.resume()
    }
}

impl<T: Profiler + Copy> Drop for Started<T> {
    fn drop(&mut self) {
        self.0.finish();
    }
}

impl<T, U> Parent<T> for Started<U>
where
    U: Parent<T> + Profiler + Copy,
    T: Profiler + Copy,
{
    fn new_child(&self, label: Label) -> Started<T> {
        self.0.new_child(label)
    }

    fn new_child_same_start(&self, label: Label) -> Started<T> {
        self.0.new_child_same_start(label)
    }
}



// ===============
// === await_! ===
// ===============

/// Await a future, logging appropriate await events for the given profiler.
#[macro_export]
macro_rules! await_ {
    ($evaluates_to_future:expr, $profiler:ident) => {{
        let future = $evaluates_to_future;
        profiler::Profiler::pause(&$profiler);
        let result = future.await;
        profiler::Profiler::resume(&$profiler);
        result
    }};
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
/// The argument to the macro is a profiler type name, identifying the
/// [profiling level](#profiling-levels) at which to instrument the function.
///
/// ```
/// # use enso_profiler as profiler;
/// # use enso_profiler::profile;
/// #[profile(Detail)]
/// fn example(input: u32) -> u32 {
///     input
/// }
/// ```
///
/// This will expand to the equivalent of:
///
/// ```
/// # use enso_profiler as profiler;
/// # use enso_profiler::profile;
/// fn example(input: u32) -> u32 {
///     let __profiler_scope = {
///         use profiler::Profiler;
///         let parent = profiler::IMPLICIT_ID;
///         let now = Some(profiler::Timestamp::now());
///         let profiler = profiler::Detail::start(parent, "example (profiler/src/lib.rs:78)", now);
///         profiler::Started(profiler)
///     };
///     {
///         input
///     }
/// }
/// ```
///
/// The macro is used the same way with async functions:
///
/// ```
/// # use enso_profiler as profiler;
/// # use enso_profiler::profile;
/// #[profile(Detail)]
/// async fn example(input: u32) -> u32 {
///     input
/// }
/// ```
///
/// The implementation for async is a little more complicated:
///
/// ```
/// # use enso_profiler as profiler;
/// # use enso_profiler::profile;
/// fn async_example(input: u32) -> impl std::future::Future<Output = u32> {
///     let __profiler_scope = {
///         use profiler::Profiler;
///         let parent = profiler::IMPLICIT_ID;
///         let now = Some(profiler::Timestamp::now());
///         let profiler = profiler::Task::start(parent, "async_example (lib.rs:571)", now);
///         profiler.pause();
///         profiler::Started(profiler)
///     };
///     async move {
///         profiler::Profiler::resume(&__profiler_scope.0);
///         let result = { input };
///         std::mem::drop(__profiler_scope);
///         result
///     }
/// }
/// ```
///
/// # Limitations
///
/// ## `.await` expressions with attributes
///
/// `#[profile]` must rewrite `.await` expressions; it separates the base expression from the
/// `.await` in order to insert instrumentation between them. Since the literal expression the
/// attribute was applied to does not exist in the output, there is no way to handle the
/// attribute that would be correct for any type of attribute.
///
/// ## Send approximation
///
/// `#[profile]` breaks
/// [Send approximation](https://rust-lang.github.io/async-book/07_workarounds/03_send_approximation.html);
/// when it is applied to an `async fn`, the `Future` returned will always be `!Send`.
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
        let log = profiler::take_log();
        match &log[..] {
            [profiler::Event::Start(m0), profiler::Event::End { id, timestamp: end_time }] => {
                assert_eq!(m0.parent, profiler::APP_LIFETIME.0);
                assert_eq!(id.0, 0);
                assert!(m0.label.starts_with("test "));
                assert!(*end_time >= m0.start.unwrap());
            }
            _ => panic!("log: {:?}", log),
        }
    }

    #[test]
    fn with_same_start() {
        {
            let _profiler0 = start_objective!(profiler::APP_LIFETIME, "test0");
            let _profiler1 = objective_with_same_start!(_profiler0, "test1");
        }
        let log = profiler::take_log();
        use profiler::Event::*;
        match &log[..] {
            [Start(m0), Start(m1), End { id: id1, .. }, End { id: id0, .. }] => {
                // _profiler0 has a start time
                assert!(m0.start.is_some());
                // _profiler1 is with_same_start, indicated by None in the log
                assert_eq!(m1.start, None);
                assert_eq!(id1.0, 1);
                assert_eq!(id0.0, 0);
            }
            _ => panic!("log: {:?}", log),
        }
    }

    #[test]
    fn profile() {
        #[profile(Objective)]
        fn profiled() {}
        profiled();
        let log = profiler::take_log();
        match &log[..] {
            [profiler::Event::Start(m0), profiler::Event::End { id: id0, .. }] => {
                assert!(m0.start.is_some());
                assert_eq!(m0.parent, profiler::IMPLICIT_ID);
                assert_eq!(id0.0, 0);
            }
            _ => panic!("log: {:?}", log),
        }
    }

    #[test]
    fn profile_async() {
        #[profile(Objective)]
        async fn profiled() -> u32 {
            let block = async { 4 };
            block.await
        }
        let future = profiled();
        futures::executor::block_on(future);
        let log = profiler::take_log();
        #[rustfmt::skip]
        match &log[..] {
            [
                profiler::Event::Start(_),
                // implicit await at start of function
                profiler::Event::Pause { id: pause0, .. },
                profiler::Event::Resume { id: resume0, .. },
                // block.await
                profiler::Event::Pause { id: pause1, .. },
                profiler::Event::Resume { id: resume1, .. },
                profiler::Event::End { id: end_id, .. },
            ] => {
                assert_eq!(pause0.0, 0);
                assert_eq!(resume0.0, 0);
                assert_eq!(pause1.0, 0);
                assert_eq!(resume1.0, 0);
                assert_eq!(end_id.0, 0);
            }
            _ => panic!("log: {:#?}", log),
        };
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
// Vec::push to grow the EVENTS log; it sometimes needs to reallocate. However even at its
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
    fn push_vec(count: usize, log: &mut Vec<profiler::Start>) {
        for _ in 0..count {
            log.push(profiler::Start {
                parent: profiler::APP_LIFETIME_ID,
                start:  None,
                label:  "",
            });
        }
        test::black_box(&log);
        log.clear();
    }

    #[bench]
    fn push_vec_1000(b: &mut test::Bencher) {
        let mut log = vec![];
        b.iter(|| push_vec(1000, &mut log));
    }

    #[bench]
    fn push_vec_10_000(b: &mut test::Bencher) {
        let mut log = vec![];
        b.iter(|| push_vec(10_000, &mut log));
    }
}

#[cfg(test)]
#[allow(unused)]
mod compile_tests {
    use crate as profiler;
    use profiler::profile;

    /// Decorating a pub fn.
    #[profile(Task)]
    pub fn profiled_pub() {}

    #[profile(Objective)]
    async fn profiled_async() {}

    #[profile(Detail)]
    #[allow(unsafe_code)]
    unsafe fn profiled_unsafe() {}

    fn mut_binding() {
        #[profile(Objective)]
        fn profiled(mut _x: u32) {
            _x = 4;
        }
    }

    #[profile(Task)]
    fn profiled_destructuring((_x, _y): (u32, u32)) {}

    fn root() {
        let _profiler = start_task!(profiler::APP_LIFETIME, "test");
    }
}
