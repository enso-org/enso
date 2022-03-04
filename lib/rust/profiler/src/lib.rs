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

pub mod internal;
pub mod log;

extern crate test;

use std::rc;
use std::str;

use internal::*;



// ======================
// === MetadataLogger ===
// ======================

/// An object that supports writing a specific type of metadata to the profiling log.
#[derive(Debug)]
pub struct MetadataLogger<T> {
    id:      u32,
    entries: rc::Rc<log::Log<T>>,
}

impl<T: 'static + serde::Serialize> MetadataLogger<T> {
    /// Create a MetadataLogger for logging a particular type.
    ///
    /// The name given here must match the name used for deserialization.
    pub fn new(name: &'static str) -> Self {
        let id = METADATA_LOGS.len() as u32;
        let entries = rc::Rc::new(log::Log::new());
        METADATA_LOGS.append(rc::Rc::new(MetadataLog::<T> { name, entries: entries.clone() }));
        Self { id, entries }
    }

    /// Write a metadata object to the profiling event log.
    ///
    /// Returns an identifier that can be used to create references between log entries.
    pub fn log(&self, t: T) -> EventId {
        self.entries.append(t);
        EventLog.metadata(self.id)
    }
}



// ==============
// === Parent ===
// ==============

/// Any object representing a profiler that is a valid parent for a profiler of type T.
pub trait Parent<T: Profiler + Copy> {
    /// Start a new profiler, with `self` as its parent.
    fn new_child(&self, label: StaticLabel) -> Started<T>;
    /// Create a new profiler, with `self` as its parent, and the same start time as `self`.
    fn new_child_same_start(&self, label: StaticLabel) -> Started<T>;
}



// ===============
// === await_! ===
// ===============

/// Await a future, logging appropriate await events for the given profiler.
#[macro_export]
macro_rules! await_ {
    ($evaluates_to_future:expr, $profiler:ident) => {{
        let future = $evaluates_to_future;
        profiler::internal::Profiler::pause(&$profiler);
        let result = future.await;
        profiler::internal::Profiler::resume(&$profiler);
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
///         use profiler::internal::Profiler;
///         let parent = profiler::internal::EventId::implicit();
///         let now = Some(profiler::internal::Timestamp::now());
///         let label = profiler::internal::Label("example (profiler/src/lib.rs:78)");
///         let profiler =
///             profiler::Detail::start(parent, label, now, profiler::internal::StartState::Active);
///         profiler::internal::Started(profiler)
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
///         use profiler::internal::Profiler;
///         let parent = profiler::internal::EventId::implicit();
///         let now = Some(profiler::internal::Timestamp::now());
///         let label = profiler::internal::Label("async_example (lib.rs:571)");
///         let profiler =
///             profiler::Task::start(parent, label, now, profiler::internal::StartState::Paused);
///         profiler::internal::Started(profiler)
///     };
///     async move {
///         profiler::internal::Profiler::resume(&__profiler_scope.0);
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


// === APP_LIFETIME ===

/// Pseudo-profiler serving as the root of the measurement hierarchy.
pub const APP_LIFETIME: Objective = Objective(EventId::APP_LIFETIME);



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use crate as profiler;
    use profiler::profile;

    /// Black-box metadata object, for ignoring metadata contents.
    #[derive(Debug, Copy, Clone, serde::Serialize, serde::Deserialize)]
    pub(crate) enum OpaqueMetadata {
        /// Anything.
        #[serde(other)]
        Unknown,
    }

    /// Take and parse the log (convenience function for tests).
    fn get_log<M: serde::de::DeserializeOwned>() -> Vec<profiler::Event<M, String>> {
        serde_json::from_str(&profiler::take_log()).unwrap()
    }

    #[test]
    fn root() {
        {
            // In any other crate we would refer to the macro as `profiler::start_objective!`, but
            // "macro-expanded `macro_export` macros from the current crate cannot be referred to
            // by absolute paths" (<https://github.com/rust-lang/rust/issues/52234>).
            let _profiler = start_objective!(profiler::APP_LIFETIME, "test");
        }
        let log = get_log::<OpaqueMetadata>();
        match &log[..] {
            [profiler::Event::Start(m0), profiler::Event::End { id, timestamp: end_time }] => {
                assert_eq!(m0.parent, profiler::APP_LIFETIME.0);
                assert_eq!(id.0, 0);
                assert!(m0.label.0.starts_with("test "));
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
        let log = get_log::<OpaqueMetadata>();
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
        let log = get_log::<OpaqueMetadata>();
        match &log[..] {
            [profiler::Event::Start(m0), profiler::Event::End { id: id0, .. }] => {
                assert!(m0.start.is_some());
                assert_eq!(m0.parent, profiler::EventId::IMPLICIT);
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
        let log = get_log::<OpaqueMetadata>();
        #[rustfmt::skip]
        match &log[..] {
            [
            // async fn starts paused
            profiler::Event::StartPaused(_),
            profiler::Event::Resume { id: resume0, .. },
            // block.await
            profiler::Event::Pause { id: pause1, .. },
            profiler::Event::Resume { id: resume1, .. },
            profiler::Event::End { id: end_id, .. },
            ] => {
                assert_eq!(resume0.0, 0);
                assert_eq!(pause1.0, 0);
                assert_eq!(resume1.0, 0);
                assert_eq!(end_id.0, 0);
            }
            _ => panic!("log: {:#?}", log),
        };
    }

    #[test]
    fn store_metadata() {
        // A metadata type.
        #[derive(serde::Serialize, serde::Deserialize)]
        struct MyData(u32);

        // Attach some metadata to a profiler.
        #[profile(Objective)]
        fn demo() {
            let meta_logger = profiler::MetadataLogger::new("MyData");
            meta_logger.log(&MyData(23));
        }

        // We can deserialize a metadata entry as an enum containing a newtype-variant for each
        // type of metadata we are able to interpret; the variant name is the string given to
        // MetadataLogger::register.
        //
        // We don't use an enum like this to *write* metadata because defining it requires
        // dependencies from all over the app, but when consuming metadata we need all the datatype
        // definitions anyway.
        #[derive(serde::Deserialize)]
        enum MyMetadata {
            MyData(MyData),
        }

        demo();
        let log = get_log::<MyMetadata>();
        match &log[..] {
            #[rustfmt::skip]
            &[
            profiler::Event::Start(_),
            profiler::Event::Metadata(
                profiler::Timestamped{ timestamp: _, data: MyMetadata::MyData (MyData(23)) }),
            profiler::Event::End { .. },
            ] => (),
            _ => panic!(),
        }
    }

    #[test]
    fn format_stability() {
        #[allow(unused)]
        fn static_assert_exhaustiveness<M, L>(e: profiler::Event<M, L>) -> profiler::Event<M, L> {
            // If you define a new Event variant, this will fail to compile to remind you to:
            // - Create a new test covering deserialization of the previous format, if necessary.
            // - Update `TEST_LOG` in this test to cover every variant of the new Event definition.
            match e {
                profiler::Event::Start(_) => e,
                profiler::Event::StartPaused(_) => e,
                profiler::Event::End { .. } => e,
                profiler::Event::Pause { .. } => e,
                profiler::Event::Resume { .. } => e,
                profiler::Event::Metadata(_) => e,
            }
        }
        const TEST_LOG: &str = "[\
            {\"Start\":{\"parent\":4294967294,\"start\":null,\"label\":\"dummy label (lib.rs:23)\"}},\
            {\"StartPaused\":{\"parent\":4294967294,\"start\":1,\"label\":\"dummy label2 (lib.rs:17)\"}},\
            {\"End\":{\"id\":1,\"timestamp\":1}},\
            {\"Pause\":{\"id\":1,\"timestamp\":1}},\
            {\"Resume\":{\"id\":1,\"timestamp\":1}},\
            {\"Metadata\":{\"timestamp\":1,\"data\":\"Unknown\"}}\
            ]";
        let events: Vec<profiler::Event<OpaqueMetadata, String>> =
            serde_json::from_str(TEST_LOG).unwrap();
        let reserialized = serde_json::to_string(&events).unwrap();
        assert_eq!(TEST_LOG, &reserialized[..]);
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
            let _profiler = start_objective!(profiler::APP_LIFETIME, "log_measurement");
        }
        test::black_box(crate::EVENTS.take_all());
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
    fn push_vec(
        count: usize,
        log: &mut Vec<profiler::Event<crate::tests::OpaqueMetadata, &'static str>>,
    ) {
        for _ in 0..count {
            log.push(profiler::Event::Start(profiler::Start {
                parent: profiler::EventId::APP_LIFETIME,
                start:  None,
                label:  profiler::Label(""),
            }));
            log.push(profiler::Event::End {
                id:        profiler::EventId::implicit(),
                timestamp: Default::default(),
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
