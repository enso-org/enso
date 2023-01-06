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
//! fn root_objective() {
//!     let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "root_objective");
//!     // ...
//! }
//! ```
//!
//! ### Root profilers
//!
//! The profiler constructor macros require a parent. To create a *root profiler*, specify the
//! special value [`APP_LIFETIME`] as the parent.

// === Features ===
#![feature(test)]
#![feature(local_key_cell_methods)]
#![feature(maybe_uninit_uninit_array)]
#![feature(extend_one)]
#![feature(result_option_inspect)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]


// ==============
// === Export ===
// ==============

pub mod format;
pub mod internal;
pub mod log;



extern crate test;

use internal::*;



// ===============
// === prelude ===
// ===============

/// Widely-used exports.
pub mod prelude {
    pub use crate::internal::Profiler;
    pub use crate::profile;
}



// ========================
// === Logging Metadata ===
// ========================

/// Define a function that writes a specific type of metadata to the profiling log.
#[macro_export]
macro_rules! metadata_logger {
    ($name:expr, $fun:ident($ty:ty)) => {
        /// Write a metadata object to the profiling event log.
        pub fn $fun(data: $ty) {
            thread_local! {
                static LOGGER: $crate::internal::MetadataLogger<$ty> =
                    $crate::internal::MetadataLogger::new($name);
            }
            LOGGER.with(|logger| logger.log(data));
        }
    };
}



// ==============
// === Parent ===
// ==============

/// Any object representing a profiler that is a valid parent for a profiler of type T.
pub trait Parent<T: Profiler + Copy> {
    /// Start a new profiler, with `self` as its parent.
    fn start_child(&self, label: Label) -> Started<T>;
}



// ===============================================
// === Wrappers instrumenting async operations ===
// ===============================================

/// Await a future, logging appropriate await events for the given profiler.
#[macro_export]
macro_rules! await_ {
    ($evaluates_to_future:expr, $profiler:ident) => {{
        let future = $evaluates_to_future;
        profiler::internal::Profiler::pause($profiler.0);
        let result = future.await;
        profiler::internal::Profiler::resume($profiler.0);
        result
    }};
}

/// Await two futures concurrently, like [`futures::join`], but with more accurate profiling.
pub async fn join<T: futures::Future, U: futures::Future>(t: T, u: U) -> (T::Output, U::Output) {
    futures::join!(t, u)
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
///         profiler::internal::Profiler::resume(__profiler_scope.0);
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

enso_profiler_macros::define_profiling_levels![Objective, Task, Detail, Debug];


// === APP_LIFETIME ===

/// Pseudo-profiler serving as the root of the measurement hierarchy.
pub const APP_LIFETIME: Objective = Objective(EventId::APP_LIFETIME);



// ===================
// === EventStream ===
// ===================

/// An iterator over all logged events. This is a resumable iterator: After [`next()`] returns
/// [`None`], it can be iterated again to observe any additional events that have been logged since
/// the last it was used.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EventStream {
    next_i: usize,
}

impl Iterator for EventStream {
    type Item = Event;
    fn next(&mut self) -> Option<Self::Item> {
        let result = EVENTS.try_get(self.next_i, |e| *e);
        result.inspect(|_| self.next_i += 1);
        result
    }
}

/// Return a stream that yields all logged events.
pub fn event_stream() -> EventStream {
    EventStream::default()
}



// ======================
// === IntervalStream ===
// ======================

/// Return a stream that yields all logged events. This is a resumable iterator: After [`next()`]
/// returns [`None`], it can be iterated again to observe any additional events that have been
/// logged since the last it was used.
pub fn interval_stream() -> IntervalStream {
    IntervalStream {
        events: EventStream::default().enumerate(),
        resume: Default::default(),
        parent: Default::default(),
        stack:  Default::default(),
    }
}

/// A stream that yields all logged events.
#[derive(Debug, Clone)]
pub struct IntervalStream {
    events: std::iter::Enumerate<EventStream>,
    // [`Timestamp`]s of events that have been paused (or started paused) and resumed. If a running
    // event is not found in this collection, it has been running since its start event.
    resume: std::collections::BTreeMap<EventId, Timestamp>,
    parent: std::collections::BTreeMap<EventId, EventId>,
    stack:  Vec<EventId>,
}

impl IntervalStream {
    fn resolve_start_id(&self, id: EventId) -> usize {
        let non_empty_stack = "For an internal to end, the stack must be non-empty.";
        id.explicit().unwrap_or_else(|| *self.stack.last().expect(non_empty_stack)).0 as usize
    }

    fn record_parent(&mut self, id: EventId, parent: EventId) {
        let parent = parent
            .explicit()
            .unwrap_or_else(|| self.stack.last().copied().unwrap_or(EventId::APP_LIFETIME));
        self.parent.insert(id, parent);
    }

    fn resolve_start_time(&self, start: &Start) -> Timestamp {
        start.start.unwrap_or_else(|| self.resolve_parent_start(start.parent))
    }

    fn resolve_parent_start(&self, id: EventId) -> Timestamp {
        let parent_was_recorded =
            "If the event ID is implicit, we must have resolved its parent when we encountered it.";
        let id = id.explicit().unwrap_or_else(|| *self.parent.get(&id).expect(parent_was_recorded));
        EVENTS.get(id.0 as usize, |event| self.resolve_start_time(&event.as_start().unwrap()))
    }
}

impl Iterator for IntervalStream {
    type Item = Interval;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((i, event)) = self.events.next() {
            let id = EventId(i as u32);
            match event {
                Event::Start(start) => {
                    self.stack.push(id);
                    self.record_parent(id, start.parent);
                }
                Event::StartPaused(start) => self.record_parent(id, start.parent),
                Event::Resume { id, timestamp } => {
                    self.resume.insert(id, timestamp);
                    self.stack.push(id);
                }
                Event::End { id, timestamp } | Event::Pause { id, timestamp } => {
                    let start = self.resume.remove(&id).or_else(|| {
                        let id = self.resolve_start_id(id);
                        EVENTS.get(id, |e| match e {
                            Event::Start(start) => Some(self.resolve_start_time(start)),
                            Event::StartPaused(_) => None,
                            _ => unreachable!(),
                        })
                    });
                    if let Some(start) = start {
                        let label = EVENTS.get(id.0 as usize, |e| e.as_start().unwrap().label);
                        let end = timestamp;
                        return Some(Interval { label, start, end });
                    }
                }
                Event::Metadata { .. } => {}
            }
        }
        None
    }
}

/// Identifies a time period in which a particular profiler was running.
#[derive(Copy, Clone, Debug)]
pub struct Interval {
    label: Label,
    start: Timestamp,
    end:   Timestamp,
}

impl Interval {
    /// Return a string identifying profiler, usually by the location in the code that it was
    /// created.
    pub fn label(&self) -> &'static str {
        self.label.0
    }

    /// Return the time the profiler began running, in the same units as the [`DOMHighResTimeStamp`]
    /// web API.
    pub fn start(&self) -> f64 {
        self.start.into_ms()
    }

    /// Return the time the profiler finished running, in the same units as the
    /// [`DOMHighResTimeStamp`] web API.
    pub fn end(&self) -> f64 {
        self.end.into_ms()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod log_tests {
    use crate as profiler;
    use profiler::profile;

    fn get_log() -> Vec<profiler::internal::Event> {
        crate::internal::get_raw_log().events
    }

    #[test]
    fn root() {
        {
            // In any other crate we would refer to the macro as `profiler::start_objective!`, but
            // "macro-expanded `macro_export` macros from the current crate cannot be referred to
            // by absolute paths" (<https://github.com/rust-lang/rust/issues/52234>).
            let _profiler = start_objective!(profiler::APP_LIFETIME, "test");
        }
        let log = get_log();
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
    fn profile() {
        #[profile(Objective)]
        fn profiled() {}
        profiled();
        let log = get_log();
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
            block.await + 1
        }
        let future = profiled();
        futures::executor::block_on(future);
        let log = get_log();
        #[rustfmt::skip]
        match &log[..] {
            [
            // outer async fn: create, then start profiler
            profiler::Event::StartPaused(_),
            profiler::Event::Resume { id: profiler::internal::EventId(0), .. },
            // inner async block: create profiler
            profiler::Event::StartPaused(_),
            // block.await: pause the fn, start the block
            profiler::Event::Pause { id: profiler::internal::EventId(0), .. },
            profiler::Event::Resume { id: profiler::internal::EventId(2), .. },
            // block completes, resume fn, fn completes
            profiler::Event::End { id: profiler::internal::EventId(2), .. },
            profiler::Event::Resume { id: profiler::internal::EventId(0), .. },
            profiler::Event::End { id: profiler::internal::EventId(0), .. },
            ] => (),
            _ => panic!("log: {:#?}", log),
        };
    }

    #[test]
    fn non_move_async_block() {
        #[profile(Objective)]
        async fn profiled() {
            let mut i = 0;
            let block = async { i = 1 };
            block.await;
            assert_eq!(i, 1);
        }
        futures::executor::block_on(profiled());
        let _ = get_log();
    }

    #[test]
    fn inner_items() {
        // Ensure items inside profiled function are not transformed by the outer profiler.
        // This prevents profiled items inside profiled items from being doubly-profiled,
        // and allows deciding whether and how to profile inner items separately from outer.
        #[profile(Objective)]
        async fn outer() {
            async fn inner() {
                let block = async move {};
                block.await
            }
            inner().await
        }
        futures::executor::block_on(outer());
        let log = get_log();
        #[rustfmt::skip]
        match &log[..] {
            [
            profiler::Event::StartPaused(_),
            profiler::Event::Resume { id: profiler::internal::EventId( 0, ), .. },
            profiler::Event::Pause { id: profiler::internal::EventId( 0, ), .. },
            profiler::Event::Resume { id: profiler::internal::EventId( 0, ), .. },
            profiler::Event::End { id: profiler::internal::EventId( 0, ), .. },
            ] => (),
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
            let _profiler = start_objective!(profiler::APP_LIFETIME, "log_measurement");
        }
        let events: Vec<_> = crate::EVENTS.clone_all();
        test::black_box(events);
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
    fn push_vec(count: usize, log: &mut Vec<profiler::Event>) {
        for _ in 0..count {
            log.push(profiler::Event::Start(profiler::Start {
                parent: profiler::EventId::APP_LIFETIME,
                start:  None,
                label:  profiler::Label(""),
                level:  Default::default(),
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

    #[profile(Objective)]
    async fn polymorphic_return() -> Box<dyn PartialEq<u32>> {
        Box::new(23)
    }

    #[profile(Objective)]
    async fn input_impl_trait(foo: impl PartialEq<u32>, bar: &u32) -> bool {
        foo.eq(bar)
    }

    #[profile(Objective)]
    async fn input_t_trait<T: PartialEq<u32>>(foo: T, bar: &u32) -> bool {
        foo.eq(bar)
    }

    #[profile(Objective)]
    async fn borrows_input_references_in_output(x: &u32) -> &u32 {
        x
    }

    #[profile(Objective)]
    #[allow(clippy::needless_lifetimes)]
    async fn borrows_input_references_in_output_explicitly<'a>(x: &'a u32) -> &'a u32 {
        x
    }

    #[profile(Objective)]
    async fn borrows_input_doesnt_use(_: &u32) -> u32 {
        4
    }

    #[profile(Objective)]
    async fn borrows_input_uses(x: &u32) -> u32 {
        *x
    }

    #[profile(Objective)]
    async fn borrows_two_args(x: &u32, y: &u32) -> u32 {
        *x + *y
    }

    struct Foo(u32);
    impl Foo {
        #[profile(Objective)]
        async fn borrows_self_and_arg(&self, x: &u32) -> u32 {
            self.0 + *x
        }
    }

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
