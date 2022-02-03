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

/// Time elapsed since the time origin (https://www.w3.org/TR/hr-time-2/#sec-time-origin).
///
/// Stored in units of 100us, because that is maximum resolution of performance.now():
/// - in specification, 100us is the limit: https://www.w3.org/TR/hr-time-3
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
pub mod js {
    pub mod performance {
        use wasm_bindgen::prelude::*;

        #[wasm_bindgen]
        extern "C" {
            #[wasm_bindgen(js_namespace = performance)]
            pub fn now() -> f64;
        }
    }
}

// Mock implementation for testing.
#[cfg(not(target_arch = "wasm32"))]
pub mod js {
    pub mod performance {
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
    fn finish(&self, profiler: ProfilerId);
}

// === Implementation for disabled profilers ===

impl StartedProfiler for () {
    fn finish(&self, _: ProfilerId) {}
}



// ====================
// === ProfilerData ===
// ====================

/// Data used by a started Measurement for an enabled profile level.
#[derive(Debug, Copy, Clone)]
pub struct ProfilerData {
    pub parent: ProfilerId,
    pub start:  Option<Timestamp>,
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

/// Identifies a profiled section, the parent it was reached by, and its entry and exit times.
#[derive(Debug, Copy, Clone)]
pub struct Measurement {
    pub parent:   ProfilerId,
    pub profiler: ProfilerId,
    pub start:    Option<Timestamp>,
    pub end:      Timestamp,
    pub label:    Label,
}

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
        pub static MEASUREMENTS: LocalVecBuilder<Measurement> = LocalVecBuilder::new();
    }
}

/// Global log of [`Measurement`]s.
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
    type Started: StartedProfiler;
    fn finish(self, data: &Self::Started);
}



// ==============
// === Parent ===
// ==============

/// Any object representing a profiler that is a valid parent for a profiler of type T.
pub trait Parent<T: Profiler> {
    fn new_child(&self, label: Label) -> Started<T>;
    fn new_child_same_start(&self, label: Label) -> Started<T>;
}



// ===============
// === Started ===
// ===============

/// A profiler that has a start time set, and will complete its measurement when dropped.
#[derive(Debug)]
pub struct Started<T: Profiler> {
    profiler: T,
    data:     T::Started,
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
            // by absolute paths" (https://github.com/rust-lang/rust/issues/52234).
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
        fn profilee(_profiler: profiler::Objective) {}
        profilee(profiler::APP_LIFETIME);
        let measurements = profiler::take_log();
        assert_eq!(measurements.len(), 1);
    }

    #[test]
    fn profile_async() {
        #[profile]
        async fn profilee(_profiler: profiler::Objective) {}
        let _future = profilee(profiler::APP_LIFETIME);
    }

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
