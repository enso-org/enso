//! This module provides utilities for gathering runtime performance statistics of the GUI.
//!
//! The module provides a structure which defines the statistics we are interested in ([`Stats`]),
//! and contains methods for modifying as well as retrieving the current values of the statistics
//! (often also referred to with the shortcut term "stats"). It also provides methods that need
//! to be called to ensure that some of the statistics are properly calculated per each frame, and
//! helper utility types for accumulating and summarizing stats over multiple frames. The intention
//! behind this module is to aid in detecting and debugging possible performance issues in the GUI.
//!
//! Note: some statistics will not be collected (the fields will be present but always zero) when
//! this crate is compiled without the `statistics` feature flag. This is mediated by the
//! [`if_compiled_with_stats!`] macro. At the time of writing this doc, the affected stats are:
//!  - `gpu_memory_usage`
//!  - `data_upload_size`

use enso_prelude::*;
use enso_types::*;

use js_sys::ArrayBuffer;
use js_sys::WebAssembly::Memory;
use num_traits::cast;
use wasm_bindgen::JsCast;



// =============
// === Stats ===
// =============

/// Structure containing all the gathered stats.
#[derive(Debug, Clone, CloneRef)]
pub struct Stats {
    rc: Rc<RefCell<StatsData>>,
}

impl Default for Stats {
    fn default() -> Self {
        let rc = Rc::new(RefCell::new(default()));
        Self { rc }
    }
}

impl Stats {
    /// Starts tracking data for a new animation frame.
    /// Also, calculates the `fps` stat and updates `frame_counter`.
    /// Returns a snapshot of statistics data for the previous frame.
    /// Note: on first ever frame, there was no "previous frame", so all returned stats are zero
    /// (this special case can be recognized by checking `frame_counter == 0`).
    pub fn begin_frame(&self, time: f64) -> StatsData {
        self.rc.borrow_mut().begin_frame(time)
    }

    /// Ends tracking data for the current animation frame.
    /// Also, calculates the `frame_time` and `wasm_memory_usage` stats.
    pub fn end_frame(&self, time: f64) {
        self.rc.borrow_mut().end_frame(time);
    }

    /// Resets the per-frame statistics.
    pub fn reset_per_frame_statistics(&self) {
        self.rc.borrow_mut().reset_per_frame_statistics();
    }
}

/// Emits the 2nd argument only if the 1st argument is an integer type. A helper macro for
/// gen_stats!, supports only the types currently used with gen_stats!.
macro_rules! emit_if_integer {
    (u32, $($block:tt)*) => ($($block)*);
    (usize, $($block:tt)*) => ($($block)*);
    (f64, $($block:tt)*) => ();
}

macro_rules! gen_stats {
    ($($field:ident : $field_type:ty),* $(,)?) => { paste::item! {


        // === StatsData ===

        /// Raw data of all the gathered stats.
        #[derive(Debug,Default,Clone,Copy)]
        #[allow(missing_docs)]
        pub struct StatsData {
            frame_begin_time:  f64,
            pub frame_counter: u64,
            $(pub $field : $field_type),*
        }


        // === Stats fields accessors ===

        impl Stats { $(
            /// Field getter.
            pub fn $field(&self) -> $field_type {
                self.rc.borrow().$field
            }

            /// Field setter.
            pub fn [<set _ $field>](&self, value:$field_type) {
                self.rc.borrow_mut().$field = value;
            }

            /// Field modifier.
            pub fn [<mod _ $field>]<F:FnOnce($field_type)->$field_type>(&self, f:F) {
                let value = self.$field();
                let value = f(value);
                self.[<set _ $field>](value);
            }

            emit_if_integer!($field_type,
                /// Increments field's value.
                pub fn [<inc _ $field>](&self) {
                    self.[<mod _ $field>](|t| t + 1);
                }

                /// Decrements field's value.
                pub fn [<dec _ $field>](&self) {
                    self.[<mod _ $field>](|t| t - 1);
                }
            );

        )* }


        // === Accumulator ===

        /// Contains aggregated data from multiple [`StatsData`] objects. This is intended to be
        /// used as a mutable data structure, which can have new data continuously added. To
        /// calculate a summary of the data based on the aggregated samples, its [`summarize()`]
        /// method should be called.
        #[derive(Debug, Default)]
        pub struct Accumulator {
            /// How many samples of [`StatsData`] were accumulated.
            samples_count: u32,
            $($field : ValueAccumulator<$field_type>),*
        }

        impl Accumulator {
            /// Includes the data of the sample into the Accumulator.
            pub fn add_sample(&mut self, sample: &StatsData) {
                self.samples_count += 1;
                if self.samples_count == 1 {
                    $( self.$field = ValueAccumulator::new(sample.$field); )*
                } else {
                    $( self.$field.add_sample(sample.$field); )*
                }
            }

            /// Calculates a summary of data added into the Accumulator till now. Returns a
            /// non-empty result only if [`add_sample`] was called at least once.
            pub fn summarize(&self) -> Option<Summary> {
                if self.samples_count == 0 {
                    None
                } else {
                    let n = self.samples_count as f64;
                    let summary = Summary {
                        $($field : ValueSummary{
                            min: self.$field.min,
                            max: self.$field.max,
                            avg: self.$field.sum / n,
                        }),*
                    };
                    Some(summary)
                }
            }
        }


        // === Summary ===

        /// Contains summarized values of stats fields from multiple [`StatsData`] objects.
        #[derive(Copy, Clone, Debug)]
        pub struct Summary {
            $(
                #[allow(missing_docs)]
                pub $field : ValueSummary<$field_type>
            ),*
        }
    }};
}

gen_stats! {
    frame_time           : f64,
    fps                  : f64,
    wasm_memory_usage    : u32,
    gpu_memory_usage     : u32,
    draw_call_count      : usize,
    buffer_count         : usize,
    data_upload_count    : usize,
    data_upload_size     : u32,
    sprite_system_count  : usize,
    sprite_count         : usize,
    symbol_count         : usize,
    mesh_count           : usize,
    shader_count         : usize,
    shader_compile_count : usize,
}


// === StatsData methods ===

impl StatsData {
    fn begin_frame(&mut self, time: f64) -> StatsData {
        // See [Stats::begin_frame()] docs for explanation of this check.
        let previous_frame_snapshot = if self.frame_counter == 0 {
            default()
        } else {
            let end_time = time;
            self.fps = 1000.0 / (end_time - self.frame_begin_time);
            *self
        };
        self.frame_counter += 1;
        self.frame_begin_time = time;
        previous_frame_snapshot
    }

    fn end_frame(&mut self, time: f64) {
        self.frame_time = time - self.frame_begin_time;

        // TODO[MC,IB]: drop the `cfg!` (outlier in our codebase) once wasm_bindgen::memory()
        // doesn't panic in non-WASM builds (https://www.pivotaltracker.com/story/show/180978631)
        if cfg!(target_arch = "wasm32") {
            let memory: Memory = wasm_bindgen::memory().dyn_into().unwrap();
            let buffer: ArrayBuffer = memory.buffer().dyn_into().unwrap();
            self.wasm_memory_usage = buffer.byte_length();
        }
    }

    fn reset_per_frame_statistics(&mut self) {
        self.draw_call_count = 0;
        self.shader_compile_count = 0;
        self.data_upload_count = 0;
        self.data_upload_size = 0;
    }
}

/// Keeps the body if the `statistics` compilation flag was enabled.
#[macro_export]
macro_rules! if_compiled_with_stats {
    ($($tok:tt)*) => {
        #[cfg(feature = "statistics")]
        {$($tok)*}
        #[cfg(not(feature = "statistics"))]
        {}
    };
}



// ========================
// === ValueAccumulator ===
// ========================

#[derive(Debug, Default)]
struct ValueAccumulator<T> {
    min: T,
    max: T,
    sum: f64,
}

impl<T: Min + Max + PartialOrd + cast::AsPrimitive<f64> + Copy> ValueAccumulator<T> {
    fn new(v: T) -> Self {
        Self { min: v, max: v, sum: v.as_() }
    }

    fn add_sample(&mut self, v: T) {
        self.min = min(self.min, v);
        self.max = max(self.max, v);
        self.sum += v.as_();
    }
}



// ====================
// === ValueSummary ===
// ====================

/// Summary for multiple values of type T. Intended to be used for storing a summary of multiple
/// samples of some runtime stat.
#[derive(Copy, Clone, Debug)]
#[allow(missing_docs)]
pub struct ValueSummary<T> {
    pub min: T,
    pub max: T,
    pub avg: f64,
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use assert_approx_eq::assert_approx_eq;



    macro_rules! test_with_new_sample {
        (
            $accumulator:expr;
            $(
                $field:ident : $type:tt = $sample:literal
                    => min: $min:literal avg: $avg:literal max: $max:literal
            )*
        ) => {
            $(let $field: $type = $sample;)*
            let sample_stats = StatsData { $($field,)* ..default() };
            $accumulator.add_sample(&sample_stats);
            let tested_summary = $accumulator.summarize().unwrap();
            $(
                test_with_new_sample!($type, tested_summary.$field.min, $min);
                test_with_new_sample!(f64, tested_summary.$field.avg, $avg);
                test_with_new_sample!($type, tested_summary.$field.max, $max);
            )*
        };

        // Helper rules for asserting equality on various types
        (f64, $val1:expr, $val2:expr) => { assert_approx_eq!($val1, $val2); };
        (u32, $val1:expr, $val2:expr) => { assert_eq!($val1, $val2); };
        (usize, $val1:expr, $val2:expr) => { assert_eq!($val1, $val2); };
    }

    #[test]
    fn stats_summaries() {
        // This tests attempts to verify calculation of proper summaries for stats of each
        // primitive type supported by `gen_stats!`.

        let mut accumulator: Accumulator = default();
        assert!(matches!(accumulator.summarize(), None));

        test_with_new_sample!(accumulator;
            fps:               f64   = 55.0 => min: 55.0 avg: 55.0   max: 55.0
            wasm_memory_usage: u32   = 1000 => min: 1000 avg: 1000.0 max: 1000
            buffer_count:      usize = 3    => min: 3    avg: 3.0    max: 3
        );
        test_with_new_sample!(accumulator;
            fps:               f64   = 57.0 => min: 55.0 avg: 56.0   max: 57.0
            wasm_memory_usage: u32   = 2000 => min: 1000 avg: 1500.0 max: 2000
            buffer_count:      usize = 2    => min: 2    avg: 2.5    max: 3
        );
        test_with_new_sample!(accumulator;
            fps:               f64   = 56.0 => min: 55.0 avg: 56.0   max: 57.0
            wasm_memory_usage: u32   = 3000 => min: 1000 avg: 2000.0 max: 3000
            buffer_count:      usize = 1    => min: 1    avg: 2.0    max: 3
        );
    }
}
