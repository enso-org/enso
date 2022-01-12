//! This module defines a structure gathering statistics of the running engine. The statistics are
//! an amazing tool for debugging what is really happening under the hood and understanding the
//! performance characteristics.

use enso_prelude::*;
use enso_types::*;

use js_sys::ArrayBuffer;
use js_sys::WebAssembly::Memory;
use num_traits::cast;
use serde::Deserialize;
use serde::Serialize;
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
    pub fn begin_frame(&self, time: f64) {
        self.rc.borrow_mut().begin_frame(time);
    }

    /// Ends tracking data for the current animation frame.
    pub fn end_frame(&self, time: f64) {
        self.rc.borrow_mut().end_frame(time);
    }

    /// Resets the per-frame statistics.
    pub fn reset_per_frame_statistics(&self) {
        self.rc.borrow_mut().reset_per_frame_statistics();
    }

    /// Field getter. Returns the ordinal number of the current animation frame.
    pub fn frame_counter(&self) -> u64 {
        self.rc.borrow().frame_counter
    }

    /// Returns a read-only reference to the underlying raw data.
    pub fn data(&self) -> Ref<StatsData> {
        self.rc.borrow()
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

        /// Raw data of all the gathered stats.
        #[derive(Debug,Default,Clone,Copy)]
        #[allow(missing_docs)]
        pub struct StatsData {
            frame_begin_time: f64,
            frame_counter:    u64,
            $($field : $field_type),*
        }

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

        #[derive(Debug, Default)]
        #[allow(missing_docs)]
        pub struct Accumulator {
            /// How many samples were accumulated.
            samples_count: u32,
            $($field : ValueAccumulator<$field_type>),*
        }

        impl Accumulator {
            /// Includes the data of the sample into the Accumulator.
            pub fn push(&mut self, sample: &StatsData) {
                self.samples_count += 1;
                if self.samples_count == 1 {
                    $( self.$field = ValueAccumulator::new(sample.$field); )*
                } else {
                    $( self.$field.push(sample.$field); )*
                }
            }

            /// Calculates a summary of data pushed into the Accumulator till now. Returns a
            /// meaningful result only if `push` was called at least once.
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

        #[derive(Clone, Debug, Serialize, Deserialize)]
        #[allow(missing_docs)]
        #[serde(rename_all = "camelCase")]
        pub struct Summary {
            $(pub $field : ValueSummary<$field_type>),*
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

impl StatsData {
    fn begin_frame(&mut self, time: f64) {
        self.frame_counter += 1;

        if self.frame_begin_time > 0.0 {
            let end_time = time;
            self.fps = 1000.0 / (end_time - self.frame_begin_time);
        }
        self.frame_begin_time = time;
    }

    fn end_frame(&mut self, time: f64) {
        self.frame_time = time - self.frame_begin_time;

        let memory: Memory = wasm_bindgen::memory().dyn_into().unwrap();
        let buffer: ArrayBuffer = memory.buffer().dyn_into().unwrap();
        self.wasm_memory_usage = buffer.byte_length();
    }

    fn reset_per_frame_statistics(&mut self) {
        self.draw_call_count = 0;
        self.shader_compile_count = 0;
        self.data_upload_count = 0;
        self.data_upload_size = 0;
    }
}

#[derive(Debug, Default)]
struct ValueAccumulator<T> {
    pub min: T,
    pub max: T,
    pub sum: f64,
}

impl<T: Min + Max + PartialOrd + cast::AsPrimitive<f64> + Copy> ValueAccumulator<T> {
    fn new(v: T) -> Self {
        Self { min: v, max: v, sum: v.as_() }
    }

    fn push(&mut self, v: T) {
        self.min = min(self.min, v);
        self.max = max(self.max, v);
        self.sum += v.as_();
    }
}

/// Summarized data for a single metric.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ValueSummary<T> {
    /// Smallest observed value of the metric.
    pub min: T,
    /// Largest observed value of the metric.
    pub max: T,
    /// Average of the observed values of the metric.
    pub avg: f64,
}
