//! This module defines a structure gathering statistics of the running engine. The statistics are
//! an amazing tool for debugging what is really happening under the hood and understanding the
//! performance characteristics.

use enso_prelude::*;

use js_sys::ArrayBuffer;
use js_sys::WebAssembly::Memory;
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
    pub fn begin(&self, time: f64) {
        self.rc.borrow_mut().begin(time);
    }

    pub fn end(&self, time: f64) {
        self.rc.borrow_mut().end(time);
    }

    /// Resets the per-frame statistics.
    pub fn reset_per_frame_statistics(&self) {
        self.rc.borrow_mut().reset_per_frame_statistics();
    }
}

macro_rules! gen_stats {
    ($($field:ident : $field_type:ty),* $(,)?) => { paste::item! {

        #[derive(Debug,Default,Clone,Copy)]
        struct StatsData {
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

            /// Increments field's value.
            pub fn [<inc _ $field>](&self) {
                #[allow(trivial_numeric_casts)]
                self.[<mod _ $field>](|t| t + (1 as $field_type));
            }

            /// Decrements field's value.
            pub fn [<dec _ $field>](&self) {
                #[allow(trivial_numeric_casts)]
                self.[<mod _ $field>](|t| t - (1 as $field_type));
            }

        )* }

        #[derive(Debug,Default)]
        struct StatsAccumulator {
            /// How many samples were accumulated.
            samples_count: u32,

            $($field : Accumulator<$field_type>),*
        }

        impl StatsAccumulator {
            fn push(&mut self, sample: &StatsData) {
                self.samples_count += 1;
                if self.samples_count == 1 {
                    $( self.$field = Accumulator::new(sample.$field); )*
                } else {
                    $( self.$field.push(sample.$field); )*
                }
            }
        }

        #[derive(Clone, Debug, Serialize, Deserialize)]
        #[serde(rename_all = "camelCase")]
        pub struct StatsSummary {
            pub $($field : Summary<$field_type>),*
        }

        impl TryFrom<StatsAccumulator> for StatsSummary {
            type Error = NoSamplesError;
            fn try_from(acc: StatsAccumulator) -> Result<Self, NoSamplesError> {
                if acc.samples_count == 0 {
                    Err(NoSamplesError{})
                } else {
                    let n = acc.samples_count as f64;
                    Ok(StatsSummary {
                        $($field : Summary{
                            min: acc.$field.min,
                            max: acc.$field.max,
                            avg: acc.$field.sum / n,
                        }),*
                    })
                }
            }
        }

    }};
}

gen_stats! {
    begin_time           : f64,

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
    pub fn begin(&mut self, time: f64) {
        if self.begin_time > 0.0 {
            let end_time = time;
            self.fps = 1000.0 / (end_time - self.begin_time);
        }
        self.begin_time = time;
    }

    pub fn end(&mut self, time: f64) {
        self.frame_time = time - self.begin_time;

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
struct Accumulator<T> {
    pub min: T,
    pub max: T,
    pub sum: f64,
}

impl<T: MinMax + Clone> Accumulator<T> {
    fn new(v: T) -> Self {
        #![allow(trivial_numeric_casts)]
        Self { min: v.clone(), max: v.clone(), sum: v.to_f64() }
    }

    fn push(&mut self, v: T) {
        self.min = self.min.min(v.clone());
        self.max = self.min.max(v.clone());
        self.sum += v.to_f64();
    }
}

#[derive(Clone, Debug)]
pub struct NoSamplesError {}

/// Summarized data for a single metric.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Summary<T> {
    pub min: T,
    pub max: T,
    pub avg: f64,
}

trait MinMax {
    fn min(&self, other: Self) -> Self;
    fn max(&self, other: Self) -> Self;
    fn to_f64(&self) -> f64;
}

impl MinMax for f64 {
    fn min(&self, other: f64) -> f64 { f64::min(*self, other) }
    fn max(&self, other: f64) -> f64 { f64::max(*self, other) }
    fn to_f64(&self) -> f64 { *self }
}

impl MinMax for u32 {
    fn min(&self, other: Self) -> Self { std::cmp::min(*self, other) }
    fn max(&self, other: Self) -> Self { std::cmp::max(*self, other) }
    fn to_f64(&self) -> f64 { *self as f64 }
}

impl MinMax for usize {
    fn min(&self, other: Self) -> Self { std::cmp::min(*self, other) }
    fn max(&self, other: Self) -> Self { std::cmp::max(*self, other) }
    fn to_f64(&self) -> f64 { *self as f64 }
}
