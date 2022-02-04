//! This module defines a structure gathering statistics of the running engine. The statistics are
//! an amazing tool for debugging what is really happening under the hood and understanding the
//! performance characteristics.

use enso_prelude::*;

use js_sys::ArrayBuffer;
use js_sys::WebAssembly::Memory;
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
    pub fn begin_frame(&self, time: f64) {
        self.rc.borrow_mut().begin_frame(time);
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

    /// Field getter. Returns the ordinal number of the current animation frame.
    pub fn frame_counter(&self) -> u64 {
        self.rc.borrow().frame_counter
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

        #[derive(Debug,Default,Clone,Copy)]
        struct StatsData {
            frame_begin_time: f64,
            frame_counter:    u64,
            $($field : $field_type),*
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
    fn begin_frame(&mut self, time: f64) {
        self.frame_counter += 1;

        // The check below ensures that on 1st ever frame, we don't try to calculate FPS of the
        // previous frame (which did not exist).
        //
        // BUG: this code carries over a known bug from earlier version of the code, making it not
        // calculate FPS correctly for any frame when the `time` parameter happens to be `0.0` (or
        // negative). This bug is planned to be fixed in a later PR as part of:
        // https://www.pivotaltracker.com/story/show/181140499
        if self.frame_begin_time > 0.0 {
            let end_time = time;
            self.fps = 1000.0 / (end_time - self.frame_begin_time);
        }
        self.frame_begin_time = time;
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
