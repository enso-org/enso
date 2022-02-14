//! This module provides utilities for gathering runtime performance statistics of the GUI.
//!
//! The module provides a structure which defines the statistics we are interested in ([`Stats`]),
//! and contains methods for modifying as well as retrieving the current values of the statistics
//! (often also referred to with the shortcut term "stats"). It also provides methods that need
//! to be called to ensure that some of the statistics are properly calculated per each frame. The
//! intention behind this module is to aid in detecting and debugging possible performance issues
//! in the GUI.
//!
//! Note: some statistics will not be collected (the fields will be present but always zero) when
//! this crate is compiled without the `statistics` feature flag. This is mediated by the
//! [`if_compiled_with_stats!`] macro. At the time of writing this doc, the affected stats are:
//!  - `gpu_memory_usage`
//!  - `data_upload_size`

use enso_prelude::*;

use enso_web::Performance;
use js_sys::ArrayBuffer;
use js_sys::WebAssembly::Memory;
use wasm_bindgen::JsCast;



// ====================
// === TimeProvider ===
// ====================

pub trait TimeProvider {
    fn now(&self) -> f64;
}

impl TimeProvider for Performance {
    fn now(&self) -> f64 { self.now() }
}



// =============
// === Stats ===
// =============

pub type Stats = FrameStats<Performance>;


// == FrameStats ==

#[derive(Debug, Clone, CloneRef)]
pub struct FrameStats<T: TimeProvider + Clone> {
    rc: Rc<RefCell<StatsCollector<T>>>,
}

impl<T: TimeProvider + Clone> FrameStats<T> {
    /// Constructor.
    pub fn new(time_provider: T) -> Self {
        let stats_collector = StatsCollector::new(time_provider);
        let rc = Rc::new(RefCell::new(stats_collector));
        Self { rc }
    }

    /// Starts tracking data for a new animation frame.
    /// Also, calculates the [`fps`] stat.
    /// Returns a snapshot of statistics data for the previous frame.
    ///
    /// Note: on first ever frame, there was no "previous frame", so all returned stats are zero
    /// (this special case can be recognized by checking that [`StatsData::initialized`] is
    /// `false`).
    ///
    /// Note: the code works under an assumption that [`begin_frame()`] and [`end_frame()`] are
    /// called properly on every frame (behavior in case of missed frames or missed calls is not
    /// specified).
    pub fn begin_frame(&self) -> StatsData {
        self.rc.borrow_mut().begin_frame()
    }

    /// Ends tracking data for the current animation frame.
    /// Also, calculates the `frame_time` and `wasm_memory_usage` stats.
    pub fn end_frame(&self) {
        self.rc.borrow_mut().end_frame();
    }
}



// ======================
// === StatsCollector ===
// ======================

#[derive(Debug, Clone)]
pub struct StatsCollector<T: TimeProvider + Clone> {
    time_provider: T,
    stats_data: StatsData,
    frame_begin_time: Option<f64>,
}

impl<T: TimeProvider + Clone> StatsCollector<T> {
    /// Constructor.
    pub fn new(time_provider: T) -> Self {
        let stats_data = default();
        let had_previous_frame = false;
        let frame_begin_time = None;
        Self { time_provider, stats_data, frame_begin_time }
    }

    /// Starts tracking data for a new animation frame.
    /// Also, calculates the [`fps`] stat.
    /// Returns a snapshot of statistics data for the previous frame.
    ///
    /// Note: on first ever frame, there was no "previous frame", so all returned stats are zero
    /// (this special case can be recognized by checking that [`StatsData::initialized`] is
    /// `false`).
    ///
    /// Note: the code works under an assumption that [`begin_frame()`] and [`end_frame()`] are
    /// called properly on every frame (behavior in case of missed frames or missed calls is not
    /// specified).
    pub fn begin_frame(&mut self) -> StatsData {
        let time = self.time_provider.now();
        let new_frame = StatsData {
            draw_call_count: 0,
            shader_compile_count: 0,
            data_upload_count: 0,
            data_upload_size: 0,
            ..self.stats_data
        };
        let mut previous_frame = mem::replace(&mut self.stats_data, new_frame);
        match self.frame_begin_time.replace(time) {
            Some(previous_frame_begin_time) => {
                let end_time = time;
                previous_frame.fps = 1000.0 / (end_time - previous_frame_begin_time);
            },
            None => (),
        }
        previous_frame
    }

    /// Ends tracking data for the current animation frame.
    /// Also, calculates the `frame_time` and `wasm_memory_usage` stats.
    pub fn end_frame(&mut self) {
        if let Some(begin_time) = self.frame_begin_time {
            let end_time = self.time_provider.now();
            self.stats_data.frame_time = end_time - begin_time;
        }

        // TODO[MC,IB]: drop the `cfg!` (outlier in our codebase) once wasm_bindgen::memory()
        // doesn't panic in non-WASM builds (https://www.pivotaltracker.com/story/show/180978631)
        if cfg!(target_arch = "wasm32") {
            let memory: Memory = wasm_bindgen::memory().dyn_into().unwrap();
            let buffer: ArrayBuffer = memory.buffer().dyn_into().unwrap();
            self.stats_data.wasm_memory_usage = buffer.byte_length();
        }
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
            pub initialized:  bool,
            $(pub $field : $field_type),*
        }


        // === Stats fields accessors ===

        impl<T: TimeProvider + Clone> FrameStats<T> { $(
            /// Field getter.
            pub fn $field(&self) -> $field_type {
                self.rc.borrow().stats_data.$field
            }

            /// Field setter.
            pub fn [<set _ $field>](&self, value:$field_type) {
                self.rc.borrow_mut().stats_data.$field = value;
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
