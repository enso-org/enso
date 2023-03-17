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

use crate::display::world;
use crate::display::SymbolId;

use enso_types::unit2::Duration;
use enso_web::Performance;
use enso_web::TimeProvider;
use js_sys::ArrayBuffer;
use js_sys::WebAssembly::Memory;
use wasm_bindgen::JsCast;



// =============
// === Stats ===
// =============

/// Contains all the gathered stats, and provides methods for modifying and retrieving their
/// values. Uses the Web Performance API to access current time for calculating time-dependent
/// stats (e.g. FPS).
pub type Stats = StatsWithTimeProvider<Performance>;



// =============================
// === StatsWithTimeProvider ===
// =============================

/// Contains all the gathered stats, and provides methods for modifying and retrieving their
/// values.
/// Uses [`T`] to access current time for calculating time-dependent stats (e.g. FPS).
#[derive(Debug, Deref, CloneRef)]
pub struct StatsWithTimeProvider<T> {
    rc: Rc<RefCell<FramedStatsData<T>>>,
}

impl<T> Clone for StatsWithTimeProvider<T> {
    fn clone(&self) -> Self {
        Self { rc: self.rc.clone() }
    }
}

impl<T: TimeProvider> StatsWithTimeProvider<T> {
    /// Constructor.
    pub fn new(time_provider: T) -> Self {
        let framed_stats_data = FramedStatsData::new(time_provider);
        let rc = Rc::new(RefCell::new(framed_stats_data));
        Self { rc }
    }

    /// Calculate FPS for the last frame. This function should be called on the very beginning of
    /// every frame. Please note, that it does not clean the per-frame statistics. You want to run
    /// the [`reset_per_frame_statistics`] function before running rendering operations.
    pub fn calculate_prev_frame_fps(&self, time: Duration) {
        self.rc.borrow_mut().calculate_prev_frame_fps(time)
    }

    /// Clean the per-frame statistics, such as the per-frame number of draw calls. This function
    /// should be called before any rendering calls were made.
    pub fn reset_per_frame_statistics(&self) {
        self.rc.borrow_mut().reset_per_frame_statistics()
    }

    /// Ends tracking data for the current animation frame.
    /// Also, calculates the `frame_time` and `wasm_memory_usage` stats.
    pub fn end_frame(&self) {
        self.rc.borrow_mut().end_frame();
    }

    /// Register a new draw call for the given symbol.
    pub fn register_draw_call(&self, symbol_id: SymbolId) {
        let label = world::with_context(|ctx| {
            ctx.get_symbol(symbol_id).map(|t| t.label).unwrap_or("Unknown")
        });
        self.rc.borrow_mut().stats_data.register_draw_call(label);
    }
}



// =======================
// === FramedStatsData ===
// =======================

/// Internal representation of [`StatsWithTimeProvider`].
#[allow(missing_docs)]
#[derive(Debug)]
pub struct FramedStatsData<T> {
    time_provider:    T,
    pub stats_data:   StatsData,
    frame_begin_time: Option<f64>,
}

impl<T: TimeProvider> FramedStatsData<T> {
    /// Constructor.
    fn new(time_provider: T) -> Self {
        let stats_data = default();
        let frame_begin_time = None;
        Self { time_provider, stats_data, frame_begin_time }
    }

    /// Calculate FPS for the last frame. This function should be called on the very beginning of
    /// every frame. Please note, that it does not clean the per-frame statistics. You want to run
    /// the [`reset_per_frame_statistics`] function before running rendering operations.
    fn calculate_prev_frame_fps(&mut self, time: Duration) {
        let time = time.unchecked_raw() as f64;
        if let Some(previous_frame_begin_time) = self.frame_begin_time.replace(time) {
            self.stats_data.fps = 1000.0 / (time - previous_frame_begin_time);
        }
    }

    fn end_frame(&mut self) {
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

    /// Clean the per-frame statistics, such as the per-frame number of draw calls. This function
    /// should be called before any rendering calls were made.
    fn reset_per_frame_statistics(&mut self) {
        self.stats_data.draw_calls = default();
        self.stats_data.shader_compile_count = 0;
        self.stats_data.data_upload_count = 0;
        self.stats_data.data_upload_size = 0;
    }
}



// ========================
// === Macro gen_stats! ===
// ========================

/// Emits the 2nd argument only if the 1st argument is an integer type. A helper macro for
/// gen_stats!, supports only the types currently used with gen_stats!.
macro_rules! emit_if_integer {
    (u32, $($block:tt)*) => ($($block)*);
    (usize, $($block:tt)*) => ($($block)*);
    ($other:ty, $($block:tt)*) => ();
}

/// Emits the StatsData struct, and extends StatsWithTimeProvider with accessors to StatsData
/// fields.
macro_rules! gen_stats {
    ($($field:ident : $field_type:ty),* $(,)?) => { paste! {


        // === StatsData ===

        /// Raw data of all the gathered stats.
        #[derive(Debug, Default, Clone)]
        #[allow(missing_docs)]
        pub struct StatsData {
            $(pub $field : $field_type),*
        }


        // === StatsWithTimeProvider fields accessors ===

        impl<T: TimeProvider> StatsWithTimeProvider<T> { $(
            /// Field getter.
            pub fn $field(&self) -> $field_type {
                self.rc.borrow().stats_data.$field.clone()
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
                    self.[<mod _ $field>](|t| t.saturating_add(1));
                }

                /// Decrements field's value.
                pub fn [<dec _ $field>](&self) {
                    self.[<mod _ $field>](|t| t.saturating_sub(1));
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
    draw_calls           : Vec<&'static str>,
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
    /// Register a new draw call for the given symbol.
    pub fn register_draw_call(&mut self, symbol_name: &'static str) {
        self.draw_calls.push(symbol_name);
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
