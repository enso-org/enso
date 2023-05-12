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

use crate::prelude::*;
use enso_web::traits::*;

use crate::display::world;
use crate::display::SymbolId;

use enso_types::unit2::Duration;
use enso_web::Performance;
use js_sys::ArrayBuffer;
use js_sys::WebAssembly::Memory;
use wasm_bindgen::JsCast;



// =============
// === Stats ===
// =============

/// Contains all the gathered stats, and provides methods for modifying and retrieving their
/// values.
#[derive(Clone, CloneRef, Debug, Deref, Default)]
pub struct Stats {
    rc: Rc<RefCell<StatsInternal>>,
}

impl Stats {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Calculate FPS for the last frame. This function should be called on the very beginning of
    /// every frame. Please note, that it does not clean the per-frame statistics. You want to run
    /// the [`reset_per_frame_statistics`] function before running rendering operations.
    pub fn calculate_prev_frame_stats(&self, time: Duration) {
        self.rc.borrow_mut().calculate_prev_frame_stats(time)
    }

    /// Clean the per-frame statistics, such as the per-frame number of draw calls. This function
    /// should be called before any rendering calls were made.
    pub fn reset_per_frame_statistics(&self) {
        self.rc.borrow_mut().reset_per_frame_statistics()
    }

    /// Register a new draw call for the given symbol.
    pub fn register_draw_call(&self, symbol_id: SymbolId) {
        let label = world::with_context(|ctx| {
            ctx.get_symbol(symbol_id).map(|t| t.label).unwrap_or("Unknown")
        });
        self.rc.borrow_mut().stats_data.register_draw_call(label);
    }
}



// =====================
// === StatsInternal ===
// =====================

/// Internal representation of [`Stats`].
#[allow(missing_docs)]
#[derive(Debug)]
pub struct StatsInternal {
    time_provider:     Performance,
    pub stats_data:    StatsData,
    frame_start:       Option<f64>,
    wasm_memory_usage: u32,
}

impl StatsInternal {
    /// Constructor.
    fn new() -> Self {
        let time_provider = enso_web::window.performance_or_panic();
        let stats_data = default();
        let frame_start = None;
        let wasm_memory_usage = default();
        Self { time_provider, stats_data, frame_start, wasm_memory_usage }
    }

    /// Calculate FPS for the last frame. This function should be called on the very beginning of
    /// every frame. Please note, that it does not clean the per-frame statistics. You want to run
    /// the [`reset_per_frame_statistics`] function before running rendering operations.
    fn calculate_prev_frame_stats(&mut self, frame_start: Duration) {
        let frame_start = frame_start.unchecked_raw() as f64;
        if let Some(prev_frame_start) = self.frame_start.replace(frame_start) {
            let prev_frame_time = frame_start - prev_frame_start;
            self.stats_data.frame_time = prev_frame_time;
            self.stats_data.fps = 1000.0 / prev_frame_time;
        }
        if cfg!(target_arch = "wasm32") {
            let memory: Memory = wasm_bindgen::memory().dyn_into().unwrap();
            let buffer: ArrayBuffer = memory.buffer().dyn_into().unwrap();
            let prev_frame_wasm_memory_usage = self.wasm_memory_usage;
            self.wasm_memory_usage = buffer.byte_length();
            self.stats_data.wasm_memory_usage = prev_frame_wasm_memory_usage;
        }
    }

    /// Clean the per-frame statistics, such as the per-frame number of draw calls. This function
    /// should be called before any rendering calls were made.
    fn reset_per_frame_statistics(&mut self) {
        self.stats_data.draw_calls.clear();
        self.stats_data.shader_compile_count = 0;
        self.stats_data.data_upload_count = 0;
        self.stats_data.data_upload_size = 0;
        self.stats_data.cpu_and_idle_time = None;
        self.stats_data.gpu_time = None;
    }
}

impl Default for StatsInternal {
    fn default() -> Self {
        Self::new()
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

/// Emits the [`StatsData`] struct, and extends [`Stats`] with accessors to [`StatsData`].
macro_rules! gen_stats {
    ($($field:ident : $field_type:ty),* $(,)?) => { paste! {


        // === StatsData ===

        /// Raw data of all the gathered stats.
        #[derive(Debug, Default, Clone)]
        #[allow(missing_docs)]
        pub struct StatsData {
            $(pub $field : $field_type),*
        }


        // === Stats fields accessors ===

        impl Stats { $(
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
    fps                  : f64,
    frame_time           : f64,
    // To learn more why we are not computing CPU-time only, please refer to the docs of
    // [`crate::core::animation::loops::LoopRegistry`].
    cpu_and_idle_time    : Option<f64>,
    gpu_time             : Option<f64>,
    idle_time            : f64,
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
