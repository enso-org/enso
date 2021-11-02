//! This module defines a structure gathering statistics of the running engine. The statistics are
//! an amazing tool for debugging what is really happening under the hood and understanding the
//! performance characteristics.

use crate::prelude::*;



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
                self.[<mod _ $field>](|t| t+1);
            }

            /// Decrements field's value.
            pub fn [<dec _ $field>](&self) {
                self.[<mod _ $field>](|t| t-1);
            }

        )* }
    }};
}

gen_stats! {
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
