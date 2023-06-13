//! Metrics for the FRP engine, providing insights into the FRP engine, such as the number of FRP
//! networks or the count of FRP nodes. Only available when the `metrics` feature is enabled.

// === Non-Standard Linter Configuration ===
#![allow(dead_code)]

use crate::prelude::*;



// ===============
// === Metrics ===
// ===============

macro_rules! def_metrics {
    (
        $(#$meta:tt)*
        pub struct $name:ident {
            $($field:ident),*
        }
    ) => { paste! {
        $(#$meta)*
        #[cfg(not(feature = "metrics"))]
        #[derive(Clone, Copy, Default, Debug)]
        pub struct $name;

        $(#$meta)*
        #[cfg(feature = "metrics")]
        #[derive(Default, Clone)]
        #[allow(missing_docs)]
        pub struct $name {
            $(pub $field: Cell<u64>,)*
            $(pub [<$field _ever>]: Cell<u64>,)*
        }

        #[cfg(feature = "metrics")]
        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($name))
                    $(.field(stringify!($field), &self.$field.get()))*
                    .finish()
            }
        }

        #[cfg(feature = "metrics")]
        #[allow(missing_docs)]
        impl $name {$(
            #[inline(always)]
            pub fn [<inc_ $field>](&self) {
                self.$field.set(self.$field.get().wrapping_add(1));
                self.[<$field _ever>].set(self.[<$field _ever>].get().wrapping_add(1));
            }

            #[inline(always)]
            pub fn [<dec_ $field>](&self) {
                self.$field.set(self.$field.get().wrapping_sub(1));
            }
        )*}

        #[cfg(not(feature = "metrics"))]
        #[allow(missing_docs)]
        impl $name {$(
            #[inline(always)]
            pub fn [<inc_ $field>](&self) {}
            #[inline(always)]
            pub fn [<dec_ $field>](&self) {}
        )*}
    }};
}

def_metrics! {
    /// Metrics for the FRP engine. Only available when the `metrics` feature is enabled.
    ///
    /// Each metric is measured both as the current value and as a cumulative value since the start
    /// of the application. For example, the field [`Self::networks`] measures the number of
    /// currently living FRP networks, while the field [`Self::networks_ever`] measures the total
    /// number of FRP networks that have ever been created.
    ///
    /// For each metric, there are two methods defined: [`Self::inc_*`] and [`Self::dec_*`], which
    /// increase and decrease the metric by one, respectively.
    pub struct Metrics {
        networks, nodes
    }
}
